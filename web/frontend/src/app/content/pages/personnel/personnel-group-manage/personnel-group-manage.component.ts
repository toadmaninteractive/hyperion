import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, combineLatest, Observable, Subject, Subscription } from 'rxjs';
import { debounceTime, distinctUntilChanged, filter, finalize, map, switchMap, takeUntil, tap } from 'rxjs/operators';
import { PageEvent } from '@angular/material/paginator';
import { Sort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { AlertMessage } from '../../../../shared/interfaces/alert-message';
import { Breadcrumb } from '../../../../shared/interfaces/breadcrumb';
import { SubheaderService } from '../../../../core/services/metronic/layout/subheader.service';
import { ClipboardService } from '../../../../core/services/clipboard.service';
import { NotificationService } from '../../../../core/services/notification.service';
import { HyperionPersonnelService } from '../../../../protocol/personnel-protocol.service';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as PersonnelProtocol from '../../../../protocol/personnel-protocol.data';

interface QueryArguments {
    groupId: number;
    needle: string;
    orderBy: PersonnelProtocol.PersonnelGroupRoleOrderBy;
    orderDir: DataProtocol.OrderDirection;
    limit: number;
    offset: number;
}

enum Column {
    ProjectId = 'project_id',
    ProjectTitle = 'project_title',
    GroupRole = 'group_role',
}

const DEFAULT_ORDER_BY = Column.ProjectId,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.ProjectId, Column.ProjectTitle, Column.GroupRole];

@Component({
    selector: 'm-personnel-group-manage',
    templateUrl: './personnel-group-manage.component.html',
    styleUrls: ['./personnel-group-manage.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PersonnelGroupManageComponent implements OnDestroy {
    public config: any;
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    group$ = new BehaviorSubject<DbProtocol.PersonnelGroup>(null);
    filteredMembers$ = new BehaviorSubject<string[]>([]);
    alert$ = new BehaviorSubject<AlertMessage | null>(null);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    memberNeedle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort> { active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR });
    page$ = new BehaviorSubject<PageEvent>(<PageEvent> { pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE });
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<DbProtocol.PersonnelGroupRole>>(new MatTableDataSource<DbProtocol.PersonnelGroupRole>());
    subData: Subscription;
    displayedColumns = [...DEFAULT_COLUMNS];
    column = Column;
    accessRole = DbProtocol.AccessRole;
    pageSizes = [10, 25, 50, 100];
    args: QueryArguments;

    constructor(
        private activatedRoute: ActivatedRoute,
        private router: Router,
        private cdr: ChangeDetectorRef,
        private clipboardService: ClipboardService,
        private subheaderService: SubheaderService,
        private hyperionPersonnelService: HyperionPersonnelService,
        private notificationService: NotificationService,
    ) {
        combineLatest(
            this.activatedRoute.paramMap.pipe(
                map(params => params.get('name')),
                distinctUntilChanged(),
                switchMap(name => this.hyperionPersonnelService.getPersonnelGroupByName(name)),
                tap(group => this.onGroupChange(group)),
            ),
            this.reload$.asObservable(),
            this.needle$.asObservable().pipe(
                distinctUntilChanged(),
                debounceTime(450),
            ),
            this.sort$.asObservable(),
            this.page$.asObservable(),
        )
            .pipe(
                takeUntil(this.destroy$),
                map(([group, reload, needle, sort, page]) => <QueryArguments> {
                    groupId: group.id,
                    needle: needle,
                    orderBy: PersonnelProtocol.PersonnelGroupRoleOrderBy.fromJson(sort.active || DEFAULT_ORDER_BY),
                    orderDir: DataProtocol.OrderDirection.fromJson(sort.direction || DEFAULT_ORDER_DIR),
                    limit: page.pageSize,
                    offset: page.pageIndex * page.pageSize,
                }),
                filter(args => {
                    const shouldRestart = !(this.args
                        && this.args.groupId === args.groupId
                        && this.args.needle === args.needle
                        && this.args.orderBy === args.orderBy
                        && this.args.orderDir === args.orderDir
                        && this.args.limit === args.limit);

                    this.args = args;

                    if (shouldRestart) {
                        this.page$.next(<PageEvent> { pageSize: args.limit, pageIndex: 0 });
                    }

                    return !shouldRestart;
                }),
            )
            .subscribe(args => this.loadItems(args));

        combineLatest(
            this.group$.asObservable().pipe(
                filter(group => group instanceof DbProtocol.PersonnelGroup),
            ),
            this.memberNeedle$.asObservable().pipe(
                debounceTime(50)
            )
        )
        .pipe(takeUntil(this.destroy$))
        .subscribe(([group, needle]) => {
            const trimmedNeedle = (needle || '').trim().toLocaleLowerCase(),
                members = group.members
                    .filter(member => !trimmedNeedle || member.toLocaleLowerCase().indexOf(trimmedNeedle) !== -1)
                    .sort((a, b) => a < b ? -1 : 1);

            this.filteredMembers$.next(members);
        });
    }

    ngOnDestroy(): void {
        if (this.subData instanceof Subscription) {
            this.subData.unsubscribe();
            this.subData = null;
        }

        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
    }

    private getData(args: QueryArguments): Observable<DbProtocol.PersonnelGroupRole[]> {
        return this.hyperionPersonnelService
            .getPersonnelGroupRoles(args.groupId, args.needle, args.orderBy, args.orderDir, args.offset, args.limit)
            .pipe(
                takeUntil(this.destroy$),
                tap((response: DataProtocol.CollectionSlice<DbProtocol.PersonnelGroupRole>) => this.total$.next(response.total)),
                map((response: DataProtocol.CollectionSlice<DbProtocol.PersonnelGroupRole>) => response.items),
            );
    }

    loadItems(args: QueryArguments): void {
        // Show loading indicator
        this.loading$.next(true);

        if (this.subData instanceof Subscription) {
            this.subData.unsubscribe();
            this.subData = null;
        }

        // Load items
        this.subData = this.getData(args)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe(response => {
                const dataSource = new MatTableDataSource<DbProtocol.PersonnelGroupRole>();
                dataSource.data = response;
                this.dataSource$.next(dataSource);
            });
    }

    roleName(role: string | DbProtocol.AccessRole | any): string {
        const actualRole = typeof role === 'string' ? DbProtocol.AccessRole.fromJson(role) : role;
        return DbProtocol.AccessRole.getDescription(actualRole);
    }

    updatePageStatus(group: DbProtocol.PersonnelGroup): void {
        this.subheaderService.setTitle(group.name);

        this.subheaderService.setBreadcrumbs([
            <Breadcrumb>{ title: 'Personnel Groups', page: '/personnel/groups' },
            <Breadcrumb>{ title: group.name, page: `/personnel/groups/${group.name}` },
        ]);

        this.alert$.next(null);
        this.cdr.detectChanges();
    }

    onNeedleChange(needle: string): void {
        this.needle$.next((needle || '').trim());
    }

    onMemberNeedleChange(needle: string): void {
        this.memberNeedle$.next((needle || '').trim());
    }

    onSortChange(sort: Sort): void {
        this.sort$.next(sort);
    }

    onPageChange(page: PageEvent): void {
        this.page$.next(page);
    }

    onReload(): void {
        this.reload$.next(true);
    }

    onGroupChange(group: DbProtocol.PersonnelGroup): void {
        this.group$.next(group);
        this.updatePageStatus(group);
    }

    onRoleChange(obj: DbProtocol.PersonnelGroupRole, role: DbProtocol.AccessRole): void {
        if (obj.groupRole === role) {
            return;
        }

        this.setPersonnelGroupRole(obj.groupId, obj.projectId, role)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    onRoleReset(obj: DbProtocol.PersonnelGroupRole): void {
        if (!obj.groupRole) {
            return;
        }

        this.resetPersonnelGroupRole(obj.groupId, obj.projectId)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    private setPersonnelGroupRole(groupId: number, projectId: number, role: DbProtocol.AccessRole): Observable<DataProtocol.GenericResponse> {
        const request = new PersonnelProtocol.AccessRoleUpdateRequest();
        request.role = role;

        return this.hyperionPersonnelService
            .setPersonnelGroupRole(request, groupId, projectId)
            .pipe(takeUntil(this.destroy$));
    }

    private resetPersonnelGroupRole(groupId: number, projectId: number): Observable<DataProtocol.GenericResponse> {
        return this.hyperionPersonnelService
            .resetPersonnelGroupRole(groupId, projectId)
            .pipe(takeUntil(this.destroy$));
    }
}

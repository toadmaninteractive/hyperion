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
import * as DbProtocol from '../../../../protocol/db-protocol.data.js';
import * as PersonnelProtocol from '../../../../protocol/personnel-protocol.data';

interface QueryArguments {
    accountId: number;
    needle: string;
    orderBy: PersonnelProtocol.PersonnelAccountRoleOrderBy;
    orderDir: DataProtocol.OrderDirection;
    limit: number;
    offset: number;
}

interface EffectiveRole {
    role: DbProtocol.AccessRole | null;
    groupName: string | null;
}

enum Column {
    ProjectId = 'project_id',
    ProjectTitle = 'project_title',
    GroupRoles = 'group_roles',
    UserRole = 'user_role',
    EffectiveRole = 'effective_role',
}

const DEFAULT_ORDER_BY = Column.ProjectId,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.ProjectId, Column.ProjectTitle, Column.GroupRoles, Column.UserRole, Column.EffectiveRole];

@Component({
    selector: 'm-personnel-account-manage',
    templateUrl: './personnel-account-manage.component.html',
    styleUrls: ['./personnel-account-manage.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PersonnelAccountManageComponent implements OnDestroy {
    public config: any;
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    account$ = new BehaviorSubject<DbProtocol.PersonnelAccount>(null);
    alert$ = new BehaviorSubject<AlertMessage | null>(null);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort> { active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR });
    page$ = new BehaviorSubject<PageEvent>(<PageEvent> { pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE });
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<DbProtocol.PersonnelAccountRole>>(new MatTableDataSource<DbProtocol.PersonnelAccountRole>());
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
                map(params => params.get('username')),
                distinctUntilChanged(),
                switchMap(username => this.hyperionPersonnelService.getPersonnelAccountByUsername(username)),
                tap(account => this.onAccountChange(account)),
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
            map(([account, reload, needle, sort, page]) => <QueryArguments> {
                accountId: account.id,
                needle: needle,
                orderBy: PersonnelProtocol.PersonnelAccountRoleOrderBy.fromJson(sort.active || DEFAULT_ORDER_BY),
                orderDir: DataProtocol.OrderDirection.fromJson(sort.direction || DEFAULT_ORDER_DIR),
                limit: page.pageSize,
                offset: page.pageIndex * page.pageSize,
            }),
            filter(args => {
                const shouldRestart = !(this.args
                    && this.args.accountId === args.accountId
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

    private getData(args: QueryArguments): Observable<DbProtocol.PersonnelAccountRole[]> {
        return this.hyperionPersonnelService
            .getPersonnelAccountRoles(args.accountId, args.needle, args.orderBy, args.orderDir, args.offset, args.limit)
            .pipe(
                takeUntil(this.destroy$),
                tap((response: DataProtocol.CollectionSlice<DbProtocol.PersonnelAccountRole>) => this.total$.next(response.total)),
                map((response: DataProtocol.CollectionSlice<DbProtocol.PersonnelAccountRole>) => response.items),
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
                map(response => {
                    response = response.map(item => {
                        const effective = this.effectiveRole(item.userRole, item.groupRoles);
                        item['effectiveRole'] = effective.role;
                        item['effectiveGroup'] = effective.groupName;
                        return item;
                    });

                    return response;
                }),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe(response => {
                const dataSource = new MatTableDataSource<DbProtocol.PersonnelAccountRole>();
                dataSource.data = response;
                this.dataSource$.next(dataSource);
            });
    }

    roleName(role: string | DbProtocol.AccessRole | any): string {
        const actualRole = typeof role === 'string' ? DbProtocol.AccessRole.fromJson(role) : role;
        return DbProtocol.AccessRole.getDescription(actualRole);
    }

    private mostPrivilegedGroup(groupRoles: any): string | null {
        const roles = groupRoles || {},
            groups = Object.keys(roles);

        return groups.length > 0
            ? groups.sort((a, b) => DbProtocol.AccessRole.fromJson(roles[a].role) > DbProtocol.AccessRole.fromJson(roles[b].role) ? -1 : 1)[0]
            : null;
    }

    private effectiveRole(userRole: DbProtocol.AccessRole, groupRoles: any): EffectiveRole {
        const privilegedGroup = this.mostPrivilegedGroup(groupRoles),
            groupRole = privilegedGroup ? DbProtocol.AccessRole.fromJson(groupRoles[privilegedGroup].role) : null,
            groupOverridesUser = (groupRole || 0) > (userRole || 0),
            result = <EffectiveRole> { role: null, groupName: null };

        if (!userRole && !privilegedGroup) {
            return result;
        }

        result.role = groupOverridesUser ? groupRole : userRole;
        result.groupName = groupOverridesUser ? privilegedGroup : null;

        return result;
    }

    updatePageStatus(account: DbProtocol.PersonnelAccount): void {
        this.subheaderService.setTitle(account.name);

        this.subheaderService.setBreadcrumbs([
            <Breadcrumb>{ title: 'Personnel Accounts', page: '/personnel/accounts' },
            <Breadcrumb>{ title: account.name, page: `/personnel/accounts/${account.username}` },
        ]);

        this.alert$.next(null);
        this.cdr.detectChanges();
    }

    onNeedleChange(needle: string): void {
        this.needle$.next((needle || '').trim());
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

    onAccountChange(account: DbProtocol.PersonnelAccount): void {
        this.account$.next(account);
        this.updatePageStatus(account);
    }

    onRoleChange(obj: DbProtocol.PersonnelAccountRole, role: DbProtocol.AccessRole): void {
        if (obj.userRole === role) {
            return;
        }

        this.setPersonnelAccountRole(obj.personnelId, obj.projectId, role)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    onRoleReset(obj: DbProtocol.PersonnelAccountRole): void {
        if (!obj.userRole) {
            return;
        }

        this.resetPersonnelAccountRole(obj.personnelId, obj.projectId)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    private setPersonnelAccountRole(personnelId: number, projectId: number, role: DbProtocol.AccessRole): Observable<DataProtocol.GenericResponse> {
        const request = new PersonnelProtocol.AccessRoleUpdateRequest();
        request.role = role;

        return this.hyperionPersonnelService
            .setPersonnelAccountRole(request, personnelId, projectId)
            .pipe(takeUntil(this.destroy$));
    }

    private resetPersonnelAccountRole(personnelId: number, projectId: number): Observable<DataProtocol.GenericResponse> {
        return this.hyperionPersonnelService
            .resetPersonnelAccountRole(personnelId, projectId)
            .pipe(takeUntil(this.destroy$));
    }
}

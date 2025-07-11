import { ChangeDetectionStrategy, Component, OnInit, OnDestroy, ViewChild, ChangeDetectorRef, AfterViewInit } from '@angular/core';
import { DomSanitizer, SafeUrl } from '@angular/platform-browser';
import { MatDialog } from '@angular/material/dialog';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { BehaviorSubject, merge, Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { compare } from '../../../../shared/functions/compare';
import { AccountService } from '../../../../core/services/account.service';
import { NotificationService } from '../../../../core/services/notification.service';
import { ProjectService } from '../../../../core/services/project.service';
import { ProjectCreateDialogComponent } from '../project-create-dialog/project-create-dialog.component';
import { ProjectEditDialogComponent } from '../project-edit-dialog/project-edit-dialog.component';
import { HyperionProjectService } from '../../../../protocol/project-protocol.service';
import { WebsocketService } from '../../../../core/services/websocket.service';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';

enum Column {
    Id = 'id',
    Title = 'title',
    Key = 'key',
    SlackReceivers = 'slack_receiver',
    Owner = 'owner',
    JiraTitle = 'jira_title',
    JiraKey = 'jira_key',
    CreatedAt = 'created_at',
    UpdatedAt = 'updated_at',
    Actions = 'actions',
}

const DEFAULT_ORDER_BY = Column.Id,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.Id, Column.Title, Column.Key, Column.SlackReceivers, Column.JiraTitle, Column.JiraKey, Column.CreatedAt, Column.UpdatedAt, Column.Actions];

@Component({
    selector: 'm-projects-page',
    templateUrl: './projects-overview.component.html',
    styleUrls: ['./projects-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ProjectsOverviewComponent implements OnInit, OnDestroy, AfterViewInit {
    @ViewChild(MatPaginator, {static: true}) paginator: MatPaginator;
    @ViewChild(MatSort) sort: MatSort;
    destroy$ = new Subject<any>();
    ready$ = new BehaviorSubject<boolean>(false);
    projects: Array<DbProtocol.Project>;
    dataSource: MatTableDataSource<DbProtocol.Project>;
    displayedColumns = [...DEFAULT_COLUMNS];
    column = Column;
    sortDir = DEFAULT_ORDER_DIR;
    pageSize = 10;
    pageSizes = [10, 25, 50, 100];

    constructor(
        private cdr: ChangeDetectorRef,
        private sanitizer: DomSanitizer,
        private dialog: MatDialog,
        private hyperionProjectService: HyperionProjectService,
        private notificationService: NotificationService,
        private websocketService: WebsocketService,
        public accountService: AccountService,
        private projectService: ProjectService,
    ) {
    }

    ngOnInit(): void {

    }

    ngAfterViewInit() {
        this.projectService.projects$
            .asObservable()
            .pipe(
                takeUntil(this.destroy$),
                filter(projects => projects instanceof Array),
            )
            .subscribe(projects => this.initialize(projects));
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.ready$.complete();
    }

    private initialize(projects: Array<DbProtocol.Project>): void {
        this.projects = projects;
        this.dataSource = new MatTableDataSource(projects);
        this.dataSource.paginator = this.paginator;
        this.dataSource.sort = this.sort;
        this.ready$.next(true);
    }

    applyFilter(filterValue: string) {
        this.dataSource.filter = filterValue.trim().toLowerCase();

        if (this.dataSource.paginator) {
            this.dataSource.paginator.firstPage();
        }
    }

    jiraProjectUrl(jiraUrl: string, jiraKey: string): SafeUrl {
        return this.sanitizer.bypassSecurityTrustUrl(`${jiraUrl}/projects/${jiraKey}`);
    }

    refresh(): void {
        this.projectService.refresh().subscribe(_ => null);
    }

    showProjectCreateDialog(): void {
        const dialogRef = this.dialog.open(ProjectCreateDialogComponent, {width: '450px', data: null});

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.success(`Project ${result.title} added`);
                    this.refresh();
                }
            });
    }

    showProjectEditDialog(project: DbProtocol.Project): void {
        const dialogRef = this.dialog.open(ProjectEditDialogComponent, {width: '400px', data: project});

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.success(`Project ${result.title} updated`);
                    this.refresh();
                }
            });
    }

    sortTable(sort: MatSort): void {
        const data = this.dataSource.data.slice();

        if (!sort.active || sort.direction === '') {
            this.dataSource.data = data;
            return;
        }

        this.dataSource.data = data.sort((a, b) => {
            const isAsc = sort.direction === 'asc';
            return compare(a[sort.active], b[sort.active], isAsc);
        });

        setTimeout(() => this.cdr.detectChanges());
    }

    clickPageSize(event) {
        this.pageSize = event.pageSize;
        this.initialize(this.projectService.projects$.getValue());
    }

    deleteProject(project: DbProtocol.Project): void {
        this.hyperionProjectService
            .deleteProject(project.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.notificationService.success(`Project ${project.title} deleted`);
                    this.refresh();
                },
                error => {
                    if (error instanceof DataProtocol.NotFoundError) {
                        this.refresh();
                    }
                    this.notificationService.error(error);
                }
            );
    }
}

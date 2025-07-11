import { ChangeDetectionStrategy, Component, OnInit, OnDestroy, ViewChild, ChangeDetectorRef } from '@angular/core';
import { DomSanitizer } from '@angular/platform-browser';
import { MatDialog } from '@angular/material/dialog';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { BehaviorSubject, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { NotificationService } from '../../../../core/services/notification.service';
import { JiraCreateDialogComponent } from '../jira-create-dialog/jira-create-dialog.component';
import { JiraEditDialogComponent } from '../jira-edit-dialog/jira-edit-dialog.component';
import { HyperionJiraService } from '../../../../protocol/jira-protocol.service';
import * as CommonProtocol from '../../../../protocol/common-protocol.data';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';

enum Column {
    Id = 'id',
    Title = 'title',
    Url = 'url',
    CreatedAt = 'created_at',
    UpdatedAt = 'updated_at',
    Actions = 'actions',
}

const DEFAULT_ORDER_BY = Column.Id,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.Id, Column.Title, Column.Url, Column.CreatedAt, Column.UpdatedAt, Column.Actions];

@Component({
    selector: 'm-jira-overview-page',
    templateUrl: './jira-overview.component.html',
    styleUrls: ['./jira-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class JiraOverviewComponent implements OnInit, OnDestroy {
    @ViewChild(MatPaginator) paginator: MatPaginator;
    @ViewChild(MatSort) sort: MatSort;
    destroy$ = new Subject<any>();
    ready$ = new BehaviorSubject<boolean>(false);
    jiraInstances: DbProtocol.JiraInstance[];
    dataSource: MatTableDataSource<DbProtocol.JiraInstance>;
    displayedColumns = [...DEFAULT_COLUMNS];
    column = Column;
    sortDir = DEFAULT_ORDER_DIR;
    pageSize = 10;
    pageSizes = [10, 25, 50, 100];

    constructor (
        private cdr: ChangeDetectorRef,
        private sanitizer: DomSanitizer,
        private dialog: MatDialog,
        private hyperionJiraService: HyperionJiraService,
        private notificationService: NotificationService,
    ) { }

    ngOnInit(): void {
        this.refresh();
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.ready$.complete();
    }

    private initialize(jiraInstances: DbProtocol.JiraInstance[]): void {
        this.jiraInstances = jiraInstances;
        this.dataSource = new MatTableDataSource(jiraInstances);
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

    refresh(): void {
        this.hyperionJiraService.getJiraInstances()
            .pipe(takeUntil(this.destroy$))
            .subscribe(response => this.initialize(response.items));
    }

    showJiraCreateDialog(): void {
        const dialogRef = this.dialog.open(JiraCreateDialogComponent, { width: '450px', data: null });

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.success(`${result.title} added`);
                    this.refresh();
                }
            });
    }

    showJiraEditDialog(jiraInstance: DbProtocol.JiraInstance): void {
        const dialogRef = this.dialog.open(JiraEditDialogComponent, { width: '400px', data: jiraInstance });

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.success(`${result.title} updated`);
                    this.refresh();
                }
            });
    }

    deleteJiraInstance(jiraInstance: DbProtocol.JiraInstance): void {
        this.hyperionJiraService
            .deleteJiraInstance(jiraInstance.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.notificationService.success(`${jiraInstance.title} deleted`);
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

import { AfterViewInit, ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { DomSanitizer } from '@angular/platform-browser';
import { ActivatedRoute } from '@angular/router';
import { MatDialog } from '@angular/material/dialog';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort } from '@angular/material/sort';
import { merge, Observable, Subject } from 'rxjs';
import { filter, map, takeUntil, tap } from 'rxjs/operators';
import { ServerDataSource } from '../../../../shared/classes/server-data-source';
import { HyperionProjectService } from '../../../../protocol/project-protocol.service';
import { NotificationService } from '../../../../core/services/notification.service';
import { ProjectService } from '../../../../core/services/project.service';
import { WebsocketService } from '../../../../core/services/websocket.service';
import { HyperionTestCaseService } from '../../../../protocol/test-case-protocol.service';
import { HyperionTestRunService } from '../../../../protocol/test-run-protocol.service';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as NotificationProtocol from '../../../../protocol/notification-protocol.data';
import * as TestRunProtocol from '../../../../protocol/test-run-protocol.data';

@Component({
    selector: 'm-testing',
    templateUrl: './testing-overview.component.html',
    styleUrls: ['./testing-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TestingOverviewComponent implements OnInit, OnDestroy, AfterViewInit {
    @ViewChild(MatPaginator) paginator: MatPaginator;
    @ViewChild(MatSort) sort: MatSort;
    destroy$ = new Subject<any>();
    project: DbProtocol.Project;
    displayedColumns: string[] = ['id', 'status' , 'progress', 'createdAt', 'startedAt', 'finishedAt'];
    dataSource = new ServerDataSource<DbProtocol.TestRun>();
    defaultOrderBy = 'id';
    defaultOrderDir = 'desc';
    orderBy = TestRunProtocol.TestRunOrderBy.Id;
    orderDir = DataProtocol.OrderDirection.Desc;
    offset = 0;
    pageIndex = 0;
    pageSize = 10;
    pageSizes = [10, 25, 50, 100];
    total = 0;
    statusEnum = DbProtocol.TestRunStatus;
    currentStatus = DbProtocol.TestRunStatus.InProgress;

    constructor (
        private cdr: ChangeDetectorRef,
        private sanitizer: DomSanitizer,
        private dialog: MatDialog,
        private hyperionProjectService: HyperionProjectService,
        private hyperionTestRunsService: HyperionTestRunService,
        private hyperionTestCaseService: HyperionTestCaseService,
        private notificationService: NotificationService,
        private projectService: ProjectService,
        private websocketService: WebsocketService,
        private route: ActivatedRoute,
    ) { }

    ngOnInit(): void {
        this.route.data
            .pipe(takeUntil(this.destroy$))
            .subscribe(data => this.initialize(data.activeProject));

        merge(
            // this.websocketService.testRunCreated,
            this.websocketService.testRunUpdated,
            this.websocketService.testRunStarted,
            this.websocketService.testRunClosed,
            this.websocketService.testRunReopened,
            this.websocketService.testRunDeleted,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => this.project && this.project.id === event.data.projectId),
        ).subscribe(event => {
            let reload = false;

            if (event instanceof NotificationProtocol.TestRunStarted || event instanceof NotificationProtocol.TestRunReopened) {
                reload = this.canReloadOnStart();
            } else if (event instanceof NotificationProtocol.TestRunDeleted || event instanceof NotificationProtocol.TestRunClosed) {
                reload = this.dataSource.containsItem(this.equalityComparer, event.data);
            } else {
                this.dataSource.updateItem(this.equalityComparer, event.data);
            }

            if (reload) {
                this.loadItems(this.offset);
            }
        });
    }

    ngAfterViewInit(): void {
        merge(this.paginator.page, this.sort.sortChange)
            .pipe(
                takeUntil(this.destroy$),
                tap(($event: any) => {
                    if ($event.active && $event.direction) {
                        this.paginator.pageIndex = 0;
                    }
                    this.pageSize = this.paginator.pageSize;
                    this.dataSource.loadItems(() => this.getData(this.pageSize * this.paginator.pageIndex));
                })
            )
            .subscribe();
        this.loadItems();
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    private initialize(project: DbProtocol.Project): void {
        this.project = project;
        this.loadItems();
    }

    private getData(offset: number): Observable<DbProtocol.TestRun[]> {
        return this.hyperionTestRunsService
            .getTestRuns(this.project.id, this.orderBy, this.orderDir, offset, this.pageSize, this.currentStatus)
            .pipe(
                takeUntil(this.destroy$),
                tap(response => this.total = response.total),
                map(response => response.items),
            );
    }

    private equalityComparer(a: DbProtocol.TestRun, b: DbProtocol.TestRun): boolean {
        return (a instanceof DbProtocol.TestRun) && (b instanceof DbProtocol.TestRun) && a.id === b.id;
    }

    private canReloadOnStart(): boolean {
        const isSerialColumn = this.orderBy === TestRunProtocol.TestRunOrderBy.Id || this.orderBy === TestRunProtocol.TestRunOrderBy.CreatedAt;

        return this.total < this.pageSize
            || (isSerialColumn && this.orderDir === DataProtocol.OrderDirection.Desc);
    }

    loadItems(offset = 0): void {
        this.offset = offset;
        this.dataSource.loadItems(() => this.getData(offset));
    }

    badgeStatusClass(status: DbProtocol.TestRunStatus) {
        switch (status) {
            case DbProtocol.TestRunStatus.Draft: return 'm-timeline-3__item--metal';
            case DbProtocol.TestRunStatus.InProgress: return 'm-timeline-3__item--info';
            case DbProtocol.TestRunStatus.Closed: return 'm-timeline-3__item--warning';
        }
    }

    onStatusChange(status: DbProtocol.TestRunStatus): void {
        if (!status || this.currentStatus === status) return;
        this.currentStatus = status;
        this.offset = 0;
        if (this.paginator) {
            this.paginator.firstPage();
        }
        this.loadItems();
    }
}

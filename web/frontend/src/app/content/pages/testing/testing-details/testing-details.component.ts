import { ChangeDetectionStrategy, ChangeDetectorRef, Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { DomSanitizer } from '@angular/platform-browser';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { HttpEvent, HttpEventType, HttpUploadProgressEvent } from '@angular/common/http';
import { MatDialog } from '@angular/material/dialog';
import { BehaviorSubject, combineLatest, merge, Observable, Subject } from 'rxjs';
import { debounceTime, filter, finalize, first, takeUntil } from 'rxjs/operators';
import { Lightbox } from 'ngx-lightbox';
import { FileSystemDirectoryEntry, FileSystemFileEntry, NgxFileDropEntry } from 'ngx-file-drop';
import Swal from 'sweetalert2';
import { FilterStatus } from '../../../../shared/interfaces/filter-status';
import { IJiraAuthData, JiraAuthDialogComponent } from '../jira-auth-dialog/jira-auth-dialog.component';
import { IJiraIssueData, IJiraIssueResult, JiraIssueDialogComponent } from '../jira-issue-dialog/jira-issue-dialog.component';
import { NotificationService } from '../../../../core/services/notification.service';
import { SubheaderService } from '../../../../core/services/metronic/layout/subheader.service';
import { UploadService } from '../../../../core/services/upload.service';
import { WebsocketService } from '../../../../core/services/websocket.service';
import { HyperionAttachmentService } from '../../../../protocol/attachment-protocol.service';
import { HyperionJiraService } from '../../../../protocol/jira-protocol.service';
import { HyperionProjectService } from '../../../../protocol/project-protocol.service';
import { HyperionSetupService } from '../../../../protocol/setup-protocol.service';
import { HyperionTestCaseService } from '../../../../protocol/test-case-protocol.service';
import { HyperionTestRunService } from '../../../../protocol/test-run-protocol.service';
import * as CommonProtocol from '../../../../protocol/common-protocol.data';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as NotificationProtocol from '../../../../protocol/notification-protocol.data';
import * as TestRunProtocol from '../../../../protocol/test-run-protocol.data';

interface ITestMetadata {
    $param: string;
    $title: string;
    $parentTitle: string;
    $sortTitle: string;
}

type TestRunItemWithMetadata = DbProtocol.TestRunItem & ITestMetadata;

@Component({
    selector: 'm-testing-details',
    templateUrl: './testing-details.component.html',
    styleUrls: ['./testing-details.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TestingDetailsComponent implements OnInit, OnDestroy {
    @ViewChild('LoaderUploader') private loader: ElementRef;
    destroy$ = new Subject<any>();
    project: DbProtocol.Project;
    testRun: DbProtocol.TestRun;
    testRunStatus = DbProtocol.TestRunStatus;
    testStatusEnum = DbProtocol.TestStatus;
    checkedFilter$ = new BehaviorSubject<FilterStatus>(FilterStatus.Checked);
    testRunItems$ = new BehaviorSubject<TestRunItemWithMetadata[]>([]);
    filteredTestRunItems$ = new BehaviorSubject<TestRunItemWithMetadata[]>([]);
    showFilter$ = new BehaviorSubject(false);
    testStatusFilters$ = new BehaviorSubject<Set<DbProtocol.TestStatus>>(new Set<DbProtocol.TestStatus>([
        DbProtocol.TestStatus.Pending,
        DbProtocol.TestStatus.InProgress,
        DbProtocol.TestStatus.Passed,
        DbProtocol.TestStatus.Failed,
        DbProtocol.TestStatus.Blocked,
    ]));
    attachments = new Map<number, DbProtocol.FileAttachment[]>();
    attachmentsProgress = new Map<string, BehaviorSubject<number>>();
    setupMap = new Map<number, DbProtocol.SetupStep>();
    testCaseMap = new Map<number, DataProtocol.TreeNode<DbProtocol.TestCase>>();
    activeFileAttachments: DbProtocol.FileAttachment[];
    activeAlbum = [];
    files: NgxFileDropEntry[] = [];
    inheritedPreconditionTestCase?: DbProtocol.TestCase;
    initialized = false;
    filterStatusEnum = FilterStatus;
    selectedTestCase: DbProtocol.TestCase;
    selectedPristineTestItem: TestRunItemWithMetadata;
    selectedTestItem: TestRunItemWithMetadata;
    selectedTestItemSetup?: Array<DbProtocol.SetupStep>;
    selectedTestItemPrecondition?: Array<DbProtocol.SetupStep>;
    inheritedSetupTestCase?: DbProtocol.TestCase;
    showMoreAttachments = false;
    showSummaryEditor = false;
    typeFilterEnum = DbProtocol.TestStatus;
    activeQueryId: number;
    authenticatedInJira = false;
    search$ = new BehaviorSubject<string>('');
    checkedSet = new Set<number>();

    constructor(
        private cdr: ChangeDetectorRef,
        private dialog: MatDialog,
        private hyperionAttachmentService: HyperionAttachmentService,
        private hyperionJiraService: HyperionJiraService,
        private hyperionProjectService: HyperionProjectService,
        private hyperionTestRunsService: HyperionTestRunService,
        private hyperionTestCaseService: HyperionTestCaseService,
        private hyperionSetupService: HyperionSetupService,
        private notificationService: NotificationService,
        private route: ActivatedRoute,
        private router: Router,
        private sanitizer: DomSanitizer,
        private lightbox: Lightbox,
        private uploadService: UploadService,
        private subheaderService: SubheaderService,
        private websocketService: WebsocketService,
    ) {
    }

    ngOnInit(): void {
        this.route.data
            .pipe(takeUntil(this.destroy$))
            .subscribe(data => this.initialize(data.activeProject, data.testRun));

        combineLatest([
            this.route.data,
            this.route.queryParamMap
        ]).pipe(
            takeUntil(this.destroy$),
        ).subscribe(([activeProject, queryParams]) => {
            this.activeQueryId = parseInt(queryParams.get('item_id'), 10);
        });

        // Setup step notifications
        merge(
            this.websocketService.setupStepCreated,
            this.websocketService.setupStepUpdated,
            this.websocketService.setupStepDeleted,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => this.initialized && event.data.projectId === this.project.id),
        ).subscribe(event => {
            if (event instanceof NotificationProtocol.SetupStepDeleted) {
                this.setupMap.delete(event.data.id);
            } else {
                this.setupMap.set(event.data.id, event.data);
            }

            if (this.selectedTestItem) {
                this.selectTestRunItem(this.selectedTestItem);
            }

            this.cdr.detectChanges();
        });

        // Test case notifications
        merge(
            this.websocketService.testCaseCreated,
            this.websocketService.testCaseUpdated,
            this.websocketService.testCaseDeleted,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => this.initialized && event.data.projectId === this.project.id),
        ).subscribe(event => {
            let node: DataProtocol.TreeNode<DbProtocol.TestCase>,
                parentNode: DataProtocol.TreeNode<DbProtocol.TestCase>;

            if (event instanceof NotificationProtocol.TestCaseCreated) {
                // Add new node
                node = new DataProtocol.TreeNode<DbProtocol.TestCase>();
                node.item = event.data;
                node.children = [];
                this.testCaseMap.set(event.data.id, node);

                // Update parent if applicable
                parentNode = node.item.parentId ? this.testCaseMap.get(node.item.parentId) : null;

                if (parentNode) {
                    parentNode.children.push(node);
                }
            } else if (event instanceof NotificationProtocol.TestCaseDeleted) {
                // Get existing node
                node = this.testCaseMap.get(event.data.id);

                // Remove node
                this.testCaseMap.delete(event.data.id);

                // Update parent if applicable
                parentNode = node.item.parentId ? this.testCaseMap.get(node.item.parentId) : null;

                if (parentNode) {
                    const itemIndex = parentNode.children.findIndex(n => n.item.id === node.item.id);

                    if (itemIndex !== -1) {
                        parentNode.children.splice(itemIndex, 1);
                    }
                }
            } else {
                // Get existing node
                node = this.testCaseMap.get(event.data.id);

                // Check if node parent has been changed
                const oldParentId = node.item.parentId;

                // Update node
                node.item = event.data;
                this.walkTestCaseTreeNode(node);
                this.testCaseMap.set(event.data.id, node);

                // Update parents
                if (oldParentId !== node.item.parentId) {
                    // Remove node from old parent if applicable
                    parentNode = node.item.parentId ? this.testCaseMap.get(oldParentId) : null;

                    if (parentNode) {
                        const itemIndex = parentNode.children.findIndex(n => n.item.id === node.item.id);

                        if (itemIndex !== -1) {
                            parentNode.children.splice(itemIndex, 1);
                        }
                    }

                    // Add node to new parent if applicable
                    parentNode = node.item.parentId ? this.testCaseMap.get(node.item.parentId) : null;

                    if (parentNode) {
                        parentNode.children.push(node);
                    }
                }
            }

            this.testRunItems$.next(this.reorderTestRunItems(this.testRunItems$.getValue()));

            if (this.selectedTestItem) {
                this.selectTestRunItem(this.selectedTestItem);
            }

            this.cdr.detectChanges();
        });

        // Test run notifications
        merge(
            this.websocketService.testRunUpdated,
            this.websocketService.testRunStarted,
            this.websocketService.testRunClosed,
            this.websocketService.testRunReopened,
            this.websocketService.testRunDeleted,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => this.testRun && this.testRun.id === event.data.id),
        ).subscribe(event => {
            if (event instanceof NotificationProtocol.TestRunDeleted) {
                Swal.fire('Test run has been deleted', `by <strong>${event.actorName}</strong>`, 'info')
                    .then(() => this.router.navigate([`/testing/${this.project ? this.project.key : ':project'}`]));
                return;
            }

            if (event instanceof NotificationProtocol.TestRunClosed) {
                Swal.fire('Test run has been closed', `by <strong>${event.actorName}</strong>`, 'info')
                    .then(() => this.router.navigate([`/testing/${this.project ? this.project.key : ':project'}`]));
                return;
            }

            this.testRun = event.data;
            this.subheaderService.setStatus(this.runStatusText(this.testRun), this.badgeRunClass(this.testRun));
            this.cdr.detectChanges();
        });

        // Test run item notifications
        merge(
            this.websocketService.testCreated,
            this.websocketService.testUpdated,
            this.websocketService.testStarted,
            this.websocketService.testPassed,
            this.websocketService.testFailed,
            this.websocketService.testBlocked,
            this.websocketService.testDeleted,
            this.websocketService.testReopened,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => this.initialized && this.testRun && this.testRun.id === event.data.runId),
        ).subscribe(event => {
            const itemIndex = this.testRunItems$.getValue().findIndex(item => item.id === event.data.id),
                isUpdate = !((event instanceof NotificationProtocol.TestCreated) || (event instanceof NotificationProtocol.TestDeleted));

            if (event instanceof NotificationProtocol.TestCreated && itemIndex === -1) {
                this.testRunItems$.next(this.testRunItems$.getValue().concat(this.addTestRunItemMetadata(event.data as TestRunItemWithMetadata)));
            } else if (event instanceof NotificationProtocol.TestDeleted && itemIndex !== -1) {
                this.testRunItems$.next(this.testRunItems$.getValue().splice(itemIndex, 1));
            } else if (isUpdate) {
                this.testRunItems$.getValue()[itemIndex] = (event as any).data;
                this.testRunItems$.getValue()[itemIndex] = this.addTestRunItemMetadata(this.testRunItems$.getValue()[itemIndex]);
                this.testRunItems$.next(this.reorderTestRunItems(this.testRunItems$.getValue()));
            }

            this.cdr.detectChanges();
        });

        // File attachments
        merge(
            this.websocketService.attachmentLinked,
            this.websocketService.attachmentUnlinked,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => /*this.testRun && this.testRun.id === event.data.id*/ event.link.owner === DbProtocol.AttachmentOwner.TestRunItem),
        ).subscribe(event => {
            if (!this.attachments.has(event.link.linkedId))
                return;

            const attachments = this.attachments.get(event.link.linkedId),
                itemIndex = attachments.findIndex(a => a.id === event.attachment.id);

            if (event instanceof NotificationProtocol.AttachmentLinked) {
                itemIndex >= 0
                    ? attachments[itemIndex] = event.attachment
                    : attachments.push(event.attachment);
            } else if (event instanceof NotificationProtocol.AttachmentUnlinked) {
                if (itemIndex !== -1)
                    attachments.splice(itemIndex, 1);
            }

            this.attachments.set(event.link.linkedId, attachments);

            if (this.selectedTestItem && this.selectedTestItem.id === event.link.linkedId)
                this.activeFileAttachments = attachments;

            this.cdr.detectChanges();
        });

        this.router.events
            .pipe(filter(item => item instanceof NavigationEnd),
                takeUntil(this.destroy$))
            .subscribe(result => {
                this.subheaderService.setTitle(this.testRun.title ? this.testRun.title : `Test run #${this.testRun.id}`);
                this.subheaderService.setStatus(this.runStatusText(this.testRun), this.badgeRunClass(this.testRun));
                this.cdr.detectChanges();
            });
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    showFilter(event) {
        if (this.showFilter$.getValue()) {
            this.showFilter$.next(false);
        } else {
            this.showFilter$.next(true);
        }
    }

    changeFilter(status: DbProtocol.TestStatus | null): void {
        if (!this.testStatusFilters$.getValue().has(status) && status !== null) {
            this.testStatusFilters$.next(this.testStatusFilters$.getValue().add(status));
        } else {
            const currFilter = this.testStatusFilters$.getValue();
            currFilter.delete(status);
            this.checkedFilter$.next(FilterStatus.Indeterminate);
            this.testStatusFilters$.next(currFilter);
        }

        if (this.testStatusFilters$.getValue().size === 5) {
            this.checkedFilter$.next(FilterStatus.Checked);
        }

        if (status === null) {
            if (this.testStatusFilters$.getValue().size >= 0 && this.testStatusFilters$.getValue().size <= 4) {
                this.testStatusFilters$.next(new Set<DbProtocol.TestStatus>([
                    DbProtocol.TestStatus.Pending,
                    DbProtocol.TestStatus.InProgress,
                    DbProtocol.TestStatus.Passed,
                    DbProtocol.TestStatus.Failed,
                    DbProtocol.TestStatus.Blocked
                ]));
                this.checkedFilter$.next(FilterStatus.Checked);
            } else {
                this.checkedFilter$.next(FilterStatus.Unchecked);
                this.testStatusFilters$.next(new Set<DbProtocol.TestStatus>([]));
            }
        }
    }

    openGallery(index: number): void {
        this.activeFileAttachments
            .filter(item => item.contentType.includes('image'))
            .forEach(item => {
                const src = '/download/attachments/' + item.filename,
                    caption = item.originalFilename,
                    thumb = '/download/attachments/' + item.thumbFilename,
                    albumItem = { src: src, caption: caption, thumb: thumb, };

                this.activeAlbum.push(albumItem);
            });

        this.lightbox.open(this.activeAlbum, index);
    }

    private initialize(project: DbProtocol.Project, testRun: DbProtocol.TestRun): void {
        if (testRun === null) {
            this.notificationService.warning(`Test run not exists`);
            this.router.navigate([`/test-runs/${project ? project.key : ':project'}`]);
            return;
        }

        if (testRun.projectId !== project.id) {
            this.notificationService.warning(`Test run belongs to another project`);
            this.router.navigate([`/test-runs/${project.key}`]);
            return;
        }

        // Store project and test run locally
        this.project = project;
        this.testRun = testRun;

        // Update page header
        this.subheaderService.setTitle(this.testRun.title ? this.testRun.title : `Test run #${this.testRun.id}`);
        this.subheaderService.setStatus(this.runStatusText(this.testRun), this.badgeRunClass(this.testRun));
        this.cdr.detectChanges();

        const observables: Observable<any>[] = [this.hyperionTestRunsService.getTestRunItems(this.testRun.id)];

        let querySetupStepsIndex = -1,
            queryTestCasesIndex = -1;

        if (this.setupMap.size === 0) {
            querySetupStepsIndex = observables.length;
            observables.push(this.hyperionSetupService.getSetup(this.project.id));
        }

        if (this.testCaseMap.size === 0) {
            queryTestCasesIndex = observables.length;
            observables.push(this.hyperionTestCaseService.getTestCaseTree(this.project.id));
        }

        if (project.jiraId) {
            this.hyperionJiraService
                .getAuthenticationStatus(project.jiraId)
                .pipe(takeUntil(this.destroy$))
                .subscribe(
                    response => this.authenticatedInJira = response.result,
                    error => this.authenticatedInJira = false,
                );
        }

        combineLatest([
            this.testRunItems$.asObservable(),
            this.testStatusFilters$.asObservable(),
            this.search$.asObservable().pipe(debounceTime(150)),
        ]).pipe(
            takeUntil(this.destroy$),
        ).subscribe(([items, filters, search]) => {
            const needle = search.trim().toLocaleLowerCase(),
                filtered = items.filter(item => filters.has(item.status) && (needle ? item.$sortTitle.toLocaleLowerCase().includes(needle) : true));

            this.filteredTestRunItems$.next(this.reorderTestRunItems(filtered));
            this.initialized = true;
            this.cdr.detectChanges();
        });

        combineLatest([
            ...observables
        ]).pipe(
            takeUntil(this.destroy$),
            first(),
        ).subscribe((result) => {
            // Setup
            if (querySetupStepsIndex !== -1) {
                this.setupMap.clear();
                result[querySetupStepsIndex].items.forEach(node => this.walkSetupTreeNode(node));
            }

            // Test cases
            if (queryTestCasesIndex !== -1) {
                this.testCaseMap.clear();
                result[queryTestCasesIndex].items.forEach(node => this.walkTestCaseTreeNode(node));
            }

            // Test run items
            this.testRunItems$.next(this.reorderTestRunItems(result[0].items));

            if (this.activeQueryId) {
                const testRunItm = this.testRunItems$.getValue().find(item => item.id === this.activeQueryId);
                if (testRunItm) {
                    this.selectTestRunItem(testRunItm);
                } else {
                    this.router.navigate([],
                        {
                            queryParams: {
                                'testrunid': null,
                            },
                            replaceUrl: true,
                            queryParamsHandling: 'merge',
                        });
                }
            }

            // Finalize
            this.initialized = true;
            this.cdr.detectChanges();
        });
    }

    // Test runs
    runStatusText(testRun: DbProtocol.TestRun): string {
        switch (testRun.status) {
            case DbProtocol.TestRunStatus.Draft:
                return 'Draft';
            case DbProtocol.TestRunStatus.InProgress:
                return 'In Progress';
            case DbProtocol.TestRunStatus.Closed:
                return 'Closed';
        }
    }

    badgeRunClass(testRun: DbProtocol.TestRun): string {
        switch (testRun.status) {
            case DbProtocol.TestRunStatus.Draft:
                return 'm-badge--metal';
            case DbProtocol.TestRunStatus.InProgress:
                return 'm-badge--info';
            case DbProtocol.TestRunStatus.Closed:
                return 'm-badge--warning';
        }
    }

    // Tests (test run items)
    testStatusText(testStatus: DbProtocol.TestStatus): string {
        switch (testStatus) {
            case DbProtocol.TestStatus.Pending:
                return 'Pending';
            case DbProtocol.TestStatus.Blocked:
                return 'Blocked';
            case DbProtocol.TestStatus.Passed:
                return 'Passed';
            case DbProtocol.TestStatus.Failed:
                return 'Failed';
            case DbProtocol.TestStatus.InProgress:
                return 'In Progress';
        }
    }

    badgeTestClass(status: DbProtocol.TestStatus) {
        switch (status) {
            case DbProtocol.TestStatus.Pending:
                return 'm-timeline-3__item--metal';
            case DbProtocol.TestStatus.InProgress:
                return 'm-timeline-3__item--info';
            case DbProtocol.TestStatus.Blocked:
                return 'm-timeline-3__item--warning';
            case DbProtocol.TestStatus.Failed:
                return 'm-timeline-3__item--danger';
            case DbProtocol.TestStatus.Passed:
                return 'm-timeline-3__item--success';
        }
    }

    isBulkChecked(): boolean {
        const displayedItems = this.filteredTestRunItems$.getValue();
        return displayedItems.filter(item => this.checkedSet.has(item.id)).length === displayedItems.length;
    }

    isBulkIndeterminate(): boolean {
        const displayedItems = this.filteredTestRunItems$.getValue(),
            selectedItems = displayedItems.filter(item => this.checkedSet.has(item.id));

        return selectedItems.length > 0 && selectedItems.length < displayedItems.length;
    }

    canPerformBulkActions(): boolean {
        const displayedItems = this.filteredTestRunItems$.getValue(),
            selectedItems = displayedItems.filter(item => this.checkedSet.has(item.id));

        return selectedItems.length > 0;
    }

    canPerformBulkStart(): boolean {
        const displayedItems = this.filteredTestRunItems$.getValue(),
            selectedItems = displayedItems.filter(item => this.checkedSet.has(item.id)),
            fnCanStart = (item) => item.status === DbProtocol.TestStatus.Pending;

        return selectedItems.filter(item => fnCanStart(item)).length === selectedItems.length;
    }

    canPerformBulkPass(): boolean {
        const displayedItems = this.filteredTestRunItems$.getValue(),
            selectedItems = displayedItems.filter(item => this.checkedSet.has(item.id)),
            fnCanPass = (item) => [DbProtocol.TestStatus.Pending, DbProtocol.TestStatus.InProgress].includes(item.status);

        return selectedItems.filter(item => fnCanPass(item)).length === selectedItems.length;
    }

    canPerformBulkReopen(): boolean {
        const displayedItems = this.filteredTestRunItems$.getValue(),
            selectedItems = displayedItems.filter(item => this.checkedSet.has(item.id)),
            fnCanReopen = (item) => [DbProtocol.TestStatus.Passed, DbProtocol.TestStatus.Failed, DbProtocol.TestStatus.Blocked].includes(item.status);

        return selectedItems.filter(item => fnCanReopen(item)).length === selectedItems.length;
    }

    canPerformBlock(testStatus: DbProtocol.TestStatus, runStatus: DbProtocol.TestRunStatus): boolean {
        return (testStatus === this.testStatusEnum.Pending || testStatus === this.testStatusEnum.InProgress)
            && runStatus === DbProtocol.TestRunStatus.InProgress;
    }

    onBulkChange(): void {
        const displayedItems = this.filteredTestRunItems$.getValue(),
            doAdd = this.isBulkIndeterminate() || !this.isBulkChecked();

        displayedItems.forEach(item => doAdd ? this.checkedSet.add(item.id) : this.checkedSet.delete(item.id));
    }

    onBulkStart(): void {
        this.onBulkAction((id) => this.startTestRunItem(id, false));
    }

    onBulkPass(): void {
        this.onBulkAction((id) => this.passTestRunItem(id, false));
    }

    onBulkReopen(): void {
        this.onBulkAction((id) => this.reopenTestRunItem(id, false));
    }

    onBulkAction(fnAction: (id: number) => void): void {
        this.selectedTestItem = null;
        this.selectedPristineTestItem = null;
        this.selectedTestItemSetup = null;
        this.selectedTestItemPrecondition = null;
        this.selectedTestCase = null;
        this.cdr.detectChanges();

        const displayedItems = this.filteredTestRunItems$.getValue(),
            selectedItems = displayedItems.filter(item => this.checkedSet.has(item.id));

        selectedItems.forEach(item => fnAction(item.id));

        this.checkedSet.clear();
        this.cdr.detectChanges();
    }

    // Node walking
    private walkSetupTreeNode(node: DataProtocol.TreeNode<DbProtocol.SetupStep>): void {
        this.setupMap.set(node.item.id, node.item);
        node.children.forEach(child => this.walkSetupTreeNode(child));
    }

    private getParentWithValidPrecondition(node: DataProtocol.TreeNode<DbProtocol.TestCase>): DataProtocol.TreeNode<DbProtocol.TestCase> {
        let itemId = node.item.id;

        while (node && !this.testCaseMap.get(itemId).item.preconditionId) {
            itemId = node.item.parentId;
            node = this.testCaseMap.get(itemId);
        }

        return node || null;
    }

    private lookUpPreconditionSteps(preconditionId: number): Array<DbProtocol.SetupStep> {
        const result = new Array<DbProtocol.SetupStep>();

        if (!preconditionId) {
            const node = this.testCaseMap.get(this.selectedTestCase.id),
                parent = this.getParentWithValidPrecondition(node);

            preconditionId = parent ? parent.item.preconditionId : 0;
        }

        let setupStep: DbProtocol.SetupStep;

        while (preconditionId) {
            setupStep = this.setupMap.get(preconditionId);
            result.unshift(setupStep);
            preconditionId = setupStep ? setupStep.parentId : null;
        }

        return result;
    }

    private getParentWithValidSetup(node: DataProtocol.TreeNode<DbProtocol.TestCase>): DataProtocol.TreeNode<DbProtocol.TestCase> {
        let itemId = node.item.id;

        while (node && !this.testCaseMap.get(itemId).item.setupId) {
            itemId = node.item.parentId;
            node = this.testCaseMap.get(itemId);
        }

        return node || null;
    }

    private lookUpSetupSteps(setupId: number): Array<DbProtocol.SetupStep> {
        const result = new Array<DbProtocol.SetupStep>();

        if (!setupId) {
            const node = this.testCaseMap.get(this.selectedTestCase.id),
                parent = this.getParentWithValidSetup(node);

            setupId = parent ? parent.item.setupId : 0;
        }

        let setupStep: DbProtocol.SetupStep;

        while (setupId) {
            setupStep = this.setupMap.get(setupId);
            result.unshift(setupStep);
            setupId = setupStep ? setupStep.parentId : null;
        }

        return result;
    }

    private walkTestCaseTreeNode(node: DataProtocol.TreeNode<DbProtocol.TestCase>, parentTitle = ''): void {
        node.item = this.addTestCaseMetadata(node.item, parentTitle);
        this.testCaseMap.set(node.item.id, node);
        node.children.forEach(child => this.walkTestCaseTreeNode(child, node.item['$title']));
    }

    // Metadata
    private addTestCaseMetadata(item: DbProtocol.TestCase, parentTitle: string): DbProtocol.TestCase {
        item['$parentTitle'] = parentTitle;
        item['$title'] = `${parentTitle}${parentTitle ? ' › ' : ''}${item.title}`;
        return item;
    }

    private addTestRunItemMetadata(item: TestRunItemWithMetadata): TestRunItemWithMetadata {
        const testCaseNode = this.testCaseMap.has(item.caseId) ? this.testCaseMap.get(item.caseId) : null;
        item.$parentTitle = testCaseNode ? testCaseNode.item['$parentTitle'] : null;
        item.$title = testCaseNode ? testCaseNode.item.title : `Test run item #${item.id}`;
        let parameterList = '';
        item.params.specs.forEach((spec, index) => {
            parameterList += `[${spec.value}]`;
            if (item.params.specs.length - 1 !== index) {
                parameterList += ', ';
            }
        });

        item.$param = parameterList;
        item.$sortTitle = `${item.$parentTitle}${item.$parentTitle ? ' › ' : ''}${item.$title}${item.$param}`.toLowerCase();

        return item;
    }

    /* Problem with autogenereted test run items with param
     * Each have same orderNum property in group with same param
     * move to sort based on sort title
     * 01.09.2020 N.Prasolov
     */
    private reorderTestRunItems(items: TestRunItemWithMetadata[]): TestRunItemWithMetadata[] {
        return items
            .map((item: DbProtocol.TestRunItem | any) => this.addTestRunItemMetadata(item))
            .sort((a, b) => a.$sortTitle > b.$sortTitle ? 1 : -1);
            // .sort((a, b) => a.orderNum > b.orderNum ? 1 : -1);
    }

    // UI interaction
    selectTestRunItem(testRunItem: DbProtocol.TestRunItem): void {
        if (testRunItem.id) {
            this.router.navigate([],
                {
                    queryParams: {
                        'item_id': testRunItem.id,
                    },
                    replaceUrl: true,
                    queryParamsHandling: 'merge',
                });
        }

        this.selectedTestItem = testRunItem as TestRunItemWithMetadata;
        this.selectedPristineTestItem = DbProtocol.TestRunItem.fromJson(testRunItem.toJson()) as TestRunItemWithMetadata;
        const node = this.testCaseMap.get(this.selectedTestItem.caseId);
        this.selectedTestCase = node ? node.item : null;
        this.inheritedSetupTestCase = null;
        this.inheritedPreconditionTestCase = null;

        if (!this.selectedTestCase.setupId) {
            const parentNode = this.getParentWithValidSetup(node);
            this.inheritedSetupTestCase = parentNode ? parentNode.item : null;
        }

        if (!this.selectedTestCase.preconditionId) {
            const parentNode = this.getParentWithValidPrecondition(node);
            this.inheritedPreconditionTestCase = parentNode ? parentNode.item : null;
        }

        this.selectedTestItemSetup = this.lookUpSetupSteps(this.selectedTestCase.setupId);
        this.selectedTestItemPrecondition = this.lookUpPreconditionSteps(this.selectedTestCase.preconditionId);

        if (this.attachments.has(testRunItem.id)) {
            this.activeFileAttachments = this.attachments.get(testRunItem.id);
        }

        this.getLinkedAttachments(testRunItem.id);
    }

    private findNextActiveTestRunItem(testItem: DbProtocol.TestRunItem): DbProtocol.TestRunItem {
        const currentIndex = this.testRunItems$.getValue().findIndex(item => item.id === testItem.id),
            activeItems = this.testRunItems$.getValue().filter((item, index) =>
                index >= currentIndex && (item.status === this.testStatusEnum.Pending || item.status === this.testStatusEnum.InProgress));

        return activeItems.length > 0
            ? activeItems[0]
            : this.testRunItems$.getValue().filter(item => item.status === this.testStatusEnum.Pending || item.status === this.testStatusEnum.InProgress)[0];
    }

    // not sure that needed, commented at 01/09/2020 N.Prasolov
    private updateLocalTest(testItem: TestRunItemWithMetadata) {
        const index = this.testRunItems$.getValue().findIndex(item => item.id === testItem.id);
        if (index !== -1) {
            this.testRunItems$.getValue()[index] = testItem;
            this.testRunItems$.next(this.reorderTestRunItems(this.testRunItems$.getValue()));
            this.filteredTestRunItems$.next(this.reorderTestRunItems(this.filteredTestRunItems$.getValue()));
        }
        if (this.selectedTestItem && this.selectedTestItem.id === testItem.id) {
            this.selectTestRunItem(this.testRunItems$.getValue()[index]);
        }

        this.filteredTestRunItems$.next(this.reorderTestRunItems(this.filteredTestRunItems$.getValue()));
        // this.testRunItems$.next(this.reorderTestRunItems(this.testRunItems$.getValue()));
        this.cdr.detectChanges();
    }

    revertTestRunItem(): void {
        this.selectedTestItem.summary = this.selectedPristineTestItem.summary;
        this.cdr.detectChanges();
    }

    maybeUpdateTestRunItem(): void {
        if (!(this.selectedTestItem && this.selectedPristineTestItem))
            return;

        if ((this.selectedTestItem.summary || '') !== (this.selectedPristineTestItem.summary || '')) {
            this.updateTestRunItem();
        }
    }

    affectedCount(setupId: number): number {
        const spec = this.selectedTestItem.params.specs.filter(s => s.setupId === setupId)[0],
            affectedItems = this.testRunItems$.getValue()
                .filter(item => item.status === DbProtocol.TestStatus.Pending || item.status === DbProtocol.TestStatus.InProgress)
                .filter(item => {
                    const testCase = this.testCaseMap.get(item.caseId),
                        parentSetups = this.lookUpSetupSteps(testCase.item.setupId),
                        parentPreconditions = this.lookUpPreconditionSteps(testCase.item.preconditionId),
                        hasSetupStep = testCase.item.setupId && parentSetups.includes(this.setupMap.get(setupId)),
                        hasPreconditionStep = testCase.item.preconditionId && parentPreconditions.includes(this.setupMap.get(setupId)),
                        matchesSpec = spec ? item.params.specs.filter(s => s.setupId === setupId && s.value === spec.value).length > 0 : true;

                    return (hasSetupStep && matchesSpec) || hasPreconditionStep;
                });

        return affectedItems.length;
    }

    // File attachments
    public dropped(files: NgxFileDropEntry[]) {
        for (const droppedFile of files) {
            if (droppedFile.fileEntry.isFile) {
                const fileEntry = droppedFile.fileEntry as FileSystemFileEntry,
                    subj$ = new BehaviorSubject(0);

                this.attachmentsProgress.set(droppedFile.fileEntry.name, subj$);

                fileEntry.file((file: File) => {
                    this.uploadService
                        .uploadAttachment(file, DbProtocol.AttachmentOwner.TestRunItem, this.selectedTestItem.id)
                        .pipe(
                            filter((event: HttpEvent<any>) => event.type === HttpEventType.UploadProgress),
                            finalize(() => {
                                subj$.complete();
                                this.attachmentsProgress.delete(file.name);
                                this.cdr.detectChanges();
                            })
                        )
                        .subscribe(
                            (event: HttpUploadProgressEvent) => {
                                const percentDone = Math.round(100 * event.loaded / event.total);
                                subj$.next(percentDone);
                            },
                            error => this.notificationService.warning(`Failed to upload attachment error: ${file.name}`)
                        );
                });
            } else {
                // It was a directory (empty directories are added, otherwise only files)
                const fileEntry = droppedFile.fileEntry as FileSystemDirectoryEntry;
            }
        }
    }

    // REST API
    updateTestRunItem(): void {
        const request = new TestRunProtocol.UpdateTestRunItemRequest();
        request.summary = (this.selectedTestItem.summary || '').trim();

        this.hyperionTestRunsService
            .updateTestRunItem(request, this.selectedTestItem.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                testItem => {
                    this.notificationService.success(`Test run item #${testItem.id} updated`);
                    this.showSummaryEditor = !this.showSummaryEditor;
                    // this.updateLocalTest(testItem);
                },
                error => {
                    if (error instanceof DataProtocol.BadRequestError) {
                        this.notificationService.warning(`Test run item #${this.selectedTestItem.id} was not updated`);
                    } else {
                        this.notificationService.error(error);
                    }
                });
    }

    startTestRunItem(id: number, keepSelected = true): void {
        this.hyperionTestRunsService
            .startTestRunItem(new CommonProtocol.Empty(), id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                testItem => {
                    this.notificationService.success('Test run item started', 'Success');
                    // this.updateLocalTest(testItem);

                    if (keepSelected) {
                        this.selectedTestItem = this.addTestRunItemMetadata(testItem as TestRunItemWithMetadata);
                        this.cdr.detectChanges();
                    }
                },
                error => {
                    if (error instanceof DataProtocol.BadRequestError) {
                        switch (error.error as TestRunProtocol.StartTestRunItemError) {
                            case TestRunProtocol.StartTestRunItemError.AlreadyStarted:
                                this.notificationService.warning(`Test run item #${id} already started`);
                                break;
                            case TestRunProtocol.StartTestRunItemError.AlreadyFinished:
                                this.notificationService.warning(`Test run item #${id} already finished`);
                                break;
                            case TestRunProtocol.StartTestRunItemError.AssigneeNotExists:
                                this.notificationService.error(`Test run item #${id} cannot be assigned`);
                                break;
                        }
                    } else {
                        this.notificationService.error(error);
                    }
                });
    }

    passTestRunItem(id: number, keepSelected = true): void {
        this.hyperionTestRunsService
            .passTestRunItem(new CommonProtocol.Empty(), id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                testItem => {
                    this.notificationService.success('Test run item marked as passed', 'Success');
                    // this.updateLocalTest(testItem);

                    if (keepSelected) {
                        this.selectedTestItem = this.findNextActiveTestRunItem(testItem) as TestRunItemWithMetadata;
                        this.cdr.detectChanges();
                    }
                },
                error => {
                    if (error instanceof DataProtocol.BadRequestError) {
                        switch (error.error as TestRunProtocol.FinishTestRunItemError) {
                            case TestRunProtocol.FinishTestRunItemError.AlreadyFinished:
                                this.notificationService.warning(`Test run item #${id} already finished`);
                                break;
                            case TestRunProtocol.FinishTestRunItemError.AssigneeNotExists:
                                this.notificationService.error(`Test run item #${id} cannot be assigned`);
                                break;
                        }
                    } else {
                        this.notificationService.error(error);
                    }
                });
    }

    failTestRunItem(id: number): void {
        const request = new TestRunProtocol.FinishTestRunItemRequest();
        request.summary = this.selectedTestItem.summary;

        this.hyperionTestRunsService
            .failTestRunItem(request, id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                testItem => {
                    this.notificationService.success(`Test run item #${id} marked as failed`);
                    // this.updateLocalTest(testItem);
                    this.selectedTestItem = this.findNextActiveTestRunItem(testItem) as TestRunItemWithMetadata;
                    this.cdr.detectChanges();
                },
                error => {
                    if (error instanceof DataProtocol.BadRequestError) {
                        switch (error.error as TestRunProtocol.FinishTestRunItemError) {
                            case TestRunProtocol.FinishTestRunItemError.AlreadyFinished:
                                this.notificationService.warning(`Test run item #${id} already finished`);
                                break;
                            case TestRunProtocol.FinishTestRunItemError.AssigneeNotExists:
                                this.notificationService.error(`Test run item #${id} cannot be assigned`);
                                break;
                        }
                    } else {
                        this.notificationService.error(error);
                    }
                });
    }

    blockTestRunItem(setupId: number): void {
        const spec = this.selectedTestItem.params.specs.filter(s => s.setupId === setupId)[0],
            request = new TestRunProtocol.BlockTestRunItemRequest();

        request.failedSetupId = setupId;

        if (spec) {
            request.setupParameterValue = spec.value;
        }

        this.hyperionTestRunsService
            .blockTestRunItem(request, this.selectedTestItem.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                testItem => {
                    this.notificationService.success(`Test run item #${this.selectedTestItem.id} marked as blocked`);
                    // this.updateLocalTest(testItem);
                    this.selectedTestItem = this.findNextActiveTestRunItem(testItem) as TestRunItemWithMetadata;
                    this.cdr.detectChanges();
                },
                error => {
                    if (error instanceof DataProtocol.BadRequestError) {
                        switch (error.error as TestRunProtocol.BlockTestRunItemError) {
                            case TestRunProtocol.BlockTestRunItemError.AlreadyFinished:
                                this.notificationService.warning(`Test run item #${this.selectedTestItem.id} already finished`);
                                break;
                            case TestRunProtocol.BlockTestRunItemError.AssigneeNotExists:
                                this.notificationService.error(`Test run item #${this.selectedTestItem.id} cannot be assigned`);
                                break;
                            case TestRunProtocol.BlockTestRunItemError.SetupNotExists:
                                this.notificationService.error(`Test run item #${this.selectedTestItem.id} failed setup step cannot be set`);
                                break;
                        }
                    } else {
                        this.notificationService.error(error);
                    }
                });
    }

    reopenTestRunItem(id: number, keepSelected = true): void {
        this.hyperionTestRunsService
            .reopenTestRunItem(new CommonProtocol.Empty(), id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                testItem => {
                    this.notificationService.success('Test run item reopened', 'Success');
                    // this.updateLocalTest(testItem);

                    if (keepSelected) {
                        this.selectedTestItem = this.addTestRunItemMetadata(testItem as TestRunItemWithMetadata);
                        this.cdr.detectChanges();
                    }
                },
                error => {
                    if (error instanceof DataProtocol.BadRequestError) {
                        switch (error.error as TestRunProtocol.ReopenTestRunItemError) {
                            case TestRunProtocol.ReopenTestRunItemError.AlreadyOpened:
                                this.notificationService.warning(`Test run item #${id} already started`);
                                break;
                            case TestRunProtocol.ReopenTestRunItemError.AssigneeNotExists:
                                this.notificationService.error(`Test run item #${id} cannot be assigned`);
                                break;
                        }
                    } else {
                        this.notificationService.error(error);
                    }
                });
    }

    openJiraAuthDialog(): void {
        const dialogRef = this.dialog.open(JiraAuthDialogComponent, {
            width: '350px',
            data: <IJiraAuthData>{ jiraId: this.project.jiraId }
        });

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe((result: boolean | any) => {
                if (result) {
                    this.authenticatedInJira = true;
                    this.cdr.detectChanges();
                }
            });
    }

    openJiraIssueDialog(): void {
        const dialogRef = this.dialog.open(JiraIssueDialogComponent, {
            width: '600px',
            data: <IJiraIssueData>{
                testRunItemId: this.selectedTestItem.id,
                summary: `Failed: ${this.selectedTestCase.title}`,
                description: `[View in Hyperion|${window.location.href}]`,
            }
        });

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe((result: IJiraIssueResult | any) => {
                if (result) {
                    this.selectedPristineTestItem.jiraIssueKey = this.selectedTestItem.jiraIssueKey = result.jiraIssueKey;
                    this.selectedPristineTestItem.jiraIssueUrl = this.selectedTestItem.jiraIssueUrl = result.jiraIssueUrl;
                    this.cdr.detectChanges();
                }
            });
    }

    private getLinkedAttachments(testRunItemId: number): void {
        this.hyperionAttachmentService
            .getLinkedAttachments(DbProtocol.AttachmentOwner.TestRunItem, testRunItemId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.attachments.set(testRunItemId, response.items);
                    this.activeFileAttachments = response.items;
                    this.cdr.detectChanges();
                },
                error => this.notificationService.error(error)
            );
    }

    unlinkFileAttachment(attachmentId: number, linkedId: number): void {
        this.hyperionAttachmentService
            .deleteAttachmentLink(attachmentId, DbProtocol.AttachmentOwner.TestRunItem, linkedId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.activeFileAttachments = this.activeFileAttachments.filter(fa => fa.id !== attachmentId);
                    this.cdr.detectChanges();
                },
                error => this.notificationService.error(error)
            );
    }

    selectParamForTestItemSetup(item): number {
        let result = null;

        this.selectedTestItem.params.specs.forEach((itm, indx) => {
            if (itm.setupId === item.id) {
                result = indx;
            }
        });

        return result;
    }
}

import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy, OnInit } from '@angular/core';
import { DomSanitizer } from '@angular/platform-browser';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { HttpEvent, HttpEventType, HttpUploadProgressEvent } from '@angular/common/http';
import { CdkDragDrop, moveItemInArray } from '@angular/cdk/drag-drop';
import { MatDialog } from '@angular/material/dialog';
import { BehaviorSubject, combineLatest, merge, Observable, Subject } from 'rxjs';
import { filter, finalize, first,  takeUntil } from 'rxjs/operators';
import { Lightbox } from 'ngx-lightbox';
import { FileSystemDirectoryEntry, FileSystemFileEntry, NgxFileDropEntry } from 'ngx-file-drop';
import Swal from 'sweetalert2';
import { FilterStatus } from '../../../../shared/interfaces/filter-status';
import { IBrowseCase, TestCaseBrowseDialogComponent } from '../test-case-browse-dialog/test-case-browse-dialog.component';
import { NotificationService } from '../../../../core/services/notification.service';
import { SubheaderService } from '../../../../core/services/metronic/layout/subheader.service';
import { UploadService } from '../../../../core/services/upload.service';
import { WebsocketService } from '../../../../core/services/websocket.service';
import { HyperionAttachmentService } from '../../../../protocol/attachment-protocol.service';
import { HyperionProjectService } from '../../../../protocol/project-protocol.service';
import { HyperionSetupService } from '../../../../protocol/setup-protocol.service';
import { HyperionTestCaseService } from '../../../../protocol/test-case-protocol.service';
import { HyperionTestRunService } from '../../../../protocol/test-run-protocol.service';
import * as CommonProtocol from '../../../../protocol/common-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as NotificationProtocol from '../../../../protocol/notification-protocol.data';
import * as TestRunProtocol from '../../../../protocol/test-run-protocol.data';


@Component({
    selector: 'm-test-run-edit-page',
    templateUrl: './test-run-edit.component.html',
    styleUrls: ['./test-run-edit.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TestRunEditComponent implements OnInit, OnDestroy {
    destroy$ = new Subject<any>();
    filteredTestRunItems$ = new BehaviorSubject<DbProtocol.TestRunItem[]>([]);
    attachmentsProgress = new Map<string, BehaviorSubject<number>>();
    testStatusFilters$ = new BehaviorSubject<Set<DbProtocol.TestStatus>>(new Set<DbProtocol.TestStatus>([
        DbProtocol.TestStatus.Pending,
        DbProtocol.TestStatus.InProgress,
        DbProtocol.TestStatus.Passed,
        DbProtocol.TestStatus.Failed,
        DbProtocol.TestStatus.Blocked,
    ]));
    checkedFilter$ = new BehaviorSubject<FilterStatus>(FilterStatus.Checked);
    showFilter$ = new BehaviorSubject(false);
    typeFilterEnum = DbProtocol.TestStatus;
    project: DbProtocol.Project;
    testRun: DbProtocol.TestRun;
    testStatusEnum = DbProtocol.TestStatus;
    statusEnum = DbProtocol.TestRunStatus;
    setupMap = new Map<number, DbProtocol.SetupStep>();
    testCaseMap = new Map<number, DataProtocol.TreeNode<DbProtocol.TestCase>>();
    testRunItems$ = new BehaviorSubject<DbProtocol.TestRunItem[]>([]);
    attachments = new Map<number, DbProtocol.FileAttachment[]>();
    activeFileAttachments: DbProtocol.FileAttachment[];
    activeAlbum = [];
    filterStatusEnum = FilterStatus;
    selectedTestCase: DbProtocol.TestCase;
    selectedTestItem: DbProtocol.TestRunItem;
    selectedTestItemSetup?: Array<DbProtocol.SetupStep>;
    inheritedSetupTestCase?: DbProtocol.TestCase;
    selectedTestItemPrecondition?: Array<DbProtocol.SetupStep>;
    inheritedPreconditionTestCase?: DbProtocol.TestCase;
    activeCaseSet: Set<number>;
    initialized = false;
    files: NgxFileDropEntry[] = [];
    showMoreAttachments = false;
    id: number;
    activeQueryId: number;

    constructor(
        private cdr: ChangeDetectorRef,
        private dialog: MatDialog,
        private hyperionAttachmentService: HyperionAttachmentService,
        private hyperionProjectService: HyperionProjectService,
        private hyperionSetupService: HyperionSetupService,
        private hyperionTestCaseService: HyperionTestCaseService,
        private hyperionTestRunsService: HyperionTestRunService,
        private lightbox: Lightbox,
        private notificationService: NotificationService,
        private route: ActivatedRoute,
        private router: Router,
        private sanitizer: DomSanitizer,
        private uploadService: UploadService,
        private subheaderService: SubheaderService,
        private websocketService: WebsocketService,
    ) { }


    ngOnInit(): void {
         combineLatest(this.route.data, this.route.queryParamMap)
            .pipe(
                takeUntil(this.destroy$),
            )
            .subscribe(([activeProject, queryParams,]) => {
                this.activeQueryId = parseInt(queryParams.get('item_id'), 10);
            });

        this.route.data
            .pipe(takeUntil(this.destroy$))
            .subscribe(data => this.initialize(data.activeProject, data.testRun));

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
                    this.walkTestCaseTreeNode(this.testCaseMap.get(parentNode.item.id));
                }
            }

            this.testRunItems$.next(this.reoderTestRunItems(this.testRunItems$.getValue()));

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
                    .then(() => this.router.navigate([`/test-runs/${this.project ? this.project.key : ':project'}`]));

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
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => this.initialized && this.testRun && this.testRun.id === event.data.runId),
        ).subscribe(event => {

            const itemIndex = this.testRunItems$.getValue().findIndex(item => item.id === event.data.id),
                isUpdate = !((event instanceof NotificationProtocol.TestCreated) || (event instanceof NotificationProtocol.TestDeleted));

            if (event instanceof NotificationProtocol.TestCreated && itemIndex === -1) {
                const testRunItem = event.data;
                this.testRunItems$.next(this.testRunItems$.getValue().concat(this.addTestRunItemMetadata(event.data)));
            } else if (event instanceof NotificationProtocol.TestDeleted && itemIndex !== -1) {
                this.testRunItems$.getValue().splice(itemIndex, 1);
                this.filteredTestRunItems$.getValue().splice(this.filteredTestRunItems$.getValue().findIndex(item => item.id === event.data.id), 1);
            } else if (isUpdate) {
                this.testRunItems$.getValue()[itemIndex] = (event as any).data;
                this.testRunItems$.getValue()[itemIndex] = this.addTestRunItemMetadata(this.testRunItems$.getValue()[itemIndex]);
                this.testRunItems$.next(this.reoderTestRunItems(this.testRunItems$.getValue()));
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
        combineLatest(
            this.testStatusFilters$,
            this.testRunItems$,
        ).pipe(
            takeUntil(this.destroy$),
        ).subscribe(([filters, response]) => {
            const items = this.reoderTestRunItems(status !== null ? response.filter(item => this.testStatusFilters$.getValue().has(item.status)) : response);
            this.filteredTestRunItems$.next(items);
            this.initialized = true;
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
                    albumItem = {
                        src: src,
                        caption: caption,
                        thumb: thumb
                    };
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

        combineLatest(...observables)
            .pipe(
                takeUntil(this.destroy$),
                first(),
            )
            .subscribe(result => {
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
                this.testRunItems$.next(this.reoderTestRunItems(result[0].items));

                if (this.activeQueryId) {
                    const testRunItm = this.testRunItems$.getValue().find(item => item.id === this.activeQueryId);
                    if (testRunItm) {
                        this.selectTestRunItem(testRunItm);
                    } else {
                        this.router.navigate([],
                            {
                                queryParams: {
                                    'item_id': null,
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

        let setupStep = new DbProtocol.SetupStep();

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

    private addTestRunItemMetadata(item: DbProtocol.TestRunItem): DbProtocol.TestRunItem {
        const testCaseNode = this.testCaseMap.has(item.caseId) ? this.testCaseMap.get(item.caseId) : null;
        item['$parentTitle'] = testCaseNode ? testCaseNode.item['$parentTitle'] : null;
        item['$title'] = testCaseNode ? testCaseNode.item.title : `Test run item #${item.id}`;
        item['$sortTitle'] = `${item['$parentTitle']}${item['$parentTitle'] ? ' › ' : ''}${item['$title']}`.toLowerCase();
        let parameterList = '';
        item.params.specs.forEach((spec, index) => {
            parameterList += `[${spec.value}]`;
            if (item.params.specs.length - 1 !== index) {
                parameterList += ', ';
            }
        });
        item['$param'] = parameterList;

        if (item['$parentTitle'].length > 100) {
            item['$reducedParentTitle'] = item['$parentTitle'].substr(0, item['$parentTitle'].lastIndexOf(' ', 100));
            item['$reducedTitle'] = '';
        } else if (item['$title'].length > 100) {
            item['$reducedParentTitle'] = item['$parentTitle'];
            item['$reducedTitle'] = item['$title'].substr(0, item['$title'].lastIndexOf(' ', 100));
        }

        return item;
    }

    private reoderTestRunItems(items: DbProtocol.TestRunItem[]): DbProtocol.TestRunItem[] {
        return items
            .map((item: DbProtocol.TestRunItem | any) => this.addTestRunItemMetadata(item))
            // .sort((a, b) => a['$sortTitle'] > b['$sortTitle'] ? 1 : -1);
            .sort((a, b) => a.orderNum > b.orderNum ? 1 : -1);
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

        this.selectedTestItem = testRunItem;
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
        this.cdr.detectChanges();
    }

    showBrowseTestCaseDialog(): void {
        this.activeCaseSet ? this.activeCaseSet.clear() : this.activeCaseSet = new Set<number>();
        const activeCaseStatusMap = new Map<number, DbProtocol.TestStatus>();

        this.testRunItems$.getValue().forEach(item => {
            this.activeCaseSet.add(item.caseId);
            activeCaseStatusMap.set(item.caseId, item.status);
        });

        const dialogRef = this.dialog.open(TestCaseBrowseDialogComponent, {
            width: '750px',
            data: <IBrowseCase>{ projectId: this.project.id, activeCaseSet: this.activeCaseSet, activeCaseStatusMap: activeCaseStatusMap }
        });

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe((result: null | Set<number> | any) => {
                if (result === null || typeof result === 'object') {
                    let index = 1;

                    result.forEach(caseId => {
                        if (!this.activeCaseSet.has(caseId) && !this.testCaseMap.get(caseId).item.isGroup) {
                            this.createTestRunItem(caseId, index);
                        }

                        index++;
                        this.selectedTestItem = null;
                        this.cdr.detectChanges();
                    });

                    this.activeCaseSet.forEach(caseId => {
                        if (!result.has(caseId)) {
                            this.testRunItems$.getValue().map(item => {
                                if (item.caseId === caseId) {
                                    this.deleteTestRunItem(item.id);
                                }
                                if (this.selectedTestItem && this.selectedTestItem.id === item.id) {
                                    this.selectedTestItem = null;
                                }
                            });


                        }
                    });
                }
            });
    }

    drop(event: CdkDragDrop<DbProtocol.TestRunItem[]>) {
        moveItemInArray(this.testRunItems$.getValue(), event.previousIndex, event.currentIndex);
        this.updateTestRunItem(this.testRunItems$.getValue()[event.currentIndex].id, event.currentIndex);
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
                            }),
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
    private createTestRunItem(caseId: number, index: number): void {
        const request = new TestRunProtocol.CreateTestRunItemRequest();
        request.caseId = caseId;
        request.orderNum = (this.testRunItems$.getValue().length + index) * 1000;

        this.hyperionTestRunsService
            .createTestRunItems(request, this.testRun.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.notificationService.success(`Test run items created`, 'Success');
                    this.cdr.detectChanges();
                },
                error => {
                    if (error instanceof DataProtocol.BadRequestError) {
                        switch (error.error as TestRunProtocol.CreateTestRunItemError) {
                            case TestRunProtocol.CreateTestRunItemError.TestRunNotExists:
                                this.notificationService.warning(`Test run not exists`);
                                break;
                            case TestRunProtocol.CreateTestRunItemError.TestCaseNotExists:
                                this.notificationService.warning(`Test cases not exists`);
                                break;
                            case TestRunProtocol.CreateTestRunItemError.ReporterNotExists:
                                this.notificationService.error(`Test run item cannot be assigned`);
                                break;
                        }
                    } else {
                        this.notificationService.error(error);
                    }
                });
    }

    deleteTestRunItem(id: number): void {
        this.hyperionTestRunsService
            .deleteTestRunItem(id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.notificationService.info(`Test run item deleted`, 'Success');
                    if (this.selectedTestItem && this.selectedTestItem.id === id) {
                        this.selectedTestItem = null;
                    }
                    this.router.navigate([], {queryParams: {'item_id': null }, replaceUrl: true, queryParamsHandling: 'merge'});
                    this.cdr.detectChanges();
                },
                error => this.notificationService.error(error)
            );
    }

    updateTestRunItem(id: number, currentIndex: number): void {
        const request = new TestRunProtocol.UpdateTestRunItemRequest(),
            items = this.testRunItems$.getValue();

        if (items[currentIndex - 1] && items[currentIndex + 1]) {
            request.orderNum = Math.round((this.testRunItems$.getValue()[currentIndex - 1].orderNum + this.testRunItems$.getValue()[currentIndex + 1].orderNum) / 2);

            if (request.orderNum === this.testRunItems$.getValue()[currentIndex - 1].orderNum || request.orderNum === this.testRunItems$.getValue()[currentIndex + 1].orderNum) {
                this.reorderAllTestRunItems();
                return;
            }
        } else if (!this.testRunItems$.getValue()[currentIndex + 1]) {
            request.orderNum = this.testRunItems$.getValue()[currentIndex - 1].orderNum + 1000;
        } else if (!this.testRunItems$.getValue()[currentIndex - 1]) {
            request.orderNum = 1000;
            this.testRunItems$.getValue()[currentIndex].orderNum = 1000; // TODO: refactor
            this.updateTestRunItem(this.testRunItems$.getValue()[currentIndex + 1].id, currentIndex + 1);
        }

        this.hyperionTestRunsService.updateTestRunItem(request, id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.notificationService.success(`Test run item updated`, 'Success');
                    this.cdr.detectChanges();
                },
                error => {
                    if (error instanceof DataProtocol.BadRequestError) {
                        switch (error.error as TestRunProtocol.UpdateTestRunItemError) {
                            case TestRunProtocol.UpdateTestRunItemError.NothingToUpdate:
                                this.notificationService.warning(`Nothing to update`);
                                break;
                        }
                    } else {
                        this.notificationService.error(error);
                    }
                });
    }

    reorderAllTestRunItems(): void {
        this.testRunItems$.getValue().forEach((item, index) => {
            const request = new TestRunProtocol.UpdateTestRunItemRequest();
            request.orderNum = (index + 1) * 1000;

            this.hyperionTestRunsService.updateTestRunItem(request, item.id)
                .pipe(takeUntil(this.destroy$))
                .subscribe(
                    response => {
                        this.notificationService.success(`Test run item updated`, 'Success');
                        this.cdr.detectChanges();
                    },
                    error => {
                        if (error instanceof DataProtocol.BadRequestError) {
                            switch (error.error as TestRunProtocol.UpdateTestRunItemError) {
                                case TestRunProtocol.UpdateTestRunItemError.NothingToUpdate:
                                    this.notificationService.warning(`Nothing to update`);
                                    break;
                            }
                        } else {
                            this.notificationService.error(error);
                        }
                    });
        });
    }

    startTestRun(id: number) {
        this.hyperionTestRunsService
            .startTestRun(new CommonProtocol.Empty(), id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                run => this.notificationService.success(`Test run #${id} started`),
                error => this.notificationService.error(error)
            );
    }

    closeTestRun(id: number) {
        this.hyperionTestRunsService
            .closeTestRun(new CommonProtocol.Empty(), id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                run => this.notificationService.success(`Test run #${id} closed`),
                error => this.notificationService.error(error)
            );
    }

    reopenTestRun(id: number) {
        this.hyperionTestRunsService
            .reopenTestRun(new CommonProtocol.Empty(), id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                run => this.notificationService.success(`Test run #${id} reopened`),
                error => this.notificationService.error(error)
            );
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

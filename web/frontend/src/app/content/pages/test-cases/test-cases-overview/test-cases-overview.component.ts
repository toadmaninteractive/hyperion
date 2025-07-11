import { ChangeDetectionStrategy, ChangeDetectorRef, Component, ElementRef, OnDestroy, OnInit, ViewChild, Renderer2 } from '@angular/core';
import { HttpEvent, HttpEventType, HttpUploadProgressEvent } from '@angular/common/http';
import { ActivatedRoute, Data, Router } from '@angular/router';
import { MatDialog } from '@angular/material/dialog';
import { BehaviorSubject, combineLatest, forkJoin, merge, Observable, Subject, throwError } from 'rxjs';
import { filter, finalize, switchMap, take, takeUntil } from 'rxjs/operators';
import { BrowseSetupDialogComponent } from '../browse-setup-dialog/browse-setup-dialog.component';
import { Lightbox } from 'ngx-lightbox';
import { FileSystemDirectoryEntry, FileSystemFileEntry, NgxFileDropEntry } from 'ngx-file-drop';
import Swal, { SweetAlertOptions } from 'sweetalert2';
import { SwalComponent } from '@sweetalert2/ngx-sweetalert2';
import { DropPlace } from '../../../../shared/interfaces/drop-place';
import { NotificationService, NotificationType } from '../../../../core/services/notification.service';
import { UploadService } from '../../../../core/services/upload.service';
import { WebsocketService } from '../../../../core/services/websocket.service';
import { HyperionAttachmentService } from '../../../../protocol/attachment-protocol.service';
import { HyperionSetupService } from '../../../../protocol/setup-protocol.service';
import { HyperionTestCaseService } from '../../../../protocol/test-case-protocol.service';
import { HyperionParameterService } from '../../../../protocol/parameter-protocol.service';
import * as NotificationProtocol from '../../../../protocol/notification-protocol.data';
import * as TestCaseProtocol from '../../../../protocol/test-case-protocol.data';
import * as CommonProtocol from '../../../../protocol/common-protocol.data';
import { AttachmentOwner, FileAttachment, Parameter, ParameterValue, PersonnelAccountProfile, Project, SetupStep, SpecType, TestCase, TestCaseSpecialization } from '../../../../protocol/db-protocol.data';
import { BadRequestError, NotFoundError, TreeNode } from '../../../../protocol/data-protocol.data';
import { TestCaseService } from '../../../../core/services/testcase.service';
import { SetupService } from '../../../../core/services/setup.service';
import { ParameterService } from '../../../../core/services/parameter.service';
import { DynamicTreeMapComponent } from '../../../../components/general/dynamic-tree-map/dynamic-tree-map.component';
import { AccountService } from '../../../../core/services/account.service';
import { WindowRefService } from '../../../../core/services/window-ref.service';

export interface ISelectedParam {
    specType: SpecType;
    value: string | number;
}

@Component({
    selector: 'm-test-cases-new-page',
    templateUrl: './test-cases-overview.component.html',
    styleUrls: ['./test-cases-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TestCasesOverviewComponent implements OnInit, OnDestroy {
    @ViewChild('afterDropDialog', { static: true }) afterDropDialog: SwalComponent;
    @ViewChild('checkbox1', { static: true }) checkbox1: ElementRef;
    @ViewChild(DynamicTreeMapComponent) dynamicTree: DynamicTreeMapComponent;

    destroy$ = new Subject<any>();
    hiddenNodeSet = new Set<TreeNode<TestCase>>();
    ready$ = new BehaviorSubject<boolean>(false);
    websocketNodeAdd$ = new BehaviorSubject<TreeNode<TestCase>>(null);
    websocketNodeDelete$ = new BehaviorSubject<TreeNode<TestCase>>(null);
    websocketNodeUpdate$ = new BehaviorSubject<TreeNode<TestCase>>(null);
    attachmentsProgress = new Map<string, BehaviorSubject<number>>();
    flatSetup = new Map<number, SetupStep>();
    setupTree = new Map<number, TreeNode<SetupStep>>();
    flatParameter = new Map<number, Parameter>();
    parameterList = new Map<number, TreeNode<Parameter>>();
    nodeWithSameSetupMap = new Map<number, TreeNode<TestCase>>();
    nodeWithSamePreconditionMap = new Map<number, TreeNode<TestCase>>();
    activeNode?: TreeNode<TestCase>;
    activeNodeId = -1;
    activeNodeSetup?: Array<SetupStep>;
    activeNodePrecondition?: Array<SetupStep>;
    inheritedPreconditionTestCase?: TestCase;
    inheritedSetupTestCase?: TestCase;
    filteredNodeArray: TreeNode<TestCase>[] = [];
    lastNode?: TreeNode<TestCase>;
    lastNodeId = 0;
    project: Project;
    searchNeedle = '';
    newSetupTitle = '';
    initialized = false;
    attachments = new Map<number, FileAttachment[]>();
    activeFileAttachments: FileAttachment[];
    activeAlbum = [];
    showMoreAttachments = false;
    nodeSetupChangeCheckbox: HTMLInputElement;
    childrenChangeCheckbox: HTMLInputElement;
    afterDropOptions = {
        title: 'Test case parent changed',
        text: 'Do you want change setup and precondition?',
        html: `<div class="row">
                <div class="col">
                    <input type="checkbox" id="nodeSetup" checked="checked" style="transform: scale(1.5)" class="m-1"/>
                    <label  for="nodeSetup" class="m--regular-font-size-lg2">
                        Change for item
                    </label>
                </div>
                </div>
                <div class="row">
                <div class="col">
                    <input type="checkbox" id="childrenSetup" checked="checked" style="transform: scale(1.5)" class="m-1"//>
                    <label  for="childrenSetup"  class="m--regular-font-size-lg2">
                        Change for children
                    </label>
                    </div>
                </div>`,
        type: 'question',
        showCancelButton: true,
        confirmButtonText: 'Yes',
        cancelButtonText: 'No',
        preConfirm: () => {
            this.nodeSetupChangeCheckbox = <HTMLInputElement>document.getElementById('nodeSetup');
            this.childrenChangeCheckbox = <HTMLInputElement>document.getElementById('childrenSetup');
        }
    };

    showMoreParamMap = new Map<number, boolean>();
    paramTypeMap = new Map<number, SpecType>();
    specTypeEnum = SpecType;
    levelParamMap = new Map<number, Array<any>>();
    childParamArray = [];
    activeQueryId: number;
    createState = false;

    dimmedNodeSetId = new Set<number>();
    hiddenNodeSetId = new Set<number>();
    pristineTestCaseMap = new Map<number, TestCase>();
    testCaseList: Map<number, TreeNode<TestCase>> = new Map();
    profile: PersonnelAccountProfile;
    rootLevelTestCase: Map<number, TestCase> = new Map();


    constructor(
        private route: ActivatedRoute,
        private router: Router,
        private render: Renderer2,
        private cdr: ChangeDetectorRef,
        private dialog: MatDialog,
        private lightbox: Lightbox,
        private accountService: AccountService,
        private hyperionAttachmentService: HyperionAttachmentService,
        private hyperionParameterService: HyperionParameterService,
        private hyperionSetupService: HyperionSetupService,
        private hyperionTestCaseService: HyperionTestCaseService,
        private notificationService: NotificationService,
        private uploadService: UploadService,
        private websocketService: WebsocketService,
        private parameterService: ParameterService,
        private setupService: SetupService,
        public testCaseService: TestCaseService,
        private windowRefService: WindowRefService,
    ) {
    }

    ngOnInit(): void {

        this.accountService.profile$
            .asObservable()
            .pipe(takeUntil(this.destroy$))
            .subscribe(profile => this.profile = profile);

        combineLatest([
            this.setupService.setupTree$,
            this.setupService.flatSetup$,
            this.parameterService.parameterList$,
            this.parameterService.flatParameter$,
            this.testCaseService.testCaseList$,
            this.testCaseService.flatTestCase$])
            .pipe(
                filter(([st, fs, pl, fp, tl, ft]) => !!st && !!fs && !!pl && !!fp && !!tl && !!ft ),
                takeUntil(this.destroy$))
            .subscribe(([setupTree, flatSetup, parameterList, flatParameter, testCaseList, flatTestCase]) => {
                this.pristineTestCaseMap.clear();
                this.setupTree = setupTree;
                this.flatSetup = flatSetup;
                this.parameterList = parameterList;
                this.flatParameter = flatParameter;
                this.testCaseList = testCaseList;
                flatTestCase.forEach(testCase => this.pristineTestCaseMap.set(testCase.id, TestCase.fromJson(testCase.toJson())));

                this.ready$.next(true);
            });

        this.testCaseService.testCaseTree$
            .asObservable()
            .pipe(
                filter(s => !!s),
                takeUntil(this.destroy$))
            .subscribe(testCaseTree => {
                    this.rootLevelTestCase.clear();
                    testCaseTree.forEach(elem => this.rootLevelTestCase.set(elem.item.id, elem.item));
                }
            );

        this.route.data
            .pipe(takeUntil(this.destroy$))
            .subscribe(data => this.initialize(data.activeProject));

        combineLatest(this.route.data, this.route.queryParamMap)
            .pipe(
                takeUntil(this.destroy$),
            )
            .subscribe(([activeProject, queryParams]) => {
                this.activeQueryId = parseInt(queryParams.get('id'), 10);
            });

        merge(
            this.websocketService.testCaseUpdated,
            this.websocketService.testCaseDeleted,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => event.data.projectId === this.project.id),
        ).subscribe(event => {
            const node = new TreeNode<TestCase>();
            node.item = event.data;
            if (event instanceof NotificationProtocol.TestCaseUpdated) {

                if (this.activeNodeId !== -1 && node.item.id === this.activeNodeId && this.profile.username !== event.actorName) {
                    Swal.fire({
                        type: 'information',
                        title: `Your active node has been changed by ${event.actorName }`,
                        confirmButtonText: 'ok',
                    } as SweetAlertOptions).then((result) => {
                        if (result.value !== undefined) {
                            console.log('merge update test case');
                            this.setActiveTreeNode(node);
                        }
                    });
                }
            } else if (event instanceof NotificationProtocol.TestCaseDeleted) {
                if (this.activeNode && node.item.id === this.activeNode.item.id && this.profile.username !== event.actorName) {

                    Swal.fire({
                        type: 'warning',
                        title: `Your active node has been deleted by ${event.actorName }`,
                        confirmButtonText: 'ok',
                    } as SweetAlertOptions).then((result) => {
                        if (result.value !== undefined) {
                            this.resetActiveTreeNode();
                        }
                    });
                }
            }
        });

        // File attachments
        merge(
            this.websocketService.attachmentLinked,
            this.websocketService.attachmentUnlinked,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => event.link.owner === AttachmentOwner.TestCase),
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

            if (this.activeNode.item.id && this.activeNode.item.id === event.link.linkedId)
                this.activeFileAttachments = attachments;

            this.cdr.detectChanges();
        });
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.ready$.complete();
    }

    // Page initialization and refresh
    private initialize(project: Project): void {
        this.project = project;



        this.refresh();
    }

    refresh(): void {
        this.searchNeedle = '';
        this.activeNode = null;
        this.activeNodeId = -1;
        this.activeNodeSetup = [];
        this.newSetupTitle = '';
        this.initialized = true;
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

    applyFilter(filterText: string): void {
        this.dimmedNodeSetId = new Set<number>(Array.from(this.pristineTestCaseMap).map(item => !this.filterCallback(item[1], filterText) ? item[1].id : null));
        this.hiddenNodeSetId = new Set<number>(Array.from(this.testCaseList).filter(node => this.getAllChildrenId(node[1]).every(id => this.dimmedNodeSetId.has(id)))
            .map(node => node[1].item.id));
        this.expandAllTreeNodes();
    }

    getAllChildrenId(node: TreeNode<TestCase>): Array<number> {
        return node.children.reduce(
            (acc, value) => [
                ...acc,
                [value.item.id],
                value.children.map(child => this.getAllChildrenId(child))
                    .reduce((prev, curr) => prev.concat(curr), [])], []).concat(node.item.id)
            .reduce((prev, curr) => prev.concat(curr), []);
    }

    getAllParentId(node: TreeNode<TestCase>): Array<number> {
        const result = [];
        while (node && node.item.parentId !== 0) {
            const parentNode = this.testCaseList.get(node.item.parentId);
            result.push(parentNode ? parentNode.item.id : null);
            node = parentNode;
        }
        return result.filter(elem => !!elem);
    }

    showChildrenBeforeAddDraftNode(node?: TreeNode<SetupStep>): void {
        if (node) {
            node.children.forEach(child => this.hiddenNodeSetId.delete(child.item.id));
        } else {
            this.hiddenNodeSetId.forEach(elem => {
                if (this.pristineTestCaseMap.get(elem).parentId === null) {
                    this.hiddenNodeSetId.delete(elem);
                }
            });
        }
        this.cdr.detectChanges();
    }


    // Tree helpers
    private initializeTreeNode(parent: TreeNode<TestCase>, isGroup: boolean): TreeNode<TestCase> {
        const result = new TreeNode<TestCase>(),
            item = new TestCase();

        // Define test case
        item.id = 0;
        item.parentId = parent ? parent.item.id : null;
        item.projectId = this.project.id;
        item.setupId = parent ? parent.item.setupId : null;
        item.preconditionId = parent ? parent.item.preconditionId : null;
        item.isGroup = isGroup;
        item.title = '';
        item.description = parent ? parent.item.description : '';
        item.testSteps = '';
        item.expectedResult = '';
        item.orderNum = parent
            ? (parent.children.length ? parent.children.slice(-1)[0].item.orderNum + 1000 : 1000)
            : (this.rootLevelTestCase.size ? [...this.rootLevelTestCase.entries()]
                .map(elem => elem[1])
                .sort((a, b) => a.orderNum > b.orderNum ? 1 : -1)
                .slice(-1)[0].orderNum + 1000 : 1000);

        // Define test case node
        result.item = item;
        result.children = [];

        return result;
    }

    treeNodeHasDraftChildren(node: TreeNode<TestCase>): boolean {
        return node
            ? node.children.filter(child => child.item.id === 0).length > 0
            : this.rootLevelTestCase.has(0);
    }

    treeNodeChanged(node: TreeNode<TestCase>, field?: string): boolean {
        const pristine = this.pristineTestCaseMap.get(node.item.id);
        let isValidField = false;

        switch (field) {
            case 'setupId':
            case 'preconditionId':
            case 'title':
            case 'description':
            case 'testSteps':
            case 'expectedResult':
            case 'isDraft':
            case null:
                isValidField = true;
                break;
        }

        if (!isValidField) {
            return false;
        } else if (!pristine) {
            return true;
        }

        switch (field) {
            case 'setupId':
                return pristine.setupId !== node.item.setupId;
            case 'preconditionId':
                return pristine.preconditionId !== node.item.preconditionId;
            case 'title':
                return (pristine.title || '').trim() !== (node.item.title || '').trim();
            case 'description':
                return (pristine.description || '').trim() !== (node.item.description || '').trim();
            case 'testSteps':
                return (pristine.testSteps || '').trim() !== (node.item.testSteps || '').trim();
            case 'expectedResult':
                return (pristine.expectedResult || '').trim() !== (node.item.expectedResult || '').trim();
            case 'isDraft':
                return pristine.isDraft !== node.item.isDraft;
            default:
                return pristine.setupId !== node.item.setupId
                    || (pristine.title || '').trim() !== (node.item.title || '').trim()
                    || (pristine.description || '').trim() !== (node.item.description || '').trim()
                    || (pristine.testSteps || '').trim() !== (node.item.testSteps || '').trim()
                    || (pristine.expectedResult || '').trim() !== (node.item.expectedResult || '').trim();
        }
    }

    canSubmitTreeNode(node: TreeNode<TestCase>, field: string): boolean {
        let isValidField = false;

        switch (field) {
            case 'setupId':
            case 'preconditionId':
            case 'title':
            case 'description':
            case 'testSteps':
            case 'expectedResult':
            case 'isDraft':
            case null:
                isValidField = true;
                break;
        }

        if (!isValidField) {
            return false;
        }

        switch (field) {
            case 'setupId':
            case 'preconditionId':
            case 'description':
            case 'testSteps':
            case 'expectedResult':
            case 'isDraft':
                return true;
            case 'title':
            default:
                return (node.item.title || '').trim().length > 0;
        }
    }

    addDraftTreeNode(isGroup: boolean): void {
        this.createState = true;
        const parentNode = this.testCaseList.get(this.lastNodeId);
        this.resetActiveTreeNode();
        const newNode = this.initializeTreeNode(parentNode, isGroup);
        if (parentNode) {
            this.testCaseService.addDraftTestCase(newNode, parentNode);
        } else {
            this.testCaseService.addDraftTestCase(newNode);
        }

        this.cdr.detectChanges();
        this.windowRefService.focusElementById('input-testcase-0', 100);

    }

    revertTreeNode(node: TreeNode<TestCase>): void {
        const pristine = this.pristineTestCaseMap.get(node.item.id);
        node.item.setupId = pristine ? pristine.setupId : null;
        node.item.title = pristine ? pristine.title : '';
        node.item.description = pristine ? pristine.description : null;
        node.item.testSteps = pristine ? pristine.testSteps : '';
        node.item.expectedResult = pristine ? pristine.expectedResult : '';
        this.activeNode = null;
        this.activeNodeId = -1;
        this.newSetupTitle = '';
        this.cdr.detectChanges();
    }

    revertTreeNodeTitle(node: TreeNode<TestCase>): void {
        const pristine = this.pristineTestCaseMap.get(node.item.id);
        node.item.title = pristine ? pristine.title : '';
        this.cdr.detectChanges();
    }

    revertTreeNodeDescription(node: TreeNode<TestCase>): void {
        const pristine = this.pristineTestCaseMap.get(node.item.id);
        node.item.description = pristine ? pristine.description : null;
        this.cdr.detectChanges();
    }

    revertTreeNodeTestSteps(node: TreeNode<TestCase>): void {
        const pristine = this.pristineTestCaseMap.get(node.item.id);
        node.item.testSteps = pristine ? pristine.testSteps : '';
        this.cdr.detectChanges();
    }

    revertTreeNodeExpectedResult(node: TreeNode<TestCase>): void {
        const pristine = this.pristineTestCaseMap.get(node.item.id);
        node.item.expectedResult = pristine ? pristine.expectedResult : '';
        this.cdr.detectChanges();
    }

    invalidateTreeNode(node: TreeNode<TestCase>, field?: string, value?: string): void {
        const pristine = this.pristineTestCaseMap.get(node.item.id);
        let isValidField = false;

        switch (field) {
            case 'parentId':
            case 'setupId':
            case 'preconditionId':
            case 'title':
            case 'description':
            case 'testSteps':
            case 'expectedResult':
            case 'isDraft':
            case null:
                isValidField = true;
                break;
        }

        if ((!field || field === 'title') && value) {
            node.item.title = value.trim();

            if ((node.item.title || '').trim().length === 0) {
                node.item.title = pristine ? pristine.title : '';
                return;
            }
        }

        if (!(this.treeNodeChanged(node, field) && this.canSubmitTreeNode(node, field))) {
            return;
        }

        node.item.id === 0 ? this.createTestCase(node) : this.updateTestCase(node, field);
    }

    resetActiveTreeNode(): void {
        this.activeNode = null;
        this.activeNodeId = -1;
        this.activeQueryId = null;
        this.newSetupTitle = '';
        this.cdr.detectChanges();
    }

    // walkTreeRootNode(root: TreeNode<TestCase>): TreeNode<TestCase> {
    //     root.children.forEach((item, index) => {
    //         if (item.item.id === 0) {
    //             root.children.splice(index, 1);
    //         }
    //         item.children.forEach((itm, indx) => {
    //             if (item.children[indx].item.id === 0) {
    //                 item.children.splice(indx, 1);
    //             } else {
    //                 item.children[indx] = this.walkTreeRootNode(itm);
    //             }
    //         });
    //     });
    //     return root;
    // }

    // walkTestCaseNode(root: TreeNode<TestCase>, id: number): TreeNode<TestCase> {
    //     if (root.item.id === id) {
    //         return root;
    //     }
    //     root.children.forEach((item, index) => {
    //         if (item.item.id === id) {
    //             return item;
    //         }
    //
    //         item.children.forEach((itm, indx) => {
    //             if (item.children[indx].item.id === id) {
    //                 return item;
    //             } else {
    //                 return this.walkTestCaseNode(itm, id);
    //             }
    //         });
    //     });
    // }

    private findParentWithValidSetup(node: TreeNode<TestCase>): TreeNode<TestCase> {
        let itemId = node.item.id;

        while (node && !node.item.setupId) {
            itemId = node.item.parentId;
            node = this.testCaseList.get(itemId);
        }

        return node || null;
    }

    treeNodeSetupSteps(node: TreeNode<TestCase>): Array<SetupStep> {
        const result = [];
        let setupId = node.item.setupId ? node.item.setupId : this.findParentWithValidSetup(node) ? this.findParentWithValidSetup(node).item.setupId : 0,
            setupItem: SetupStep;

        while (setupId) {
            setupItem = this.flatSetup.get(setupId);
            result.unshift(setupItem ? setupItem : null);
            setupId = setupItem ? setupItem.parentId : null;
        }
        return result;
    }


    private findParentWithValidPrecondition(node: TreeNode<TestCase>): TreeNode<TestCase> {
        let itemId = node.item.id;

        while (node && !node.item.preconditionId) {
            itemId = node.item.parentId;
            node = this.testCaseList.get(itemId);
        }

        return node || null;
    }

    treeNodePreconditionSteps(node: TreeNode<TestCase>): Array<SetupStep> {
        const result = new Array<SetupStep>();

        let preconditionId = node.item.preconditionId
            ? node.item.preconditionId
            : this.findParentWithValidPrecondition(node)
                ? this.findParentWithValidPrecondition(node).item.preconditionId
                : 0,
            setupItem: SetupStep;

        while (preconditionId) {
            setupItem = this.flatSetup.get(preconditionId);
            result.unshift(setupItem ? setupItem : null);
            preconditionId = setupItem ? setupItem.parentId : null;
        }

        return result;
    }

    setActiveTreeNode(node: TreeNode<TestCase>): void {
    console.log('set active node');
        if (node.item.id) {
            this.router.navigate([],
                {
                    queryParams: {
                        'id': node.item.id,
                    },
                    replaceUrl: true,
                    queryParamsHandling: 'merge',
                });
        }
        this.activeNode = node;
        this.activeNodeId = node.item.id;
        this.newSetupTitle = '';
        this.inheritedSetupTestCase = null;
        this.inheritedPreconditionTestCase = null;

        if (!this.activeNode.item.setupId) {
            const parentNode = this.findParentWithValidSetup(node);
            this.inheritedSetupTestCase = parentNode ? parentNode.item : null;
        }

        if (!this.activeNode.item.preconditionId) {
            const parentNode = this.findParentWithValidPrecondition(node);
            this.inheritedPreconditionTestCase = parentNode ? parentNode.item : null;
        }

        this.activeNodeSetup = this.treeNodeSetupSteps(node);
        this.activeNodePrecondition = this.treeNodePreconditionSteps(this.activeNode);
        if (this.attachments.has(this.activeNode.item.id)) {
            this.activeFileAttachments = this.attachments.get(this.activeNode.item.id);
        }

        this.getLinkedAttachments(this.activeNode.item.id);
        this.showMoreParamMap.clear();
        this.paramTypeMap.clear();
        this.activeNodeSetup.forEach(setup => {
            if (setup) {
                this.childParamArray = [];
                this.showMoreParamMap.set(setup.id, false);
                this.paramTypeMap.set(setup.id, this.activeNode.item.specs.find(spec => spec.setupId === setup.id) ? this.activeNode.item.specs.find(spec => spec.setupId === setup.id).specType : null);
                this.flatteringTree(this.parameterList.get(setup.parameterId), 0);
                this.levelParamMap.set(setup.id, this.childParamArray);
            }
        });
        this.cdr.detectChanges();
    }

    showBrowsePreconditionDialog(node: TreeNode<TestCase>, startSetup: SetupStep = null): void {
        const inheritedSetupId = this.inheritedSetupTestCase ? this.inheritedSetupTestCase.setupId : null;
        const inheritedPreconditionId = this.inheritedPreconditionTestCase ? this.inheritedPreconditionTestCase.preconditionId : null;

        const dialogRef = this.dialog.open(BrowseSetupDialogComponent, {
            width: '850px',
            data: {
                project: this.project,
                testCaseId: this.activeNode.item.id,
                setupId: node.item.setupId,
                preconditionId: node.item.preconditionId,
                inheritedSetupId: inheritedSetupId,
                inheritedPreconditionId: inheritedPreconditionId,
                startSetup: startSetup,
                setPrecondition: true,
            }
        });

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe((result: null | number | any) => {
                if (result === null || typeof result === 'number') {
                    this.activeNodePrecondition = this.treeNodePreconditionSteps(node);
                    this.nodeWithSamePreconditionMap.clear();
                    this.getNodeWithSamePrecondition(node, node.item.preconditionId);

                    if (node.children.length > 0 && this.nodeWithSamePreconditionMap.size > 1) {
                        this.showPreconditionChangedDialog(node, result);
                    } else {
                        node.item.preconditionId = result;
                        this.invalidateTreeNode(node, 'preconditionId');
                    }

                    this.cdr.detectChanges();
                }
            });
    }

    removePreconditionStep(precondition: SetupStep): void {
        this.activeNode.item.preconditionId = precondition.parentId;
        this.activeNodePrecondition = this.treeNodePreconditionSteps(this.activeNode);
        this.invalidateTreeNode(this.activeNode, 'preconditionId');
        this.cdr.detectChanges();
    }

    clearPrecondition(): void {
        this.activeNode.item.preconditionId = null;
        this.activeNodePrecondition = [];
        this.invalidateTreeNode(this.activeNode, 'preconditionId');
        this.cdr.detectChanges();
    }

    showBrowseSetupDialog(node: TreeNode<TestCase>, startSetup: SetupStep = null): void {
        const inheritedSetupId = this.inheritedSetupTestCase ? this.inheritedSetupTestCase.setupId : null;
        const inheritedPreconditionId = this.inheritedPreconditionTestCase ? this.inheritedPreconditionTestCase.preconditionId : null;

        const dialogRef = this.dialog.open(BrowseSetupDialogComponent, {
            width: '850px',
            data: {
                project: this.project,
                testCaseId: this.activeNode.item.id,
                setupId: node.item.setupId,
                preconditionId: node.item.preconditionId,
                inheritedSetupId: inheritedSetupId,
                inheritedPreconditionID: inheritedPreconditionId,
                startSetup: startSetup,
                setSetup: true,
            }
        });

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe((result: null | number | any) => {
                if (result === null || typeof result === 'number') {

                    this.nodeWithSameSetupMap.clear();
                    this.getNodeWithSameSetup(node, node.item.setupId);

                    if (node.children.length > 0 && this.nodeWithSameSetupMap.size > 1) {
                        this.showSetupChangedDialog(node, result);
                    } else {
                        node.item.setupId = result;
                        this.activeNodeSetup = this.treeNodeSetupSteps(node);
                        this.invalidateTreeNode(node, 'setupId');
                    }

                    this.cdr.detectChanges();
                }
            });
    }

    removeSetupStep(setup: SetupStep): void {
        this.activeNode.item.setupId = setup.parentId;
        this.activeNodeSetup = this.treeNodeSetupSteps(this.activeNode);
        this.invalidateTreeNode(this.activeNode, 'setupId');
        this.cdr.detectChanges();
    }

    clearSetup(): void {

        const removeParameters = this.activeNode.item.specs.map(
            spec => this.hyperionTestCaseService.despecializeTestCase(new CommonProtocol.Empty(), this.activeNode.item.id, spec.setupId));

        if (removeParameters.length > 0) {
            combineLatest(removeParameters)
                .pipe(takeUntil(this.destroy$))
                .subscribe(resultOfDeleteParameter => {
                    if (resultOfDeleteParameter.every(item => item instanceof TestCase)) {
                        this.activeNode.item.setupId = null;
                        this.activeNodeSetup = [];
                        this.invalidateTreeNode(this.activeNode, 'setupId');
                    } else {
                        this.notificationService.error('Can\'t remove specialized parameter');
                    }
                });
        } else {
            this.activeNode.item.setupId = null;
            this.activeNodeSetup = [];
            this.invalidateTreeNode(this.activeNode, 'setupId');
        }

        this.cdr.detectChanges();
    }

    getOrderNum(targetNode: TreeNode<TestCase>, dropType: DropPlace): number {
        switch (dropType) {
            case DropPlace.Before: {
                if (targetNode.item.parentId) {
                    const index = this.testCaseList.get(targetNode.item.parentId).children.indexOf(targetNode);

                    if (index === 0) {
                        return 1000;
                    } else {
                        return Math.round((targetNode.item.orderNum + this.testCaseList.get(targetNode.item.parentId).children[index - 1].item.orderNum) / 2);
                    }
                } else {
                    const index = [...this.rootLevelTestCase.entries()]
                        .map(elem => elem[1]).findIndex(elem => elem.id === targetNode.item.id);

                    if (index === 0) {
                        return 1000;
                    } else {
                        return Math.round((targetNode.item.orderNum +
                            [...this.rootLevelTestCase.entries()]
                                .map(elem => elem[1])[index - 1].orderNum) / 2);                    }
                }
            }

            case DropPlace.After: {
                if (targetNode.item.parentId) {
                    const index = this.testCaseList.get(targetNode.item.parentId).children.indexOf(targetNode);

                    if (index === this.testCaseList.get(targetNode.item.parentId).children.length - 1) {
                        return targetNode.item.orderNum + 1000;
                    } else {
                        return Math.round((targetNode.item.orderNum + this.testCaseList.get(targetNode.item.parentId).children[index + 1].item.orderNum) / 2);
                    }
                } else {
                    const index = [...this.rootLevelTestCase.entries()]
                        .map(elem => elem[1]).findIndex(elem => elem.id === targetNode.item.id);
                    if (index === this.rootLevelTestCase.size - 1) {
                        return targetNode.item.orderNum + 1000;
                    } else {
                        return Math.round((targetNode.item.orderNum +
                            [...this.rootLevelTestCase.entries()]
                                .map(elem => elem[1])[index + 1].orderNum) / 2);
                    }
                }
            }
        }
    }

    getNodeWithSameSetup(node: TreeNode<TestCase>, setupId: number): void {
        if (node.item.setupId === setupId) {
            this.nodeWithSameSetupMap.set(node.item.id, node);
        }

        node.children.forEach(child => this.getNodeWithSameSetup(child, setupId));
    }

    showSetupChangedDialog(node: TreeNode<TestCase>, setupId: number): void {
        const pristine = this.pristineTestCaseMap.get(node.item.id),
            childrenTitles = Array.from(this.nodeWithSameSetupMap)
                .filter(elem => elem[0] !== node.item.id)
                .map(child => child[1].item.title).join(',');

        node.item.setupId = setupId;

        if (setupId !== pristine.setupId && this.nodeWithSameSetupMap.size > 1) {
            Swal
                .fire({
                    title: 'Setup changed',
                    text: 'Do you want change setup for following children (' + childrenTitles + ')',
                    type: 'question',
                    showCancelButton: true,
                    confirmButtonText: 'Yes',
                    cancelButtonText: 'No'
                } as SweetAlertOptions)
                .then(result => {
                    if (result.value) {
                        this.nodeWithSameSetupMap.forEach(elem => {
                            elem.item.setupId = node.item.setupId;
                            this.updateTestCase(elem, 'setupId');
                        });

                        this.updateTestCase(node, 'setupId');
                    }
                });

            this.setActiveTreeNode(this.testCaseList.get(node.item.id));
        }
    }

    getNodeWithSamePrecondition(node: TreeNode<TestCase>, preconditionId: number): void {
        if (node.item.preconditionId === preconditionId) {
            this.nodeWithSamePreconditionMap.set(node.item.id, node);
        }

        node.children.forEach(child => this.getNodeWithSamePrecondition(child, preconditionId));
    }

    showPreconditionChangedDialog(node: TreeNode<TestCase>, preconditionId: number): void {
        const pristine = this.pristineTestCaseMap.get(node.item.id);
        const childrenTitles = Array.from(this.nodeWithSamePreconditionMap)
            .filter(elem => elem[0] !== node.item.id)
            .map(child => child[1].item.title).join(',');

        node.item.preconditionId = preconditionId;

        if (preconditionId !== pristine.preconditionId && this.nodeWithSamePreconditionMap.size > 1) {
            Swal
                .fire({
                    title: 'Precondition changed',
                    text: 'Do you want change precondition for following children (' + childrenTitles + ')',
                    type: 'question',
                    showCancelButton: true,
                    confirmButtonText: 'Yes',
                    cancelButtonText: 'No'
                } as SweetAlertOptions)
                .then(result => {
                    if (result.value) {
                        this.nodeWithSameSetupMap.forEach(elem => {
                            elem.item.preconditionId = node.item.preconditionId;
                            this.updateTestCase(elem, 'preconditionId');
                        });

                        this.updateTestCase(node, 'preconditionId');
                    }
                });

            this.setActiveTreeNode(this.testCaseList.get(node.item.id));
        }
    }

    // File attachments drag'n'drop
    public dropped(files: NgxFileDropEntry[]) {
        for (const droppedFile of files) {
            if (droppedFile.fileEntry.isFile) {
                const fileEntry = droppedFile.fileEntry as FileSystemFileEntry,
                    subj$ = new BehaviorSubject(0);

                this.attachmentsProgress.set(droppedFile.fileEntry.name, subj$);

                fileEntry.file((file: File) => {
                    this.uploadService
                        .uploadAttachment(file, AttachmentOwner.TestCase, this.activeNode.item.id)
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

    public fileOver(event) {
    }

    public fileLeave(event) {
    }

    // REST API

    createTestCase(node: TreeNode<TestCase>): void {
        const body = new TestCaseProtocol.CreateTestCaseRequest();
        body.parentId = node.item.parentId;
        body.setupId = node.item.setupId;
        body.preconditionId = node.item.preconditionId;
        body.isGroup = node.item.isGroup;
        body.title = node.item.title === null ? null : node.item.title.trim();
        body.description = node.item.description === null ? null : node.item.description.trim();
        body.testSteps = node.item.testSteps.trim();
        body.expectedResult = node.item.expectedResult.trim();
        body.isDraft = false;
        body.orderNum = node.item.orderNum;

        this.hyperionTestCaseService
            .createTestCase(body, node.item.projectId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.activeQueryId = response.id;
                    this.createState = false;
                    this.setActiveTreeNode(<TreeNode<TestCase>>{
                        children: [],
                        item: {
                            description: response.description,
                            expectedResult: response.expectedResult,
                            id: response.id,
                            isDraft: response.isDraft,
                            isGroup: response.isGroup,
                            orderNum: response.orderNum,
                            parentId: response.parentId,
                            preconditionId: response.preconditionId,
                            projectId: response.projectId,
                            rev: response.rev,
                            setupId: response.setupId,
                            specs: response.specs,
                            testSteps: response.testSteps,
                            title: response.title,
                            createdAt: response.createdAt,
                            updatedAt: response.updatedAt,
                        }
                    });

                    this.testCaseService.clearDraftNode(node.item.parentId);
                    this.cdr.detectChanges();
                    this.notificationService.success(`Test ${node.item.isGroup ? 'group' : 'case'} created`);
                },
                error => {
                    if (error instanceof BadRequestError) {
                        let errorMessage: string = null;

                        switch (error.error as TestCaseProtocol.TestCaseError) {
                            case TestCaseProtocol.TestCaseError.ParentNotExists:
                                errorMessage = 'Parent test case does not exist';
                                break;
                            case TestCaseProtocol.TestCaseError.ProjectNotExists:
                                errorMessage = 'Project does not exist';
                                break;
                            case TestCaseProtocol.TestCaseError.PreconditionNotExists:
                                errorMessage = 'Precondition setup step does not exists';
                                break;
                            case TestCaseProtocol.TestCaseError.SetupNotExists:
                                errorMessage = 'Setup step does not exist';
                                break;
                            case TestCaseProtocol.TestCaseError.TitleAlreadyExists:
                                errorMessage = 'Test case title already exists';
                                break;
                            case TestCaseProtocol.TestCaseError.HasChildren:
                                errorMessage = 'Setup step has children';
                                break;
                        }

                        this.notificationService.error(errorMessage || error);
                    } else {
                        this.notificationService.error(error);
                    }
                }
            );
    }

    updateTestCase(node: TreeNode<TestCase>, field?: string, parentId?: number, orderNum?: number): void {
        const body = new TestCaseProtocol.UpdateTestCaseRequest(),
            pristine = this.pristineTestCaseMap.get(node.item.id);

        body.parentId = parentId
            ? ((parentId === -1) ? null : parentId)
            : node.item.parentId !== pristine.parentId
                ? node.item.parentId
                : pristine.parentId;

        body.orderNum = orderNum
            ? orderNum
            : node.item.orderNum !== pristine.orderNum
                ? node.item.orderNum
                : pristine.orderNum;
        if (node.item.setupId !== pristine.setupId && (field === null || field === 'setupId')) body.setupId = node.item.setupId;
        if (node.item.preconditionId !== pristine.preconditionId && (field === null || field === 'preconditionId')) body.preconditionId = node.item.preconditionId;
        if (node.item.title !== pristine.title && (field === null || field === 'title')) body.title = node.item.title;
        if (node.item.description !== pristine.description && (field === null || field === 'description')) body.description = node.item.description;
        if (node.item.testSteps !== pristine.testSteps && (field === null || field === 'testSteps')) body.testSteps = node.item.testSteps;
        if (node.item.expectedResult !== pristine.expectedResult && (field === null || field === 'expectedResult')) body.expectedResult = node.item.expectedResult;
        if (node.item.isDraft !== pristine.isDraft && (field === null || field === 'isDraft')) body.isDraft = node.item.isDraft;

        this.hyperionTestCaseService
            .updateTestCase(body, node.item.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.notificationService.success(`Test ${node.item.isGroup ? 'group' : 'case'} updated`);
                    this.activeNodeSetup
                        .filter(setup => setup.parameterId && response.specs.findIndex(param => param.setupId === setup.id) === -1)
                        .forEach(setup => {
                            this.specializeParam(this.specTypeEnum.Source + '|' + this.flatParameter.get(setup.parameterId).id, setup.id);
                        });
                    this.cdr.detectChanges();
                },
                error => {
                    if (error instanceof BadRequestError) {
                        let errorMessage: string = null;

                        switch (error.error as TestCaseProtocol.TestCaseError) {
                            case TestCaseProtocol.TestCaseError.ParentNotExists:
                                errorMessage = 'Parent test case does not exist';
                                break;
                            case TestCaseProtocol.TestCaseError.ProjectNotExists:
                                errorMessage = 'Project does not exist';
                                break;
                            case TestCaseProtocol.TestCaseError.PreconditionNotExists:
                                errorMessage = 'Precondition setup step does not exists';
                                break;
                            case TestCaseProtocol.TestCaseError.SetupNotExists:
                                errorMessage = 'Setup step does not exist';
                                break;
                            case TestCaseProtocol.TestCaseError.TitleAlreadyExists:
                                errorMessage = 'Test case title already exists';
                                break;
                            case TestCaseProtocol.TestCaseError.HasChildren:
                                errorMessage = 'Setup step has children';
                                break;
                        }

                        this.notificationService.error(errorMessage || error);
                    } else {
                        this.notificationService.error(error);
                    }
                }
            );
    }

    deleteTestCase(node: TreeNode<TestCase>): void {
        if (node === this.activeNode) {
            this.activeNode = null;
            this.activeNodeId = -1;
        }

        if (node.item.id === 0) {
            this.testCaseService.clearDraftNode(node.item.parentId ? node.item.parentId : null);

            this.cdr.detectChanges();
            return;
        }

        const removeParameters = node.item.specs.map(spec => this.hyperionTestCaseService.despecializeTestCase(new CommonProtocol.Empty(), node.item.id, spec.setupId));

        if (removeParameters.length > 0) {
            combineLatest(removeParameters)
                .pipe(switchMap(resultOfDeleteParameter => {
                    if (resultOfDeleteParameter.every(item => item instanceof TestCase)) {
                        return this.hyperionTestCaseService
                            .deleteTestCase(node.item.id)
                            .pipe(takeUntil(this.destroy$));
                    } else {
                        return throwError(new BadRequestError());
                    }
                }))
                .subscribe(
                    response => {
                        this.activeNode = null;
                        this.activeNodeId = -1;
                        this.notificationService.success(`Test ${node.item.isGroup ? 'group' : 'case'} deleted`);
                        this.cdr.detectChanges();
                    },
                    error => {
                        if (error instanceof BadRequestError && error.error === TestCaseProtocol.TestCaseError.HasChildren) {
                            this.notificationService.warning(`Test ${node.item.isGroup ? 'group' : 'case'} can not be deleted because it is used elsewhere`);
                        } else if (error instanceof NotFoundError) {
                            this.notificationService.warning(`Test ${node.item.isGroup ? 'group' : 'case'} is not found. Maybe it was deleted already`);

                            if (this.activeNode && node.item.id === this.activeNode.item.id) {
                                this.activeNode = null;
                                this.activeNodeId = -1;
                            }

                            this.cdr.detectChanges();
                        } else {
                            this.notificationService.error(error);
                        }
                    }
                );
        } else {
            this.hyperionTestCaseService
                .deleteTestCase(node.item.id)
                .pipe(takeUntil(this.destroy$))
                .subscribe(
                    response => {
                        this.activeNode = null;
                        this.activeNodeId = -1;
                        this.notificationService.success(`Test ${node.item.isGroup ? 'group' : 'case'} deleted`);
                        this.cdr.detectChanges();
                    },
                    error => {
                        if (error instanceof BadRequestError && error.error === TestCaseProtocol.TestCaseError.HasChildren) {
                            this.notificationService.warning(`Test ${node.item.isGroup ? 'group' : 'case'} can not be deleted because it is used elsewhere`);
                        } else if (error instanceof NotFoundError) {
                            this.notificationService.warning(`Test ${node.item.isGroup ? 'group' : 'case'} is not found. Maybe it was deleted already`);

                            if (this.activeNode && node.item.id === this.activeNode.item.id) {
                                this.activeNode = null;
                                this.activeNodeId = -1;
                            }

                            this.cdr.detectChanges();
                        } else {
                            this.notificationService.error(error);
                        }
                    }
                );
        }
    }

    // Attachments REST
    private getLinkedAttachments(testCaseId: number): void {
        if (testCaseId === 0) return;
        this.hyperionAttachmentService
            .getLinkedAttachments(AttachmentOwner.TestCase, testCaseId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.attachments.set(testCaseId, response.items);
                    this.activeFileAttachments = response.items;
                    this.cdr.detectChanges();
                },
                error => this.notificationService.error(error)
            );
    }

    unlinkFileAttachment(attachmentId: number, linkedId: number): void {
        this.hyperionAttachmentService
            .deleteAttachmentLink(attachmentId, AttachmentOwner.TestCase, linkedId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.activeFileAttachments = this.activeFileAttachments.filter(fa => fa.id !== attachmentId);
                    this.cdr.detectChanges();
                },
                error => this.notificationService.error(error)
            );
    }

    // Callbacks
    expandAllTreeNodes(): void {
        this.dynamicTree.expandTreeNodes();
    }

    collapseAllTreeNodes(): void {
        this.dynamicTree.collapseTreeNodes();
    }

    // canDropInto(draggedNode: TreeNode<TestCase>, targetNode: TreeNode<TestCase>): boolean {
    //     if (draggedNode && targetNode) {
    //         const verifyChildrenNode = (dragged: TreeNode<TestCase>, target: TreeNode<TestCase>): boolean => {
    //             return target.item.parentId === dragged.item.id
    //                 ? false
    //                 : target.item.parentId
    //                     ? verifyChildrenNode(dragged, this.nodeMap.get(target.item.parentId))
    //                     : true;
    //         };
    //
    //         return draggedNode.item.parentId !== targetNode.item.id && verifyChildrenNode(draggedNode, targetNode) && targetNode.item.isGroup;
    //     } else {
    //         return;
    //     }
    // }

    // canDropAfter(draggedNode: TreeNode<TestCase>, targetNode: TreeNode<TestCase>) {
    //     if (draggedNode && targetNode) {
    //         const verifyChildrenNode = (dragged: TreeNode<TestCase>, target: TreeNode<TestCase>): boolean => {
    //             return target.item.parentId === dragged.item.id
    //                 ? false
    //                 : target.item.parentId
    //                     ? verifyChildrenNode(dragged, this.nodeMap.get(target.item.parentId))
    //                     : true;
    //         };
    //
    //         return draggedNode.item.id !== targetNode.item.id && verifyChildrenNode(draggedNode, targetNode);
    //     } else {
    //         return;
    //     }
    // }

    // canDropBefore(draggedNode: TreeNode<TestCase>, targetNode: TreeNode<TestCase>) {
    //     if (draggedNode && targetNode) {
    //         const verifyChildrenNode = (dragged: TreeNode<TestCase>, target: TreeNode<TestCase>): boolean => {
    //             return target.item.parentId === dragged.item.id
    //                 ? false
    //                 : target.item.parentId
    //                     ? verifyChildrenNode(dragged, this.nodeMap.get(target.item.parentId))
    //                     : true;
    //         };
    //
    //         return draggedNode.item.id !== targetNode.item.id && verifyChildrenNode(draggedNode, targetNode);
    //     } else {
    //         return;
    //     }
    // }

    filterCallback(item: TestCase, filterText: string): boolean {
        if (!filterText)
            return true;

        return item.title.toLocaleLowerCase().indexOf(filterText.toLocaleLowerCase()) > -1;
    }

    orderingCallback(firstNode: TreeNode<TestCase>, secondNode: TreeNode<TestCase>): number {
        return firstNode.item.orderNum > secondNode.item.orderNum ? 1 : -1;
    }

    updateAfterDrop(event: { dropType: DropPlace, draggedNode: TreeNode<TestCase>, targetNode: TreeNode<TestCase> }): void {
        const dropType = event.dropType,
            draggedNode = event.draggedNode,
            targetNode = event.targetNode,
            pristine = this.pristineTestCaseMap.get(draggedNode.item.id),
            prevParent = this.pristineTestCaseMap.get(pristine.parentId);
        let orderNum = null,
            parentId = null;


        switch (dropType) {
            case DropPlace.Before:
            case DropPlace.After:
                parentId = targetNode.item.parentId;
                orderNum = this.getOrderNum(targetNode, dropType);
                break;
            case DropPlace.Into:
                parentId = targetNode.item.id;
                orderNum = targetNode.children.length ? targetNode.children.slice(-1)[0].item.orderNum + 1000 : 1000;
                break;
            case DropPlace.Root:
                parentId = -1;
                draggedNode.item.orderNum = [...this.rootLevelTestCase.entries()]
                    .map(elem => elem[1])
                    .sort((a, b) => a.orderNum > b.orderNum ? 1 : -1)
                    .slice(-1)[0].orderNum + 1000;
                break;
        }

        const canDrop = draggedNode.item.parentId !== pristine.parentId
            && prevParent
            && draggedNode.item.parentId
            && (draggedNode.item.setupId === prevParent.setupId || draggedNode.item.preconditionId === prevParent.preconditionId);

        if (canDrop) {
            this.afterDropDialog.fire().then(result => {
                if (result.value) {
                    if (this.nodeSetupChangeCheckbox.checked) {
                        if (draggedNode.item.setupId === prevParent.setupId) draggedNode.item.setupId = targetNode.item.setupId;
                        if (draggedNode.item.preconditionId === prevParent.preconditionId) draggedNode.item.preconditionId = targetNode.item.preconditionId;
                    }

                    if (this.childrenChangeCheckbox.checked) {
                        draggedNode.children
                            .filter(child => child.item.setupId === prevParent.setupId || child.item.preconditionId === prevParent.preconditionId)
                            .forEach(child => {
                                if (child.item.setupId === prevParent.setupId) {
                                    child.item.setupId = targetNode.item.setupId;
                                }

                                if (child.item.preconditionId === prevParent.preconditionId) {
                                    child.item.preconditionId = targetNode.item.preconditionId;
                                }

                                this.updateTestCase(child, null);
                            });
                    }
                }

                this.updateTestCase(draggedNode, null, parentId, orderNum);
            });
        } else {
            this.updateTestCase(draggedNode, null, parentId, orderNum);
        }
    }

    showNotification(event: { type: NotificationType, message: string }): void {
        const type = event.type,
            message = event.message;
        switch (type) {
            case NotificationType.Success:
                this.notificationService.success(message);
                break;
            case NotificationType.Info:
                this.notificationService.info(message);
                break;
            case NotificationType.Warning:
                this.notificationService.warning(message);
                break;
            case NotificationType.Error:
                this.notificationService.error(message);
                break;
        }
    }

    getSpecValue(setupId: number): string | boolean {
        if (this.activeNode.item.specs.find(item => item.setupId === setupId)) {
            const specItem = this.activeNode.item.specs.find(item => item.setupId === setupId);
            switch (specItem.specType) {
                case SpecType.Any:
                    return SpecType.Any + '|any';
                case SpecType.Random:
                    return SpecType.Random + '|random';
                case SpecType.Value:
                    return SpecType.Value + '|' + specItem.value;
                case SpecType.Source:
                    return SpecType.Source + '|' + specItem.parameterId;
                default:
                    return false;
            }
        } else return false;
    }

    getSpec(setupId: number): TestCaseSpecialization | null {

        if (this.inheritedSetupTestCase) return this.inheritedSetupTestCase.specs.find(item => item.setupId === setupId) || null;
        return this.activeNode.item.specs.find(item => item.setupId === setupId) || null;
    }

    specializeParam(value: string, setupId: number): void {
        // if (value === 'reset') {
        //     if (this.getSpec(setupId)) {
        //         this.hyperionTestCaseService
        //             .despecializeTestCase(new CommonProtocol.Empty(), this.activeNode.item.id, setupId)
        //             .subscribe(result => {
        //                     this.showMoreParamMap.set(setupId, false);
        //                     this.showNotification({ type: NotificationType.Success, message: 'Parameter successfully changed' });
        //                 },
        //                 error => this.showNotification({ type: NotificationType.Error, message: 'Something going wrong' })
        //             );
        //     } else {
        //         this.showMoreParamMap.set(setupId, false);
        //     }
        //     return;
        // }

        const mapParamDepend = new Map();
        this.activeNode.item.specs.forEach((itm, ind) => {
            mapParamDepend.set(itm.setupId, this.flatParameter.get(this.flatSetup.get(itm.setupId).parameterId));
        });
        const request2 = new TestCaseProtocol.SpecializeTestCaseRequest();
        mapParamDepend.forEach((value, key) => {
            if (value.dependentId === this.flatSetup.get(setupId).parameterId) {
                this.activeNode.item.specs.forEach((item, index) => {
                    if (item.setupId === key) {
                        request2.specType = SpecType.Source;
                        request2.value = null;
                        request2.parameterId = value.id;
                        this.hyperionTestCaseService
                            .specializeTestCase(request2, this.activeNode.item.id, key)
                            .subscribe(result => {
                                    // const updatedNode = this.itemNodes.find(elem => elem.item.id === result.id);
                                    // updatedNode.item = result;
                                    // this.nodeMap.set(result.id, updatedNode);
                                    // this.showMoreParamMap.set(key, false);
                                    this.showNotification({ type: NotificationType.Success, message: 'Parameter successfully changed' });
                                },
                                error => this.showNotification({ type: NotificationType.Error, message: 'Something going wrong' })
                            );
                    }
                });
            }
        });


        const request = new TestCaseProtocol.SpecializeTestCaseRequest(),
            eventValue = { specType: value.split('|')[0], value: value.split('|')[1] };


        switch (eventValue.specType) {
            case '3':
                request.specType = SpecType.Any;
                break;
            case '4':
                request.specType = SpecType.Random;
                break;
            case '1':
                request.specType = SpecType.Value;
                request.value = eventValue.value + '';
                break;
            case '2':
                request.specType = SpecType.Source;
                request.parameterId = +eventValue.value;
                break;
        }

        this.hyperionTestCaseService
            .specializeTestCase(request, this.activeNode.item.id, setupId)
            .subscribe(result => {
                    // const updatedNode = this.nodeMap.get(result.id);
                    // updatedNode.item = result;
                    // this.nodeMap.set(result.id, updatedNode);
                    // this.showMoreParamMap.set(setupId, false);
                    this.showNotification({ type: NotificationType.Success, message: 'Parameter successfully changed' });
                },
                error => this.showNotification({ type: NotificationType.Error, message: 'Something going wrong' })
            );
    }

    despecializeParam(setupId: number): void {
        const request = new CommonProtocol.Empty();
        this.hyperionTestCaseService.despecializeTestCase(request, this.activeNode.item.id, setupId).subscribe(result => {
                this.showMoreParamMap.set(setupId, false);
                this.showNotification({ type: NotificationType.Success, message: 'Parameter successfully changed' });
            },
            error => this.showNotification({ type: NotificationType.Error, message: 'Something going wrong' }));
    }

    flatteringTree(rootNode: TreeNode<Parameter>, level: number): void {
        if (!rootNode) return;
        level++;
        const tmpNode = new TreeNode<Parameter>();
        tmpNode.item = new Parameter();
        tmpNode.item.title = '';
        tmpNode.item.values = rootNode.item.values;
        tmpNode.item.id = rootNode.item.id;
        tmpNode.item.parentId = rootNode.item.parentId;
        tmpNode.item.projectId = rootNode.item.projectId;
        tmpNode.children = rootNode.children;
        tmpNode.item.title = '&nbsp;&nbsp;&nbsp;&nbsp;'.repeat(level - 1) + rootNode.item.title;
        this.childParamArray.push({ level: level, item: tmpNode.item });
        rootNode.children.forEach(child => this.flatteringTree(child, level));
    }

    getParameters(id: number): Array<ParameterValue> {
        const dependentSetup = [...this.flatSetup.values()].filter(setup => setup.parameterId).find(setup => setup.parameterId === this.flatParameter.get(id).dependentId),
            specValueDependentParam = dependentSetup ? this.activeNode.item.specs.find(spec => spec.setupId === dependentSetup.id) : null;

        return (specValueDependentParam && specValueDependentParam.value)
            ? this.flatParameter.get(id).values.filter(paramValue => paramValue.dependentValue === specValueDependentParam.value)
            : this.flatParameter.get(id).values;
    }

    focusTestCase() {
        if (this.activeQueryId) {
            this.ready$
                .asObservable()
                .pipe(
                    filter(ready => !!ready),
                    take(1))
                .subscribe(() => {
                    const node = this.testCaseList.get(this.activeQueryId);
                    if (node) {
                        const parentsNodeId = this.getAllParentId(node);
                        this.dynamicTree.expandTreeNodesById(parentsNodeId);

                        setTimeout(() => {
                            const element = this.render.selectRootElement('#input-testcase-' + node.item.id);
                            setTimeout(() => element.focus(), 10);
                            console.log('focus test case');
                            this.setActiveTreeNode(node);
                        }, 20);
                    } else {
                        this.router.navigate([],
                            {
                                queryParams: {
                                    'id': null,
                                },
                                replaceUrl: true,
                                queryParamsHandling: 'merge',
                            });
                    }
                });
        }
    }

    tooltipTextShow(node: TreeNode<TestCase>): string {
        if (node.item.specs.length === 0) return null;
        const setupList = this.treeNodeSetupSteps(node),
            parametersList = setupList.map(setup => setup.parameterId).filter(param => !!param),
            dependentParameters = parametersList.map(param => {
                if (this.flatParameter.get(param).dependentId) return param;
            }).filter(param => !!param);

        if (dependentParameters.map(parameter => parametersList.findIndex(elem => elem === this.flatParameter.get(parameter).dependentId) !== -1)
            .every(hasDependentOnParam => hasDependentOnParam === true)) {
            return null;
        }

        const badParam = dependentParameters.filter(parameter => parametersList.findIndex(elem => elem === this.flatParameter.get(parameter).dependentId) === -1);
        let result = '';
        badParam.forEach(param => {
            result += '[' + this.flatParameter.get(param).title + ']'
                + ' parameter depends on [' +
                this.flatParameter.get(this.flatParameter.get(param).dependentId).title + '] parameter which is not set for any of the parent setup steps \n';
        });

        return result;
    }

    afterTreeRendered(event: boolean): void {
        if (event) {
            this.focusTestCase();
        }
    }
}

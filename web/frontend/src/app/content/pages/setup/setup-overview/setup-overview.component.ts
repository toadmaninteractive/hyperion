import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy, OnInit, ViewChild, ElementRef, Renderer2 } from '@angular/core';
import { HttpEvent, HttpEventType, HttpUploadProgressEvent } from '@angular/common/http';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, combineLatest, merge, Subject } from 'rxjs';
import { filter, finalize, takeUntil, switchMap, tap } from 'rxjs/operators';
import { FileSystemDirectoryEntry, FileSystemFileEntry, NgxFileDropEntry } from 'ngx-file-drop';
import { Lightbox } from 'ngx-lightbox';
import { MatAutocompleteTrigger } from '@angular/material/autocomplete';
import { MatDialog } from '@angular/material/dialog';
import { HyperionAttachmentService } from '../../../../protocol/attachment-protocol.service';
import { HyperionSetupService } from '../../../../protocol/setup-protocol.service';
import { NotificationService, NotificationType } from '../../../../core/services/notification.service';
import { HyperionParameterService } from '../../../../protocol/parameter-protocol.service';
import { UploadService } from '../../../../core/services/upload.service';
import { WebsocketService } from '../../../../core/services/websocket.service';
import { WindowRefService } from '../../../../core/services/window-ref.service';
import { DropPlace } from '../../../../shared/interfaces/drop-place';
import { IBrowseParamData } from '../browse-params-dialog/browse-params-dialog.component';
import { BrowseParamsDialogComponent } from '../browse-params-dialog/browse-params-dialog.component';
import Swal, { SweetAlertOptions } from 'sweetalert2';
import * as CommonProtocol from '../../../../protocol/common-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as NotificationProtocol from '../../../../protocol/notification-protocol.data';
import * as SetupProtocol from '../../../../protocol/setup-protocol.data';
import { BadRequestError, Collection, NotFoundError, TreeNode } from '../../../../protocol/data-protocol.data';
import { AttachmentOwner, FileAttachment, Parameter, PersonnelAccountProfile, Project, SetupStep } from '../../../../protocol/db-protocol.data';
import { AccountService } from '../../../../core/services/account.service';
import { SetupService } from '../../../../core/services/setup.service';
import { DynamicTreeMapComponent } from '../../../../components/general/dynamic-tree-map/dynamic-tree-map.component';
import { ParameterService } from '../../../../core/services/parameter.service';

@Component({
    selector: 'm-setup-page',
    templateUrl: './setup-overview.component.html',
    styleUrls: ['./setup-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})

export class SetupOverviewComponent implements OnInit, OnDestroy {
    @ViewChild(MatAutocompleteTrigger) trigger: MatAutocompleteTrigger;
    @ViewChild(DynamicTreeMapComponent, {static: true}) dynamicTree: DynamicTreeMapComponent;

    destroy$ = new Subject<any>();
    setup$ = new BehaviorSubject<Collection<TreeNode<SetupStep>>>(null);
    ready$ = new BehaviorSubject<boolean>(false);
    project: Project;
    profile: PersonnelAccountProfile;

    pristineSetupStep = new Map<number, SetupStep>();
    setupList: Map<number, TreeNode<SetupStep>> = new Map();
    rootLevelSetupSteps: Map<number, SetupStep> = new Map();

    activeNode?: TreeNode<SetupStep>;
    activeNodeId = -1;

    attachments = new Map<number, FileAttachment[]>();
    activeFileAttachments: FileAttachment[];
    showMoreAttachments = false;
    attachmentsProgress = new Map<string, BehaviorSubject<number>>();
    activeAlbum = [];

    parametersTree = new Map<number, TreeNode<Parameter>>();
    parametersList: Map<number, TreeNode<Parameter>>;
    activeParam: TreeNode<Parameter>;
    activeQueryId: number;
    dimmedNodeSetId = new Set<number>();
    hiddenNodeSetId = new Set<number>();
    filterText: string;

    constructor(
        private route: ActivatedRoute,
        private router: Router,
        private cdr: ChangeDetectorRef,
        private lightbox: Lightbox,
        private hyperionAttachmentService: HyperionAttachmentService,
        private hyperionSetupService: HyperionSetupService,
        private hyperionParameterService: HyperionParameterService,
        private notificationService: NotificationService,
        private render: Renderer2,
        private uploadService: UploadService,
        private websocketService: WebsocketService,
        private windowRefService: WindowRefService,
        private dialog: MatDialog,
        private accountService: AccountService,
        public setupService: SetupService,
        private parameterService: ParameterService,
    ) {
    }

    ngOnInit(): void {
        this.accountService.profile$
            .asObservable()
            .pipe(takeUntil(this.destroy$))
            .subscribe(profile => this.profile = profile);

        combineLatest([
            this.setupService.flatSetup$,
            this.setupService.setupList$,
            this.parameterService.parameterTree$,
            this.parameterService.parameterList$])
            .pipe(
                filter(([fs, sl, pt, pl]) => !!fs && !!sl && !!pt && !!pl),
                takeUntil(this.destroy$))
            .subscribe(([flatSetup, setupList, parameterTree, parameterList]) => {
                this.pristineSetupStep.clear();
                flatSetup.forEach(item => this.pristineSetupStep.set(item.id, SetupStep.fromJson(item.toJson())));
                this.setupList = setupList;
                this.parametersTree = parameterTree;
                this.parametersList = parameterList;
                this.ready$.next(true);
            });

        this.route.data
            .pipe(takeUntil(this.destroy$))
            .subscribe(data => this.initialize(data.activeProject));

        this.setupService.setupTree$
            .asObservable()
            .pipe(
                filter(s => !!s),
                takeUntil(this.destroy$))
            .subscribe(setupTree => {
                    this.rootLevelSetupSteps.clear();
                    setupTree.forEach(setupStep => this.rootLevelSetupSteps.set(setupStep.item.id, setupStep.item));
                }
            );

        combineLatest(this.route.data, this.route.queryParamMap)
            .pipe(
                takeUntil(this.destroy$),
            )
            .subscribe(([activeProject, queryParams]) => {
                this.activeQueryId = parseInt(queryParams.get('id'), 10);
            });

        // Setup step notifications
        merge(
            this.websocketService.setupStepUpdated,
            this.websocketService.setupStepDeleted,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => event.data.projectId === this.project.id),
        ).subscribe(event => {
            const node = new TreeNode<SetupStep>();
            node.item = event.data;
            if (event instanceof NotificationProtocol.SetupStepUpdated) {

                if (this.activeNodeId !== -1 && node.item.id === this.activeNodeId && this.profile.username !== event.actorName) {
                    Swal.fire({
                        type: 'information',
                        title: `Your active node has been changed by ${event.actorName}`,
                        confirmButtonText: 'ok',
                    } as SweetAlertOptions).then((result) => {
                        if (result.value !== undefined) {
                            this.setActiveTreeNode(node);
                        }
                    });
                }
            } else if (event instanceof NotificationProtocol.SetupStepDeleted) {
                if (this.activeNode && node.item.id === this.activeNode.item.id && this.profile.username !== event.actorName) {

                    Swal.fire({
                        type: 'warning',
                        title: `Your active node has been deleted by ${event.actorName}`,
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
            filter(event => event.link.owner === DbProtocol.AttachmentOwner.SetupStep),
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
                if (itemIndex !== -1) {
                    attachments.splice(itemIndex, 1);
                }
            }

            this.attachments.set(event.link.linkedId, attachments);

            if (this.activeNode.item.id && this.activeNode.item.id === event.link.linkedId) {
                this.activeFileAttachments = attachments;
            }

            this.cdr.detectChanges();
        });
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.setup$.complete();
        this.ready$.complete();
    }

    // Page initialization and refresh
    private initialize(project: Project): void {
        this.project = project;
        this.activeNode = null;
        this.activeNodeId = -1;
    }

    openGallery(index: number): void {
        this.activeFileAttachments
            .filter(item => item.contentType.includes('image'))
            .forEach(item => {
                const src = '/download/attachments/' + item.filename,
                    caption = item.originalFilename,
                    thumb = '/download/attachments/' + item.thumbFilename,
                    albumItem = {src: src, caption: caption, thumb: thumb};

                this.activeAlbum.push(albumItem);
            });

        this.lightbox.open(this.activeAlbum, index);
    }

    getAllChildrenId(node: TreeNode<SetupStep>): Array<number> {
        return node.children.reduce(
            (acc, value) => [
                ...acc,
                [value.item.id],
                value.children.map(child => this.getAllChildrenId(child))
                    .reduce((prev, curr) => prev.concat(curr), [])], []).concat(node.item.id)
            .reduce((prev, curr) => prev.concat(curr), []);
    }

    getAllParentId(node: TreeNode<SetupStep>): Array<number> {
        const result = [];
        while (node && node.item.parentId !== 0) {
            const parentNode = this.setupList.get(node.item.parentId);
            result.push(parentNode ? parentNode.item.id : null);
            node = parentNode;
        }
        return result.filter(elem => !!elem);
    }

    applyFilter(filterText: string): void {
        this.dimmedNodeSetId = new Set<number>(Array.from(this.pristineSetupStep).map(item => !this.filterCallback(item[1], filterText) ? item[1].id : null));
        this.hiddenNodeSetId = new Set<number>(Array.from(this.setupList).filter(node => this.getAllChildrenId(node[1]).every(id => this.dimmedNodeSetId.has(id)))
            .map(node => node[1].item.id));
        this.expandAllTreeNodes();
    }

    showChildrenBeforeAddDraftNode(node?: TreeNode<SetupStep>): void {
        if (node) {
            node.children.forEach(child => this.hiddenNodeSetId.delete(child.item.id));
        } else {
            this.hiddenNodeSetId.forEach(elem => {
                if (this.pristineSetupStep.get(elem).parentId === null) {
                    this.hiddenNodeSetId.delete(elem);
                }
            });
        }
        this.cdr.detectChanges();
    }

    // Tree helpers
    private initializeTreeNode(parent?: TreeNode<SetupStep>): TreeNode<SetupStep> {
        const result = new TreeNode<SetupStep>(),
            item = new SetupStep();

        // Define setup step
        item.id = 0;
        item.parentId = parent ? parent.item.id : null;
        item.projectId = this.project.id;
        item.title = '';
        item.description = null;
        item.orderNum = parent
            ? (parent.children.length ? parent.children.slice(-1)[0].item.orderNum + 1000 : 1000)
            : (this.rootLevelSetupSteps.size
                ? [...this.rootLevelSetupSteps.entries()]
                .map(elem => elem[1])
                .sort((a, b) => a.orderNum > b.orderNum ? 1 : -1)
                .slice(-1)[0].orderNum + 1000 : 1000);

        // Define setup step node
        result.item = item;
        result.children = [];

        return result;
    }

    treeNodeHasDraftChildren(node: TreeNode<SetupStep>): boolean {
        return node
            ? node.children.filter(child => child.item.id === 0).length > 0
            : this.rootLevelSetupSteps.has(0);
    }

    treeNodeChanged(node: TreeNode<SetupStep>, field?: string): boolean {
        const pristine = this.pristineSetupStep.get(node.item.id),
            isValidField = field ? (field === 'title' || field === 'description' || field === 'isDraft') : true;
        if (!isValidField) {
            return false;
        } else if (!pristine) {
            return true;
        }

        switch (field) {
            case 'title':
                return (pristine.title || '').trim() !== (node.item.title || '').trim();
            case 'description':
                return (pristine.description || '').trim() !== (node.item.description || '').trim();
            case 'isDraft':
                return pristine.isDraft !== node.item.isDraft;
            default:
                return (pristine.title || '').trim() !== (node.item.title || '').trim()
                    || (pristine.description || '').trim() !== (node.item.description || '').trim();
        }
    }

    canSubmitTreeNode(node: TreeNode<SetupStep>, field?: string): boolean {
        const isValidField = field ? (field === 'title' || field === 'description' || field === 'isDraft') : true;

        if (!isValidField) {
            return false;
        }

        switch (field) {
            case 'description':
            case 'isDraft':
                return true;
            case 'title':
            default:
                return (node.item.title || '').trim().length > 0;
        }
    }

    addDraftTreeNode(parentId?: number): void {

        const parentNode = parentId ? this.setupList.get(parentId) : null;

        this.resetActiveTreeNode();
        if (parentNode) {
            this.showChildrenBeforeAddDraftNode(parentNode);
        }

        const newNode = this.initializeTreeNode(parentNode);

        if (parentNode) {
            this.setupService.addDraftSetupStep(newNode, parentNode);
        } else {
            this.setupService.addDraftSetupStep(newNode);
        }

        this.cdr.detectChanges();

        this.windowRefService.focusElementById('input-setup-0', 100);
    }

    revertTreeNode(node: TreeNode<SetupStep>): void {
        const pristine = this.pristineSetupStep.get(node.item.id);
        node.item.title = pristine ? pristine.title : '';
        node.item.description = pristine ? pristine.description : null;
        this.activeNode = null;
        this.activeNodeId = -1;
        this.cdr.detectChanges();
    }

    revertTreeNodeTitle(node: TreeNode<SetupStep>): void {
        const pristine = this.pristineSetupStep.get(node.item.id);
        node.item.title = pristine ? pristine.title : '';
        this.cdr.detectChanges();
    }

    revertTreeNodeDescription(node: TreeNode<SetupStep>): void {
        const pristine = this.pristineSetupStep.get(node.item.id);
        node.item.description = pristine ? pristine.description : null;
        this.cdr.detectChanges();
    }

    invalidateTreeNode(node: TreeNode<SetupStep>, field?: string, value?: string): void {
        const pristine = this.pristineSetupStep.get(node.item.id);

        if ((field === null || field === 'title') && value) {
            node.item.title = value.trim();

            if ((node.item.title || '').trim().length === 0) {
                node.item.title = pristine ? pristine.title : '';
                return;
            }
        }

        if (!(this.treeNodeChanged(node, field) && this.canSubmitTreeNode(node, field))) {
            return;
        }

        node.item.id === 0 ? this.createTreeNode(node) : this.updateSetupStep(node, field);
    }

    setActiveTreeNode(node: TreeNode<SetupStep>): void {
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
        this.setActiveParamNode(node);
        this.showMoreAttachments = false;

        if (this.attachments.has(this.activeNode.item.id)) {
            this.activeFileAttachments = this.attachments.get(this.activeNode.item.id);
        }

        this.getLinkedAttachments(this.activeNode.item.id);
    }

    resetActiveTreeNode(): void {
        this.activeNode = null;
        this.activeNodeId = -1;
        this.activeParam = null;
        this.activeQueryId = null;
        this.cdr.detectChanges();
    }

    getOrderNum(targetNode: TreeNode<SetupStep>, dropType: DropPlace): number {
        switch (dropType) {
            case DropPlace.Before: {
                if (targetNode.item.parentId) {
                    const index = this.setupList.get(targetNode.item.parentId).children.indexOf(targetNode);
                    if (index === 0) {
                        return 1000;
                    } else {
                        return Math.round((targetNode.item.orderNum + this.setupList.get(targetNode.item.parentId).children[index - 1].item.orderNum) / 2);
                    }
                } else {
                    const index = [...this.rootLevelSetupSteps.entries()]
                        .map(elem => elem[1]).findIndex(elem => elem.id === targetNode.item.id);
                    if (index === 0) {
                        return 1000;
                    } else {
                        return Math.round((targetNode.item.orderNum +
                            [...this.rootLevelSetupSteps.entries()]
                                .map(elem => elem[1])[index - 1].orderNum) / 2);
                    }
                }
            }

            case DropPlace.After: {
                if (targetNode.item.parentId) {
                    const index = this.setupList.get(targetNode.item.parentId).children.indexOf(targetNode);
                    if (index === this.setupList.get(targetNode.item.parentId).children.length - 1) {
                        return targetNode.item.orderNum + 1000;
                    } else {
                        return Math.round((targetNode.item.orderNum + this.setupList.get(targetNode.item.parentId).children[index + 1].item.orderNum) / 2);
                    }
                } else {
                    const index = [...this.rootLevelSetupSteps.entries()]
                        .map(elem => elem[1]).findIndex(elem => elem.id === targetNode.item.id);
                    if (index === this.rootLevelSetupSteps.size - 1) {
                        return targetNode.item.orderNum + 1000;
                    } else {
                        return Math.round((targetNode.item.orderNum +
                            [...this.rootLevelSetupSteps.entries()]
                                .map(elem => elem[1])[index + 1].orderNum) / 2);
                    }
                }
            }
        }
    }

    public dropped(files: NgxFileDropEntry[]) {
        for (const droppedFile of files) {
            if (droppedFile.fileEntry.isFile) {
                const fileEntry = droppedFile.fileEntry as FileSystemFileEntry,
                    subj$ = new BehaviorSubject(0);

                this.attachmentsProgress.set(droppedFile.fileEntry.name, subj$);

                fileEntry.file((file: File) => {
                    this.uploadService
                        .uploadAttachment(file, AttachmentOwner.SetupStep, this.activeNode.item.id)
                        .pipe(
                            takeUntil(this.destroy$),
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
    createTreeNode(node: TreeNode<SetupStep>): void {
        const body = new SetupProtocol.CreateSetupStepRequest();
        body.parentId = node.item.parentId;
        body.title = node.item.title === null ? null : node.item.title.trim();
        body.description = node.item.description === null ? null : node.item.description.trim();
        body.isDraft = false;
        body.orderNum = node.item.orderNum;

        this.hyperionSetupService
            .createSetupStep(body, node.item.projectId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.activeQueryId = response.id;
                    this.activeNode = <TreeNode<SetupStep>>{
                        children: [],
                        item: {
                            description: response.description,
                            id: response.id,
                            isDraft: response.isDraft,
                            orderNum: response.orderNum,
                            parameterId: response.parameterId,
                            parentId: response.parentId,
                            projectId: response.projectId,
                            rev: response.rev,
                            title: response.title,
                            createdAt: response.createdAt,
                            updatedAt: response.updatedAt,
                        }
                    };
                    this.activeNodeId = response.id;
                    this.notificationService.success('Setup step created');
                    this.setupService.clearDraftNode(response.parentId);
                },
                error => {
                    if (error instanceof BadRequestError) {
                        let errorMessage: string = null;

                        switch (error.error as SetupProtocol.SetupError) {
                            case SetupProtocol.SetupError.ParentNotExists:
                                errorMessage = 'Setup step does not exist';
                                break;
                            case SetupProtocol.SetupError.ProjectNotExists:
                                errorMessage = 'Project does not exist';
                                break;
                            case SetupProtocol.SetupError.TitleAlreadyExists:
                                errorMessage = 'Setup step title already exists';
                                break;
                            case SetupProtocol.SetupError.HasChildren:
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

    updateSetupStep(node: TreeNode<SetupStep>, field?: string, parentId?: number, orderNum?: number): void {
        const body = new SetupProtocol.UpdateSetupStepRequest(),
            pristine = this.pristineSetupStep.get(node.item.id);

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

        if (node.item.title !== pristine.title && (field === null || field === 'title')) body.title = node.item.title;
        if (node.item.description !== pristine.description && (field === null || field === 'description')) body.description = node.item.description;
        if (node.item.isDraft !== pristine.isDraft && (field === null || field === 'isDraft')) body.isDraft = node.item.isDraft;

        this.hyperionSetupService
            .updateSetupStep(body, node.item.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.notificationService.success('Setup step updated');
                },
                error => {
                    if (error instanceof BadRequestError) {
                        let errorMessage: string = null;

                        switch (error.error as SetupProtocol.SetupError) {
                            case SetupProtocol.SetupError.ParentNotExists:
                                errorMessage = 'Setup step does not exist';
                                break;
                            case SetupProtocol.SetupError.ProjectNotExists:
                                errorMessage = 'Project does not exist';
                                break;
                            case SetupProtocol.SetupError.TitleAlreadyExists:
                                errorMessage = 'Setup step title already exists';
                                break;
                            case SetupProtocol.SetupError.HasChildren:
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

    deleteSetupStep(node: TreeNode<SetupStep>): void {
        if (node.item.id === 0) {
            this.setupService.clearDraftNode(node.item.parentId ? node.item.parentId : null);

            if (node === this.activeNode) {
                this.activeNode = null;
                this.activeNodeId = -1;
            }
            this.cdr.detectChanges();
            return;
        }

        this.hyperionSetupService
            .deleteSetupStep(node.item.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.activeNode = null;
                    this.activeNodeId = -1;
                    this.notificationService.success('Setup step deleted');
                },
                error => {
                    if (error instanceof BadRequestError && error.error === SetupProtocol.SetupError.HasChildren) {
                        this.notificationService.warning('Setup step can not be deleted because has children');
                    } else if (error instanceof NotFoundError) {
                        this.notificationService.warning('Setup step is not found. Maybe it was deleted already');
                        this.cdr.detectChanges();
                    } else {
                        this.notificationService.error('Probably used in test case or other server error');
                    }
                }
            );
    }

    // Attachments REST
    private getLinkedAttachments(setupId: number): void {
        if (setupId === 0) return;
        this.hyperionAttachmentService
            .getLinkedAttachments(DbProtocol.AttachmentOwner.SetupStep, setupId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.attachments.set(setupId, response.items);
                    this.activeFileAttachments = response.items;
                    this.cdr.detectChanges();
                },
                error => this.notificationService.error(error)
            );
    }

    unlinkFileAttachment(attachmentId: number, linkedId: number): void {
        this.hyperionAttachmentService
            .deleteAttachmentLink(attachmentId, DbProtocol.AttachmentOwner.SetupStep, linkedId)
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


    filterCallback(item: SetupStep, filterText: string): boolean {
        if (!filterText)
            return true;

        return item.title.toLocaleLowerCase().indexOf(filterText.toLocaleLowerCase()) > -1;
    }

    orderingCallback(firstNode: TreeNode<SetupStep>, secondNode: TreeNode<SetupStep>): number {
        return firstNode.item.orderNum > secondNode.item.orderNum ? 1 : -1;
    }

    updateAfterDrop(event: { dropType: DropPlace, draggedNode: TreeNode<SetupStep>, targetNode: TreeNode<SetupStep> }): void {
        const dropType = event.dropType,
            draggedNode = event.draggedNode,
            targetNode = event.targetNode;
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
                orderNum = [...this.rootLevelSetupSteps.entries()]
                    .map(elem => elem[1])
                    .sort((a, b) => a.orderNum > b.orderNum ? 1 : -1)
                    .slice(-1)[0].orderNum + 1000;
                break;
        }

        this.updateSetupStep(draggedNode, null, parentId, orderNum);
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

    updateParameter(node: TreeNode<SetupStep>, paramId: number): void {
        this.hyperionSetupService
            .linkSetupStepToParameter(new CommonProtocol.Empty(), node.item.id, paramId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(setupStep => {
                this.notificationService.success('Parameter linked');
                node.item.parameterId = setupStep.parameterId;
                this.setActiveParamNode(node);
            });
    }

    deleteParameter(node: TreeNode<SetupStep>): void {
        this.hyperionSetupService
            .unlinkSetupStepFromParameter(new CommonProtocol.Empty(), node.item.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(setupStep => {
                this.notificationService.success('Parameter unlinked');
                this.activeParam = null;
            });
    }

    changeParameter(): void {
        const dialogRef = this.dialog.open(BrowseParamsDialogComponent, {
            width: '850px',
            autoFocus: false,
            data: {
                project: this.project,
                paramData: this.getMapAsIterable(this.parametersTree),
                setupNode: this.activeNode,
            } as IBrowseParamData
        });

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe((result: { paramNode: TreeNode<Parameter>, setup: TreeNode<SetupStep> } | null) => {
                if (result && result.paramNode) {
                    this.updateParameter(result.setup, result.paramNode.item.id);
                }
            });
    }

    getMapAsIterable(map: Map<number, TreeNode<Parameter>>): Array<TreeNode<Parameter>> {
        const parameters = [];
        map.forEach(mapItem => parameters.push(mapItem));
        parameters.sort((a, b) => a.id - b.id);
        return parameters;
    }

    setActiveParamNode(node: TreeNode<SetupStep>): void {
        if (node.item.parameterId) {
            this.activeParam = this.parametersList.get(node.item.parameterId);
        } else {
            this.activeParam = null;
        }
    }

    focusSetup(): void {
        if (this.activeQueryId) {
            this.ready$
                .asObservable()
                .pipe(
                    filter(ready => !!ready),
                    takeUntil(this.destroy$))
                .subscribe(() => {
                    const node = this.setupList.get(this.activeQueryId);
                    if (node) {
                        const parentsNodeId = this.getAllParentId(node);
                        this.dynamicTree.expandTreeNodesById(parentsNodeId);

                        setTimeout(() => {
                            const element = this.render.selectRootElement('#input-setup-' + node.item.id);
                            setTimeout(() => element.focus(), 10);
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

    tooltipTextShow(node: TreeNode<SetupStep>): string {
        if (this.parametersList?.size > 0) {
            const hasDependentParameter = node.item.parameterId
                && this.parametersList.get(node.item.parameterId)
                && this.parametersList.get(node.item.parameterId).item.dependentId;
            if (!hasDependentParameter) return null;

            const dependentParameter = this.parametersList.get(node.item.parameterId).item,
                dependOnParameter = this.parametersList.get(this.parametersList.get(node.item.parameterId).item.dependentId).item;

            return this.isSetDependOnParameter(node, dependOnParameter.id)
                ? null
                : '[' + dependentParameter.title + ']' + ' parameter depends on [' + dependOnParameter.title + '] parameter which is not set for any of the parent setup steps';
        }

    }

    isSetDependOnParameter(setupNode: TreeNode<SetupStep>, dependOnId: number): boolean {
        if (setupNode.item.parameterId === dependOnId) {
            return true;
        } else {
            if (setupNode.item.parentId) {
                return this.isSetDependOnParameter(this.setupList.get(setupNode.item.parentId), dependOnId);
            } else {
                return false;
            }
        }
    }

    // refresh() {
    //
    // }

    afterTreeRendered(event: boolean): void {
        if (event) {
            this.focusSetup();
        }
    }
}

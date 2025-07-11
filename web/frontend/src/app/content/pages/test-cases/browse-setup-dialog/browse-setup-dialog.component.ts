import { AfterViewInit, ChangeDetectorRef, Component, Inject, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { merge, Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import Swal from 'sweetalert2';
import { HyperionSetupService } from '../../../../protocol/setup-protocol.service';
import { NotificationService } from '../../../../core/services/notification.service';
import { WebsocketService } from '../../../../core/services/websocket.service';
import { WindowRefService } from '../../../../core/services/window-ref.service';
import * as NotificationProtocol from '../../../../protocol/notification-protocol.data';
import * as SetupProtocol from '../../../../protocol/setup-protocol.data';
import { DynamicTreeComponent } from '../../../../components/general/dynamic-tree/dynamic-tree.component';
import { BadRequestError, Collection, TreeNode } from '../../../../protocol/data-protocol.data';
import { Project, SetupStep } from '../../../../protocol/db-protocol.data';
import { SetupService } from '../../../../core/services/setup.service';


interface IBrowseSetup {
    project: Project;
    testCaseId: number;
    setupId: number | null;
    inheritedSetupId: number | null;
    preconditionId: number | null;
    inheritedPreconditionId: number | null;
    startSetup: SetupStep | null;
    setPrecondition: boolean | null;
    setSetup: boolean | null;
}

@Component({
    selector: 'm-browse-setup-dialog',
    templateUrl: 'browse-setup-dialog.component.html',
    styleUrls: ['./browse-setup-dialog.component.scss'],
})

export class BrowseSetupDialogComponent implements OnInit, OnDestroy, AfterViewInit {
    @ViewChild(DynamicTreeComponent) dynamicTree: DynamicTreeComponent;

    destroy$: Subject<any>;
    setupsItems = new Array<TreeNode<SetupStep>>();
    setupList: Map<number, TreeNode<SetupStep>> = new Map();

    constructor (
        public dialogRef: MatDialogRef<BrowseSetupDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: IBrowseSetup,
        private cdr: ChangeDetectorRef,
        private router: Router,
        private hyperionSetupService: HyperionSetupService,
        private notificationService: NotificationService,
        private websocketService: WebsocketService,
        private windowRefService: WindowRefService,
        public setupService: SetupService,
    ) { }

    ngOnInit(): void {
        this.destroy$ = new Subject();

        merge(
            this.websocketService.testCaseDeleted,
            this.websocketService.testCaseUpdated,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => this.data.project.id === event.data.projectId && this.data.testCaseId === event.data.id)
        ).subscribe(event => {
            if (event instanceof NotificationProtocol.TestCaseDeleted) {
                Swal.fire('Test case has been deleted', `by <strong>${event.actorName}</strong>`, 'info')
                    .then(() => this.dialogRef.close(null));
            } else if (event instanceof NotificationProtocol.TestCaseUpdated) {
                if (this.data.setupId !== event.data.setupId) {
                    this.data.setupId = event.data.setupId;
                }
            }
        });

        this.setupService.setupList$
            .asObservable()
            .pipe(takeUntil(this.destroy$))
            .subscribe(sl => this.setupList = sl);
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    ngAfterViewInit(): void {
        if (this.data.setSetup && this.data.setupId) {
            setTimeout(() => this.windowRefService.scrollToElementCenter('link-setup-' + this.data.setupId, true), 250);
        }

        if (this.data.setPrecondition && this.data.preconditionId) {
            setTimeout( () => this.windowRefService.scrollToElementCenter('link-setup-' + this.data.preconditionId, true), 250);
        }
    }

    // Page initialization and refresh


    getAllChildrenId(node: TreeNode<SetupStep>): Array<number> {
        return node.children.reduce(
            (acc, value) => [
                ...acc,
                [value.item.id],
                value.children.map(child => this.getAllChildrenId(child))
                    .reduce((prev, curr) => prev.concat(curr), [])], []).concat(node.item.id)
            .reduce((prev, curr) => prev.concat(curr), []);
    }

    chooseSetup(node: TreeNode<SetupStep>): void {
        if (this.data.setSetup && this.data.setupId) {
            this.dialogRef.close(node.item.id === this.data.setupId ? false : node.item.id);
        }

        if (this.data.setPrecondition && this.data.preconditionId) {
            this.dialogRef.close(node.item.id === this.data.preconditionId ? false : node.item.id);
        }

        this.dialogRef.close(node.item.id);
    }

    findTreeNodeStart(setup: TreeNode<SetupStep>, setupStart: SetupStep): void {
        if (setup.item.id === setupStart.id) {
            this.setupsItems = [setup];
            return;
        } else {
            setup.children.forEach(child => this.findTreeNodeStart(child, setupStart));
        }
    }

    addDraftTreeNode(parentId?: number): void {

        const parentNode = parentId ? this.setupList.get(parentId) : null;

        const newNode = this.initializeTreeNode(parentNode);

        if (parentNode) {
            this.setupService.addDraftSetupStep(newNode, parentNode);
        } else {
            this.setupService.addDraftSetupStep(newNode);
        }

        this.cdr.detectChanges();

        this.windowRefService.focusElementById('input-setup-0', 100);
    }

    private initializeTreeNode(parent?: TreeNode<SetupStep>): TreeNode<SetupStep> {
        const result = new TreeNode<SetupStep>(),
            item = new SetupStep();

        // Define setup step
        item.id = 0;
        item.parentId = parent ? parent.item.id : null;
        item.projectId = this.data.project.id;
        item.title = '';
        item.description = null;

        // Define setup step node
        result.item = item;
        result.children = [];

        return result;
    }

    invalidateTreeNode(node: TreeNode<SetupStep>, field: string): void {
        node.item.title = node.item.title.trim();

        if ((node.item.title || '').trim().length === 0) {
            return;
        }

        this.createTreeNode(node);
    }


    // REST API
    createTreeNode(node: TreeNode<SetupStep>): void {
        const body = new SetupProtocol.CreateSetupStepRequest();
        body.parentId = node.item.parentId;
        body.title = node.item.title === null ? null : node.item.title.trim();
        body.description = node.item.description === null ? null : node.item.description.trim();
        body.isDraft = false;
        body.orderNum = node.children
            .sort((a, b) => a.item.orderNum > b.item.orderNum ? 1 : -1)
            .slice(-1)[0].item.orderNum + 1000;

        this.hyperionSetupService
            .createSetupStep(body, node.item.projectId)
            .subscribe(
                response => {
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
}

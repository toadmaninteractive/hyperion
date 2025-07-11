import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy, OnInit, Renderer2, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, combineLatest, merge, Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { NotificationService } from '../../../../core/services/notification.service';
import { WebsocketService } from '../../../../core/services/websocket.service';
import { WindowRefService } from '../../../../core/services/window-ref.service';
import { HyperionParameterService } from '../../../../protocol/parameter-protocol.service';
import Swal, { SweetAlertOptions } from 'sweetalert2';

import * as ParameterProtocol from '../../../../protocol/parameter-protocol.data';
import { BadRequestError, Collection, TreeNode } from '../../../../protocol/data-protocol.data';
import { Parameter, ParameterValue, PersonnelAccountProfile, Project, SetupStep } from '../../../../protocol/db-protocol.data';
import { ParameterService } from '../../../../core/services/parameter.service';
import { DynamicTreeMapComponent } from '../../../../components/general/dynamic-tree-map/dynamic-tree-map.component';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as NotificationProtocol from '../../../../protocol/notification-protocol.data';
import { AccountService } from '../../../../core/services/account.service';
import { DropPlace } from '../../../../shared/interfaces/drop-place';

@Component({
    selector: 'm-parameter-page',
    templateUrl: './parameters-overview.component.html',
    styleUrls: ['./parameters-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})

export class ParametersOverviewComponent implements OnInit, OnDestroy {
    @ViewChild(DynamicTreeMapComponent) dynamicTree: DynamicTreeMapComponent;
    destroy$ = new Subject<any>();
    collectionParameter$ = new BehaviorSubject<Collection<TreeNode<Parameter>>>(null);
    hiddenNodeSet = new Set<TreeNode<Parameter>>();
    filteredDepListOfValues = new Map<string, any>();
    filterDepParameter = new Set<string>();
    activeNode?: TreeNode<Parameter>;
    activeNodeId = -1;
    parentNode?: TreeNode<Parameter>;
    parentNodeId = 0;
    project: Project;
    filteredListOfValues = [];
    dependentListOfValues = [];
    idParameterDependent;
    initialized = false;
    createdState = false;
    filterParameter = false;
    parameterized = false;
    activeQueryId: number;

    dimmedNodeSetId = new Set<number>();
    hiddenNodeSetId = new Set<number>();
    pristineItemMap = new Map<number, Parameter>();
    parameterList: Map<number, TreeNode<Parameter>> = new Map();
    rootLevelParameters: Map<number, Parameter> = new Map();
    profile: PersonnelAccountProfile;

    constructor(
        private route: ActivatedRoute,
        private router: Router,
        private render: Renderer2,
        private cdr: ChangeDetectorRef,
        private accountService: AccountService,
        private hyperionParameterService: HyperionParameterService,
        private notificationService: NotificationService,
        private websocketService: WebsocketService,
        private windowRefService: WindowRefService,
        public parameterService: ParameterService,
    ) {
    }

    ngOnInit(): void {
        this.accountService.profile$
            .asObservable()
            .pipe(takeUntil(this.destroy$))
            .subscribe(profile => this.profile = profile);

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
            this.websocketService.parameterUpdated,
            this.websocketService.parameterDeleted,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => event.data.projectId === this.project.id),
        ).subscribe(event => {
            const node = new TreeNode<Parameter>();
            node.item = event.data;
            if (event instanceof NotificationProtocol.ParameterUpdated) {

                if (this.activeNodeId !== -1 && node.item.id === this.activeNodeId && this.profile.username !== event.actorName) {
                    Swal.fire({
                        type: 'information',
                        title: `Your active node has been changed by ${event.actorName }`,
                        confirmButtonText: 'ok',
                    } as SweetAlertOptions).then((result) => {
                        if (result.value !== undefined) {
                            this.setActiveTreeNode(node);
                        }
                    });
                }
            } else if (event instanceof NotificationProtocol.ParameterDeleted) {
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
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.collectionParameter$.complete();
    }

    afterTreeRendered(event: boolean): void {
        if (event) {
            this.focusParameter();
        }
    }

    // Page initialization and refresh
    private initialize(project: Project): void {
        this.project = project;
        this.refresh();

        this.parameterService.parameterTree$
            .asObservable()
            .pipe(
                filter(s => !!s),
                takeUntil(this.destroy$))
            .subscribe(parameters => {
                this.rootLevelParameters.clear();
                parameters.forEach(param => this.rootLevelParameters.set(param.item.id, param.item));
            });

        this.parameterService.flatParameter$
            .asObservable()
            .pipe(
                filter(s => !!s),
                takeUntil(this.destroy$))
            .subscribe(parameter => {
                this.pristineItemMap.clear();
                parameter.forEach(item => {
                    this.pristineItemMap.set(item.id, Parameter.fromJson(item.toJson()));
                });
            });

        this.parameterService.parameterList$
            .asObservable()
            .pipe(
                filter(s => !!s),
                takeUntil(this.destroy$))
            .subscribe(parameter => {
                this.parameterList = parameter;
                this.focusParameter();
            });
    }

    // Node method
    refresh(): void {
        this.activeNode = null;
        this.activeNodeId = -1;
        this.cdr.detectChanges();
    }

    // Data filtering
    applyFilter(filterText: string): void {
        this.dimmedNodeSetId = new Set<number>(Array.from(this.pristineItemMap).map(item => !this.filterCallback(item[1], filterText) ? item[1].id : null));
        this.hiddenNodeSetId = new Set<number>(Array.from(this.parameterList).filter(node => this.getAllChildrenId(node[1]).every(id => this.dimmedNodeSetId.has(id)))
            .map(node => node[1].item.id));
        this.expandAllTreeNodes();
    }

    getAllChildrenId(node: TreeNode<Parameter>): Array<number> {
        return node.children.reduce(
            (acc, value) => [
                ...acc,
                [value.item.id],
                value.children.map(child => this.getAllChildrenId(child))
                    .reduce((prev, curr) => prev.concat(curr), [])], []).concat(node.item.id)
            .reduce((prev, curr) => prev.concat(curr), []);
    }

    getAllParentId(node: TreeNode<Parameter>): Array<number> {
        const result = [];
        while (node && node.item.parentId !== 0) {
            const parentNode = this.parameterList.get(node.item.parentId);
            result.push(parentNode ? parentNode.item.id : null);
            node = parentNode;
        }
        return result.filter(elem => !!elem);
    }

    revertTreeNode(node: TreeNode<Parameter>): void {
        const pristine = this.pristineItemMap.get(node.item.id);
        node.item.title = pristine ? pristine.title : '';
        this.activeNode = null;
        this.activeNodeId = -1;
        this.cdr.detectChanges();
    }

    // Tree helpers
    private initializeTreeNode(parent: TreeNode<Parameter>, parameterized: boolean): TreeNode<Parameter> {
        const result = new TreeNode<Parameter>(),
            item = new Parameter();
        item.id = 0;
        item.parentId = parent ? parent.item.id : null;
        item.projectId = this.project.id;

        if (parameterized) {
            item.dependentId = 0;
        } else {
            item.dependentId = null;
        }

        item.values = parent ? parent.item.values : [];
        item.title = '';
        result.item = item;
        result.children = [];

        return result;
    }

    treeNodeHasDraftChildren(node: TreeNode<SetupStep>): boolean {
        return node
            ? node.children.filter(child => child.item.id === 0).length > 0
            : this.rootLevelParameters.has(0);
    }

    treeNodeChanged(node: TreeNode<Parameter>, field?: string): boolean {
        const pristine = this.pristineItemMap.get(node.item.id);
        let isValidField = false;

        switch (field) {
            case 'rev':
            case 'projectId':
            case 'parentId':
            case 'dependentId':
            case 'title':
            case 'values':
            case 'createdAt':
            case 'updatedAt':
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
            case 'rev':
                return pristine.rev !== node.item.rev;
            case 'parentId':
                return pristine.parentId !== node.item.parentId;
            case 'dependentId':
                return pristine.dependentId !== node.item.dependentId;
            case 'title':
                return (pristine.title || '').trim() !== (node.item.title || '').trim();
            default:
                return pristine.rev !== node.item.rev
                    || pristine.parentId !== node.item.parentId
                    || pristine.dependentId !== node.item.dependentId
                    || (pristine.title || '').trim() !== (node.item.title || '').trim();
        }
    }

    canSubmitTreeNode(node: TreeNode<Parameter>, field: string): boolean {
        let isValidField = false;

        switch (field) {
            case 'rev':
            case 'projectId':
            case 'parentId':
            case 'dependentId':
            case 'title':
            case 'values':
            case 'createdAt':
            case 'updatedAt':
            case null:
                isValidField = true;
                break;
        }

        if (!isValidField) {
            return false;
        }

        switch (field) {
            case 'rev':
            case 'projectId':
            case 'parentId':
            case 'dependentId':
            case 'title':
            case 'values':
            case 'createdAt':
            case 'updatedAt':
            default:
                return (node.item.title || '').trim().length > 0;
        }
    }

    addDraftTreeNode(parameterized: boolean): void {
        this.parentNode = this.parameterList.get(this.parentNodeId);
        this.resetActiveTreeNode();
        this.parameterized = parameterized;
        this.createdState = true;
        const newNode = this.initializeTreeNode(this.parentNode, parameterized);
        this.activeNode = newNode;
        this.activeNodeId = newNode.item.id;

        if (parameterized === null) {
            this.parentNode = null;
        }

        if (this.parentNode) {
            this.parameterService.addDraftParameter(newNode, this.parentNode);
        } else {
            this.parameterService.addDraftParameter(newNode);
        }

        this.activeNode = newNode;
        this.activeNodeId = newNode.item.id;

        if (this.parameterized) {

            const inpOpt = [...this.getMapAsIterable(this.rootLevelParameters)];
            const mp = new Map<string, string>();
            inpOpt.forEach(item => {
                mp.set(String(item.id), item.title);
            });
            let html = '<input id="swal-title" required="required" class="swal2-input m-1" placeholder="Start typing title..."/>' +
                '<select id="swal-dependent" required="required" class="swal2-input swal2-select m-1">' +
                '<option value="" disabled selected>Select parameter...</option>';
            mp.forEach((title, key) => html += '<option value="' + key + '">' + title + '</option>');
            html += '</select>';
            Swal
                .fire({
                    html: html,
                    showCancelButton: true,
                    preConfirm: () => {
                        if (this.windowRefService.nativeWindow.document.getElementById('swal-title').value === '') {
                            Swal.showValidationMessage('Please fill title');
                            return false;
                        }

                        if (this.windowRefService.nativeWindow.document.getElementById('swal-dependent').value === '') {
                            Swal.showValidationMessage('Please select dependent parameter');
                            return false;
                        }
                        return {
                            title: this.windowRefService.nativeWindow.document.getElementById('swal-title').value,
                            dependentId: this.windowRefService.nativeWindow.document.getElementById('swal-dependent').value
                        };
                    }
                })
                .then(result => {
                    if (result.value) {
                        this.idParameterDependent = parseInt(result.value.dependentId, 10);
                        this.activeNode.item.dependentId = parseInt(result.value.dependentId, 10);
                        this.activeNode.item.title = result.value.title;
                        this.invalidateTreeNode(this.activeNode, null);
                    }
                    // @ts-ignore
                    if (result.dismiss === 'cancel' || result.dismiss === 'backdrop') {
                        this.resetActiveTreeNode();
                    }
                });
        }

        this.cdr.detectChanges();
    }

    revertTreeNodeTitle(node: TreeNode<Parameter>): void {
        const pristine = this.pristineItemMap.get(node.item.id);
        node.item.title = pristine ? pristine.title : '';
        this.cdr.detectChanges();
    }

    invalidateTreeNode(node: TreeNode<Parameter>, field?: string, value?: string): void {
        const pristine = this.pristineItemMap.get(node.item.id);
        let isValidField = false;

        if ((field === null || field === 'title') && value) {
            node.item.title = value.trim();

            if ((node.item.title || '').trim().length === 0) {
                node.item.title = pristine ? pristine.title : '';
                return;
            }
        }


        switch (field) {
            case 'rev':
            case 'projectId':
            case 'parentId':
            case 'dependentId':
            case 'title':
            case 'values':
            case 'createdAt':
            case 'updatedAt':
            case null:
                isValidField = true;
                break;
        }

        if (node.item.title === '' || (this.parameterized && node.item.dependentId === 0) || (this.parameterized && node.item.dependentId === null)) {
            this.notificationService.error(`Fill in title and dependent parameter`);
            return;
        }

        if (this.createdState && node.item.title === '' && !this.parameterized) {
            this.resetActiveTreeNode();
        }

        if (this.createdState && this.parameterized && node.item.dependentId > 0 && node.item.id === 0) {
            this.resetActiveTreeNode();
            this.parameterized = false;
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

        if (node.item.id !== 0) {
            this.updateParameter(node);
        } else {
            this.createParameter(node);
        }
    }

    setActiveTreeNode(node: TreeNode<Parameter>): void {
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

        this.createdState = false;
        this.parameterized = false;
        node.item.values.sort(this.compareValues);
        this.activeNode = node;
        this.activeNodeId = node.item.id;
        this.filteredListOfValues = [];
        this.dependentListOfValues = [];
        this.filteredDepListOfValues = new Map<string, any>();

        if (node.item.dependentId) {
            this.filteredDepListOfValues = new Map<string, any>();
            this.parameterList.get(node.item.dependentId).item.values.forEach(val => {
                const values = [];
                this.parameterList.get(node.item.parentId).item.values.forEach(item => {
                    values.push(item.value);
                });
                this.filteredDepListOfValues.set(val.value, values);
            });
        }

        if (!this.filterParameter) {
            node.item.values.forEach(item => {
                this.filteredListOfValues.push(item.value);
            });
        }

        if (this.filteredListOfValues.length === 0) {
            this.filterParameter = true;
            if (node.item.parentId) {
                this.parameterList.get(node.item.parentId).item.values.forEach(item => {
                    this.filteredListOfValues.push(item.value);
                });
            }
        } else {
            this.filterParameter = false;
        }

        if (node.item.dependentId) {
            const dependentValueList = this.parameterList.get(node.item.dependentId);
            dependentValueList.item.values.sort(this.compareValues);
            dependentValueList.item.values.forEach(item => {
                this.dependentListOfValues.push(item.value);
            });
        }
        this.cdr.detectChanges();
    }

    resetActiveTreeNode(): void {
        this.activeNode = null;
        this.activeNodeId = -1;
        if (this.createdState) {
            this.createdState = false;
        }
        this.activeQueryId = null;
        this.cdr.detectChanges();
    }

    // Callbacks
    expandAllTreeNodes(): void {
        this.dynamicTree.expandTreeNodes();
    }

    collapseAllTreeNodes(): void {
        this.dynamicTree.collapseTreeNodes();
    }

    filterCallback(item: Parameter, filterText: string): boolean {
        if (!filterText)
            return true;

        return item.title.toLocaleLowerCase().indexOf(filterText.toLocaleLowerCase()) > -1;
    }

    // API methods

    createParameter(node: TreeNode<Parameter>): void {
        const body = new ParameterProtocol.CreateParameterRequest();
        body.parentId = node.item.parentId;
        body.dependentId = node.item.dependentId;
        body.title = node.item.title;

        this.hyperionParameterService
            .createParameter(body, this.project.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.activeQueryId = response.id;
                    this.setActiveTreeNode(<TreeNode<Parameter>>{
                        children: [],
                        item: {
                            dependentId: response.dependentId,
                            id: response.id,
                            parentId: response.parentId,
                            projectId: response.projectId,
                            title: response.title,
                            values: response.values,
                            createdAt: response.createdAt,
                            updatedAt: response.updatedAt,
                        }
                    });
                    this.parameterized = false;
                    this.createdState = false;
                    this.parameterService.clearDraftNode(response.parentId);
                    this.notificationService.success(`Parameter created`);
                },
                error => {
                    if (error instanceof BadRequestError) {
                        let errorMessage: string = null;

                        switch (error.error as ParameterProtocol.ParameterCreateError) {
                            case ParameterProtocol.ParameterCreateError.ProjectNotExists:
                                errorMessage = 'Project does not exist';
                                break;
                            case ParameterProtocol.ParameterCreateError.ParameterAlreadyExists:
                                errorMessage = 'Parameter already exists';
                                break;
                            case ParameterProtocol.ParameterCreateError.ParentNotExists:
                                errorMessage = 'Parent parameter does not exist';
                                break;
                            case ParameterProtocol.ParameterCreateError.ParentIsIndependent:
                                errorMessage = 'Parent parameter is independent';
                                break;
                            case ParameterProtocol.ParameterCreateError.ParameterSourceAlreadyExists:
                                errorMessage = 'Parameter source already exists';
                                break;
                        }

                        this.notificationService.error(errorMessage || error);
                    } else {
                        this.notificationService.error(error);
                    }
                }
            );
    }

    updateParameter(node: TreeNode<Parameter>, parentId?: number): void {
        const body = new ParameterProtocol.RenameParameterRequest,
            pristine = this.pristineItemMap.get(node.item.id);

        if (node.item.title !== pristine.title) {
            body.newTitle = node.item.title;
        }

        this.hyperionParameterService
            .renameParameter(body, node.item.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.notificationService.success(`Parameter updated`);
                    this.cdr.detectChanges();
                },
                error => {
                    if (error instanceof BadRequestError) {
                        let errorMessage: string = null;

                        switch (error.error as ParameterProtocol.ParameterRenameError) {
                            case ParameterProtocol.ParameterRenameError.ParameterNotExists:
                                errorMessage = 'Parent parameter does not exist';
                                break;
                            case ParameterProtocol.ParameterRenameError.TitleAlreadyExists:
                                errorMessage = 'Parameter title already exists';
                                break;
                        }
                        this.notificationService.error(errorMessage || error);
                    } else {
                        this.notificationService.error(error);
                    }
                }
            );
    }

    deleteParameter(node: TreeNode<Parameter>): void {
        if (node.item.id === 0) {
            this.parameterService.clearDraftNode(node.item.parentId ? node.item.parentId : null);
            this.resetActiveTreeNode();
            this.cdr.detectChanges();
            return;
        }

        this.hyperionParameterService
            .deleteParameter(node.item.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.notificationService.success(`Parameter ${node.item.title} deleted`);
                },
                error => {
                    if (error instanceof BadRequestError) {
                        let errorMessage: string = null;

                        switch (error.error as ParameterProtocol.ParameterDeleteError) {
                            case ParameterProtocol.ParameterDeleteError.ParameterNotExists:
                                errorMessage = 'Parent parameter does not exist';
                                break;
                            case ParameterProtocol.ParameterDeleteError.HasChildren:
                                errorMessage = 'Parameter has children';
                                break;
                            case ParameterProtocol.ParameterDeleteError.HasDependants:
                                errorMessage = 'Parameter has dependants';
                                break;
                        }
                        this.notificationService.error(errorMessage || error);

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

    addValue(value, dependencyValue) {
        const body = new ParameterProtocol.AddParameterValueRequest;
        body.value = value;
        body.dependentValue = dependencyValue;

        this.hyperionParameterService
            .addParameterValue(body, this.activeNode.item.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.activeNode.item.values = response.values.sort(this.compareValues);
                    this.notificationService.success(`Parameter added`);
                    this.cdr.detectChanges();
                },
                error => {
                    if (error instanceof BadRequestError) {
                        let errorMessage: string = null;

                        switch (error.error as ParameterProtocol.ParameterValueAddError) {
                            case ParameterProtocol.ParameterValueAddError.ParameterNotExists:
                                errorMessage = 'Parent parameter does not exist';
                                break;
                            case ParameterProtocol.ParameterValueAddError.ParameterSourceNotExists:
                                errorMessage = 'Parameter source not exists';
                                break;
                            case ParameterProtocol.ParameterValueAddError.ValueAlreadyExists:
                                errorMessage = 'Parameter value already exists';
                                break;
                            case ParameterProtocol.ParameterValueAddError.ValueNotExists:
                                errorMessage = 'Parameter value not exists';
                                break;
                            case ParameterProtocol.ParameterValueAddError.DependentParameterNotExists:
                                errorMessage = 'Dependent parameter not exists';
                                break;
                            case ParameterProtocol.ParameterValueAddError.DependentSourceNotExists:
                                errorMessage = 'Dependent source not exists';
                                break;
                            case ParameterProtocol.ParameterValueAddError.DependentValueNotExists:
                                errorMessage = 'Dependent value not exists';
                                break;
                        }
                        this.notificationService.error(errorMessage || error);
                    } else {
                        this.notificationService.error(error);
                    }
                }
            );
    }

    renameValue(newValue, oldValue, id) {
        const body = new ParameterProtocol.RenameParameterValueRequest();
        body.newValue = newValue;
        body.oldValue = oldValue;
        this.hyperionParameterService
            .renameParameterValue(body, id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.notificationService.success(`Parameter renamed`);
                    this.cdr.detectChanges();
                },
                error => {
                    if (error instanceof BadRequestError) {
                        let errorMessage: string = null;

                        switch (error.error as ParameterProtocol.ParameterValueRenameError) {
                            case ParameterProtocol.ParameterValueRenameError.ParameterNotExists:
                                errorMessage = 'Parameter does not exist';
                                break;
                            case ParameterProtocol.ParameterValueRenameError.InvalidParameter:
                                errorMessage = 'Parameter invalid';
                                break;
                            case ParameterProtocol.ParameterValueRenameError.ValueNotExists:
                                errorMessage = 'Parameter value not exists';
                                break;
                            case ParameterProtocol.ParameterValueRenameError.ValueAlreadyExists:
                                errorMessage = 'Parameter value already exists';
                                break;
                        }
                        this.notificationService.error(errorMessage || error);
                    } else {
                        this.notificationService.error(error);
                    }
                }
            );
    }

    deleteValue(index) {
        const body = new ParameterProtocol.RemoveParameterValueRequest;
        body.dependentValue = this.activeNode.item.values[index].dependentValue;
        body.value = this.activeNode.item.values[index].value;
        this.hyperionParameterService
            .removeParameterValue(body, this.activeNode.item.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(
                response => {
                    this.activeNode.item.values.splice(index, 1);
                    this.notificationService.success(`Parameter updated`);
                    this.cdr.detectChanges();
                },
                error => {
                    if (error instanceof BadRequestError) {
                        let errorMessage: string = null;

                        switch (error.error as ParameterProtocol.ParameterValueRemoveError) {
                            case ParameterProtocol.ParameterValueRemoveError.ParameterNotExists:
                                errorMessage = 'Parameter not exists';
                                break;
                            case ParameterProtocol.ParameterValueRemoveError.ValueNotExists:
                                errorMessage = 'Value not exists';
                                break;
                            case ParameterProtocol.ParameterValueRemoveError.LinkedToChildren:
                                errorMessage = 'Linked to children';
                                break;
                            case ParameterProtocol.ParameterValueRemoveError.LinkedToDependants:
                                errorMessage = 'Linked to dependants';
                                break;
                            case ParameterProtocol.ParameterValueRemoveError.DependentParameterNotExists:
                                errorMessage = 'Dependent parameter not exists';
                                break;
                            case ParameterProtocol.ParameterValueRemoveError.DependentSourceNotExists:
                                errorMessage = 'Dependent source not exists';
                                break;
                            case ParameterProtocol.ParameterValueRemoveError.DependentValueNotExists:
                                errorMessage = 'Dependent value not exists';
                                break;
                        }
                        this.notificationService.error(errorMessage || error);
                    } else {
                        this.notificationService.error(error);
                    }
                }
            );
    }

    // Other method

    addListValue() {
        if (this.activeNode.item.values.indexOf(null) === -1) {
            this.activeNode.item.values.push(<ParameterValue>{value: null, dependentValue: null});
        }
        setTimeout(() => {
            this.windowRefService.focusElementById('lov' + (this.activeNode.item.values.length - 1), 20);
        }, 100);
    }

    createParameterDependent(node: TreeNode<Parameter>) {
        if (node.item.title === '' || (this.parameterized && node.item.dependentId === 0) || (this.parameterized && node.item.dependentId === null)) {
            this.notificationService.error(`Fill in title and dependent parameter`);
        } else {
            this.createParameter(this.activeNode);
        }
        this.cdr.detectChanges();
    }

    compareValues(a, b) {
        let comparison = 0;
        if (a.value > b.value) {
            comparison = 1;
        } else {
            comparison = -1;
        }
        return comparison;
    }

    checkUncheckParameterValue(value) {
        let result = false;
        this.activeNode.item.values.forEach((item, index) => {
            if (item.value === value) {
                this.deleteValue(index);
                result = true;
            }
        });

        if (!result) {
            this.activeNode.item.values.push(<ParameterValue>{value: value, dependentValue: null});
            this.addValue(value, null);
        }
    }

    checkUncheckDepParameterValue(value, dependentValue) {
        let result = false;
        this.activeNode.item.values.forEach((item, index) => {
            if (item.value === value && item.dependentValue === dependentValue) {
                this.deleteValue(index);
                result = true;
            }
        });

        if (!result) {
            this.activeNode.item.values.push(<ParameterValue>{value: value, dependentValue: dependentValue});
            this.addValue(value, dependentValue);
        }
    }

    checkedDepListValues(value, dependentValue): boolean {
        let result = false;
        this.activeNode.item.values.forEach((item, index) => {
            if (item.value === value && item.dependentValue === dependentValue) {
                result = true;
            }
        });
        return result;
    }

    checkedListOfValue(value): boolean {
        let result = false;
        this.activeNode.item.values.forEach((item, index) => {
            if (item.value === value) {
                result = true;
            }
        });
        return result;
    }

    filterValueChecked() {
        if (this.filterParameter) {
            this.filteredListOfValues = [];
            this.parameterList.get(this.activeNode.item.parentId).item.values.filter(
                (v) => {
                    return this.activeNode.item.values.some((v2) => {
                        return v.value === v2.value; // && v.item == v2.item
                    });
                }).forEach(item => {
                this.filteredListOfValues.push(item.value);
            });
            this.filterParameter = false;
        } else {
            this.filteredListOfValues = [];
            this.parameterList.get(this.activeNode.item.parentId).item.values.forEach(item => this.filteredListOfValues.push(item.value));
            this.filterParameter = true;
        }

        this.cdr.detectChanges();
    }

    filterDepValuesCollapse() {
        if (this.filterDepParameter.size === 0) {
            this.dependentListOfValues.forEach(item => {
                this.filterDepValuesChecked(item);
            });
        } else {
            this.dependentListOfValues.forEach(item => {
                this.filterDepParameter.add(item);
            });
            this.dependentListOfValues.forEach(item => {
                this.filterDepValuesChecked(item);
            });
        }
    }

    /**
     * Fill filteredDepListOfValues with values of dependent parameter
     *
     * @param depValue - value of the parameter that it depends on
     */

    filterDepValuesChecked(depValue: string): void {
        this.filteredDepListOfValues = new Map<string, any>();
        if (this.filterDepParameter.has(depValue)) {
            this.filterDepParameter.delete(depValue);
        } else {
            this.filterDepParameter.add(depValue);
        }

        if (this.activeNode.item.dependentId) {
            this.parameterList.get(this.activeNode.item.dependentId).item.values.forEach(val => {
                const values = [];
                if (this.filterDepParameter.has(val.value)) {
                    this.parameterList.get(this.activeNode.item.parentId).item.values.filter(
                        (v) => {
                            return this.activeNode.item.values.some((v2) => {
                                return v.value === v2.value && v2.dependentValue === val.value;
                            });
                        }).forEach(item => {
                        values.push(item.value);
                    });
                } else {
                    this.parameterList.get(this.activeNode.item.parentId).item.values.forEach(item => {
                        values.push(item.value);
                    });
                }
                this.filteredDepListOfValues.set(val.value, values);
            });
        }
        this.cdr.detectChanges();
    }


    saveListValues(event, index) {
        if (event.target.value) {
            if (this.activeNode.item.values[index].value === null && this.activeNode.item.values.filter(item => item.value === event.target.value).length === 0) {
                this.activeNode.item.values[index].value = event.target.value;
                this.addValue(event.target.value, null);
            } else {
                if (this.activeNode.item.values[index].value !== event.target.value) {
                    this.renameValue(event.target.value, this.activeNode.item.values[index].value, this.activeNode.item.id);
                    this.activeNode.item.values[index].value = event.target.value;
                }
            }
        } else {
            this.activeNode.item.values.forEach((item, indx) => {
                if (item.value === null) {
                    this.activeNode.item.values.splice(indx, 1);
                }
            });
        }
        if (event instanceof KeyboardEvent && event.key === 'Enter') {
            this.addListValue();
        }
    }

    focusParameter() {
        if (this.activeQueryId) {
            const node = this.parameterList.get(this.activeQueryId);
            if (node) {
                const parentsNodeId = this.getAllParentId(node);
                setTimeout(() => {
                    this.dynamicTree.expandTreeNodesById(parentsNodeId);
                    const element = this.render.selectRootElement('#input-parameter-' + node.item.id);
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
        }
    }

    getMapAsIterable(map: Map<number, any>): Array<DbProtocol.Parameter> {
        const parameters = [];
        const currentId = this.rootParentNode(this.activeNode);
        map.forEach(mapItem => {
            if (mapItem.id !== currentId) {
                parameters.push(mapItem);
            }
        });
        parameters.sort((a, b) => {
            return a.id - b.id;
        });
        return parameters;
    }

    rootParentNode(node: DataProtocol.TreeNode<DbProtocol.Parameter>): any {
        if (node.item.parentId) {
            return this.rootParentNode(this.parameterList.get(node.item.parentId));
        } else {
            return node.item.id;
        }
    }

    updateAfterDrop(event: { dropType: DropPlace, draggedNode: TreeNode<Parameter>, targetNode: DataProtocol.TreeNode<DbProtocol.SetupStep> }): void {
        const dropType = event.dropType,
            draggedNode = event.draggedNode,
            targetNode = event.targetNode;
        let parentId = null;

        switch (dropType) {
            case DropPlace.Before:
            case DropPlace.After:
                parentId = targetNode.item.parentId;
                break;
            case DropPlace.Into:
                parentId = targetNode.item.id;

                break;
            case DropPlace.Root:
                draggedNode.item.parentId = null;
                break;
        }

        this.updateParameter(draggedNode, parentId);
    }
}

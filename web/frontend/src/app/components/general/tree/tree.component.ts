import { Component, Input, Output, EventEmitter, TemplateRef, ContentChild, ElementRef, OnInit, OnChanges, OnDestroy, ChangeDetectorRef, ViewChild } from '@angular/core';
import { NestedTreeControl } from '@angular/cdk/tree';
import { MatTree, MatTreeNestedDataSource } from '@angular/material/tree';
import { Subject } from 'rxjs';
import { IConfigTree } from '../../../shared/interfaces/config-tree';
import { DropPlace } from '../../../shared/interfaces/drop-place';
import { NotificationService, NotificationType } from '../../../core/services/notification.service';
import { WindowRefService } from '../../../core/services/window-ref.service';
import * as DataProtocol from '../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../protocol/db-protocol.data';

@Component({
    selector: 'm-tree',
    templateUrl: 'tree.component.html',
    styleUrls: ['./tree.component.scss'],
})

export class TreeComponent implements OnChanges, OnDestroy {
    @ContentChild('addButtonTemplate') addButtonTemplate: TemplateRef<ElementRef>;
    @ContentChild('addParaButtonTemplate') addParaButtonTemplate: TemplateRef<ElementRef>;
    @ContentChild('nodeTitleTemplate') nodeTitleTemplate: TemplateRef<ElementRef>;
    @ViewChild('treeSelector') tree: MatTree<any>;
    @Input() rootNode: any;
    @Input() filterFn: (node: any, filterText: string) => boolean;
    @Input() filterText: string;
    @Input() hiddenNodeSet: Set<any>;
    @Input() orderingFn: (a: any, b: any) => number;
    @Input() config: IConfigTree;
    @Input() canDrag: boolean;
    @Input() canDropInto: (draggedNode: any, targetNode: any) => boolean;
    @Input() canDropBefore: (draggedNode: any, targetNode: any) => boolean;
    @Input() canDropAfter: (draggedNode: any, targetNode: any) => boolean;
    @Input() customAddTemplate: string;
    @Input() precheckedNodeSet: Set<any>;
    @Input() showCheckboxes: boolean;
    @Input() websocketNodeAdd: any;
    @Input() websocketNodeRemove: any;
    @Input() websocketNodeModifyEvent: any;
    @Input() nodeMap: Map<number, any>;
    @Input() itemMap: Map<number, any>;
    @Output() nodeMapChange = new EventEmitter();
    @Output() itemMapChange = new EventEmitter();
    @Output() itemMapRefresh = new EventEmitter();
    @Output() getCheckedNodeSet = new EventEmitter<Set<any>>();
    @Output() selectNode = new EventEmitter();
    @Output() treeChanged = new EventEmitter();
    @Output() nodeAdded = new EventEmitter<any>();
    @Output() parametricNodeAdded = new EventEmitter<any>();
    @Output() nodeRemoved = new EventEmitter<any>();
    @Output() nodeModified = new EventEmitter<object>();
    @Output() showNotification = new EventEmitter<{ type: NotificationType, message: string }>();
    destroy$ = new Subject<any>();
    treeControl = new NestedTreeControl<any>(node => node.children);
    dataSource = new MatTreeNestedDataSource<any>();
    expandedNodeId = new Set<number>();
    expandedNodeSet = new Set<any>();
    checkedNodeSet = new Set<any>();
    indeterminatedNodeSet = new Set<any>();
    fullNodeMap = [];
    draggedNode: any;
    dragOverRoot = false;
    delayBeforeHoveredNodeExpand = 300;
    hoveredNode: any;
    draggedNodeHoveredSince: number;
    draggedNodeHoveredType: number;
    dropPlaceEnum = DropPlace;
    notificationTypeEnum = NotificationType;
    prevNewNode: any;
    prevUpdatedNode: any;
    prevRemovedNode: any;
    alreadySetPrechecked = false;
    // checkNodeType: (node: any) => string = checkNodeType;

    constructor(
        private cdr: ChangeDetectorRef,
        private notificationService: NotificationService,
        private windowRefService: WindowRefService,
    ) {
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    ngOnChanges(): void {
        if (this.rootNode && this.rootNode.children.length > 0) {
            console.log('========== onChanges ===========');

            this.dataSource.data = [];
            this.dataSource = new MatTreeNestedDataSource<any>();
            this.dataSource.data = this.rootNode.children;
            this.dataSource.data.sort(this.orderingFn);
            this.updateDataSource();
            this.expandedNodeSet.clear();
            this.expandedNodeId.forEach(id => this.findNodeInDataSource(this.dataSource.data, id));
            this.expandedNodeSet.forEach(node => this.toggleTreeNode(node, true));
            this.nodeMap.clear();
            if (!this.itemMap) this.itemMap = new Map<number, any>();
            this.rootNode.children.forEach(child => this.walkTreeNode(child));
            // @FIXME
            setTimeout(() => this.windowRefService.focusElementById('input-setup-0', 0), 200)
            setTimeout(() => this.windowRefService.focusElementById('input-parameter-0', 0), 200)
            if (this.precheckedNodeSet) {
                const newArr = [];
                this.rootNode.children.forEach((item, index) => {
                    newArr.push(this.flatTree(this.rootNode.children[index])
                    );
                });

                // clear flat tree
                newArr.forEach(item => {
                    this.fullNodeMap.push(item.reduce((acc, current) => {
                        const x = acc.find(el => el.id === current.id);
                        if (!x) {
                            return acc.concat([current]);
                        } else {
                            return acc;
                        }
                    }, []));
                });

                this.fullNodeMap.forEach(item => {
                    const result = [];
                    item.forEach(elem => {
                        if (elem.draft === false && elem.group === false && this.checkedNodeSet.has(elem.id)) {
                            result.push(elem);
                        }
                    });
                    if (item.length === result.length) {
                    }
                });
                // preload tree
                this.precheckedNodeSet.forEach(item => this.checkUncheckNodeParent(this.searchNode(item), true, true));
            }
        } else {
            this.dataSource.data = [];
        }

        if (this.websocketNodeAdd) {
            if (this.prevNewNode !== this.websocketNodeAdd) {

                this.prevNewNode = this.websocketNodeAdd;
                if (!this.nodeMap.has(this.websocketNodeAdd.item.id)) {
                    this.websocketNodeAdd.item.parentId
                        ?  this.nodeMap.get(this.websocketNodeAdd.item.parentId).children.push(this.websocketNodeAdd)
                        : this.rootNode.children.push(this.websocketNodeAdd);
                    this.itemMap.set(this.websocketNodeAdd.item.id, this.websocketNodeAdd.item);
                    this.updateDataSource();
                    this.tree.renderNodeChanges(this.dataSource.data);
                }
            }
        }

        if (this.websocketNodeModifyEvent) {
            if (this.prevUpdatedNode !== this.websocketNodeModifyEvent) {
                this.prevUpdatedNode = this.websocketNodeModifyEvent;
                if (this.nodeMap.has(this.websocketNodeModifyEvent.item.id)) {
                    const pristine = this.itemMap.get(this.websocketNodeModifyEvent.item.id),
                        node = this.nodeMap.get(this.websocketNodeModifyEvent.item.id);

                    node.item = this.websocketNodeModifyEvent.item;
                    this.nodeMap.set(node.item.id, node);
                    this.setItemMapByType(node);
                    this.moveTreeNode(node, pristine.parentId, this.websocketNodeModifyEvent.item.parentId);
                    this.dataSource.data.sort(this.orderingFn);
                    this.tree.renderNodeChanges(this.dataSource.data);
                }
            }
        }

        if (this.websocketNodeRemove) {
            if (this.prevRemovedNode !== this.websocketNodeRemove) {
                if (this.nodeMap.has(this.websocketNodeRemove.item.id)) {
                    this.nodeMap.delete(this.websocketNodeRemove.item.id);
                    this.itemMap.delete(this.websocketNodeRemove.item.id);

                    const parent = this.websocketNodeRemove.item.parentId
                        ? this.nodeMap.get(this.websocketNodeRemove.item.parentId)
                        : null,
                        childIndex = parent
                            ? parent.children.findIndex(node => node.item.id === this.websocketNodeRemove.item.id)
                            : this.rootNode.children.findIndex(node => node.item.id === this.websocketNodeRemove.item.id);

                    if (childIndex !== -1) {
                        parent ? parent.children.splice(childIndex, 1) : this.rootNode.children.splice(childIndex, 1);
                    }

                    this.updateDataSource();
                }
            }
        }
        if (!this.alreadySetPrechecked && this.precheckedNodeSet) {
            this.checkedNodeSet = this.precheckedNodeSet;
            this.alreadySetPrechecked = true;
        }

        this.itemMapRefresh.emit(this.nodeMap);
    }

    flatTree(node: any): any {
        const result = new Array;
        result.push({ id: node.item.id, parentId: node.item.parentId, node: node });
        if (node.children) {
            node.children.forEach(child => {
                result.push({ id: child.item.id, parentId: child.item.parentId, node: child });
                if (child.children.length > 0) {
                    result.push(this.flatTree(child));
                }
            });
        }
        // @ts-ignore
        const res = result.flat(1);
        return res;
    }

    private walkTreeNode(node: any): void {
        if (node && node.item.id !== 0 && this.itemMap) {
            this.setItemMapByType(node);
            this.nodeMap.set(node.item.id, node);
            node.children.forEach(child => this.walkTreeNode(child));
        }
    }

    private setItemMapByType(node: any): void {
        switch (node.item.constructor) {
            case DbProtocol.SetupStep:
                this.itemMap.set(node.item.id, DbProtocol.SetupStep.fromJson(node.item.toJson()));
                break;
            case DbProtocol.TestCase:
                this.itemMap.set(node.item.id, DbProtocol.TestCase.fromJson(node.item.toJson()));
                break;
                case DbProtocol.Parameter:
                this.itemMap.set(node.item.id, DbProtocol.Parameter.fromJson(node.item.toJson()));
                break;
        }
    }

    private moveTreeNode(node: DataProtocol.TreeNode<DbProtocol.SetupStep>, fromParentId: number | null, toParentId: number | null): void {
        if (fromParentId === toParentId) {
            return;
        }

        node.item.parentId = toParentId;

        const parentNode = fromParentId ? this.nodeMap.get(fromParentId) : null,
            childIndex = parentNode
                ? parentNode.children.findIndex(elem => elem.item.id === node.item.id)
                : this.rootNode.children.findIndex(elem => elem.item.id === node.item.id);

        if (childIndex !== -1) {
            parentNode ? parentNode.children.splice(childIndex, 1) : this.rootNode.children.splice(childIndex, 1);
        }

        toParentId ? this.nodeMap.get(toParentId).children.push(node) : this.rootNode.children.push(node);

        this.dataSource.data = this.rootNode.children;
        this.updateDataSource();
    }

    private updateDataSource(): void {

        this.dataSource.data = [];
        const time = performance.now();
        this.dataSource = new MatTreeNestedDataSource<any>();
        this.dataSource.data = this.rootNode.children;
        this.dataSource.data.sort(this.orderingFn);
    }

    treeNodeHasDraftChildren(node: any): boolean {
        return node.children.filter(child => child.item.id === 0).length > 0;
    }

    findNodeInDataSource(childArray: Array<any>, id: number): void {
        childArray.forEach(node => {
            if (node.item.id === id) {
                this.expandedNodeSet.add(node);
                return;
            } else {
                this.findNodeInDataSource(node.children, id);
            }
        });
    }

    toggleTreeNode(node: any, expand: boolean) {
        expand ? this.treeControl.expand(node) : this.treeControl.collapse(node);
        expand ? this.expandedNodeId.add(node.item.id) : this.expandedNodeId.delete(node.item.id);
    }

    toggleAllChildTreeNode(node: any, expand: boolean): void {
        expand ? this.treeControl.expand(node) : this.treeControl.collapse(node);
        expand ? this.expandedNodeId.add(node.item.id) : this.expandedNodeId.delete(node.item.id);
        node.children.forEach(child => this.toggleAllChildTreeNode(child, expand));
    }

    toggleOneChildTreeNode(node: any, expand: boolean): void {
        expand ? this.treeControl.expand(node) : this.treeControl.collapse(node);
        expand ? this.expandedNodeId.add(node.item.id) : this.expandedNodeId.delete(node.item.id);
        if (node.item.parentId) {
            this.toggleOneChildTreeNode(this.nodeMap.get(node.item.parentId), true);
        }
    }

    expandAllTreeNodes(): void {
        this.dataSource.data.forEach(node => this.toggleAllChildTreeNode(node, true));
    }

    collapseAllTreeNodes(): void {
        this.dataSource.data.forEach(node => this.toggleAllChildTreeNode(node, false));
    }

    checkUncheckNodeParent(node: any, checked: boolean, start: boolean): void {
        if (node) {
            if (start) {
                this.indeterminatedNodeSet.delete(node.item.id);
                checked && !node.item.isDraft ? this.checkedNodeSet.add(node.item.id) : this.checkedNodeSet.delete(node.item.id);

                if (node.children.length > 0) {
                    node.children.forEach(item => {
                        checked && !node.item.isDraft ? this.checkedNodeSet.add(item.item.id) : this.checkedNodeSet.delete(item.item.id);
                        this.indeterminatedNodeSet.delete(item.item.id);
                        this.checkUncheckNodeChild(item, checked);
                    });
                }
            }

            let countChilCheck = 0,
                countChilCheckIndeterminate = 0,
                countChilLength = 0;

            if (node.children.length > 0) {
                node.children.forEach((chil) => {
                    if (this.indeterminatedNodeSet.has(chil.item.id)) {
                        countChilCheckIndeterminate++;
                    }

                    if (this.checkedNodeSet.has(chil.item.id)) {
                        countChilCheck++;
                    }
                    countChilLength++;
                });
            }

            if (countChilCheck === countChilLength && countChilCheck !== 0 && countChilLength !== 0) {
                this.indeterminatedNodeSet.delete(node.item.id);
                checked && !node.item.isDraft ? this.checkedNodeSet.add(node.item.id) : this.checkedNodeSet.delete(node.item.id);
                this.indeterminatedNodeSet.add(node.item.parentId);
                this.checkUncheckNodeParent(this.searchNode(node.item.parentId), checked, false);
            } else {
                if (node.item.parentId) {
                    this.fullNodeMap.forEach((item, index) => {
                        let nodeId = 0,
                            countNeighbor = 0,
                            countIndeterminate = 0;
                        item.forEach(el => {
                            if (el.parentId === node.item.parentId && el.parentId !== null && node.item.parentId !== null) {
                                nodeId = el.parentId; // addd set
                                countNeighbor++;
                                if (this.checkedNodeSet.has(el.id)) {
                                    countIndeterminate++;
                                }
                            }
                        });

                        if (countIndeterminate > 0) {
                            checked = true;
                        }

                        if (countNeighbor > 0) {
                            this.checkedNodeSet.delete(nodeId);
                            checked && !node.item.isDraft ? this.indeterminatedNodeSet.add(nodeId) : this.indeterminatedNodeSet.delete(nodeId);
                            this.checkUncheckNodeParent(this.searchNode(nodeId), checked, false);
                        }
                    });
                } else {
                    if (countChilCheck > 0) {
                        this.indeterminatedNodeSet.add(node.item.id);
                    }
                }
            }
        }
    }

    checkUncheckNodeChild(node: any, checked: boolean): void {
        if (node.children) {
            node.children.forEach(item => {
                this.indeterminatedNodeSet.delete(item.item.id);
                checked && !node.item.isDraft ? this.checkedNodeSet.add(item.item.id) : this.checkedNodeSet.delete(item.item.id);
                this.checkUncheckNodeChild(item, checked);
            });
        }
    }

    searchNode(nodeId): any {
        let result;

        this.fullNodeMap.forEach((item, index) => {
            item.forEach(el => {
                if (el.id === nodeId) {
                    result = el.node;
                }
            });
        });
        return result;
    }

    // Drag'n'Drop
    setDraggedNode(event: MouseEvent, node: DataProtocol.TreeNode<DbProtocol.SetupStep>) {
        this.draggedNode = node;
    }

    unsetDraggedNode(node: DataProtocol.TreeNode<DbProtocol.TestCase>) {
        this.draggedNode = null;
    }

    onDragOverRoot(event: any): void {
        this.dragOverRoot = true;
        event.preventDefault();
    }

    onDragLeaveRoot(): void {
        this.dragOverRoot = false;
    }

    onDropToRoot(event: any): void {
        this.dragOverRoot = false;
        this.nodeModified.emit({ dropType: DropPlace.Root, draggedNode: this.draggedNode, targetNode: null });
        this.onDragEnd(event);
    }

    onDragOver(event, node: DataProtocol.TreeNode<DbProtocol.SetupStep>): void {
        event.preventDefault();
        this.draggedNodeHoveredType = NaN;

        // Handle node expand
        if (this.hoveredNode && node === this.hoveredNode) {
            if ((Date.now() - this.draggedNodeHoveredSince) > this.delayBeforeHoveredNodeExpand) {
                if (!this.treeControl.isExpanded(node)) {
                    this.treeControl.expand(node);
                }
            }
        } else {
            this.hoveredNode = node;
            this.draggedNodeHoveredSince = new Date().getTime();
        }

        // Handle drag area
        const percentageY = event.offsetY / event.target.clientHeight;

        if (percentageY > 0.15 && percentageY < 0.85) {
            this.draggedNodeHoveredType = this.dropPlaceEnum.Into;
        } else if (percentageY < 0.15) {
            this.draggedNodeHoveredType = this.dropPlaceEnum.Before;
        } else if (percentageY > 0.85) {
            this.draggedNodeHoveredType = this.dropPlaceEnum.After;
        }
    }

    onDrop(event, targetNode: any): void {
        if (targetNode !== this.draggedNode) {
            if (this.draggedNodeHoveredType === this.dropPlaceEnum.Into) {
                if (!this.canDropInto(this.draggedNode, targetNode)) {
                    // FIXME: tree component should not be responsible for displaying this message
                    this.showNotification.emit({ type: this.notificationTypeEnum.Error, message: 'Forbidden' });
                } else if (this.draggedNode.item.parentId === targetNode.item.id) {
                    // FIXME: tree component should not be responsible for displaying this message
                    this.showNotification.emit({ type: this.notificationTypeEnum.Error, message: 'You try drop to parent' });
                } else {
                    this.nodeModified.emit({ dropType: DropPlace.Into, draggedNode: this.draggedNode, targetNode: targetNode });
                }
            } else if (this.draggedNodeHoveredType === this.dropPlaceEnum.Before && this.canDropBefore(this.draggedNode, targetNode)) {
                this.nodeModified.emit({ dropType: DropPlace.Before, draggedNode: this.draggedNode, targetNode: targetNode });
            } else if (this.draggedNodeHoveredType === this.dropPlaceEnum.After && this.canDropAfter(this.draggedNode, targetNode)) {
                this.nodeModified.emit({ dropType: DropPlace.After, draggedNode: this.draggedNode, targetNode: targetNode });
            }
        }

        this.onDragEnd(event);
    }

    onDragStart(event: DragEvent, node): void {
        // Required by Firefox (https://stackoverflow.com/questions/19055264/why-doesnt-html5-drag-and-drop-work-in-firefox)
        event.dataTransfer.setData('foo', 'bar');
    }

    onDragEnd(event): void {
        this.draggedNode = null;
        this.hoveredNode = null;
        this.draggedNodeHoveredSince = 0;
        this.draggedNodeHoveredType = NaN;
        event.preventDefault();
    }

    onAddNode(node: any): void {
        this.nodeAdded.emit(node);
    }

    onRemoveNode(node: any): void {
        this.nodeRemoved.emit(node);
    }

    onCheckNode(node: any, checked: boolean): void {
        this.checkUncheckNodeParent(node, checked, true);
        this.getCheckedNodeSet.emit(this.checkedNodeSet);
        this.cdr.detectChanges();
    }
}

import { ChangeDetectionStrategy, ChangeDetectorRef, Component, ContentChild, ElementRef, EventEmitter,  Input, OnChanges, OnInit, Output, TemplateRef } from '@angular/core';
import { TreeNode } from '../../../protocol/data-protocol.data';
import { Parameter, SetupStep, TestCase } from '../../../protocol/db-protocol.data';
import { NotificationType } from '../../../core/services/notification.service';
import { DropPlace } from '../../../shared/interfaces/drop-place';

export class DynamicNode<T> extends TreeNode<T> {
    expanded = false;
    item?: T = null;
    children: Array<DynamicNode<T>> = [];
    constructor(node: TreeNode<T>, expandedNodesSet: Set<number>) {
        super();
        this.item = node.item;
        this.children = this.convertToDynamicNodes(node.children, expandedNodesSet);
        // @ts-ignore  - ignored because linter doesn't know about props in  class T
        this.expanded = expandedNodesSet.has(node.item.id);
    }

    convertToDynamicNodes(nodes: Array<TreeNode<T>>, expandedNodesSet: Set<number>): Array<DynamicNode<T>> {
        return nodes.map(node => new DynamicNode(node, expandedNodesSet));
    }
}

@Component({
    selector: 'm-dynamic-tree',
    templateUrl: 'dynamic-tree.component.html',
    styleUrls: ['dynamic-tree.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class DynamicTreeComponent {

    @ContentChild('addButtonTemplate') addButtonTemplate: TemplateRef<ElementRef>;
    @ContentChild('addParaButtonTemplate') addParaButtonTemplate: TemplateRef<ElementRef>;
    @ContentChild('nodeTitleTemplate') nodeTitleTemplate: TemplateRef<ElementRef>;
    @Input() set inputNodes(value: Array<TreeNode<TestCase | SetupStep | Parameter>>) {
        if (this.nodes.length) {
            this.expandedNodeSet.clear();
            this.saveExpandedNode(this.nodes);
        }
        this.nodes = value.map(node => new DynamicNode(node, this.expandedNodeSet));
        this.flatNodes = [];
        this.walkDynamicTreeNodes(this.nodes);
        this.hasDraftChildrenSet.clear();
        this.checkDraftChildren(this.nodes);
        this.indeterminateNodeSet = this.fillIndeterminateNodeSetId();
    }

    @Input() dimmedNodeSet = new Set<number>();
    @Input() hiddenNodeSet = new Set<number>();
    @Input() canAdd = true;
    @Input() canRemove = true;

    @Input() showCheckboxes = false;
    @Input() set precheckedNodeIdSet(value: Set<number>) {
        this.checkedNodeSet = value;
    }

    @Input() canDrag: boolean;
    @Output() nodeAdded = new EventEmitter<number>();
    @Output() nodeRemoved = new EventEmitter<any>();
    @Output() nodeModified = new EventEmitter<object>();
    @Output() showNotification = new EventEmitter<{ type: NotificationType, message: string }>();
    @Output() checkedNodeSetChange = new EventEmitter<Set<any>>();

    checkedNodeSet = new Set<number>();
    indeterminateNodeSet  = new Set<any>();

    nodes: Array<DynamicNode<TestCase | SetupStep | Parameter>> = [];
    flatNodes: Array<DynamicNode<TestCase | SetupStep | Parameter>> = [];
    expandedNodeSet = new Set<number>();
    hasDraftChildrenSet = new Set<number>();
    draggedNode: DynamicNode<TestCase| SetupStep | Parameter>;
    hoveredNode: DynamicNode<TestCase| SetupStep | Parameter>;
    dragOverRoot = false;
    delayBeforeHoveredNodeExpand = 300;
    draggedNodeHoveredSince: number;
    draggedNodeHoveredType: number;
    dropPlaceEnum = DropPlace;
    notificationTypeEnum = NotificationType;
    constructor(private cdr: ChangeDetectorRef) {
    }

    public expandTreeNodes(nodes = this.nodes): void {
        nodes.forEach(node => {
            node.expanded = true;
            this.expandTreeNodes(node.children);
        });
    }

    public expandTreeNodesById(nodesId: Array<number>): void {
        this.flatNodes.filter(elem => nodesId.includes(elem.item.id)).forEach(node => node.expanded = true);
        this.cdr.detectChanges();
    }

    public collapseTreeNodes(nodes = this.nodes): void {
        nodes.forEach(node => {
            node.expanded = false;
            this.collapseTreeNodes(node.children);
        });
    }

    private walkDynamicTreeNodes(nodes: Array<DynamicNode<TestCase | SetupStep | Parameter>>): void {
        this.flatNodes = this.flatNodes.concat(nodes);
        nodes.forEach(node => this.walkDynamicTreeNodes(node.children));
    }

    public onAddNode(node: DynamicNode<TestCase | SetupStep | Parameter>): void {
        node.expanded = true;
        setTimeout(() => this.cdr.detectChanges());
        this.nodeAdded.emit(node.item.id);
    }

    public onRemoveNode(item: DynamicNode<TestCase | SetupStep | Parameter>): void {
        this.nodeRemoved.emit(item);
    }

    private saveExpandedNode(nodes: Array<DynamicNode<TestCase| SetupStep | Parameter>>): void {
        this.expandedNodeSet = new Set([...nodes.filter(node => node.expanded).map(node => node.item.id), ...this.expandedNodeSet]);
        nodes.forEach(node => this.saveExpandedNode(node.children));
    }

    private checkDraftChildren(nodes: Array<DynamicNode<TestCase| SetupStep | Parameter>>): void {
        this.hasDraftChildrenSet = new Set([
            ...nodes.filter(node => node.children.find(child => child.item.id === 0))
                    .map(node => node.item.id), ...this.hasDraftChildrenSet]);
        nodes.forEach(node => this.checkDraftChildren(node.children));
    }

    treeNodeHasDraftChildren(node: any): boolean {
        return node.children.filter(child => child.item.id === 0).length > 0;
    }

    fillIndeterminateNodeSetId(): Set<number> {
        const result = this.flatNodes.filter(node => this.checkedNodeSet.has(node.item.id))
            .reduce((acc, cur) => acc.concat(this.getAllParentId(cur)), []);
        return new Set(result);
    }

    /**
     * Function manipulate two sets - checkedNodeSet and indeterminateNodeSet.
     * Any node can be set only in one, because indeterminate state more powerful than checked
     * and shows one sign on top of the other
     * this is a feature of using the mat-checkbox
     *
     * @param node - node that was selected
     * @param checked - boolean mark or demark checkbox
     */
    onCheckNode(node: any, checked: boolean): void {
        if (checked) {
            this.checkedNodeSet = new Set<number>([...this.checkedNodeSet, node.item.id, ...this.getAllChildrenId(node)]);
            this.indeterminateNodeSet = new Set<number>([...this.indeterminateNodeSet, ...this.getAllParentId(node)]);
            if (node.item.parentId) {
                this.markParent(this.getNode(node.item.parentId));
            }
        } else {
            this.checkedNodeSet.delete(node.item.id);
            this.indeterminateNodeSet.delete(node.item.id);
            this.getAllChildrenId(node).forEach(children => {
                this.checkedNodeSet.delete(children);
                this.indeterminateNodeSet.delete(children);
            });
            if (node.item.parentId) {
                this.unmarkParent(this.getNode(node.item.parentId));
            }
        }
        this.checkedNodeSetChange.emit(this.checkedNodeSet);
    }

    markParent(node: DynamicNode<TestCase| SetupStep | Parameter>): void {
        if (this.checkAllChildrenAreChecked(node.item.id)) {
            this.checkedNodeSet.add(node.item.id);
            this.indeterminateNodeSet.delete(node.item.id);
            if (node.item.parentId) {
                this.markParent(this.getNode(node.item.parentId));
            }
        }
    }

    unmarkParent(node: DynamicNode<TestCase| SetupStep | Parameter>): void {
        if (this.checkAllChildrenAreUnchecked(node.item.id)) {
            this.checkedNodeSet.delete(node.item.id);
            this.indeterminateNodeSet.delete(node.item.id);
            if (node.item.parentId) {
                this.unmarkParent(this.getNode(node.item.parentId));
            }
        } else {
            this.checkedNodeSet.delete(node.item.id);
            this.indeterminateNodeSet.add(node.item.id);
            if (node.item.parentId) {
                this.unmarkParent(this.getNode(node.item.parentId));
            }
        }
    }

    checkAllChildrenAreChecked(id: number): boolean {
        return this.getNode(id).children.every(child => this.checkedNodeSet.has(child.item.id));
    }

    checkAllChildrenAreUnchecked(id: number): boolean {
        return this.getNode(id).children.every(child => !this.checkedNodeSet.has(child.item.id));
    }

    getAllParentId(node: DynamicNode<TestCase| SetupStep | Parameter>): Array<number> {
        const result = [];
        while (node && node.item.parentId !== 0) {
            const parentNode = this.flatNodes.find(elem => elem.item.id === node.item.parentId);
            result.push(parentNode ? parentNode.item.id : null);
            node = parentNode;
        }
        return result.filter(elem => !!elem);
    }

    getAllChildrenId(node: DynamicNode<TestCase| SetupStep | Parameter>): Array<number> {
        return node.children.reduce(
            (acc, value) => [
                ...acc,
                [value.item.id],
                value.children.map(child => this.getAllChildrenId(child))
                    .reduce((prev, curr) => prev.concat(curr), [])], []).concat(node.item.id)
            .reduce((prev, curr) => prev.concat(curr), []);
    }

    getNode(id: number): DynamicNode<TestCase| SetupStep | Parameter> {
        return this.flatNodes.find(node => node.item.id === id);
    }

    /**
     * Drag'n'drop
     */
    setDraggedNode(event: MouseEvent, node: DynamicNode<TestCase| SetupStep | Parameter>) {
        this.draggedNode = node;
    }

    unsetDraggedNode() {
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

    onDragOver(event, node: DynamicNode<TestCase| SetupStep | Parameter>): void {
        event.preventDefault();
        this.draggedNodeHoveredType = NaN;

        // Handle node expand
        if (this.hoveredNode && node === this.hoveredNode) {
            if ((Date.now() - this.draggedNodeHoveredSince) > this.delayBeforeHoveredNodeExpand) {
                if (!node.expanded) {
                    node.expanded = true;
                    this.cdr.detectChanges();
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

    onDrop(event, targetNode: DynamicNode<TestCase| SetupStep | Parameter>): void {
        if (targetNode !== this.draggedNode) {
            if (this.draggedNodeHoveredType === this.dropPlaceEnum.Into) {
                if (!this.canDropInto(this.draggedNode, targetNode)) {
                    this.showNotification.emit({ type: this.notificationTypeEnum.Error, message: 'Forbidden' });
                } else if (this.draggedNode.item.parentId === targetNode.item.id) {
                    this.showNotification.emit({ type: this.notificationTypeEnum.Error, message: 'You try drop to parent' });
                } else {
                    this.nodeModified.emit({ dropType: DropPlace.Into, draggedNode: this.nodeConverter(this.draggedNode), targetNode: this.nodeConverter(targetNode) });
                }
            } else if (this.draggedNodeHoveredType === this.dropPlaceEnum.Before && this.canDropBefore(this.draggedNode, targetNode)) {
                this.nodeModified.emit({ dropType: DropPlace.Before, draggedNode: this.nodeConverter(this.draggedNode), targetNode: this.nodeConverter(targetNode) });
            } else if (this.draggedNodeHoveredType === this.dropPlaceEnum.After && this.canDropAfter(this.draggedNode, targetNode)) {
                this.nodeModified.emit({ dropType: DropPlace.After, draggedNode: this.nodeConverter(this.draggedNode), targetNode: this.nodeConverter(targetNode) });
            }
        }

        this.onDragEnd(event);
    }

    onDragStart(event: DragEvent, node: DynamicNode<TestCase| SetupStep | Parameter>): void {
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

    nodeConverter(node: DynamicNode<TestCase| SetupStep | Parameter>): TreeNode<TestCase| SetupStep | Parameter> {
        const result  = new TreeNode<TestCase| SetupStep | Parameter>();
        result.children = node.children;
        result.item = node.item;
        return result;
    }

    verifyChildrenNode(dragged: DynamicNode<TestCase| SetupStep | Parameter>, target: DynamicNode<TestCase| SetupStep | Parameter>): boolean {
        return target.item.parentId === dragged.item.id
            ? false
            : target.item.parentId
                ? this.verifyChildrenNode(dragged, this.flatNodes.find(node => node.item.id === target.item.parentId))
                : true;
    }

    canDropInto(draggedNode: DynamicNode<TestCase| SetupStep | Parameter>, targetNode: DynamicNode<TestCase| SetupStep | Parameter>): boolean {
        if (!draggedNode || !targetNode) {
            return false;
        }

        return draggedNode.item.parentId !== targetNode.item.id && this.verifyChildrenNode(draggedNode, targetNode);
    }

    canDropAfter(draggedNode: DynamicNode<TestCase| SetupStep | Parameter>, targetNode: DynamicNode<TestCase| SetupStep | Parameter>) {
        if (!draggedNode || !targetNode) {
            return false;
        }

        return draggedNode.item.id !== targetNode.item.id && this.verifyChildrenNode(draggedNode, targetNode);
    }

    canDropBefore(draggedNode: DynamicNode<TestCase| SetupStep | Parameter>, targetNode: DynamicNode<TestCase| SetupStep | Parameter>) {
        if (!draggedNode || !targetNode) {
            return false;
        }
        return draggedNode.item.id !== targetNode.item.id && this.verifyChildrenNode(draggedNode, targetNode);
    }
}

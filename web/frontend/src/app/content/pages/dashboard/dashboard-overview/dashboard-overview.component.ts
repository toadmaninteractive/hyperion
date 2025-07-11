import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy, ViewChild } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { FileAttachment, FileAttachmentLink, Project } from '../../../../protocol/db-protocol.data';
import { TestCase } from '../../../../protocol/db-protocol.data';
import { DynamicTreeComponent } from '../../../../components/general/dynamic-tree/dynamic-tree.component';
import { HyperionAttachmentService } from '../../../../protocol/attachment-protocol.service';
import { HyperionTestCaseService } from '../../../../protocol/test-case-protocol.service';
import { FileAttachmentOrderBy } from '../../../../protocol/attachment-protocol.data';
import { OrderDirection, TreeNode } from '../../../../protocol/data-protocol.data';

@Component({
    selector: 'm-dashboard-overview',
    templateUrl: './dashboard-overview.component.html',
    styleUrls: ['./dashboard-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DashboardOverviewComponent implements OnDestroy {
    @ViewChild(DynamicTreeComponent) tree: DynamicTreeComponent;
    destroy$ = new Subject<any>();
    project: Project;
    dimmedNodeSetId = new Set<number>();
    hiddenNodeSetId = new Set<number>();
    nodes: Array<TreeNode<TestCase>> = [] ;
    flatNodes: Array<TestCase> = [];
    itemNodes: Array<TreeNode<TestCase>> = [];

    constructor (
        private route: ActivatedRoute,
        private hyperionAttachmentService: HyperionAttachmentService,
        private hyperionTestCaseService: HyperionTestCaseService,
        private cdr: ChangeDetectorRef,
    ) {
        this.route.data
            .pipe(takeUntil(this.destroy$))
            .subscribe(data => this.initialize(data.activeProject));
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    private initialize(project: Project): void {
        // FIXME: dummy usage
        this.hyperionAttachmentService
            .getProjectAttachments(project.id, FileAttachmentOrderBy.OriginalFilename, OrderDirection.Asc, 0, 10, 'bomb')
            .subscribe(resp => {
                console.log('Project file attachments', resp.items);

                const attachment = resp.items[0];

                if (attachment instanceof FileAttachment) {
                    this.hyperionAttachmentService
                        .getAttachmentLinks(attachment.id)
                        .subscribe(resp1 => console.log('Attachment links', attachment, resp1.items));
                }
            });

        this.hyperionTestCaseService
            .getTestCaseTree(project.id)
            .subscribe(resp => {
                this.nodes = resp.items;
                this.walkDynamicTreeNodes(this.nodes);
                this.cdr.detectChanges();
            });
    }

    private walkDynamicTreeNodes(nodes: Array<TreeNode<TestCase>>): void {
        this.flatNodes.push(...nodes.map(node => node.item));
        this.itemNodes.push(...nodes);
        nodes.forEach(node => this.walkDynamicTreeNodes(node.children));
    }

    public expandAllTreeNodes(): void {
        this.tree.expandTreeNodes();
    }

    public collapseAllTreeNodes(): void {
        this.tree.collapseTreeNodes();
    }

    filterCallback(item: TestCase, filterText: string): boolean {
        if (!filterText)
            return true;
        return item.title.toLocaleLowerCase().indexOf(filterText.toLocaleLowerCase()) > -1;
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

    applyFilter(filterText: string): void {
        this.dimmedNodeSetId = new Set<number>(this.flatNodes.map(item => !this.filterCallback(item, filterText) ? item.id : null));
        this.hiddenNodeSetId = new Set<number>(this.itemNodes.filter(node => this.getAllChildrenId(node).every(id => this.dimmedNodeSetId.has(id)))
                                                            .map(node => node.item.id));
        this.expandAllTreeNodes();
    }
}

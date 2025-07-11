import { ChangeDetectorRef, Component, Inject, OnDestroy, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { merge, Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { WebsocketService } from '../../../../core/services/websocket.service';
import { HyperionTestCaseService } from '../../../../protocol/test-case-protocol.service';
import { TestCase, TestStatus } from '../../../../protocol/db-protocol.data';
import { TreeNode } from '../../../../protocol/data-protocol.data';
import { TestCaseCreated, TestCaseDeleted, TestCaseUpdated } from '../../../../protocol/notification-protocol.data';


export interface IBrowseCase {
    projectId: number;
    activeCaseSet: Set<number> | null;
    activeCaseStatusMap: Map<number, TestStatus> | null;
}

@Component({
    selector: 'm-test-case-browse-dialog',
    templateUrl: 'test-case-browse-dialog.component.html',
    styleUrls: ['./test-case-browse-dialog.component.scss'],
})
export class TestCaseBrowseDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    precheckedCasesSet = new Set<number>();
    caseSet = new Set<number>();
    nodes: Array<TreeNode<TestCase>> = [];

    constructor (
        private cdr: ChangeDetectorRef,
        public dialogRef: MatDialogRef<TestCaseBrowseDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: IBrowseCase,
        private hyperionTestCaseService: HyperionTestCaseService,
        private websocketService: WebsocketService,
    ) { }

    ngOnInit(): void {
        this.destroy$ = new Subject();

        this.data.activeCaseSet.forEach(item => {
            this.precheckedCasesSet.add(item);
            this.caseSet.add(item);
        });

        this.hyperionTestCaseService
            .getTestCaseTree(this.data.projectId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(cases => {
                this.nodes = cases.items
                    .filter(testCase => !testCase.item.isDraft);
            });

        merge(
            this.websocketService.testCaseCreated,
            this.websocketService.testCaseDeleted,
            this.websocketService.testCaseUpdated,
        ).pipe(
            takeUntil(this.destroy$),
            filter(event => event.data.projectId === this.data.projectId)
        ).subscribe(event => {
            const node = new TreeNode<TestCase>();
            node.item = event.data;
            node.children = [];

            if (event instanceof TestCaseCreated) {
                if (node.item.parentId) {
                    const parentNode = this.getNode(node.item.parentId);
                    parentNode.children.push(node);
                } else {
                    this.nodes.push(node);
                }
            } else if (event instanceof TestCaseUpdated) {
                node.children = this.getNode(node.item.id).children;
                const updatedNode = this.getNode(node.item.id);
                updatedNode.children = [...node.children];
                updatedNode.item = TestCase.fromJson(node.item.toJson());
            } else if (event instanceof TestCaseDeleted) {
                const parent = this.getNode(node.item.parentId);
                parent.children.splice(parent.children.findIndex(elem => elem.item.id === node.item.id), 1);
            }
            this.nodes = [...this.nodes];
            this.cdr.detectChanges();
        });
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    // Data source helpers
    matchCasesSet(): boolean {
        if (this.data.activeCaseSet.size !== this.caseSet.size) {
            return false;
        }

        let result = true;

        this.data.activeCaseSet.forEach(item => {
            if (!this.caseSet.has(item)) {
                result = false;
            }
        });

        return result;
    }

    testStatusText(testStatus: TestStatus): string {
        switch (testStatus) {
            case TestStatus.Pending: return 'Pending';
            case TestStatus.Blocked: return 'Blocked';
            case TestStatus.Passed: return 'Passed';
            case TestStatus.Failed: return 'Failed';
            case TestStatus.InProgress: return 'In Progress';
        }
    }

    badgeTestClass(status: TestStatus) {
        switch (status) {
            case TestStatus.Pending: return 'm-badge--metal';
            case TestStatus.InProgress: return 'm-badge--info';
            case TestStatus.Blocked: return 'm-badge--warning';
            case TestStatus.Failed: return 'm-badge--danger';
            case TestStatus.Passed: return 'm-badge--success';
        }
    }

    orderingCallback(firstNode: TreeNode<TestCase>, secondNode: TreeNode<TestCase>): number {
        return firstNode.item.orderNum > secondNode.item.orderNum ? 1 : -1;
    }

    getCheckedNodeCallback(checkedSet: Set<number>): void {
        this.caseSet = checkedSet;
    }

    getNode(id: number): TreeNode<TestCase> | null {
        const stack = [];
        let node;
        stack.push(...this.nodes);

        while (stack.length > 0) {
            node = stack.pop();
            console.log(node);
            if (node.item.id === id) {
                return node;
            } else if (node.children && node.children.length) {
                stack.push(node.children);
            }
        }
        return null;
    }
}

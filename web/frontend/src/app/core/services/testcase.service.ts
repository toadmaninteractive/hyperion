import { Injectable } from '@angular/core';
import { BehaviorSubject, combineLatest, merge } from 'rxjs';
import { TestCase } from '../../protocol/db-protocol.data';
import { WebsocketService } from './websocket.service';
import { HyperionTestCaseService } from '../../protocol/test-case-protocol.service';
import { distinctUntilChanged, filter, switchMap } from 'rxjs/operators';
import { ProjectService } from './project.service';
import { TreeNode } from '../../protocol/data-protocol.data';
import * as DbProtocol from '../../protocol/db-protocol.data';
import * as DataProtocol from '../../protocol/data-protocol.data';
import * as NotificationProtocol from '../../protocol/notification-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class TestCaseService {

    testCaseTree$ = new BehaviorSubject<Map<number, TreeNode<TestCase>>>(null);
    testCaseList$ = new BehaviorSubject<Map<number, TreeNode<TestCase>>>(null);
    flatTestCase$ = new BehaviorSubject<Map<number, TestCase>>(null);
    localTestCaseTree = new Map<number, TreeNode<TestCase>>(null);
    localTestCaseList = new Map<number, TreeNode<TestCase>>(null);
    localFlatTestCase = new Map<number, TestCase>(null);

    prevProjectId: number;

    constructor(
        private hyperionTestCaseService: HyperionTestCaseService,
        private projectService: ProjectService,
        private websocketService: WebsocketService) {

        //this.consoleAll();
        this.projectService.activeProject$
            .asObservable()
            .pipe(
                distinctUntilChanged(),
                switchMap(project => this.hyperionTestCaseService.getTestCaseTree(project.id))
            )
            .subscribe(response => {
                this.localTestCaseTree.clear();
                this.localTestCaseTree = new Map(response.items.map(testCase => [testCase.item.id, testCase]));
                this.testCaseTree$.next(this.localTestCaseTree);
            });

        this.testCaseTree$
            .asObservable()
            .pipe(filter(testCase => !!testCase))
            .subscribe(nodes => {
                nodes.forEach(node => this.walkTreeNode(node));
                this.testCaseList$.next(this.localTestCaseList);
                this.flatTestCase$.next(this.localFlatTestCase);
            });

        combineLatest([
            this.projectService.activeProject$
                .asObservable()
                .pipe(distinctUntilChanged()),
            merge(
                this.websocketService.testCaseCreated,
                this.websocketService.testCaseUpdated,
                this.websocketService.testCaseDeleted,
            )]).pipe(
            filter(([project, event]) => event.data.projectId === project.id),
        ).subscribe(([project, event]) => {
            this.prevProjectId = project.id;
            const node = new DataProtocol.TreeNode<DbProtocol.TestCase>();
            node.item = event.data;

            if (event instanceof NotificationProtocol.TestCaseCreated) {
                node.children = [];
                this.localTestCaseList.set(node.item.id, node);

                if (node.item.parentId) {
                    this.localTestCaseList.get(node.item.parentId).children.push(node);
                } else {
                    this.localTestCaseTree.set(node.item.id, node);
                }

            } else if (event instanceof NotificationProtocol.TestCaseUpdated) {
                node.children = this.localTestCaseList.get(node.item.id).children;
                const prevParent = this.localTestCaseList.get(this.localTestCaseList.get(node.item.id).item.parentId),
                    prevChildIndex = prevParent
                        ? prevParent.children.findIndex(item => item.item.id === node.item.id)
                        : -1;
                this.localTestCaseList.set(node.item.id, node);
                const parent = node.item.parentId
                    ? this.localTestCaseList.get(node.item.parentId)
                    : null;
                console.log(node.item.parentId, {...prevParent}, {...parent});
                if (prevParent && parent && prevParent.item.id === parent.item.id) {
                    prevParent.children.splice(prevChildIndex, 1, node);
                } else {
                    if (prevChildIndex !== -1) {
                        prevParent.children.splice(prevChildIndex, 1);
                    } else {
                        this.localTestCaseTree.delete(node.item.id);
                    }

                    if (parent) {
                        parent.children.push(node);
                    } else  {
                        this.localTestCaseTree.set(node.item.id, node);
                    }
                }

            } else if (event instanceof NotificationProtocol.TestCaseDeleted) {
                this.localTestCaseList.delete(node.item.id);
                const parent = node.item.parentId
                    ? this.localTestCaseList.get(node.item.parentId)
                    : null;
                if (parent) {
                    const childIndex = parent.children.findIndex(item => item.item.id === node.item.id);
                    if (childIndex !== -1) {
                        parent.children.splice(childIndex, 1);
                    }
                } else {
                    this.localTestCaseTree.delete(node.item.id);
                }

            }
            this.localTestCaseTree = new Map(this.localTestCaseTree);
            this.testCaseTree$.next(this.localTestCaseTree);
        });
    }

    private walkTreeNode(node: TreeNode<TestCase>): void {
        if (node && node.item.id !== 0) {
            this.localTestCaseList.set(node.item.id, node);
            this.localFlatTestCase.set(node.item.id, node.item);
            node.children.forEach(child => this.walkTreeNode(child));
        }
    }

    public addDraftTestCase(node: TreeNode<TestCase>, parentNode?: TreeNode<TestCase>): void {
        if (parentNode) {
            parentNode.children.push(node);
        } else {
            this.localTestCaseTree.set(0, node);
        }
        this.localTestCaseTree = new Map(this.localTestCaseTree);
        this.testCaseTree$.next(this.localTestCaseTree);
    }

    public clearDraftNode(parentId?: number): void {
        if (parentId) {
            const parentNode = this.localTestCaseList.get(parentId),
                removeIndex = parentNode.children.findIndex(child => child.item.id === 0);
            if (removeIndex !== -1) {
                parentNode.children.splice(removeIndex, 1);
            }
        } else {
            this.localTestCaseTree.delete(0);
        }
        this.localTestCaseTree = new Map(this.localTestCaseTree);
        this.testCaseTree$.next(this.localTestCaseTree);
    }

    private consoleAll(): void {
        this.testCaseTree$.subscribe(res => console.log('TestCase tree', res));
        this.testCaseList$.subscribe(res => console.log('TestCase list', res));
        this.projectService.activeProject$.subscribe(res => console.log('Project::', res));
    }
}

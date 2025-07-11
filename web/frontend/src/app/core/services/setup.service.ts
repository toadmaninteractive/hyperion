import { Injectable } from '@angular/core';
import { BehaviorSubject, combineLatest, merge, of, pipe } from 'rxjs';
import { SetupStep } from '../../protocol/db-protocol.data';
import { WebsocketService } from './websocket.service';
import { HyperionSetupService } from '../../protocol/setup-protocol.service';
import { distinctUntilChanged, filter, map, subscribeOn, switchMap, takeUntil } from 'rxjs/operators';
import { ProjectService } from './project.service';
import { TreeNode } from '../../protocol/data-protocol.data';
import set = Reflect.set;
import * as DbProtocol from '../../protocol/db-protocol.data';
import * as DataProtocol from '../../protocol/data-protocol.data';
import * as NotificationProtocol from '../../protocol/notification-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class SetupService {

    setupTree$ = new BehaviorSubject<Map<number, TreeNode<SetupStep>>>(null);
    setupList$ = new BehaviorSubject<Map<number, TreeNode<SetupStep>>>(null);
    flatSetup$ = new BehaviorSubject<Map<number, SetupStep>>(null);
    ready$ = new BehaviorSubject<boolean>(false);
    localSetupTree = new Map<number, TreeNode<SetupStep>>(null);
    localSetupList = new Map<number, TreeNode<SetupStep>>(null);
    localFlatSetup = new Map<number, SetupStep>(null);

    prevProjectId: number;

    constructor(
        private hyperionSetupService: HyperionSetupService,
        private projectService: ProjectService,
        private websocketService: WebsocketService) {

        this.projectService.activeProject$
            .asObservable()
            .pipe(
                distinctUntilChanged(),
                switchMap(project => this.hyperionSetupService.getSetup(project.id))
            )
            .subscribe(response => {
                this.localSetupTree.clear();
                this.localSetupTree = new Map(response.items.map(setup => [setup.item.id, setup]));
                this.setupTree$.next(this.localSetupTree);
            });

        this.setupTree$
            .asObservable()
            .pipe(filter(setup => !!setup))
            .subscribe(nodes => {
                nodes.forEach(node => this.walkTreeNode(node));
                this.setupList$.next(this.localSetupList);
                this.flatSetup$.next(this.localFlatSetup);
                this.ready$.next(true);
            });

        combineLatest([
            this.projectService.activeProject$
                .asObservable()
                .pipe(distinctUntilChanged()),
            merge(
                this.websocketService.setupStepCreated,
                this.websocketService.setupStepUpdated,
                this.websocketService.setupStepDeleted,
            )]).pipe(
            filter(([project, event]) => event.data.projectId === project.id),
        ).subscribe(([project, event]) => {
            this.ready$.next(false);
            this.prevProjectId = project.id;
            const node = new DataProtocol.TreeNode<DbProtocol.SetupStep>();
            node.item = event.data;

            if (event instanceof NotificationProtocol.SetupStepCreated) {
                node.children = [];
                this.localSetupList.set(node.item.id, node);

                if (node.item.parentId) {
                    this.localSetupList.get(node.item.parentId).children.push(node);
                } else {
                    this.localSetupTree.set(node.item.id, node);
                }

            } else if (event instanceof NotificationProtocol.SetupStepUpdated) {
                node.children = this.localSetupList.get(node.item.id).children;
                const prevParent = this.localSetupList.get(this.localSetupList.get(node.item.id).item.parentId),
                    prevChildIndex = prevParent
                        ? prevParent.children.findIndex(item => item.item.id === node.item.id)
                        : -1;
                this.localSetupList.set(node.item.id, node);
                const parent = node.item.parentId
                    ? this.localSetupList.get(node.item.parentId)
                    : null;
                if (prevParent && parent && prevParent.item.id === parent.item.id) {
                    prevParent.children.splice(prevChildIndex, 1, node);
                } else {
                    if (prevChildIndex !== -1) {
                        prevParent.children.splice(prevChildIndex, 1);
                    } else {
                        this.localSetupTree.delete(node.item.id);
                    }

                    if (parent) {
                        parent.children.push(node);
                    } else  {
                        this.localSetupTree.set(node.item.id, node);
                    }
                }
            } else if (event instanceof NotificationProtocol.SetupStepDeleted) {
                this.localSetupList.delete(node.item.id);
                const parent = node.item.parentId
                    ? this.localSetupList.get(node.item.parentId)
                    : null;
                if (parent) {
                    const childIndex = parent.children.findIndex(item => item.item.id === node.item.id);
                    if (childIndex !== -1) {
                        parent.children.splice(childIndex, 1);
                    }
                } else {
                    this.localSetupTree.delete(node.item.id);
                }

            }
            this.localSetupTree = new Map(this.localSetupTree);
            this.setupTree$.next(this.localSetupTree);
        });
    }

    private walkTreeNode(node: TreeNode<SetupStep>): void {
        if (node && node.item.id !== 0) {
            this.localSetupList.set(node.item.id, node);
            this.localFlatSetup.set(node.item.id, node.item);
            node.children.forEach(child => this.walkTreeNode(child));
        }
    }

    public addDraftSetupStep(node: TreeNode<SetupStep>, parentNode?: TreeNode<SetupStep>): void {
        if (parentNode) {
            parentNode.children.push(node);
        } else {
            this.localSetupTree.set(0, node);
        }
        this.localSetupTree = new Map(this.localSetupTree);
        this.setupTree$.next(this.localSetupTree);
    }

    public clearDraftNode(parentId?: number): void {
        if (parentId) {
            const parentNode = this.localSetupList.get(parentId),
                removeIndex = parentNode.children.findIndex(child => child.item.id === 0);
            if (removeIndex !== -1) {
                parentNode.children.splice(removeIndex, 1);
            }
        } else {
            this.localSetupTree.delete(0);
        }
        this.localSetupTree = new Map(this.localSetupTree);
        this.setupTree$.next(this.localSetupTree);
    }
}

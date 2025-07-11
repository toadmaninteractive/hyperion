import { Injectable } from '@angular/core';
import { BehaviorSubject, combineLatest, merge } from 'rxjs';
import { Parameter } from '../../protocol/db-protocol.data';
import { WebsocketService } from './websocket.service';
import { HyperionParameterService } from '../../protocol/parameter-protocol.service';
import { distinctUntilChanged, filter, switchMap } from 'rxjs/operators';
import { ProjectService } from './project.service';
import { TreeNode } from '../../protocol/data-protocol.data';
import * as DbProtocol from '../../protocol/db-protocol.data';
import * as DataProtocol from '../../protocol/data-protocol.data';
import * as NotificationProtocol from '../../protocol/notification-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class ParameterService {

    parameterTree$ = new BehaviorSubject<Map<number, TreeNode<Parameter>>>(null);
    parameterList$ = new BehaviorSubject<Map<number, TreeNode<Parameter>>>(null);
    flatParameter$ = new BehaviorSubject<Map<number, Parameter>>(null);
    localParameterTree = new Map<number, TreeNode<Parameter>>(null);
    localParameterList = new Map<number, TreeNode<Parameter>>(null);
    localFlatParameter = new Map<number, Parameter>(null);

    prevProjectId: number;

    constructor(
        private hyperionParameterService: HyperionParameterService,
        private projectService: ProjectService,
        private websocketService: WebsocketService) {

        //this.consoleAll();
        this.projectService.activeProject$
            .asObservable()
            .pipe(
                distinctUntilChanged(),
                switchMap(project => this.hyperionParameterService.getParameters(project.id))
            )
            .subscribe(response => {
                this.localParameterTree.clear();
                this.localParameterTree = new Map(response.items.map(parameter => [parameter.item.id, parameter]));
                this.parameterTree$.next(this.localParameterTree);
            });

        this.parameterTree$
            .asObservable()
            .pipe(filter(parameter => !!parameter))
            .subscribe(nodes => {
                this.localParameterList.clear();
                this.localFlatParameter.clear();
                nodes.forEach(node => this.walkTreeNode(node));
                this.parameterList$.next(this.localParameterList);
                this.flatParameter$.next(this.localFlatParameter);
            });

        combineLatest([
            this.projectService.activeProject$
                .asObservable()
                .pipe(distinctUntilChanged()),
            merge(
                this.websocketService.parameterCreated,
                this.websocketService.parameterUpdated,
                this.websocketService.parameterDeleted,
            )]).pipe(
            filter(([project, event]) => event.data.projectId === project.id),
        ).subscribe(([project, event]) => {
            this.prevProjectId = project.id;
            const node = new DataProtocol.TreeNode<DbProtocol.Parameter>();
            node.item = event.data;

            if (event instanceof NotificationProtocol.ParameterCreated) {
                node.children = [];
                this.localParameterList.set(node.item.id, node);

                if (node.item.parentId) {
                    this.localParameterList.get(node.item.parentId).children.push(node);
                } else {
                    this.localParameterTree.set(node.item.id, node);
                }

            } else if (event instanceof NotificationProtocol.ParameterUpdated) {
                node.children = this.localParameterList.get(node.item.id).children;
                const prevParent = this.localParameterList.get(node.item.parentId),
                    prevChildIndex = prevParent
                    ? prevParent.children.findIndex(item => item.item.id === node.item.id)
                    : -1;
                this.localParameterList.set(node.item.id, node);
                const parent = node.item.parentId
                    ? this.localParameterList.get(node.item.parentId)
                    : null;

                    if (prevParent && parent && prevParent.item.id === parent.item.id) {
                        prevParent.children.splice(prevChildIndex, 1, node);
                    } else {
                        if (prevChildIndex !== -1) {
                            prevParent.children.splice(prevChildIndex, 1);
                        }

                        if (parent) {
                            parent.children.push(node);
                        } else  {
                            this.localParameterTree.set(node.item.id, node);
                        }
                    }

            } else if (event instanceof NotificationProtocol.ParameterDeleted) {
                this.localParameterList.delete(node.item.id);
                const parent = node.item.parentId
                    ? this.localParameterList.get(node.item.parentId)
                    : null;
                if (parent) {
                    const childIndex = parent.children.findIndex(item => item.item.id === node.item.id);
                    if (childIndex !== -1) {
                        parent.children.splice(childIndex, 1);
                    }
                } else {
                    this.localParameterTree.delete(node.item.id);
                }

            }
            this.localParameterTree = new Map(this.localParameterTree);
            this.parameterTree$.next(this.localParameterTree);
        });
    }

    private walkTreeNode(node: TreeNode<Parameter>): void {
        if (node && node.item.id !== 0) {
            this.localParameterList.set(node.item.id, node);
            this.localFlatParameter.set(node.item.id, node.item);
            node.children.forEach(child => this.walkTreeNode(child));
        }
    }

    public addDraftParameter(node: TreeNode<Parameter>, parentNode?: TreeNode<Parameter>): void {
        if (parentNode) {
            parentNode.children.push(node);
        } else {
            this.localParameterTree.set(0, node);
        }
        this.localParameterTree = new Map(this.localParameterTree);
        this.parameterTree$.next(this.localParameterTree);
    }

    public clearDraftNode(parentId?: number): void {
        if (parentId) {
            const parentNode = this.localParameterList.get(parentId),
                removeIndex = parentNode.children.findIndex(child => child.item.id === 0);
            if (removeIndex !== -1) {
                parentNode.children.splice(removeIndex, 1);
            }
        } else {
            this.localParameterTree.delete(0);
        }
        this.localParameterTree = new Map(this.localParameterTree);
        this.parameterTree$.next(this.localParameterTree);
    }

    private consoleAll(): void {
        this.parameterTree$.subscribe(res => console.log('Parameter tree', res));
        this.parameterList$.subscribe(res => console.log('Parameter list', res));
        this.projectService.activeProject$.subscribe(res => console.log('Project::', res));
    }
}

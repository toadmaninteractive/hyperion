import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject, combineLatest, merge, Observable, of } from 'rxjs';
import { filter, map, switchMap, distinctUntilChanged, tap } from 'rxjs/operators';
import * as objectPath from 'object-path';
import { MenuConfigService } from './metronic/menu-config.service';
import { AccountService } from './account.service';
import { StorageService } from './storage.service';
import { HyperionProjectService } from '../../protocol/project-protocol.service';
import * as DbProtocol from '../../protocol/db-protocol.data';
import * as NotificationProtocol from '../../protocol/notification-protocol.data';
import { WebsocketService } from './websocket.service';
import { NotificationService } from './notification.service';

@Injectable({
    providedIn: 'root',
})
export class ProjectService {
    projects = Array<DbProtocol.Project>();
    projects$ = new BehaviorSubject<DbProtocol.Project[] | null>(null);
    activeProject$ = new BehaviorSubject<DbProtocol.Project | null | undefined>(undefined);
    activeRole$ = new BehaviorSubject<DbProtocol.AccessRole | null | undefined>(undefined);
    roleMap$ = new BehaviorSubject<Map<string, number> | null | undefined>(undefined);

    constructor(
        private router: Router,
        private menuConfigService: MenuConfigService,
        private accountService: AccountService,
        private storageService: StorageService,
        private hyperionProjectService: HyperionProjectService,
        private websocketService: WebsocketService,
        private notificationService: NotificationService,
    ) {
        // Request projects if user is signed in
        this.accountService.isSignedIn()
            .pipe(
                filter(isSignedIn => isSignedIn === true),
                distinctUntilChanged(),
                switchMap(_ => this.refresh()),
            )
            .subscribe(projects => projects);

        this.activeProject$
            .asObservable()
            .pipe(
                filter(project => !!project),
                distinctUntilChanged(),
                tap(project => {
                    this.storageService.setLastProjectKey(project.key);
                }),
                switchMap(project => this.hyperionProjectService.getMyRolesForProject(project.id)),
            )
            .subscribe(acl => {
                // first load page
                let role: DbProtocol.AccessRole | null = null;
                if (acl instanceof DbProtocol.PersonnelAccountRole) {
                    const groupRoles = Object.values(acl.groupRoles || {}).map(gr => DbProtocol.AccessRole.fromJson(gr.role) || 0),
                        roles = [acl.userRole || 0, ...groupRoles];
                    role = roles.sort((a, b) => a < b ? 1 : -1)[0] || null;
                }
                this.activeRole$.next(role);
            });

        merge(
            this.websocketService.projectCreated,
            this.websocketService.projectUpdated,
            this.websocketService.projectDeleted
        ).subscribe(event => {
            if (event instanceof NotificationProtocol.ProjectCreated) {
                if (!this.projects.find(proj => proj.id === event.data.id)) {
                    this.projects.push(event.data);
                    this.projects.sort((a, b) => a.key > b.key ? 1 : -1);
                    this.notificationService.success(`Project ${event.data.title} added`);
                }
            } else if (event instanceof NotificationProtocol.ProjectUpdated) {
                const index = this.projects.findIndex(proj => proj.id === event.data.id);
                if (index !== -1) {
                    this.projects.splice(index, 1, event.data);
                    this.notificationService.success(`Project ${event.data.title} updated`);
                }
            } else if (event instanceof NotificationProtocol.ProjectDeleted) {
                const index = this.projects.findIndex(proj => proj.id === event.data.id);
                if (index !== -1) {
                    this.projects.splice(index, 1);
                    this.notificationService.success(`Project ${event.data.title} deleted`);
                }
            }
            this.projects$.next(this.projects);
        });

        this.projects$
            .pipe(
                filter(p => !!p),
                distinctUntilChanged(),
                switchMap((projects) => combineLatest([
                    of(projects),
                    combineLatest(projects.map(project => this.hyperionProjectService.getMyRolesForProject(project.id)))]),
                )
            ).subscribe(([project, roles]) => {
            const tempMap = new Map<string, number>();

            // FIXME
            roles.forEach((role, index) => {

                const groupRoles = Object.values(role.groupRoles || {}).map(gr => DbProtocol.AccessRole.fromJson(gr.role) || 0),
                    activeRoles = [role.userRole || 0, ...groupRoles];
                const activeRole = activeRoles.sort((a, b) => a < b ? 1 : -1)[0] || null;

                tempMap.set(project[index].key, activeRole);
            });
            this.roleMap$.next(tempMap);
        });
    }

    getActiveProject(url: string): DbProtocol.Project | null {
        const projects = this.projects$.getValue();
        let projectKey = this.storageService.getLastProjectKey();

        if (this.isDynamicRoute(url)) {
            const path = url.split(/[?#]/)[0],
                parts = path.split(/\//g);

            if (parts.length > 2) {
                projectKey = parts[2];
            }
        }

        if (!projectKey && projects && projects.length > 0) {
            projectKey = projects[0].key;
        }

        return (!projectKey || !projects)
            ? null
            : projects.filter(project => project.key === projectKey)[0] || null;
    }

    isDynamicRoute(url: string): boolean {
        // Get menu items
        const menuItems = objectPath.get(this.menuConfigService.configModel, 'config.aside.items');

        if (!(menuItems instanceof Array)) {
            return false;
        }

        const qs = url.split(/[?#]/)[1] || '';
        url = url.split(/[?#]/)[0];

        return menuItems.filter(item => item.dynamic && url.startsWith(item.page.replace(/\/:project/g, ''))).length > 0;
    }

    refresh(): Observable<DbProtocol.Project[]> {
        return this.hyperionProjectService
            .getProjects()
            .pipe(
                map(response => response.items),
                tap(projects => {
                        this.projects = projects;
                        this.projects$.next(projects);
                    }
                ),
            );
    }
}

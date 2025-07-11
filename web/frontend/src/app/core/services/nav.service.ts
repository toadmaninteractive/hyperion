import { Injectable } from '@angular/core';
import { Router, RouterEvent, NavigationEnd, NavigationStart } from '@angular/router';
import { combineLatest } from 'rxjs';
import { distinctUntilChanged, filter, map, startWith } from 'rxjs/operators';
import { ProjectService } from './project.service';
import { MenuConfigService } from './metronic/menu-config.service';
import { Constants } from '../../shared/config/constants';

@Injectable({
    providedIn: 'root',
})
export class NavService {
    constructor(
        private router: Router,
        private menuConfigService: MenuConfigService,
        private projectService: ProjectService,
    ) {
        combineLatest(
            [this.router.events.pipe(
                startWith(this.router.routerState.snapshot.url),
                filter((eventOrUrl: string | RouterEvent) => typeof eventOrUrl === 'string' || eventOrUrl instanceof NavigationStart),
            ),
                this.projectService.projects$.asObservable().pipe(
                    filter(project => !!project && project !== undefined),
                    distinctUntilChanged(),
                )]
        ).subscribe(([url, projects]) => {
            // url first load
            if (typeof url !== 'string') {
                const currentUrl = url ? url.url : this.router.routerState.snapshot.url;
                url = currentUrl.split(/[?#]/)[0];
                const urlParts = url.split(/\//g);

                if (url === '/404' && this.projectService.getActiveProject(url)) {
                    this.router.navigate(['projects']);
                }

                if (projects.filter(proj => proj.key === urlParts[2])[0]) {
                    this.projectService.activeProject$.next(projects.filter(proj => proj.key === urlParts[2])[0] || null);
                } else {
                    this.projectService.activeProject$.next(this.projectService.getActiveProject(url) || projects[0]);
                }
            }
        });

        combineLatest(
            [this.router.events.pipe(
                startWith(this.router.routerState.snapshot.url),
                filter((eventOrUrl: string | RouterEvent) => typeof eventOrUrl === 'string' || eventOrUrl instanceof NavigationEnd),
                map((eventOrUrl: string | NavigationEnd) => typeof eventOrUrl === 'string' ? eventOrUrl : (eventOrUrl.urlAfterRedirects || eventOrUrl.url)),
                map(url => decodeURI(url).split(/[?#]/)[0]),
            ),
                this.projectService.activeProject$.asObservable().pipe(
                    filter(project => project !== undefined),
                    distinctUntilChanged(),
                )]
        ).subscribe(([url, project]) => {
            // url = the first two times do not pass
            if (project === null) {
                this.router.navigate([Constants.errorUrl]);
            } else if (this.projectService.isDynamicRoute(url)) {
                const path = url.split(/[?#]/)[0],
                    parts = path.split(/\//g);
                if (parts.length < 3) {
                    this.switchProject(project.key);
                }
            }
        });
    }

    switchProject(projectKey: string = null, currentUrl: string = null): void {
        let url = currentUrl ? currentUrl : this.router.routerState.snapshot.url;
        const qs = url.split(/[?#]/)[1] || '';
        url = url.split(/[?#]/)[0];
        const isDynamic = this.projectService.isDynamicRoute(url);

        const projects = this.projectService.projects$.getValue();
        // FIXMe activeRole$ takes a long time to load(fast loading at first open a page)
        if (this.projectService.roleMap$.getValue()) {
            this.projectService.activeRole$.next(this.projectService.roleMap$.getValue().get(projectKey));
        }

        this.projectService.activeProject$.next(projects.filter(project => project.key === projectKey)[0] || null);

        if (!isDynamic) {
            return;
        }

        const urlParts = url.split(/\//g);

        while (urlParts.length < 3) {
            urlParts.push('');
        }

        urlParts[2] = projectKey;
        const newUrl = urlParts.join('/') + (qs ? `?${ qs }` : '');

        this.router.navigate([newUrl]);
    }
}

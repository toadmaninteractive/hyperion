import { AfterViewInit, ChangeDetectionStrategy, Component, ElementRef, HostBinding, Inject, OnInit, OnDestroy, ChangeDetectorRef } from '@angular/core';
import { DOCUMENT } from '@angular/common';
import { NavigationEnd, Router } from '@angular/router';
import { Subject } from 'rxjs';
import { filter } from 'rxjs/operators';
import { MenuAsideOffcanvasDirective } from '../../../shared/directives/menu-aside-offcanvas.directive';
import { ClassInitService } from '../../../core/services/metronic/class-init.service';
import { LayoutConfigService } from '../../../core/services/metronic/layout-config.service';
import { LayoutRefService } from '../../../core/services/metronic/layout/layout-ref.service';
import { MenuAsideService } from '../../../core/services/metronic/layout/menu-aside.service';
import { AccountService } from '../../../core/services/account.service';
import { ProjectService } from '../../../core/services/project.service';
import { StorageService } from '../../../core/services/storage.service';
import * as DbProtocol from '../../../protocol/db-protocol.data';

@Component({
    selector: 'm-aside-left',
    templateUrl: './aside-left.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AsideLeftComponent implements OnInit, AfterViewInit, OnDestroy {
    @HostBinding('class') classes = 'm-grid__item m-aside-left';
    @HostBinding('id') id = 'm_aside_left';
    @HostBinding('attr.mMenuAsideOffcanvas') mMenuAsideOffcanvas: MenuAsideOffcanvasDirective;
    destroy$: Subject<any>;
    currentRouteUrl = '';
    insideTm: any;
    outsideTm: any;

    constructor (
        private el: ElementRef,
        private cdr: ChangeDetectorRef,
        public classInitService: ClassInitService,
        public menuAsideService: MenuAsideService,
        public layoutConfigService: LayoutConfigService,
        private router: Router,
        private layoutRefService: LayoutRefService,
        public accountService: AccountService,
        public projectService: ProjectService,
        private storageService: StorageService,
        @Inject(DOCUMENT) private document: Document,
    ) {
        // Subscribe to menu classes update
        this.classInitService.onClassesUpdated$.subscribe(classes => {
            // Join classes array and pass to variable
            this.classes = 'm-grid__item m-aside-left ' + classes.aside_left.join(' ');
        });
    }

    ngAfterViewInit(): void {
        setTimeout(() => {
            this.mMenuAsideOffcanvas = new MenuAsideOffcanvasDirective(this.el);

            // Manually call directives' lifecycle hook method
            this.mMenuAsideOffcanvas.ngAfterViewInit();

            // Keep aside left element reference
            this.layoutRefService.addElement('asideLeft', this.el.nativeElement);
        });
    }

    ngOnInit() {
        this.destroy$ = new Subject();
        this.currentRouteUrl = this.router.url.split(/[?#]/)[0];

        this.router.events
            .pipe(filter(event => event instanceof NavigationEnd))
            .subscribe(event => this.currentRouteUrl = this.router.url.split(/[?#]/)[0]);
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    isMenuItemIsActive(item): boolean {
        if (item.submenu) {
            return this.isMenuRootItemIsActive(item);
        }

        if (!item.page) {
            return false;
        }

        const page = item.dynamic ? this.interpolate(item.page) : item.page;

        // Dashboard
        if (page !== '/' && this.currentRouteUrl.startsWith(page)) {
            return true;
        }

        return this.currentRouteUrl === page;
    }

    isMenuRootItemIsActive(item): boolean {
        for (const subItem of item.submenu) {
            if (this.isMenuItemIsActive(subItem)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Use for fixed left aside menu, to show menu on mouseenter event.
     * @param e Event
     */
    mouseEnter(e: Event) {
        // Check if left aside menu is fixed
        if (this.document.body.classList.contains('m-aside-left--fixed')) {
            if (this.outsideTm) {
                clearTimeout(this.outsideTm);
                this.outsideTm = null;
            }

            this.insideTm = setTimeout(() => {
                // If left aside menu is minimized
                if (this.document.body.classList.contains('m-aside-left--minimize') && mUtil.isInResponsiveRange('desktop')) {
                    // Show left aside menu
                    this.document.body.classList.remove('m-aside-left--minimize');
                    this.document.body.classList.add('m-aside-left--minimize-hover');
                }
            }, 300);
        }
    }

    /**
     * Use for fixed left aside menu, to show menu on mouseenter event.
     * @param e Event
     */
    mouseLeave(e: Event) {
        if (this.document.body.classList.contains('m-aside-left--fixed')) {
            if (this.insideTm) {
                clearTimeout(this.insideTm);
                this.insideTm = null;
            }

            this.outsideTm = setTimeout(() => {
                // If left aside menu is expand
                if (this.document.body.classList.contains('m-aside-left--minimize-hover') && mUtil.isInResponsiveRange('desktop')) {
                    // Hide back left aside menu
                    this.document.body.classList.remove('m-aside-left--minimize-hover');
                    this.document.body.classList.add('m-aside-left--minimize');
                }
            }, 500);
        }
    }

    matchesFor(expectedRole: string | null, profile: DbProtocol.PersonnelAccountProfile, role: DbProtocol.AccessRole): boolean {
        switch (expectedRole) {
            case 'maintainer': return profile && ((role || 0) >= DbProtocol.AccessRole.Maintainer || profile.isSuperadmin);
            case 'project_manager': return profile && (profile.isProjectManager || profile.isSuperadmin);
            case 'superadmin': return profile && profile.isSuperadmin;
            default: return true;
        }
    }

    interpolate(route: string): string {
        const project = this.projectService.activeProject$.getValue() || null,
            path = route.replace(/:project/g, project ? project.key : '');

            return path.endsWith('/') ? path.substr(0, path.length - 1) : path;
    }

    alterLink(item): string {
        const activeProject = this.projectService.activeProject$.getValue();

        return item.dynamic
            ? item.page.replace(':project', activeProject ? activeProject.key : '')
            : item.page;
    }
}

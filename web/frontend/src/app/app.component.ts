import { AfterViewInit, ChangeDetectionStrategy, Component, ElementRef, HostBinding, ViewChild } from '@angular/core';
import { DomSanitizer } from '@angular/platform-browser';
import { NavigationEnd, Router, RouteConfigLoadStart, NavigationStart  } from '@angular/router';
import { filter } from 'rxjs/operators';
import * as objectPath from 'object-path';

import { ClassInitService } from './core/services/metronic/class-init.service';
import { LayoutConfigService } from './core/services/metronic/layout-config.service';
import { PageConfigService } from './core/services/metronic/page-config.service';
import { SplashScreenService } from './core/services/metronic/splash-screen.service';
import { NavService } from './core/services/nav.service';
import { ProjectService } from './core/services/project.service';
import { StorageService } from './core/services/storage.service';

// LIST KNOWN ISSUES
// [Violation] Added non-passive event listener; https://github.com/angular/angular/issues/8866

@Component({
    // tslint:disable-next-line:component-selector
    selector: 'body[m-root]',
    templateUrl: './app.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppComponent implements AfterViewInit {
    @HostBinding('style') style: any;
    @HostBinding('class') classes: any = '';
    @ViewChild('splashScreen', { read: ElementRef }) splashScreen: ElementRef;

    constructor(
        private layoutConfigService: LayoutConfigService,
        private classInitService: ClassInitService,
        private sanitizer: DomSanitizer,
        private router: Router,
        private pageConfigService: PageConfigService,
        private splashScreenService: SplashScreenService,
        public navService: NavService,
        public projectService: ProjectService,
        public storageService: StorageService,
    ) {
        // subscribe to class update event
        this.classInitService.onClassesUpdated$.subscribe(classes => {
            // get body class array, join as string classes and pass to host binding class
            setTimeout(() => this.classes = classes.body.join(' '));
        });

        this.layoutConfigService.onLayoutConfigUpdated$
            .subscribe(model => {
                this.classInitService.setConfig(model);
                this.style = '';

                if (objectPath.get(model.config, 'self.layout') === 'boxed') {
                    const backgroundImage = objectPath.get(model.config, 'self.background');

                    if (backgroundImage) {
                        const imageUrl = objectPath.get(model.config, 'self.background');
                        this.style = this.sanitizer.bypassSecurityTrustStyle(`background-image: url(${imageUrl})`);
                    }
                }
            });

        // override config by router change from pages config
        this.router.events
            .pipe(filter(event => event instanceof NavigationEnd || event instanceof NavigationStart))
            .subscribe(event => {
                if (event instanceof NavigationEnd) {
                    this.layoutConfigService.setModel({page: objectPath.get(this.pageConfigService.getCurrentPageConfig(), 'config')}, true);
                }
            });
    }

    ngAfterViewInit(): void {
        if (this.splashScreen) {
            this.splashScreenService.init(this.splashScreen.nativeElement);
        }
    }
}

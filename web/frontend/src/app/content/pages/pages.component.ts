import { Component, OnInit, HostBinding, Input, ViewChild, ElementRef, AfterViewInit, ChangeDetectionStrategy } from '@angular/core';
import { Router, NavigationEnd, NavigationStart, RoutesRecognized } from '@angular/router';
import { AnimationBuilder, AnimationPlayer, style, animate } from '@angular/animations';
import { BehaviorSubject } from 'rxjs';
import * as objectPath from 'object-path';
import { ClassInitService } from '../../core/services/metronic/class-init.service';
import { LayoutConfigService } from '../../core/services/metronic/layout-config.service';
import { LayoutRefService } from '../../core/services/metronic/layout/layout-ref.service';

@Component({
    selector: 'm-pages',
    templateUrl: './pages.component.html',
    styleUrls: ['./pages.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PagesComponent implements OnInit, AfterViewInit {
    @HostBinding('class') classes = 'm-grid m-grid--hor m-grid--root m-page';
    @Input() selfLayout: any = 'blank';
    @Input() asideLeftDisplay: any;
    @Input() asideRightDisplay: any;
    @Input() asideLeftCloseClass: any;
    @ViewChild('mContentWrapper') contenWrapper: ElementRef;
    @ViewChild('mContent') mContent: ElementRef;
    pageBodyClass$: BehaviorSubject<string> = new BehaviorSubject<string>('');
    player: AnimationPlayer;

    constructor (
        private el: ElementRef,
        private configService: LayoutConfigService,
        public classInitService: ClassInitService,
        private router: Router,
        private layoutRefService: LayoutRefService,
        private animationBuilder: AnimationBuilder,
    ) {
        this.configService.onLayoutConfigUpdated$.subscribe(model => {
            const config = model.config;
            let pageBodyClass = '';
            this.selfLayout = objectPath.get(config, 'self.layout');

            if (this.selfLayout === 'boxed' || this.selfLayout === 'wide') {
                pageBodyClass += ' m-container m-container--responsive m-container--xxl m-page__container';
            }

            this.pageBodyClass$.next(pageBodyClass);
            this.asideLeftDisplay = objectPath.get(config, 'aside.left.display');
            this.asideRightDisplay = objectPath.get(config, 'aside.right.display');
        });

        this.classInitService.onClassesUpdated$.subscribe(classes => {
            this.asideLeftCloseClass = objectPath.get(classes, 'aside_left_close');
        });

        // Animate page load: trigger page appearance animation
        /*
        this.router.events.subscribe(event => {
            if (event instanceof NavigationStart) {
                console.log('[0] NavigationStart', event);

                if (this.contenWrapper) {
                    // Hide content
                    this.contenWrapper.nativeElement.style.display = 'none';
                }
            }

            if (event instanceof NavigationEnd) {
                const urlFrom = event.url.split(/[?#]/)[0],
                    urlTo = event.urlAfterRedirects.split(/[?#]/)[0];

                console.log('[2] NavigationEnd', event);

                if (this.contenWrapper && urlFrom !== urlTo) {
                    // Show content back
                    this.contenWrapper.nativeElement.style.display = '';

                    // Animate the content
                    this.animate(this.contenWrapper.nativeElement);
                }
            }
        });
        */
    }

    ngOnInit(): void { }

    ngAfterViewInit(): void {
        setTimeout(() => {
            if (this.mContent) {
                // keep content element in the service
                this.layoutRefService.addElement('content', this.mContent.nativeElement);
            }
        });
    }

    // Animate page load
    animate(element) {
        this.player = this.animationBuilder
            .build([
                style({ opacity: 0, transform: 'translateY(15px)' }),
                animate('500ms ease', style({ opacity: 1, transform: 'translateY(0)' })),
                style({ transform: 'none' }),
            ])
            .create(element);

        this.player.play();
    }
}

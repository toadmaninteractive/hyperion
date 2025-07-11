import { Component, HostBinding, ChangeDetectionStrategy } from '@angular/core';
import { DomSanitizer, SafeUrl } from '@angular/platform-browser';
import { BehaviorSubject } from 'rxjs';
import * as objectPath from 'object-path';
import { LayoutConfigService } from '../../../core/services/metronic/layout-config.service';
import { Constants } from '../../../shared/config/constants';

@Component({
    selector: 'm-footer',
    templateUrl: './footer.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FooterComponent {
    @HostBinding('class') classes = 'm-grid__item m-footer';
    footerContainerClass$: BehaviorSubject<string> = new BehaviorSubject('');
    yearFrom = Constants.yearFrom;
    yearTo = Constants.yearTo;
    companyTitle = Constants.companyTitle;
    slackDeveloperTeamUrl: SafeUrl;

    constructor(
        private sanitizer: DomSanitizer,
        private configService: LayoutConfigService,
    ) {
        this.slackDeveloperTeamUrl = this.sanitizer.bypassSecurityTrustUrl(Constants.slackDeveloperTeamUrl);

        this.configService.onLayoutConfigUpdated$.subscribe(model => {
            const config = model.config;

            let pageBodyClass = '';
            const selfLayout = objectPath.get(config, 'self.layout');

            if (selfLayout === 'boxed' || selfLayout === 'wide') {
                pageBodyClass += 'm-container--responsive m-container--xxl';
            } else {
                pageBodyClass += 'm-container--fluid';
            }

            this.footerContainerClass$.next(pageBodyClass);
        });
    }
}

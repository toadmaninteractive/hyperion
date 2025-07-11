import { ChangeDetectionStrategy, Component, HostBinding, OnInit, OnDestroy } from '@angular/core';
import { DomSanitizer, SafeUrl } from '@angular/platform-browser';
import { Constants } from '../../../shared/config/constants';
import { LayoutConfigService } from '../../../core/services/metronic/layout-config.service';
import { LayoutConfig } from '../../../shared/config/layout';

@Component({
    selector: 'm-auth',
    templateUrl: './auth.component.html',
    styleUrls: ['./auth.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AuthComponent implements OnInit, OnDestroy {
    @HostBinding('id') id = 'm_login';
    @HostBinding('class') classes = 'm-grid m-grid--hor m-grid--root m-page';
    yearFrom = Constants.yearFrom;
    yearTo = Constants.yearTo;
    companyTitle = Constants.companyTitle;
    companyWebsiteUrl: SafeUrl;
    slackSysadminDmUrl: SafeUrl;

    constructor (
        private sanitizer: DomSanitizer,
        private layoutConfigService: LayoutConfigService,
    ) {
        this.companyWebsiteUrl = this.sanitizer.bypassSecurityTrustUrl(Constants.companyWebsiteUrl);
        this.slackSysadminDmUrl = this.sanitizer.bypassSecurityTrustUrl(Constants.slackSysadminDmUrl);
    }

    ngOnInit(): void {
        this.layoutConfigService.setModel(new LayoutConfig({ content: { skin: '' } }), true);
    }

    ngOnDestroy(): void {
        this.layoutConfigService.reloadSavedConfig();
    }
}

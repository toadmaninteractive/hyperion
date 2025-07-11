import { ChangeDetectionStrategy, Component, HostBinding, Input, OnInit, Output, EventEmitter } from '@angular/core';
import { DomSanitizer, SafeStyle } from '@angular/platform-browser';
import { Router } from '@angular/router';

@Component({
    selector: 'm-user-profile',
    templateUrl: './user-profile.component.html',
    styleUrls: ['./user-profile.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class UserProfileComponent implements OnInit {
    // tslint:disable-next-line:max-line-length
    @HostBinding('class') classes = 'm-nav__item m-topbar__user-profile m-topbar__user-profile--img m-dropdown m-dropdown--medium m-dropdown--arrow m-dropdown--header-bg-fill m-dropdown--align-right m-dropdown--mobile-full-width m-dropdown--skin-light';
    @HostBinding('attr.m-dropdown-toggle') attrDropdownToggle = 'click';
    @Input() avatar = './assets/app/media/img/avatars/toadman_small.png';
    @Input() avatarBg: SafeStyle = '';
    @Input() fullName = '';
    @Input() companyTitle = '';
    @Output() signOut = new EventEmitter<any>();

    constructor (
        private router: Router,
        private sanitizer: DomSanitizer,
    ) { }

    ngOnInit(): void {
        if (!this.avatarBg) {
            this.avatarBg = this.sanitizer.bypassSecurityTrustStyle('url(./assets/app/media/img/misc/user_profile_bg.jpg)');
        }
    }
}

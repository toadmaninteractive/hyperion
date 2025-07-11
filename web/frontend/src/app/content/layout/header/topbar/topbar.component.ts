import { Component, OnInit, OnDestroy, HostBinding, Input, ChangeDetectionStrategy } from '@angular/core';
import { LayoutConfigService } from '../../../../core/services/metronic/layout-config.service';
import { Router } from '@angular/router';
import { Observable, Subject, of } from 'rxjs';
import { filter, map, takeUntil } from 'rxjs/operators';
import * as objectPath from 'object-path';

// Hyperion stuff
import { AccountService } from '../../../../core/services/account.service';
import { ChangeCase } from '../../../../shared/functions/change-case';
import { Constants } from '../../../../shared/config/constants';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';

@Component({
    selector: 'm-topbar',
    templateUrl: './topbar.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TopbarComponent implements OnInit, OnDestroy {
    @HostBinding('id') id = 'm_header_nav';
    @HostBinding('class') classes = 'm-stack__item m-stack__item--fluid m-header-head';
    @Input() searchType: any;
    destroy$: Subject<any>;
    fullName$: Observable<string>;
    companyTitle = Constants.companyTitle;

    constructor(
        private router: Router,
        private accountService: AccountService,
        private layoutConfigService: LayoutConfigService,
    ) {
        this.layoutConfigService.onLayoutConfigUpdated$
            .subscribe(model => {
                const config = model.config;
                this.searchType = objectPath.get(config, 'header.search.type');
            });
    }

    ngOnInit(): void {
        this.destroy$ = new Subject();

        this.fullName$ = this.accountService.profile$
            .pipe(
                filter(profile => profile instanceof DbProtocol.PersonnelAccountProfile),
                map(profile => ChangeCase.toTitleCase(profile.name || profile.username.replace(/\./g, ' '))),
                takeUntil(this.destroy$)
            );
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    public signOut() {
        this.accountService
            .signOut()
            .subscribe(
                (response: DataProtocol.GenericResponse): void => { /* TODO: maybe display error box */ },
                error => console.log('Sign-out failed, error: ', error)
            );
    }
}

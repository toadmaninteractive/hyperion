import { Component, ChangeDetectionStrategy, ChangeDetectorRef, HostBinding } from '@angular/core';
import { DomSanitizer, SafeUrl } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { combineLatest } from 'rxjs';
import { filter, finalize, first } from 'rxjs/operators';

// Hyperion stuff
import { Constants } from '../../../../shared/config/constants';
import { AccountService } from '../../../../core/services/account.service';
import { ProjectService } from '../../../../core/services/project.service';
import { StorageService } from '../../../../core/services/storage.service';
import * as AuthProtocol from '../../../../protocol/auth-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';

@Component({
    selector: 'm-login',
    templateUrl: './login.component.html',
    styleUrls: ['./login.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class LoginComponent {
    @HostBinding('class') classes = 'm-login__signin';
    slackSysadminDmUrl: SafeUrl;
    username = '';
    password = '';
    signingIn = false;
    signInError?: string = null;

    constructor (
        private router: Router,
        private sanitizer: DomSanitizer,
        private cdr: ChangeDetectorRef,
        private accountService: AccountService,
        private projectService: ProjectService,
        private storageService: StorageService,
    ) {
        this.slackSysadminDmUrl = this.sanitizer.bypassSecurityTrustUrl(Constants.slackSysadminDmUrl);

        combineLatest(
            this.accountService.isSignedIn().pipe(filter(value => value)),
            this.projectService.projects$.asObservable().pipe(filter(projects => projects instanceof Array)),
        ).pipe(
            first(),
        ).subscribe(([signedIn, projects]) => {
            if (projects.length > 0) {
                const storedRoute = this.storageService.getStoredRoute();

                if (storedRoute && storedRoute !== Constants.errorUrl && storedRoute !== Constants.loginUrl) {
                    this.storageService.resetStoredRoute();
                    this.router.navigate([storedRoute]);
                } else {
                    this.router.navigate([`/dashboard/${projects[0].key}`]);
                }
            } else {
                this.router.navigate([Constants.errorUrl]);
            }
        });
    }

    submit() {
        this.signingIn = true;
        this.signInError = null;
        this.cdr.detectChanges();

        this.accountService
            .signIn(this.username, this.password)
            .pipe(
                finalize(() => {
                    this.signingIn = false;
                    this.cdr.detectChanges();
                }),
            )
            .subscribe((response: DbProtocol.PersonnelAccountProfile | AuthProtocol.PersonnelLoginResponse) => {
                if (response instanceof DbProtocol.PersonnelAccountProfile) {
                    // Success
                } else {
                    let message = 'Unpredicted error happened';

                    switch (response.error) {
                        case AuthProtocol.PersonnelLoginError.Failure: message = 'Internal server error, try again later'; break;
                        case AuthProtocol.PersonnelLoginError.AlreadyLoggedIn: message = 'Already logged in, reload this page'; break;
                        case AuthProtocol.PersonnelLoginError.AccountNotExists: message = 'Account does not exist'; break;
                        case AuthProtocol.PersonnelLoginError.AccountIsBlocked: message = 'Account is blocked, contact sysadmin'; break;
                        case AuthProtocol.PersonnelLoginError.AccountIsDeleted: message = 'Account is deleted, sorry about it'; break;
                        case AuthProtocol.PersonnelLoginError.InvalidPassword: message = 'Invalid credentials'; break;
                    }

                    this.signInError = message;
                }
            });
    }
}

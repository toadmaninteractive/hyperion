import { ChangeDetectorRef, Component, Inject, OnDestroy, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { BehaviorSubject, combineLatest, Observable, Subject } from 'rxjs';
import { finalize, first, switchMap, takeUntil, tap } from 'rxjs/operators';
import { AccountService } from '../../../../core/services/account.service';
import { HyperionJiraService } from '../../../../protocol/jira-protocol.service';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as JiraProtocol from '../../../../protocol/jira-protocol.data';
import { BadRequestError } from '../../../../protocol/data-protocol.data';

export interface IJiraAuthData {
    jiraId: number;
}

@Component({
    selector: 'm-jira-auth-dialog',
    templateUrl: './jira-auth-dialog.component.html',
    styleUrls: ['./jira-auth-dialog.component.scss'],
})
export class JiraAuthDialogComponent implements OnInit, OnDestroy {
    destroy$ = new Subject();
    ready$ = new BehaviorSubject<boolean>(false);
    loading$ = new BehaviorSubject<boolean>(false);
    jira: DbProtocol.JiraInstance;
    username = '';
    password = '';
    errorMessage: string;

    constructor(
        private cdr: ChangeDetectorRef,
        public dialogRef: MatDialogRef<JiraAuthDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: IJiraAuthData,
        private hyperionJiraService: HyperionJiraService,
        private accountService: AccountService,
    ) {
    }

    ngOnInit(): void {
        combineLatest([
            this.accountService.profile$.asObservable().pipe(
                takeUntil(this.destroy$),
                first(),
                tap(profile => this.username = profile.username),
            ),
            this.hyperionJiraService.getJiraInstance(this.data.jiraId).pipe(
                takeUntil(this.destroy$),
                tap(jira => this.jira = jira),
            ),
        ]).pipe(
            switchMap(([profile, jira]) => this.hyperionJiraService.getAuthenticationStatus(jira.id).pipe(takeUntil(this.destroy$))),
            finalize(() => this.ready$.next(true)),
        ).subscribe(response => response.result ? this.onSuccess() : null);
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.ready$.complete();
        this.loading$.complete();
    }

    canSubmit(): boolean {
        return this.username.trim().length > 0
            && this.password.trim().length > 0;
    }

    onAuthenticate(): void {
        this.errorMessage = null;
        this.loading$.next(true);

        this.authenticate(this.data.jiraId, this.username, this.password)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe(
                response => response.result ? this.onSuccess() : this.errorMessage = 'Authentication failed',
                (error: DataProtocol.BadRequestError<JiraProtocol.JiraAuthenticationError> | DataProtocol.NotFoundError | DataProtocol.InternalServerError) => {
                    if (error instanceof BadRequestError) {
                        this.errorMessage = JiraProtocol.JiraAuthenticationError.getDescription(error.error);
                    } else if (error instanceof DataProtocol.NotFoundError) {
                        this.errorMessage = 'Invalid JIRA instance';
                    } else if (error instanceof DataProtocol.InternalServerError) {
                        this.errorMessage = error.error;
                    }
                }
            );
    }

    onSuccess(): void {
        this.dialogRef.close(true);
    }

    private authenticate(jiraId: number, username: string, password: string): Observable<DataProtocol.GenericResponse> {
        const request = new JiraProtocol.AuthenticateJiraRequest();
        request.username = username.trim();
        request.password = password.trim();
        return this.hyperionJiraService.authenticate(request, jiraId);
    }
}

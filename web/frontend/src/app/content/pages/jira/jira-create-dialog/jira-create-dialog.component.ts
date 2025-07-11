import { Component, Inject, OnDestroy, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { NotificationService } from '../../../../core/services/notification.service';
import { HyperionJiraService } from '../../../../protocol/jira-protocol.service';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as JiraProtocol from '../../../../protocol/jira-protocol.data';

@Component({
    selector: 'm-jira-create-dialog',
    templateUrl: 'jira-create-dialog.component.html',
})
export class JiraCreateDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    newJiraInstance: DbProtocol.JiraInstance;
    errorMessage?: string = null;

    constructor (
        public dialogRef: MatDialogRef<JiraCreateDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: any,
        private notificationService: NotificationService,
        private hyperionJiraService: HyperionJiraService,
    ) {
        this.newJiraInstance = this.initialize();
    }

    ngOnInit(): void {
        this.destroy$ = new Subject();
        this.loading$ = new BehaviorSubject<boolean>(false);
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
    }

    private initialize(): DbProtocol.JiraInstance {
        const jiraInstance = new DbProtocol.JiraInstance();
        jiraInstance.title = 'New JIRA';
        jiraInstance.url = 'https://jira.yourcompany.com';

        return jiraInstance;
    }

    isJiraInstanceValid(): boolean {
        return this.newJiraInstance.title.trim().length > 0
            && this.newJiraInstance.url.trim().length > 0;
    }

    createJiraInstance(): void {
        const request = new JiraProtocol.CreateJiraInstanceRequest();
        request.title = this.newJiraInstance.title.trim();
        request.url = this.newJiraInstance.url.trim();
        this.loading$.next(true);

        this.hyperionJiraService
            .createJiraInstance(request)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe(
                response => this.dialogRef.close(response),
                error => {
                    if (error instanceof DataProtocol.BadRequestError) {
                        const errorMessage = JiraProtocol.JiraInstanceError.getDescription(error.error);

                        errorMessage
                            ? this.errorMessage = errorMessage
                            : this.notificationService.error(error);
                    } else {
                        this.notificationService.error(error);
                    }
                }
            );
    }
}

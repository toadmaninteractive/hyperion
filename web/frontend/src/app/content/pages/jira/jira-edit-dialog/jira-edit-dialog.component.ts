import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { NotificationService } from '../../../../core/services/notification.service';
import { HyperionJiraService } from '../../../../protocol/jira-protocol.service';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as JiraProtocol from '../../../../protocol/jira-protocol.data';

@Component({
    selector: 'm-jira-edit-dialog',
    templateUrl: 'jira-edit-dialog.component.html',
})
export class JiraEditDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    jiraInstance: DbProtocol.JiraInstance;
    pristineJiraInstance: DbProtocol.JiraInstance;
    errorMessage?: string = null;

    constructor(
        public dialogRef: MatDialogRef<JiraEditDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: DbProtocol.JiraInstance,
        private notificationService: NotificationService,
        private hyperionJiraService: HyperionJiraService,
    ) {
        this.jiraInstance = DbProtocol.JiraInstance.fromJson(data.toJson());
        this.pristineJiraInstance = DbProtocol.JiraInstance.fromJson(data.toJson());
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

    jiraInstanceInitialized(): boolean {
        return !!(this.jiraInstance && this.pristineJiraInstance);
    }

    isJiraInstanceValid(): boolean {
        return this.jiraInstance.title.trim().length > 0
            && this.jiraInstance.url.trim().length > 0;
    }

    jiraInstanceChanged(): boolean {
        return this.jiraInstance.title.trim() !== this.pristineJiraInstance.title.trim()
            || this.jiraInstance.url.trim() !== this.pristineJiraInstance.url.trim();
    }

    updateJiraInstance(): void {
        const request = new JiraProtocol.UpdateJiraInstanceRequest();
        if (this.jiraInstance.title.trim() !== this.pristineJiraInstance.title.trim()) request.title = this.jiraInstance.title.trim();
        if (this.jiraInstance.url.trim() !== this.pristineJiraInstance.url.trim()) request.url = this.jiraInstance.url.trim();
        this.loading$.next(true);

        this.hyperionJiraService
            .updateJiraInstance(request, this.jiraInstance.id)
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

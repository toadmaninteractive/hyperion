import { ChangeDetectorRef, Component, Inject, OnDestroy } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { BehaviorSubject, combineLatest, Observable, Subject } from 'rxjs';
import { finalize, first, switchMap, takeUntil, tap } from 'rxjs/operators';
import { HyperionJiraService } from '../../../../protocol/jira-protocol.service';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as JiraProtocol from '../../../../protocol/jira-protocol.data';
import { BadRequestError } from '../../../../protocol/data-protocol.data';

export interface IJiraIssueData {
    testRunItemId: number;
    summary: string;
    description: string;
}

export interface IJiraIssueResult {
    jiraIssueKey: string;
    jiraIssueUrl: string;
}

@Component({
    selector: 'm-jira-issue-dialog',
    templateUrl: './jira-issue-dialog.component.html',
    styleUrls: ['./jira-issue-dialog.component.scss'],
})
export class JiraIssueDialogComponent implements OnDestroy {
    destroy$ = new Subject();
    ready$ = new BehaviorSubject<boolean>(false);
    loading$ = new BehaviorSubject<boolean>(false);
    summary = '';
    description = '';
    errorMessage: string;

    constructor(
        private cdr: ChangeDetectorRef,
        public dialogRef: MatDialogRef<JiraIssueDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: IJiraIssueData,
        private hyperionJiraService: HyperionJiraService,
    ) {
        this.summary = data.summary;
        this.description = data.description;
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.ready$.complete();
        this.loading$.complete();
    }

    canSubmit(): boolean {
        return this.summary.trim().length > 0;
    }

    onCreateIssue(): void {
        this.errorMessage = null;
        this.loading$.next(true);

        this.createJiraIssue(this.data.testRunItemId, this.summary, this.description)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe(
                (response: JiraProtocol.CreateJiraIssueResponse) => this.onSuccess(response.jiraIssueKey, response.jiraIssueUrl),
                (error: DataProtocol.BadRequestError<JiraProtocol.JiraIssueError> | DataProtocol.NotFoundError | DataProtocol.InternalServerError) => {
                    if (error instanceof BadRequestError) {
                        this.errorMessage = JiraProtocol.JiraIssueError.getDescription(error.error);
                    } else if (error instanceof DataProtocol.NotFoundError) {
                        this.errorMessage = 'Invalid JIRA instance';
                    } else if (error instanceof DataProtocol.InternalServerError) {
                        this.errorMessage = error.error;
                    }
                }
            );
    }

    onSuccess(jiraIssueKey: string, jiraIssueUrl: string): void {
        this.dialogRef.close({ jiraIssueKey, jiraIssueUrl } as IJiraIssueResult);
    }

    private createJiraIssue(testRunItemId: number, summary: string, description: string): Observable<JiraProtocol.CreateJiraIssueResponse> {
        const request = new JiraProtocol.CreateJiraIssueRequest();
        request.summary = summary.trim();
        request.description = description.trim();
        return this.hyperionJiraService.createJiraIssue(request, testRunItemId);
    }
}

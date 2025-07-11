import { Component, Inject, OnDestroy, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { NotificationService } from '../../../../core/services/notification.service';
import { HyperionJiraService } from '../../../../protocol/jira-protocol.service';
import { HyperionProjectService } from '../../../../protocol/project-protocol.service';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as ProjectProtocol from '../../../../protocol/project-protocol.data';

@Component({
    selector: 'm-project-create-dialog',
    templateUrl: 'project-create-dialog.component.html',
})
export class ProjectCreateDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    newProject: DbProtocol.Project;
    jiraInstances: DbProtocol.JiraInstance[];
    errorMessage?: string = null;

    constructor (
        public dialogRef: MatDialogRef<ProjectCreateDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: any,
        private notificationService: NotificationService,
        private hyperionJiraService: HyperionJiraService,
        private hyperionProjectService: HyperionProjectService,
    ) {
        this.newProject = this.initializeProject();
    }

    ngOnInit(): void {
        this.destroy$ = new Subject();
        this.loading$ = new BehaviorSubject<boolean>(false);

        this.hyperionJiraService
            .getJiraInstances()
            .pipe(takeUntil(this.destroy$))
            .subscribe(response => this.jiraInstances = response.items);
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
    }

    private initializeProject(): DbProtocol.Project {
        const project = new DbProtocol.Project();
        project.title = 'New project';
        project.key = '';
        project.slackReceivers = '';
        project.jiraId = null;
        project.jiraKey = '';

        return project;
    }

    private uniqueSlackReceivers(slackReceivers: string): string | null {
        const receivers = (slackReceivers || '')
            .split(/\s*,\s*/)
            .map(receiver => receiver.trim().toLocaleLowerCase())
            .sort((receiver1, receiver2) => receiver1 < receiver2 ? -1 : 1)
            .filter(receiver => receiver.length > 0);

        const receiverSet = new Set<string>(receivers);
        return receiverSet.size > 0 ? [...receiverSet].join(',') : null;
    }

    isProjectValid(): boolean {
        return this.newProject.title.trim().length > 0
            && this.newProject.key.trim().length > 0
            && (this.newProject.jiraId > 0 ? (this.newProject.jiraKey.trim().length > 0) : true);
    }

    createProject(): void {
        const request = new ProjectProtocol.CreateProjectRequest();
        request.title = this.newProject.title.trim();
        request.key = this.newProject.key.trim();
        request.slackReceivers = this.uniqueSlackReceivers(this.newProject.slackReceivers);
        request.jiraId = this.newProject.jiraId;
        request.jiraKey = this.newProject.jiraKey || null;
        this.loading$.next(true);

        this.hyperionProjectService
            .createProject(request)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe(
                response => this.dialogRef.close(response),
                error => {
                    if (error instanceof DataProtocol.BadRequestError) {
                        let errorMessage: string = null;

                        switch (error.error as ProjectProtocol.ProjectError) {
                            case ProjectProtocol.ProjectError.InvalidTitle:
                                errorMessage = 'Project title is invalid';
                                break;
                            case ProjectProtocol.ProjectError.TitleAlreadyExists:
                                errorMessage = 'Project title already exists';
                                break;
                            case ProjectProtocol.ProjectError.InvalidKey:
                                errorMessage = 'Project key is invalid';
                                break;
                            case ProjectProtocol.ProjectError.KeyAlreadyExists:
                                errorMessage = 'Project key already exists';
                                break;
                            case ProjectProtocol.ProjectError.InvalidSlackReceivers:
                                errorMessage = 'Invalid Slack receiver';
                                break;
                            case ProjectProtocol.ProjectError.OwnerNotExists:
                                errorMessage = 'Server cannot identify your session owner';
                                break;
                            case ProjectProtocol.ProjectError.JiraNotExists:
                                errorMessage = 'JIRA instance not exists';
                                break;
                        }

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

import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { NotificationService } from '../../../../core/services/notification.service';
import { HyperionJiraService } from '../../../../protocol/jira-protocol.service';
import { HyperionProjectService } from '../../../../protocol/project-protocol.service';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import * as ProjectProtocol from '../../../../protocol/project-protocol.data';

@Component({
    selector: 'm-project-edit-dialog',
    templateUrl: 'project-edit-dialog.component.html',
})
export class ProjectEditDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    project: DbProtocol.Project;
    pristineProject: DbProtocol.Project;
    jiraInstances: DbProtocol.JiraInstance[];
    errorMessage?: string = null;

    constructor(
        public dialogRef: MatDialogRef<ProjectEditDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: DbProtocol.Project,
        private notificationService: NotificationService,
        private hyperionJiraService: HyperionJiraService,
        private hyperionProjectService: HyperionProjectService,
    ) {
        this.project = DbProtocol.Project.fromJson(data.toJson());
        this.project.jiraKey = this.project.jiraKey || '';

        this.pristineProject = DbProtocol.Project.fromJson(data.toJson());
        this.pristineProject.jiraKey = this.pristineProject.jiraKey || '';
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

    private uniqueSlackReceivers(slackReceivers: string): string | null {
        const receivers = (slackReceivers || '')
            .split(/\s*,\s*/)
            .map(receiver => receiver.trim().toLocaleLowerCase())
            .sort((receiver1, receiver2) => receiver1 < receiver2 ? -1 : 1)
            .filter(receiver => receiver.length > 0);

        const receiverSet = new Set<string>(receivers);
        return receiverSet.size > 0 ? [...receiverSet].join(',') : null;
    }

    projectInitialized(): boolean {
        return !!(this.project && this.pristineProject && this.jiraInstances);
    }

    isProjectValid(): boolean {
        return this.project.title.trim().length > 0
            && this.project.key.trim().length > 0
            && (this.project.jiraId > 0 ? (this.project.jiraKey.trim().length > 0) : true);
    }

    projectChanged(): boolean {
        return this.project.title.trim() !== this.pristineProject.title.trim()
            || this.project.key.trim() !== this.pristineProject.key.trim()
            || (this.project.slackReceivers || '').trim() !== (this.pristineProject.slackReceivers || '').trim()
            || this.project.jiraId !== this.pristineProject.jiraId
            || this.project.jiraKey !== this.pristineProject.jiraKey;
    }

    updateProject(): void {
        const request = new ProjectProtocol.UpdateProjectRequest();
        if (this.project.title.trim() !== this.pristineProject.title.trim()) request.title = this.project.title.trim();
        if (this.project.key.trim() !== this.pristineProject.key.trim()) request.key = this.project.key.trim();
        if ((this.project.slackReceivers || '').trim() !== (this.pristineProject.slackReceivers || '').trim()) request.slackReceivers = this.uniqueSlackReceivers(this.project.slackReceivers);
        if (this.project.jiraId !== this.pristineProject.jiraId) request.jiraId = this.project.jiraId;
        if (this.project.jiraKey.trim() !== this.pristineProject.jiraKey.trim()) request.jiraKey = this.project.jiraKey.trim() || null;
        this.loading$.next(true);

        this.hyperionProjectService
            .updateProject(request, this.project.id)
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

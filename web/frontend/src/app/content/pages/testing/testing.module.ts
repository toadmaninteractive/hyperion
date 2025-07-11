import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { MatButtonModule } from '@angular/material/button';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatListModule } from '@angular/material/list';
import { MatMenuModule } from '@angular/material/menu';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatSelectModule } from '@angular/material/select';
import { MatSortModule } from '@angular/material/sort';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { SweetAlert2Module } from '@sweetalert2/ngx-sweetalert2';
import { AngularSplitModule } from 'angular-split';
import { NgxFileDropModule } from 'ngx-file-drop';
import { LightboxModule } from 'ngx-lightbox';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { SharedModule } from '../../../shared/shared.module';
import { ComponentsModule } from '../../../components/components.module';
import { PortletModule } from '../../../components/general/portlet/portlet.module';
import { ProjectResolverGuard } from '../../../core/resolvers/project-resolver.guard';
import { JiraAuthDialogComponent } from './jira-auth-dialog/jira-auth-dialog.component';
import { JiraIssueDialogComponent } from './jira-issue-dialog/jira-issue-dialog.component';
import { TestingDetailsComponent } from './testing-details/testing-details.component';
import { TestingOverviewComponent } from './testing-overview/testing-overview.component';
import { TestRunResolver } from '../../../core/resolvers/test-run-resolver';
import { ConsumerGuard } from '../../../core/guards/consumer.guard';

@NgModule({
    imports: [
        // Angular modules
        CommonModule,
        FormsModule,

        // Angular Material modules
        MatButtonModule,
        MatButtonToggleModule,
        MatCheckboxModule,
        MatDialogModule,
        MatFormFieldModule,
        MatIconModule,
        MatInputModule,
        MatListModule,
        MatMenuModule,
        MatPaginatorModule,
        MatProgressSpinnerModule,
        MatSelectModule,
        MatSortModule,
        MatToolbarModule,
        MatTooltipModule,

        // Third party modules
        AngularSplitModule,
        LightboxModule,
        NgxFileDropModule,
        PerfectScrollbarModule,
        SweetAlert2Module,

        // Project modules
        ComponentsModule,
        PortletModule,
        SharedModule,

        // Angular module (should be last)
        RouterModule.forChild([
            {
                path: ':project',
                children: [
                    {
                        path: '',
                        component: TestingOverviewComponent,
                        resolve: {
                            activeProject: ProjectResolverGuard,
                        },
                    },
                    {
                        path: ':id/details',
                        component: TestingDetailsComponent,
                        resolve: {
                            activeProject: ProjectResolverGuard,
                            testRun: TestRunResolver,
                        },
                    },
                ],
                canActivate: [ConsumerGuard],
            },
        ]),
    ],
    exports: [
        JiraAuthDialogComponent,
        JiraIssueDialogComponent,
    ],
    providers: [],
    declarations: [
        JiraAuthDialogComponent,
        JiraIssueDialogComponent,
        TestingDetailsComponent,
        TestingOverviewComponent,
    ],
    entryComponents: [
        JiraAuthDialogComponent,
        JiraIssueDialogComponent,
    ]
})
export class TestingModule { }

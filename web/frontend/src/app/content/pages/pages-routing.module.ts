import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PagesComponent } from './pages.component';
import { ErrorPageComponent } from './error-page/error-page.component';
import { AuthorizedGuard } from '../../core/guards/authorized.guard';
import { ConsumerGuard } from '../../core/guards/consumer.guard';
import { MaintainerGuard } from '../../core/guards/maintainer.guard';
import { NotAuthorizedGuard } from '../../core/guards/not-authorized.guard';
import { ProjectManagerGuard } from '../../core/guards/project-manager.guard';
import { SuperadminGuard } from '../../core/guards/superadmin.guard';
import { ForbiddenPageComponent } from './forbidden -page/forbidden-page.component';
import { ProjectResolverGuard } from '../../core/resolvers/project-resolver.guard';

const routes: Routes = [
    {
        path: '',
        component: PagesComponent,
        canActivate: [AuthorizedGuard],
        children: [
            {
                path: '',
                redirectTo: 'dashboard',
                pathMatch: 'full',
            },
            {
                path: 'dashboard',
                loadChildren: () => import('./dashboard/dashboard.module').then(m => m.DashboardModule),
                canActivate: [ConsumerGuard],
            },
            {
                path: 'parameters',
                loadChildren: () => import('./parameters/parameters.module').then(m => m.ParametersModule),
                canActivate: [MaintainerGuard],
            },
            {
                path: 'setup',
                loadChildren: () => import('./setup/setup.module').then(m => m.SetupModule),
                canActivate: [MaintainerGuard],
            },
            {
                path: 'test-cases',
                loadChildren: () => import('./test-cases/test-cases.module').then(m => m.TestCasesModule),
                canActivate: [MaintainerGuard],
            },
            {
                path: 'test-runs',
                loadChildren: () => import('./test-runs/test-runs.module').then(m => m.TestRunsModule),
                canActivate: [MaintainerGuard],
            },
            {
                path: 'attachments',
                loadChildren: () => import('./attachments/attachments.module').then(m => m.AttachmentsModule),
                canActivate: [ConsumerGuard],
            },
            {
                path: 'testing',
                loadChildren: () => import('./testing/testing.module').then(m => m.TestingModule),
                canActivate: [ConsumerGuard],
            },
            {
                path: 'personnel',
                loadChildren: () => import('./personnel/personnel.module').then(m => m.PersonnelModule),
                canActivate: [SuperadminGuard],
            },
            {
                path: 'projects',
                loadChildren: () => import('./projects/projects.module').then(m => m.ProjectsModule),
                canActivate: [ProjectManagerGuard],
            },
            {
                path: 'jira',
                loadChildren: () => import('./jira/jira.module').then(m => m.JiraModule),
                canActivate: [SuperadminGuard],
            },
            {
                path: 'settings',
                loadChildren: () => import('./settings/settings.module').then(m => m.SettingsModule),
                canActivate: [SuperadminGuard],
            },
        ]
    },
    {
        path: 'login',
        canActivate: [NotAuthorizedGuard],
        loadChildren: () => import('./auth/auth.module').then(m => m.AuthModule),
    },

    {
        path: '404',
        component: ErrorPageComponent
    },

    {
        path: '403',
        component: ForbiddenPageComponent
    },

    {
        path: 'error/:type',
        component: ErrorPageComponent
    },
];

@NgModule({
    imports: [
        RouterModule.forChild(routes)
    ],
    exports: [
        RouterModule
    ]
})
export class PagesRoutingModule { }

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatListModule } from '@angular/material/list';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { MatSortModule } from '@angular/material/sort';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatTreeModule } from '@angular/material/tree';
import { AngularSplitModule } from 'angular-split';
import { LightboxModule } from 'ngx-lightbox';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { NgxFileDropModule } from 'ngx-file-drop';
import { SweetAlert2Module } from '@sweetalert2/ngx-sweetalert2';
import { SharedModule } from '../../../shared/shared.module';
import { ComponentsModule } from '../../../components/components.module';
import { PortletModule } from '../../../components/general/portlet/portlet.module';
import { ProjectResolverGuard } from '../../../core/resolvers/project-resolver.guard';
import { TestRunResolver } from '../../../core/resolvers/test-run-resolver';
import { TestCaseBrowseDialogComponent } from './test-case-browse-dialog/test-case-browse-dialog.component';
import { TestRunEditComponent } from './test-run-edit/test-run-edit.component';
import { TestRunsOverviewComponent } from './test-runs-overview/test-runs-overview.component';
import { MaintainerGuard } from '../../../core/guards/maintainer.guard';

@NgModule({
    imports: [
        // Angular modules
        CommonModule,
        DragDropModule,
        FormsModule,

        // Angular Material modules
        MatButtonModule,
        MatCheckboxModule,
        MatDialogModule,
        MatFormFieldModule,
        MatIconModule,
        MatInputModule,
        MatListModule,
        MatPaginatorModule,
        MatProgressSpinnerModule,
        MatSortModule,
        MatToolbarModule,
        MatTooltipModule,
        MatTreeModule,

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
                        component: TestRunsOverviewComponent,
                        resolve: {
                            activeProject: ProjectResolverGuard,
                        },
                    },
                    {
                        path: ':id/manage',
                        component: TestRunEditComponent,
                        resolve: {
                            activeProject: ProjectResolverGuard,
                            testRun: TestRunResolver,
                        },
                    },
                ],
                canActivate: [MaintainerGuard],
            },
        ]),
    ],
    exports: [
        TestCaseBrowseDialogComponent,
    ],
    providers: [],
    declarations: [
        TestCaseBrowseDialogComponent,
        TestRunEditComponent,
        TestRunsOverviewComponent,
    ],
    entryComponents: [
        TestCaseBrowseDialogComponent,
    ]
})
export class TestRunsModule { }

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { MatButtonModule } from '@angular/material/button';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatSortModule } from '@angular/material/sort';
import { MatTooltipModule } from '@angular/material/tooltip';
import { SweetAlert2Module } from '@sweetalert2/ngx-sweetalert2';
import { SharedModule } from '../../../shared/shared.module';
import { ComponentsModule } from '../../../components/components.module';
import { PortletModule } from '../../../components/general/portlet/portlet.module';
import { ProjectCreateDialogComponent } from './project-create-dialog/project-create-dialog.component';
import { ProjectEditDialogComponent } from './project-edit-dialog/project-edit-dialog.component';
import { ProjectsOverviewComponent } from './projects-overview/projects-overview.component';

@NgModule({
    imports: [
        // Angular modules
        CommonModule,
        FormsModule,

        // Angular Material modules
        MatButtonModule,
        MatDialogModule,
        MatFormFieldModule,
        MatIconModule,
        MatInputModule,
        MatSortModule,
        MatTooltipModule,

        // Third party modules
        SweetAlert2Module,

        // Project modules
        ComponentsModule,
        PortletModule,
        SharedModule,

        // Angular module (should be last)
        RouterModule.forChild([
            {
                path: '**',
                component: ProjectsOverviewComponent,
            },
        ]),
    ],
    exports: [
        ProjectCreateDialogComponent,
        ProjectEditDialogComponent,
    ],
    providers: [],
    declarations: [
        ProjectCreateDialogComponent,
        ProjectEditDialogComponent,
        ProjectsOverviewComponent,
    ],
    entryComponents: [
        ProjectCreateDialogComponent,
        ProjectEditDialogComponent,
    ]
})
export class ProjectsModule { }

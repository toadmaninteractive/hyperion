import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatMenuModule } from '@angular/material/menu';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatTreeModule } from '@angular/material/tree';
import { SweetAlert2Module } from '@sweetalert2/ngx-sweetalert2';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { NgxFileDropModule } from 'ngx-file-drop';
import { LightboxModule } from 'ngx-lightbox';
import { SharedModule } from '../../../shared/shared.module';
import { ComponentsModule } from '../../../components/components.module';
import { PortletModule } from '../../../components/general/portlet/portlet.module';
import { ProjectResolverGuard } from '../../../core/resolvers/project-resolver.guard';
import { ParametersOverviewComponent } from './parameters-overview/parameters-overview.component';
import { AngularSplitModule } from 'angular-split';
import { MaintainerGuard } from '../../../core/guards/maintainer.guard';

@NgModule({
    imports: [
        // Angular modules
        CommonModule,
        FormsModule,

        // Angular Material modules
        MatButtonModule,
        MatCheckboxModule,
        MatDialogModule,
        MatFormFieldModule,
        MatIconModule,
        MatInputModule,
        MatProgressSpinnerModule,
        MatMenuModule,
        MatTooltipModule,
        MatTreeModule,

        // Third party modules
        LightboxModule,
        NgxFileDropModule,
        SweetAlert2Module,
        PerfectScrollbarModule,

        // Project modules
        ComponentsModule,
        PortletModule,
        SharedModule,

        // Angular module (should be last)
        RouterModule.forChild([
            {
                path: '**',
                component: ParametersOverviewComponent,
                resolve: {
                    activeProject: ProjectResolverGuard,
                },
                canActivate: [MaintainerGuard],
            },
        ]),
        AngularSplitModule,
    ],
    exports: [
    ],
    providers: [],
    declarations: [
        ParametersOverviewComponent,
    ],
    entryComponents: [
    ]
})
export class ParametersModule { }

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
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatTreeModule } from '@angular/material/tree';
import { LightboxModule } from 'ngx-lightbox';
import { NgxGalleryModule } from 'ngx-gallery-9';
import { NgxFileDropModule } from 'ngx-file-drop';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { SweetAlert2Module } from '@sweetalert2/ngx-sweetalert2/';
import { SharedModule } from '../../../shared/shared.module';
import { ComponentsModule } from '../../../components/components.module';
import { PortletModule } from '../../../components/general/portlet/portlet.module';
import { ProjectResolverGuard } from '../../../core/resolvers/project-resolver.guard';
import { BrowseParamsDialogComponent } from './browse-params-dialog/browse-params-dialog.component';
import { SetupOverviewComponent } from './setup-overview/setup-overview.component';
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
        MatFormFieldModule,
        MatIconModule,
        MatInputModule,
        MatProgressSpinnerModule,
        MatTooltipModule,
        MatTreeModule,

        // Third party modules
        LightboxModule,
        NgxFileDropModule,
        NgxGalleryModule,
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
                component: SetupOverviewComponent,
                resolve: {
                    activeProject: ProjectResolverGuard,
                },
                canActivate: [MaintainerGuard],
            },
        ]),
        AngularSplitModule,
        MatDialogModule,
    ],
    exports: [BrowseParamsDialogComponent],
    providers: [],
    declarations: [
        BrowseParamsDialogComponent,
        SetupOverviewComponent,
    ],
    entryComponents: [BrowseParamsDialogComponent]
})
export class SetupModule { }

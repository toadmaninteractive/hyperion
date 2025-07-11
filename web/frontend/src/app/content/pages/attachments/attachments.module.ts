import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { MatButtonModule } from '@angular/material/button';
import { MatCardModule } from '@angular/material/card';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatRadioModule } from '@angular/material/radio';
import { MatSelectModule } from '@angular/material/select';
import { MatTooltipModule } from '@angular/material/tooltip';
import { SharedModule } from '../../../shared/shared.module';
import { AttachmentsComponent } from './attachments.component';
import { MaintainerGuard } from '../../../core/guards/maintainer.guard';
import { ProjectResolverGuard } from '../../../core/resolvers/project-resolver.guard';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { MatPaginatorModule } from '@angular/material/paginator';
import { ConsumerGuard } from '../../../core/guards/consumer.guard';
import { LightboxModule } from 'ngx-lightbox';

@NgModule({
    imports: [
        CommonModule,
        FormsModule,
        MatButtonModule,
        MatCardModule,
        MatCheckboxModule,
        MatFormFieldModule,
        MatIconModule,
        MatInputModule,
        MatRadioModule,
        MatSelectModule,
        MatTooltipModule,
        SharedModule,
        RouterModule.forChild([
            {
                path: ':project',
                children: [
                    {
                        path: '',
                        component: AttachmentsComponent,
                        resolve: {
                            activeProject: ProjectResolverGuard,
                        },
                    },
                ],
                canActivate: [ConsumerGuard],
            },
        ]),
        MatTableModule,
        MatSortModule,
        MatProgressSpinnerModule,
        MatPaginatorModule,
        LightboxModule,
    ],
    providers: [],
    declarations: [AttachmentsComponent]
})
export class AttachmentsModule { }

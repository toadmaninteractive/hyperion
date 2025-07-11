import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatTooltipModule } from '@angular/material/tooltip';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { ProjectResolverGuard } from '../../../core/resolvers/project-resolver.guard';
import { SharedModule } from '../../../shared/shared.module';
import { PortletModule } from '../../../components/general/portlet/portlet.module';
import { DashboardOverviewComponent } from './dashboard-overview/dashboard-overview.component';
import { ComponentsModule } from '../../../components/components.module';
import { MatButtonModule } from '@angular/material/button';
import { ConsumerGuard } from '../../../core/guards/consumer.guard';

@NgModule({
    imports: [
        CommonModule,
        MatIconModule,
        MatTooltipModule,
        PerfectScrollbarModule,
        SharedModule,
        PortletModule,
        ComponentsModule,
        RouterModule.forChild([
            {
                path: '**',
                component: DashboardOverviewComponent,
                resolve: {
                    activeProject: ProjectResolverGuard,
                },
                canActivate: [ConsumerGuard],
            },
        ]),
        MatButtonModule,
        MatInputModule
    ],
    exports: [],
    providers: [],
    declarations: [
        DashboardOverviewComponent,
    ]
})
export class DashboardModule { }

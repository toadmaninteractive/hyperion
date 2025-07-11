import { LayoutModule } from '../layout/layout.module';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { HttpClientModule } from '@angular/common/http';
import { PagesRoutingModule } from './pages-routing.module';
import { ComponentsModule } from '../../components/components.module';
import { SharedModule } from '../../shared/shared.module';
import { AuthorizedGuard } from '../../core/guards/authorized.guard';
import { ProjectResolverGuard } from '../../core/resolvers/project-resolver.guard';
import { NotAuthorizedGuard } from '../../core/guards/not-authorized.guard';
import { PagesComponent } from './pages.component';
import { ErrorPageComponent } from './error-page/error-page.component';
import { ForbiddenPageComponent } from './forbidden -page/forbidden-page.component';

@NgModule({
    declarations: [
        PagesComponent,
        ErrorPageComponent,
        ForbiddenPageComponent,
    ],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        PagesRoutingModule,
        SharedModule,
        LayoutModule,
        ComponentsModule,
    ],
    providers: [
        AuthorizedGuard,
        ProjectResolverGuard,
        NotAuthorizedGuard,
    ]
})
export class PagesModule { }

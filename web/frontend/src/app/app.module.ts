import { NgModule } from '@angular/core';
import { HttpClientModule } from '@angular/common/http';
import { BrowserModule } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { OverlayModule } from '@angular/cdk/overlay';
import { SweetAlert2Module } from '@sweetalert2/ngx-sweetalert2';
import { PerfectScrollbarConfigInterface, PERFECT_SCROLLBAR_CONFIG } from 'ngx-perfect-scrollbar';
import { ToastrModule } from 'ngx-toastr';

import { AppComponent } from './app.component';
import { AppRoutingModule } from './app-routing.module';

import { ComponentsModule } from './components/components.module';
import { CoreModule } from './core/core.module';
import { LayoutModule } from './content/layout/layout.module';
import { ProtocolModule } from './protocol/protocol.module';
import { SharedModule } from './shared/shared.module';
import { AngularSplitModule } from 'angular-split';

@NgModule({
    declarations: [AppComponent],
    imports: [
        // Angular modules
        BrowserModule,
        BrowserAnimationsModule,
        OverlayModule,
        HttpClientModule,

        // Third party modules
        AngularSplitModule.forRoot(),
        ToastrModule.forRoot({ preventDuplicates: true, countDuplicates: true, maxOpened: 10 }),
        SweetAlert2Module.forRoot(),

        // Project modules
        CoreModule,
        ProtocolModule,
        SharedModule,
        LayoutModule,
        ComponentsModule,

        // Application routing module
        AppRoutingModule,
    ],
    providers: [
        { provide: PERFECT_SCROLLBAR_CONFIG, useValue: <PerfectScrollbarConfigInterface>{ } },
    ],
    bootstrap: [AppComponent]
})
export class AppModule { }

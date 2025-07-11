import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { HeaderComponent } from './header/header.component';
import { AsideLeftComponent } from './aside/aside-left.component';
import { FooterComponent } from './footer/footer.component';
import { SubheaderComponent } from './subheader/subheader.component';
import { BrandComponent } from './header/brand/brand.component';
import { MenuSectionComponent } from './aside/menu-section/menu-section.component';
import { TopbarComponent } from './header/topbar/topbar.component';
import { SharedModule } from '../../shared/shared.module';
import { ListTimelineModule } from '../../components/layout/quick-sidebar/list-timeline/list-timeline.module';
import { UserProfileComponent } from './header/topbar/user-profile/user-profile.component';
import { SearchDropdownComponent } from './header/topbar/search-dropdown/search-dropdown.component';
import { NotificationComponent } from './header/topbar/notification/notification.component';
import { ProjectSwitchComponent } from './header/topbar/project-switch/project-switch.component';
import { QuickActionComponent } from './header/topbar/quick-action/quick-action.component';
import { MenuHorizontalComponent } from './header/menu-horizontal/menu-horizontal.component';
import { AsideRightComponent } from './aside/aside-right/aside-right.component';
import { SearchDefaultComponent } from './header/topbar/search-default/search-default.component';
import { HeaderSearchComponent } from './header/header-search/header-search.component';

import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { PERFECT_SCROLLBAR_CONFIG } from 'ngx-perfect-scrollbar';
import { PerfectScrollbarConfigInterface } from 'ngx-perfect-scrollbar';

import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { MatSelectModule } from '@angular/material/select';
import { MatTabsModule } from '@angular/material/tabs';
import { MatTooltipModule } from '@angular/material/tooltip';

import { LoadingBarModule } from '@ngx-loading-bar/core';
import { FormsModule } from '@angular/forms';

const DEFAULT_PERFECT_SCROLLBAR_CONFIG: PerfectScrollbarConfigInterface = {
    // suppressScrollX: true
};

@NgModule({
    declarations: [
        HeaderComponent,
        FooterComponent,
        SubheaderComponent,
        BrandComponent,

        // Topbar components
        TopbarComponent,
        UserProfileComponent,
        SearchDropdownComponent,
        NotificationComponent,
        QuickActionComponent,

        // Hyperion related
        ProjectSwitchComponent,

        // Aside left menu components
        AsideLeftComponent,
        MenuSectionComponent,

        // Horizontal menu components
        MenuHorizontalComponent,

        // Aside right component
        AsideRightComponent,
        SearchDefaultComponent,
        HeaderSearchComponent,
    ],
    exports: [
        HeaderComponent,
        FooterComponent,
        SubheaderComponent,
        BrandComponent,

        // Topbar components
        TopbarComponent,
        UserProfileComponent,
        SearchDropdownComponent,
        NotificationComponent,
        QuickActionComponent,

        // Hyperion related
        ProjectSwitchComponent,

        // Aside left menu components
        AsideLeftComponent,
        // MenuSectionComponent,

        // Horizontal menu components
        MenuHorizontalComponent,

        // Aside right component
        AsideRightComponent
    ],
    providers: [
        {
            provide: PERFECT_SCROLLBAR_CONFIG,
            useValue: DEFAULT_PERFECT_SCROLLBAR_CONFIG
        }
    ],
    imports: [
        CommonModule,
        RouterModule,
        SharedModule,
        PerfectScrollbarModule,
        FormsModule,
        ListTimelineModule,
        MatButtonModule,
        MatIconModule,
        MatProgressBarModule,
        MatSelectModule,
        MatTabsModule,
        MatTooltipModule,
        LoadingBarModule,
    ]
})
export class LayoutModule { }

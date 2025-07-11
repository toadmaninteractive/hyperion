import { RouterModule } from '@angular/router';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CsvModule } from '@ctrl/ngx-csv';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatButtonModule } from '@angular/material/button';
import { MatCardModule } from '@angular/material/card';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatChipsModule } from '@angular/material/chips';
import { MatNativeDateModule, MatRippleModule } from '@angular/material/core';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatDialogModule } from '@angular/material/dialog';
import { MatGridListModule } from '@angular/material/grid-list';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatListModule } from '@angular/material/list';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { MatSelectModule } from '@angular/material/select';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatSortModule } from '@angular/material/sort';
import { MatTableModule } from '@angular/material/table';
import { MatTabsModule } from '@angular/material/tabs';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatTreeModule } from '@angular/material/tree';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { SharedModule } from '../shared/shared.module';
import { ListTimelineModule } from './layout/quick-sidebar/list-timeline/list-timeline.module';
import { MessengerModule } from './layout/quick-sidebar/messenger/messenger.module';
import { AlertModule } from './general/alert/alert.module';
import { GravatarModule } from './general/gravatar/gravatar.module';
import { PortletModule } from './general/portlet/portlet.module';

import { ListSettingsComponent } from './layout/quick-sidebar/list-settings/list-settings.component';
import { QuickSidebarComponent } from './layout/quick-sidebar/quick-sidebar.component';
import { ScrollTopComponent } from './layout/scroll-top/scroll-top.component';
import { TooltipsComponent } from './layout/tooltips/tooltips.component';

import { DynamicTreeComponent } from './general/dynamic-tree/dynamic-tree.component';
import { DynamicTreeMapComponent } from './general/dynamic-tree-map/dynamic-tree-map.component';
import { LoadingIndicatorComponent } from './general/loading-indicator/loading-indicator/loading-indicator.component';
import { NoticeComponent } from './general/notice/notice.component';
import { SmartProgressBarComponent } from './general/smart-progress-bar/smart-progress-bar.component';
import { TestStatusLegendComponent } from './general/test-status-legend/test-status-legend.component';
import { TreeComponent } from './general/tree/tree.component';

@NgModule({
    providers: [ ],
    declarations: [
        QuickSidebarComponent,
        ScrollTopComponent,
        TooltipsComponent,
        ListSettingsComponent,
        NoticeComponent,
        SmartProgressBarComponent,
        TestStatusLegendComponent,
        DynamicTreeComponent,
        DynamicTreeMapComponent,
        TreeComponent,
        LoadingIndicatorComponent,
    ],
    exports: [
        CsvModule,
        MatTabsModule,
        MatTableModule,
        MatSelectModule,
        MatPaginatorModule,
        PortletModule,
        AlertModule,
        GravatarModule,
        QuickSidebarComponent,
        ScrollTopComponent,
        TooltipsComponent,
        ListSettingsComponent,
        NoticeComponent,
        SmartProgressBarComponent,
        TestStatusLegendComponent,
        DynamicTreeComponent,
        DynamicTreeMapComponent,
        TreeComponent,
        LoadingIndicatorComponent,
        MatAutocompleteModule,
    ],
    imports: [
        CommonModule,
        RouterModule,
        PerfectScrollbarModule,
        MessengerModule,
        ListTimelineModule,
        SharedModule,
        PortletModule,
        AlertModule,
        GravatarModule,
        FormsModule,
        ReactiveFormsModule,
        MatButtonModule,
        MatChipsModule,
        MatCheckboxModule,
        MatDatepickerModule,
        MatIconModule,
        MatInputModule,
        MatNativeDateModule,
        MatPaginatorModule,
        MatProgressBarModule,
        MatProgressSpinnerModule,
        MatSelectModule,
        MatSortModule,
        MatTableModule,
        MatTabsModule,
        MatTooltipModule,
        MatCardModule,
        MatListModule,
        MatRippleModule,
        MatSlideToggleModule,
        MatDialogModule,
        MatGridListModule,
        CsvModule,
        MatTreeModule,
        MatAutocompleteModule,
    ],
})
export class ComponentsModule { }

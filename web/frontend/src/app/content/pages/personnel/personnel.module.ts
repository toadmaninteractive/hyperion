import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDividerModule } from '@angular/material/divider';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatMenuModule } from '@angular/material/menu';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatSortModule } from '@angular/material/sort';
import { MatTableModule } from '@angular/material/table';
import { MatTooltipModule } from '@angular/material/tooltip';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { SweetAlert2Module } from '@sweetalert2/ngx-sweetalert2';
import { SharedModule } from '../../../shared/shared.module';
import { ComponentsModule } from '../../../components/components.module';
import { PersonnelRoutingModule } from './personnel-routing.module';
import { PersonnelAccountManageComponent } from './personnel-account-manage/personnel-account-manage.component';
import { PersonnelAccountsComponent } from './personnel-accounts/personnel-accounts.component';
import { PersonnelGroupManageComponent } from './personnel-group-manage/personnel-group-manage.component';
import { PersonnelGroupsComponent } from './personnel-groups/personnel-groups.component';

@NgModule({
    imports: [
        // Angular modules
        CommonModule,
        FormsModule,

        // Angular Material modules
        MatButtonModule,
        MatCheckboxModule,
        MatDividerModule,
        MatIconModule,
        MatInputModule,
        MatMenuModule,
        MatPaginatorModule,
        MatSortModule,
        MatTableModule,
        MatTooltipModule,

        // Third party modules
        PerfectScrollbarModule,
        SweetAlert2Module,

        // Project modules
        SharedModule,
        ComponentsModule,

        // Child routing module
        PersonnelRoutingModule,
    ],
    providers: [],
    declarations: [
        PersonnelAccountManageComponent,
        PersonnelAccountsComponent,
        PersonnelGroupManageComponent,
        PersonnelGroupsComponent,
    ]
})
export class PersonnelModule { }

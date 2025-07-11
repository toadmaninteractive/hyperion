import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PersonnelAccountsComponent } from './personnel-accounts/personnel-accounts.component';
import { PersonnelAccountManageComponent } from './personnel-account-manage/personnel-account-manage.component';
import { PersonnelGroupsComponent } from './personnel-groups/personnel-groups.component';
import { PersonnelGroupManageComponent } from './personnel-group-manage/personnel-group-manage.component';

const routes: Routes = [
    {
        path: '',
        children: [
            {
                path: 'accounts',
                component: PersonnelAccountsComponent,
            },
            {
                path: 'accounts/:username',
                component: PersonnelAccountManageComponent,
            },
            {
                path: 'groups',
                component: PersonnelGroupsComponent,
            },
            {
                path: 'groups/:name',
                component: PersonnelGroupManageComponent,
            },
            {
                path: '*',
                redirectTo: 'accounts',
            },
        ]
    }
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule]
})
export class PersonnelRoutingModule { }

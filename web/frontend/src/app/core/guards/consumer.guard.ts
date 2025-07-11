import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, Router, UrlTree } from '@angular/router';
import { Observable, combineLatest } from 'rxjs';
import { filter, map } from 'rxjs/operators';
import { Constants } from '../../shared/config/constants';
import { AccountService } from '../services/account.service';
import { ProjectService } from '../services/project.service';
import * as DbProtocol from '../../protocol/db-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class ConsumerGuard implements CanActivate {
    constructor(
        private router: Router,
        private accountService: AccountService,
        private projectService: ProjectService,
    ) {
    }

    canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean | UrlTree> {
        return combineLatest([
                this.accountService.profile$.pipe(filter(profile => profile instanceof DbProtocol.PersonnelAccountProfile)),
                this.projectService.activeRole$.asObservable().pipe(filter(role => role !== undefined))
            ]
        ).pipe(
            map(([profile, activeRole]) => {
                const isSuperadmin = profile.isSuperadmin,
                    isConsumer = (activeRole || 0) >= DbProtocol.AccessRole.Consumer;

                return (isConsumer || isSuperadmin) ? true : this.router.parseUrl(Constants.forbiddenUrl);
            }),
        );
    }
}

import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, Router, UrlTree } from '@angular/router';
import { Observable } from 'rxjs';
import { filter, map } from 'rxjs/operators';
import { Constants } from '../../shared/config/constants';
import { AccountService } from '../services/account.service';
import * as DbProtocol from '../../protocol/db-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class ProjectManagerGuard implements CanActivate {
    constructor (private router: Router, private accountService: AccountService) { }

    canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean | UrlTree> {
        return this.accountService.profile$
            .pipe(
                filter(profile => profile instanceof DbProtocol.PersonnelAccountProfile),
                map(profile => (profile.isProjectManager || profile.isSuperadmin) ? true : this.router.parseUrl(Constants.forbiddenUrl)),
            );
    }
}

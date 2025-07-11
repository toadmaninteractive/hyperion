import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { tap } from 'rxjs/operators';
import { AccountService } from '../services/account.service';
import { StorageService } from '../services/storage.service';

@Injectable({
    providedIn: 'root',
})
export class AuthorizedGuard implements CanActivate {
    constructor (
        private router: Router,
        private accountService: AccountService,
        private storageService: StorageService,
    ) { }

    canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> {
        return this.accountService.isSignedIn()
            .pipe(
                tap((signedIn: boolean) => {
                    if (!signedIn) {
                        if (!state.url.startsWith('/login')) {
                            this.storageService.setStoredRoute(state.url);
                        }

                        this.router.navigate(['/login']);
                    }
                })
            );
    }
}

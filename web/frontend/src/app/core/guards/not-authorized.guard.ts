import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { map, tap } from 'rxjs/operators';
import { AccountService } from '../services/account.service';

@Injectable({
    providedIn: 'root',
})
export class NotAuthorizedGuard implements CanActivate {
    constructor (
        private router: Router,
        private accountService: AccountService,
    ) { }

    canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> {
        return this.accountService.isSignedIn()
            .pipe(
                tap((signedIn: boolean) => signedIn && this.router.navigate(['/'])),
                map((signedIn: boolean) => !signedIn)   // Inverse signed in flag
            );
    }
}

import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject, Observable, of } from 'rxjs';
import { distinctUntilChanged, filter, finalize, switchMap, tap } from 'rxjs/operators';
import { Constants } from '../../shared/config/constants';
import { HyperionAuthService } from '../../protocol/auth-protocol.service';
import * as AuthProtocol from '../../protocol/auth-protocol.data';
import * as CommonProtocol from '../../protocol/common-protocol.data';
import * as DataProtocol from '../../protocol/data-protocol.data';
import * as DbProtocol from '../../protocol/db-protocol.data';
import { WebsocketService } from './websocket.service';

@Injectable({
    providedIn: 'root',
})
export class AccountService {
    private isInitializing$ = new BehaviorSubject(true);
    private isSignedIn$ = new BehaviorSubject<boolean | null>(null);
    isSigningIn$ = new BehaviorSubject(false);
    profile$ = new BehaviorSubject<DbProtocol.PersonnelAccountProfile | null>(null);

    constructor(
        private router: Router,
        private hyperionAuthService: HyperionAuthService,
        private websocketService: WebsocketService,
    ) {
        this.initialize();
    }

    private initialize(): void {
        if (!this.isInitializing$.getValue()) {
            this.isInitializing$.next(true);
        }

        this.isSignedIn$
            .pipe(
                filter(isSignedIn => isSignedIn !== null),
                distinctUntilChanged(),
            )
            .subscribe(isSignedIn => {
                if (isSignedIn) {
                    this.websocketService.restart();
                } else {
                    this.websocketService.disable();
                }
            });

        this.hyperionAuthService
            .getPersonnelStatus()
            .pipe(
                switchMap(response => response.loggedIn ? this.hyperionAuthService.getMyPersonnelProfile() : of(null)),
                finalize(() => this.isInitializing$.next(false)),
            )
            .subscribe((profile: DbProtocol.PersonnelAccountProfile | null) => {
                this.profile$.next(profile);
                this.isSignedIn$.next(profile instanceof DbProtocol.PersonnelAccountProfile);
            });
    }

    private reset(): void {
        this.isSignedIn$.next(false);
        this.profile$.next(null);
        this.router.navigate([Constants.loginUrl]);
    }

    isSignedIn(): Observable<boolean> {
        return this.isSignedIn$.pipe(filter(value => value !== null));
    }

    signIn(username: string, password: string): Observable<DbProtocol.PersonnelAccountProfile | AuthProtocol.PersonnelLoginResponse> {
        if (this.isSigningIn$.getValue()) {
            return;
        }

        this.isSigningIn$.next(true);

        const request = new AuthProtocol.PersonnelLoginRequest();
        request.username = username;
        request.password = password;

        return this.hyperionAuthService
            .loginPersonnel(request)
            .pipe(
                switchMap(response => response.result ? this.hyperionAuthService.getMyPersonnelProfile() : of(response)),
                tap(profile => {
                    if (profile instanceof DbProtocol.PersonnelAccountProfile) {
                        // Success
                        this.profile$.next(profile);
                        this.isSignedIn$.next(true);
                    } else {
                        // Failure
                        this.profile$.next(null);
                    }
                }),
                finalize(() => this.isSigningIn$.next(false))
            );
    }

    signOut(): Observable<DataProtocol.GenericResponse> {
        return this.hyperionAuthService
            .logoutPersonnel(new CommonProtocol.Empty())
            .pipe(
                tap((response: DataProtocol.GenericResponse) => {
                    console.log(`Sign out ${response.result ? 'successful' : 'failure'}`);
                    this.reset();
                })
            );
    }
}

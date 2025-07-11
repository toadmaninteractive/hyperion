import { ActivatedRouteSnapshot, Resolve, Router, RouterStateSnapshot } from '@angular/router';
import { HyperionTestRunService } from '../../protocol/test-run-protocol.service';
import { Observable, Subject, of } from 'rxjs';
import { Injectable } from '@angular/core';
import { catchError, takeUntil } from 'rxjs/operators';
import * as DbProtocol from '../../protocol/db-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class TestRunResolver implements Resolve<DbProtocol.TestRun> {
    destroy$ = new Subject<any>();

    constructor(
        private testRunService: HyperionTestRunService,
    ) { }

    resolve(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable <DbProtocol.TestRun | null> {
        const testRunId = +route.url[0]; // the first segment of url is :id
        return this.testRunService
            .getTestRun(testRunId)
            .pipe(
                takeUntil(this.destroy$),
                catchError(() => of(null))
            );
    }
}

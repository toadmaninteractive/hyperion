import { Injectable } from '@angular/core';
import { Resolve, ActivatedRouteSnapshot, RouterStateSnapshot, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { filter, first, map } from 'rxjs/operators';
import { Constants } from '../../shared/config/constants';
import { NavService } from '../services/nav.service';
import { ProjectService } from '../services/project.service';
import * as DbProtocol from '../../protocol/db-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class ProjectResolverGuard implements Resolve<DbProtocol.Project> {
    constructor(
        private router: Router,
        private navService: NavService,
        private projectService: ProjectService,
    ) { }

    resolve(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<DbProtocol.Project> {
        return this.projectService.projects$
            .pipe(
                filter(projects => projects instanceof Array),
                first(),
                map(projects => {
                    const activeProject = this.projectService.getActiveProject(state.url),
                        fallbackProject = projects[0];

                    if (activeProject) {
                        return activeProject;
                    } else if (fallbackProject) {
                        this.navService.switchProject(fallbackProject.key, state.url);
                        return null;
                    } else {
                        this.router.navigate([Constants.errorUrl]);
                        return null;
                    }
                }),
            );
    }
}

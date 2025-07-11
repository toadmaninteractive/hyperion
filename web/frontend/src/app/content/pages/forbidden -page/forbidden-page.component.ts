import { Component, HostBinding, Input, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { ProjectService } from '../../../core/services/project.service';
import { Project } from '../../../protocol/db-protocol.data';
import { combineLatest } from 'rxjs';

@Component({
    selector: 'm-forbidden-page',
    templateUrl: './forbidden-page.component.html',
    styleUrls: ['./forbidden-page.component.scss']
})
export class ForbiddenPageComponent implements OnInit {
    @HostBinding('class') classes = 'm-grid m-grid--hor m-grid--root m-page';
    @Input() errorType: number;
    availableProject: Project;

    constructor(private route: ActivatedRoute,
                private projectService: ProjectService) { }

    ngOnInit() {
        this.errorType = +this.route.snapshot.paramMap.get('type');

        combineLatest([this.projectService.roleMap$, this.projectService.projects$])
            .subscribe(([roleMap, projects]) => {
                this.availableProject = projects.filter(project => roleMap.has(project.key) || roleMap.get(project.key) > 0)[0] || null;
            });
        if (!this.errorType) {
            this.errorType = Math.floor((Math.random() * 3) + 1);
        }
    }

}

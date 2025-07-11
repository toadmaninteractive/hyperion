import { ChangeDetectionStrategy, Component } from '@angular/core';
import { NavService } from '../../../../../core/services/nav.service';
import { ProjectService } from '../../../../../core/services/project.service';
import { WindowRefService } from '../../../../../core/services/window-ref.service';

@Component({
    selector: 'm-project-switch',
    templateUrl: './project-switch.component.html',
    styleUrls: ['./project-switch.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ProjectSwitchComponent {
    constructor (
        public navService: NavService,
        public projectService: ProjectService,
        private windowRefService: WindowRefService,
    ) { }

    computeLength(title: string, minWidth: number = 125): number {
        const canvas = this.windowRefService.canvas,
            context = canvas.getContext('2d');

        // HACK: check if font is loaded by checking the known text size
        const isFontLoaded = context.measureText('Test Project').width > 90;
        context.font = '300 13px JetBrains Mono';

        const metrics = context.measureText(title),
            charApproximateWidth = 8,
            spacerWidth = 50,
            computedWidth = (isFontLoaded ? metrics.width : title.trim().length * charApproximateWidth) + spacerWidth;

        return computedWidth > minWidth ? computedWidth : minWidth;
    }
}

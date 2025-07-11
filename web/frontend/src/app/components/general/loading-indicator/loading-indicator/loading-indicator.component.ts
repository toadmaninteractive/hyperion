import { Component, Input } from '@angular/core';

@Component({
    moduleId: module.id,
    selector: 'm-loading-indicator',
    templateUrl: './loading-indicator.component.html',
    styleUrls: ['./loading-indicator.component.scss']
})

export class LoadingIndicatorComponent {
    @Input() isInline = false;
}

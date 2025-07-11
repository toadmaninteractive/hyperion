import { Component, Input, ChangeDetectionStrategy } from '@angular/core';

@Component({
    selector: 'm-smart-progress-bar',
    templateUrl: 'smart-progress-bar.component.html',
    styleUrls: ['./smart-progress-bar.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SmartProgressBarComponent {
    @Input() rounded = true;
    @Input() height = 10;
    @Input() smooth = true;
    @Input() total: number | null;
    @Input() pending: number | null;
    @Input() inProgress: number | null;
    @Input() passed: number | null;
    @Input() failed: number | null;
    @Input() blocked: number | null;
}

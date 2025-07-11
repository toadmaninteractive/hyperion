import { ChangeDetectionStrategy, Component, EventEmitter, Input, Output } from '@angular/core';

@Component({
    selector: 'm-alert',
    templateUrl: './alert.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AlertComponent {
    @Input() message?: string = null;
    @Output() messageChanged = new EventEmitter<string | null>();
    @Input() kind: 'success' | 'info' | 'warning' | 'danger' | 'brand' | 'primary' = 'primary';
    @Input() dismissible = true;

    constructor() { }

    close(): void {
        this.message = null;
        this.messageChanged.emit(this.message);
    }
}

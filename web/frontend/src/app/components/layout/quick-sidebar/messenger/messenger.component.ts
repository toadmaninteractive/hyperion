import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
import { of } from 'rxjs';

@Component({
    selector: 'm-messenger',
    templateUrl: './messenger.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MessengerComponent {
    @Input() messages = of([]);

    constructor() { }
}

import { Component, Input, HostBinding, ChangeDetectionStrategy } from '@angular/core';

@Component({
    selector: 'm-messenger-out',
    templateUrl: './messenger-out.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MessengerOutComponent {
    @HostBinding('class') classes = 'm-messenger__wrapper';
    @Input() message: any;

    constructor() { }
}

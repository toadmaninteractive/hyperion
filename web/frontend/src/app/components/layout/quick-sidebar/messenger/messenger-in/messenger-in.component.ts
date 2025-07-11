import { Component, Input, HostBinding, ChangeDetectionStrategy } from '@angular/core';

@Component({
    selector: 'm-messenger-in',
    templateUrl: './messenger-in.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MessengerInComponent {
    @HostBinding('class') classes = 'm-messenger__wrapper';
    @Input() message: any;
    constructor() { }
}

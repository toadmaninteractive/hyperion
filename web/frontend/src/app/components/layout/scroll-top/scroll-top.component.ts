import { Component, ChangeDetectionStrategy } from '@angular/core';

@Component({
    selector: 'm-scroll-top',
    templateUrl: './scroll-top.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ScrollTopComponent {
    constructor() { }
}

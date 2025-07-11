import { Component, HostBinding, ChangeDetectionStrategy } from '@angular/core';

@Component({
    selector: 'm-tooltips',
    templateUrl: './tooltips.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class TooltipsComponent {
    @HostBinding('class') classes = 'm-nav-sticky';
    @HostBinding('style.margin-top') marginTop = '30px';

    constructor() { }
}

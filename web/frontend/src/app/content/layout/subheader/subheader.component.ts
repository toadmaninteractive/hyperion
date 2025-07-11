import { Component, ChangeDetectionStrategy } from '@angular/core';
import { SubheaderService } from '../../../core/services/metronic/layout/subheader.service';

@Component({
    selector: 'm-subheader',
    templateUrl: './subheader.component.html',
    styleUrls: ['./subheader.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SubheaderComponent {
    constructor(public subheaderService: SubheaderService) { }
}

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ListTimelineComponent } from './list-timeline.component';
import { TimelineItemComponent } from './timeline-item/timeline-item.component';
import { SharedModule } from '../../../../shared/shared.module';

@NgModule({
    imports: [CommonModule, SharedModule],
    declarations: [ListTimelineComponent, TimelineItemComponent],
    exports: [ListTimelineComponent, TimelineItemComponent]
})
export class ListTimelineModule { }

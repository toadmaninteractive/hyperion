import { Directive } from '@angular/core';
import { EventEmitter } from '@angular/core';
import { ClipboardService } from '../../core/services/clipboard.service';

// This directive acts as a simple glue layer between the given [clipboard] property and the underlying ClipboardService.
// Upon the (click) event, the [clipboard] value will be copied to the ClipboardService and a (clipboardCopy) event will be emitted.
@Directive({
    selector: '[clipboard]',
    inputs: ['value: clipboard'],
    outputs: [
        'copyEvent: clipboardCopy',
        'errorEvent: clipboardError'
    ],
    host: {
        '(click)': 'copyToClipboard()'
    }
})
export class ClipboardDirective {
    public copyEvent = new EventEmitter<string>();
    public errorEvent = new EventEmitter<Error>();
    public value = '';

    constructor(private clipboardService: ClipboardService) { }

    public copyToClipboard(): void {
        this.clipboardService
            .copy(this.value)
            .then((value: string): void => this.copyEvent.emit(value))
            .catch((error: Error): void => this.errorEvent.emit(error));
    }
}

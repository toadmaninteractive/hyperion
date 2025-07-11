import { Injectable } from '@angular/core';

@Injectable({
    providedIn: 'root',
})
export class FilterService {
    // Track by functions
    trackByIndex(index: number) {
        return index;
    }
}

import { Injectable } from '@angular/core';
import { Observable, of } from 'rxjs';
import { map } from 'rxjs/operators';
import { LayoutConfig } from '../../../shared/config/layout';

@Injectable({
    providedIn: 'root',
})
export class LayoutConfigStorageService {
    constructor() { }

    saveConfig(layoutConfig: LayoutConfig): void {
        if (layoutConfig != null) {
            // Config storage
            localStorage.setItem('layoutConfig', JSON.stringify(layoutConfig));
        }
    }

    getSavedConfig(): Observable<LayoutConfig> {
        const config: any = localStorage.getItem('layoutConfig');

        try {
            return of(JSON.parse(config));
        } catch (e) { }
    }

    loadConfig(): Observable<LayoutConfig> {
        return this.getSavedConfig().pipe(
            map(config => Object.assign({}, new LayoutConfig(), config))
        );
    }

    resetConfig(): void {
        localStorage.removeItem('layoutConfig');
    }
}

import { Injectable } from '@angular/core';
import { Constants } from '../../shared/config/constants';

@Injectable({
    providedIn: 'root',
})
export class StorageService {
    getStoredRoute(): string | null {
        return localStorage.getItem(Constants.storedRouteKey) || null;
    }

    setStoredRoute(url: string): void {
        localStorage.setItem(Constants.storedRouteKey, url);
    }

    resetStoredRoute(): void {
        localStorage.removeItem(Constants.storedRouteKey);
    }

    getLastProjectKey(): string | null {
        return localStorage.getItem(Constants.lastProjectKey) || null;
    }

    setLastProjectKey(projectKey: string): void {
        localStorage.setItem(Constants.lastProjectKey, projectKey);
    }

    resetLastProjectKey(): void {
        localStorage.removeItem(Constants.lastProjectKey);
    }
}

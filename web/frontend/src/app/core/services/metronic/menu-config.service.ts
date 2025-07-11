import { Injectable } from '@angular/core';
import { Router, NavigationStart } from '@angular/router';
import { BehaviorSubject } from 'rxjs';
import { filter} from 'rxjs/operators';
import { MenuConfig } from '../../../shared/config/menu';

@Injectable({
    providedIn: 'root',
})
export class MenuConfigService {
    public configModel: MenuConfig = new MenuConfig();
    public onMenuUpdated$: BehaviorSubject<MenuConfig> = new BehaviorSubject(this.configModel);
    menuHasChanged: any = false;

    constructor(private router: Router) {
        this.router.events
            .pipe(filter(event => event instanceof NavigationStart))
            .subscribe(event => {
                if (this.menuHasChanged) {
                    this.resetModel();
                }
            });
    }

    setModel(menuModel: MenuConfig) {
        this.configModel = Object.assign(this.configModel, menuModel);
        this.onMenuUpdated$.next(this.configModel);
        this.menuHasChanged = true;
    }

    resetModel() {
        this.configModel = new MenuConfig();
        this.onMenuUpdated$.next(this.configModel);
        this.menuHasChanged = false;
    }
}

import { BehaviorSubject } from 'rxjs';
import { Injectable } from '@angular/core';
import * as objectPath from 'object-path';
import { ClassInitService } from '../class-init.service';
import { LayoutConfigService } from '../layout-config.service';
import { MenuConfigService } from '../menu-config.service';

@Injectable({
    providedIn: 'root',
})
export class MenuAsideService {
    classes: string;
    menuList$: BehaviorSubject<any[]> = new BehaviorSubject([]);
    isDropdown = 0;
    dropdownTimeout: number;
    isScrollable = 0;

    constructor(
        private menuConfigService: MenuConfigService,
        private classInitService: ClassInitService,
        private layoutConfigService: LayoutConfigService
    ) {
        // Get menu list
        this.menuConfigService.onMenuUpdated$.subscribe(model => {
            setTimeout(() =>
                this.menuList$.next(objectPath.get(model.config, 'aside.items'))
            );
        });

        this.layoutConfigService.onLayoutConfigUpdated$.subscribe(config => {
            if (objectPath.get(config, 'config.aside.left.fixed')) {
                this.isScrollable = 1;
                this.isDropdown = 0;
            }

            // tslint:disable-next-line:max-line-length
            if (!objectPath.get(config, 'config.aside.left.fixed') && !objectPath.get(config, 'config.menu.aside.desktop_and_mobile.submenu.accordion')) {
                this.isScrollable = 0;
                this.isDropdown = 1;
                this.dropdownTimeout = objectPath.get(config, 'config.menu.aside.desktop_and_mobile.submenu.dropdown.hover_timeout');
            }
        });
    }
}

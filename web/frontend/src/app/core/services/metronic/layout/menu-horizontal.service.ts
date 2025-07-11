import { BehaviorSubject } from 'rxjs';
import { Injectable } from '@angular/core';
import * as objectPath from 'object-path';
import { ClassInitService } from '../class-init.service';
import { LayoutConfigService } from '../layout-config.service';
import { MenuConfigService } from '../menu-config.service';

@Injectable({
    providedIn: 'root',
})
export class MenuHorizontalService {
    menuList$: BehaviorSubject<any[]> = new BehaviorSubject([]);
    attributes: any;
    menuClasses: string;

    constructor(
        private menuConfigService: MenuConfigService,
        private classInitService: ClassInitService,
        private layoutConfigService: LayoutConfigService
    ) {
        // Get menu list
        this.menuConfigService.onMenuUpdated$.subscribe(model => {
            this.menuList$.next(objectPath.get(model.config, 'header.items'));
        });

        // Subscribe to menu classes update
        this.classInitService.onClassesUpdated$.subscribe(classes => {
            // Default class
            this.menuClasses = 'm-header-menu m-aside-header-menu-mobile m-aside-header-menu-mobile--offcanvas';

            // Join the classes array and pass to variable, add classes to this host binding class
            this.menuClasses += ' ' + classes.header_menu.join(' ');
        });
    }
}

import { Injectable } from '@angular/core';
import * as objectPath from 'object-path';
import { ClassInitService } from '../class-init.service';
import { LayoutConfigService } from '../layout-config.service';

@Injectable({
    providedIn: 'root',
})
export class HeaderService {
    // Class for the header container
    containerClass: string;
    // Class for the header menu close
    headerMenuCloseClass: boolean;
    // Toggle to display menu on header
    menuHeaderDisplay: boolean;

    // Minimize enabled
    attrMinimizeDesktopEnabled: boolean;
    attrMinimizeMobileEnabled: boolean;

    // Minimize offset
    attrMinimizeOffset = '200';

    // Minimize offset on mobile
    attrMinimizeMobileOffset = '200';

    constructor(
        private layoutConfigService: LayoutConfigService,
        private classInitService: ClassInitService
    ) {
        // Subscribe to classes update
        this.classInitService.onClassesUpdated$.subscribe(classes => {
            this.headerMenuCloseClass = classes.header_menu_close.join(' ');
        });

        this.layoutConfigService.onLayoutConfigUpdated$.subscribe(model => {
            const config = model.config;

            // tslint:disable-next-line:prefer-const
            let containerClass = ['m-container', 'm-container--full-height'];
            const selfLayout = objectPath.get(config, 'self.layout');

            if (selfLayout === 'boxed' || selfLayout === 'wide') {
                containerClass.push('m-container--responsive m-container--xxl');
            } else {
                containerClass.push('m-container--fluid');
            }

            this.containerClass = containerClass.join(' ');

            // Get menu header display option
            this.menuHeaderDisplay = objectPath.get(config, 'menu.header.display');

            // Minimize desktop/mobile
            this.attrMinimizeDesktopEnabled = objectPath.get(config, 'header.self.fixed.minimize.desktop.enabled');
            this.attrMinimizeMobileEnabled = objectPath.get(config, 'header.self.fixed.minimize.mobile.enabled');
            this.attrMinimizeOffset = objectPath.get(config, 'header.self.fixed.minimize.desktop.offset');
            this.attrMinimizeMobileOffset = objectPath.get(config, 'header.self.fixed.minimize.mobile.offset');
        });
    }
}

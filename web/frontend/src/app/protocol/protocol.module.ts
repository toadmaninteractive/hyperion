import { NgModule, Optional, SkipSelf  } from '@angular/core';
import { CommonModule } from '@angular/common';

import { HyperionAuthService } from './auth-protocol.service';
import { HyperionProjectService } from './project-protocol.service';
import { HyperionSettingsService } from './settings-protocol.service';

@NgModule({
    imports: [
        CommonModule
    ],
    declarations: [],
    exports: [],
    providers: [
        HyperionAuthService,
        HyperionProjectService,
        HyperionSettingsService,
    ]
})
export class ProtocolModule {
    constructor (@Optional() @SkipSelf() parentModule: ProtocolModule) {
        if (parentModule) {
            throw new Error('ProtocolModule is already loaded. Import it in the AppModule only');
        }
    }
}

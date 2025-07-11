import { NgModule, Optional, SkipSelf  } from '@angular/core';
import { CommonModule } from '@angular/common';

// Metronic services
import { ClassInitService } from './services/metronic/class-init.service';
import { LayoutConfigService } from './services/metronic/layout-config.service';
import { LayoutConfigStorageService } from './services/metronic/layout-config-storage.service';
import { MenuConfigService } from './services/metronic/menu-config.service';
import { PageConfigService } from './services/metronic/page-config.service';
import { SplashScreenService } from './services/metronic/splash-screen.service';

// Metronic layout services
import { HeaderService } from './services/metronic/layout/header.service';
import { LayoutRefService } from './services/metronic/layout/layout-ref.service';
import { MenuAsideService } from './services/metronic/layout/menu-aside.service';
import { MenuHorizontalService } from './services/metronic/layout/menu-horizontal.service';
import { SubheaderService } from './services/metronic/layout/subheader.service';

// Hyperion services
import { AccountService } from './services/account.service';
import { ClipboardService } from './services/clipboard.service';
import { FilterService } from './services/filter.service';
import { NavService } from './services/nav.service';
import { NotificationService } from './services/notification.service';
import { ProjectService } from './services/project.service';
import { StorageService } from './services/storage.service';
import { UploadService } from './services/upload.service';
import { WebsocketService } from './services/websocket.service';
import { WindowRefService } from './services/window-ref.service';

// Hyperion fake services
import { FakeDataService } from './services/fake/fake-data.service';

// Hyperion guards
import { AuthorizedGuard } from './guards/authorized.guard';
import { ConsumerGuard } from './guards/consumer.guard';
import { MaintainerGuard } from './guards/maintainer.guard';
import { NotAuthorizedGuard } from './guards/not-authorized.guard';
import { ProjectManagerGuard } from './guards/project-manager.guard';
import { SuperadminGuard } from './guards/superadmin.guard';

// Hyperion resolvers
import { ProjectResolverGuard } from './resolvers/project-resolver.guard';
import { TestRunResolver } from './resolvers/test-run-resolver';
import { SetupService } from './services/setup.service';
import { ParameterService } from './services/parameter.service';
import { TestCaseService } from './services/testcase.service';

@NgModule({
    imports: [
        CommonModule
    ],
    declarations: [],
    exports: [],
    providers: [
        // Metronic services
        ClassInitService,
        LayoutConfigService,
        LayoutConfigStorageService,
        MenuConfigService,
        PageConfigService,
        SplashScreenService,

        // Metronic layout services
        HeaderService,
        LayoutRefService,
        MenuAsideService,
        MenuHorizontalService,
        SubheaderService,

        // Hyperion services
        AccountService,
        ClipboardService,
        FilterService,
        NavService,
        NotificationService,
        ParameterService,
        ProjectService,
        SetupService,
        TestCaseService,
        StorageService,
        UploadService,
        WebsocketService,
        WindowRefService,

        // Hyperion fake services
        FakeDataService,

        // Hyperion guards
        AuthorizedGuard,
        ConsumerGuard,
        MaintainerGuard,
        NotAuthorizedGuard,
        ProjectManagerGuard,
        SuperadminGuard,

        // Hyperion resolvers
        ProjectResolverGuard,
        TestRunResolver
    ]
})
export class CoreModule {
    constructor (@Optional() @SkipSelf() parentModule: CoreModule) {
        if (parentModule) {
            throw new Error('CoreModule is already loaded. Import it in the AppModule only');
        }
    }
}

import { ConfigModel } from '../interfaces/config';

export class MenuConfig implements ConfigModel {
    public config: any = {};

    constructor() {
        this.config = {
            header: {},
            aside: {
                self: {},
                items: [
                    {
                        title: 'Dashboard',
                        root: true,
                        icon: 'dashboard',
                        page: '/dashboard/:project',
                        dynamic: true,
                    },
                    {
                        title: 'Parameters',
                        root: true,
                        icon: 'settings',
                        page: '/parameters/:project',
                        for: 'maintainer',
                        dynamic: true,
                    },
                    {
                        title: 'Setup',
                        root: true,
                        icon: 'build',
                        page: '/setup/:project',
                        for: 'maintainer',
                        dynamic: true,
                    },
                    {
                        title: 'Test Cases',
                        root: true,
                        icon: 'work',
                        page: '/test-cases/:project',
                        for: 'maintainer',
                        dynamic: true,
                    },
                    {
                        title: 'Test Runs',
                        root: true,
                        icon: 'view_week',
                        page: '/test-runs/:project',
                        for: 'maintainer',
                        dynamic: true,
                    },
                    {
                        title: 'Testing',
                        root: true,
                        icon: 'android',
                        page: '/testing/:project',
                        dynamic: true,
                    },
                    {
                        title: 'Attachments',
                        root: true,
                        icon: 'burst_mode',
                        page: '/attachments/:project',
                        for: 'maintainer',
                        dynamic: true,
                    },
                    { section: 'Personnel', for: 'superadmin' },
                    {
                        title: 'Accounts',
                        root: true,
                        icon: 'people',
                        page: '/personnel/accounts',
                        for: 'superadmin',
                        breadcrumbs: false,
                    },
                    {
                        title: 'Groups',
                        root: true,
                        icon: 'group_work',
                        page: '/personnel/groups',
                        for: 'superadmin',
                        breadcrumbs: false,
                    },
                    { section: 'Admin', for: 'superadmin' },
                    {
                        title: 'Projects',
                        root: true,
                        icon: 'apps',
                        page: '/projects',
                        for: 'project_manager',
                    },
                    {
                        title: 'JIRA',
                        root: true,
                        image: 'assets/app/media/img/tools/jira.png',
                        page: '/jira',
                        for: 'superadmin',
                    },
                    {
                        title: 'Settings',
                        root: true,
                        icon: 'settings',
                        page: '/settings',
                        for: 'superadmin',
                    },
                ]
            }
        };
    }
}

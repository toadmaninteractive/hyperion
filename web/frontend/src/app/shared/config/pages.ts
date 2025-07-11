import { ConfigModel } from '../interfaces/config';

export class PagesConfig implements ConfigModel {
    public config: any = {};

    constructor() {
        this.config = {
            dashboard: {
                page: {
                    title: 'Dashboard',
                    desc: 'Dashboard description'
                }
            },
            'parameter-sources': {
                page: {
                    title: 'Project parameters',
                    desc: 'Create and modify parameter sources'
                }
            },
            setup: {
                page: {
                    title: 'Project setup',
                    desc: 'Create and modify setup steps'
                }
            },
            'test-cases': {
                page: {
                    title: 'Test cases',
                    desc: 'Create and modify test cases'
                }
            },
            'test-runs': {
                page: {
                    title: 'Test runs',
                    desc: 'Create and modify test runs'
                }
            },
            testing: {
                page: {
                    title: 'Testing',
                    desc: 'Interact with test runs'
                }
            },
            attachments: {
                page: {
                    title: 'Attachments',
                    desc: 'Show and filter project attachments'
                }
            },
            personnel: {
                accounts: {
                    page: {
                        title: 'Personnel accounts',
                        desc: 'Manage personnel accounts'
                    }
                },
                groups: {
                    page: {
                        title: 'Personnel groups',
                        desc: 'Manage personnel groups'
                    }
                }
            },
            projects: {
                page: {
                    title: 'Manage projects',
                    desc: 'Create and modify Hyperion projects'
                }
            },
            jira: {
                page: {
                    title: 'Manage JIRA instances',
                    desc: 'Manage JIRA instances'
                }
            },
            settings: {
                page: {
                    title: 'Manage settings',
                    desc: 'Create and modify Hyperion projects'
                }
            },
            404: {
                page: { title: '404 Not Found', desc: '', subheader: false }
            }
        };
    }
}

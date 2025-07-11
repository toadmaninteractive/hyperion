import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { NotificationService } from '../../../core/services/notification.service';
import { HyperionSettingsService } from '../../../protocol/settings-protocol.service';
import * as DataProtocol from '../../../protocol/data-protocol.data';
import * as SettingsProtocol from '../../../protocol/settings-protocol.data';

@Component({
    selector: 'm-settings',
    templateUrl: './settings.component.html',
    styleUrls: ['./settings.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SettingsComponent implements OnInit, OnDestroy {
    public config: any;
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    settings: SettingsProtocol.Settings;
    pristineSettings: SettingsProtocol.Settings;

    constructor (
        private router: Router,
        private cdr: ChangeDetectorRef,
        private hyperionSettingsService: HyperionSettingsService,
        private notificationService: NotificationService,
    ) { }

    ngOnInit(): void {
        this.destroy$ = new Subject();
        this.loading$ = new BehaviorSubject<boolean>(false);
        this.getSettings();
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
    }

    isInitialized(): boolean {
        return !!(this.settings && this.pristineSettings);
    }

    settingsChanged(): boolean {
        if (!this.isInitialized()) {
            return false;
        }

        return +this.settings.personnelSessionDuration !== this.pristineSettings.personnelSessionDuration;
    }

    settingsValid(): boolean {
        if (!this.isInitialized()) {
            return false;
        }

        return +this.settings.personnelSessionDuration > 0;
    }

    getSettings(): void {
        this.loading$.next(true);

        this.hyperionSettingsService
            .getSettings()
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: SettingsProtocol.Settings) => {
                this.settings = response;
                this.pristineSettings = SettingsProtocol.Settings.fromJson(response.toJson());
                this.cdr.detectChanges();
            });
    }

    updateSettings(): void {
        if (!this.isInitialized()) {
            return;
        }

        const body = new SettingsProtocol.SettingsUpdateRequest();
        body.personnelSessionDuration = (+this.settings.personnelSessionDuration !== this.pristineSettings.personnelSessionDuration) ? this.settings.personnelSessionDuration : null;
        this.loading$.next(true);

        this.hyperionSettingsService
            .updateSettings(body)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: DataProtocol.GenericResponse) => {
                response.result
                    ? this.notificationService.success('Settings updated')
                    : this.notificationService.error('Settings not updated');

                if (response.result) {
                    this.cdr.detectChanges();
                    this.getSettings();
                }
            });
    }
}

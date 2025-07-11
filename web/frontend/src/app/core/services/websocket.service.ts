import { Injectable, NgZone, OnDestroy } from '@angular/core';
import { BehaviorSubject, Subscription, interval, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { WebSocketSubject, WebSocketSubjectConfig } from 'rxjs/webSocket';
import { WindowRefService } from './window-ref.service';
import { AbstractNotificationService } from '../../protocol/abstract-notification.service';
import * as NotificationProtocol from '../../protocol/notification-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class WebsocketService extends AbstractNotificationService implements OnDestroy {
    destroy$ = new Subject<any>();
    private connected$ = new BehaviorSubject(false);
    private websocket$: WebSocketSubject<Object>;
    private reconnection$: Subscription;
    private data$: Subscription;
    private readonly config: WebSocketSubjectConfig<any>;
    private readonly reconnectInterval = 1000;
    private reconnect = false;
    private isOpening = false;
    private isDebug = true;

    constructor(private windowRef: WindowRefService, private ngZone: NgZone) {
        // Initialize parent class
        super();

        // Define host, URL and secure flag
        const host = this.windowRef.nativeWindow.location.host,
            isSecure = this.windowRef.nativeWindow.location.protocol.indexOf('https:') === 0;

        this.config = {
            url: (isSecure ? 'wss://' : 'ws://') + host + '/ws',
            openObserver: {
                next: (event: Event) => {
                    if (this.isDebug) {
                        console.log('Websocket connected');
                    }

                    this.isOpening = false;
                    this.connected$.next(true);
                }
            },
            closeObserver: {
                next: (event: CloseEvent) => {
                    if (this.isDebug) {
                        console.log('Websocket disconnected');
                    }

                    this.isOpening = false;
                    this.closeWebsocket();
                }
            },
        };

        this.reconnection$ = interval(this.reconnectInterval)
            .pipe(takeUntil(this.destroy$))
            .subscribe(counter => this.maybeStart());
    }

    ngOnDestroy() {
        this.destroy$.next();
        this.destroy$.complete();
        this.reconnection$.unsubscribe();
        this.disable();
    }

    private closeWebsocket(): void {
        if (this.websocket$) {
            this.websocket$.complete();
            this.websocket$.unsubscribe();
        }

        if (this.data$) {
            this.data$.unsubscribe();
        }

        this.connected$.next(false);
    }

    enable(): void {
        this.reconnect = true;
    }

    disable(): void {
        this.reconnect = false;
        this.closeWebsocket();
    }

    restart(): void {
        this.reconnect = true;
        this.closeWebsocket();
    }

    private send(json: any): void {
        this.websocket$.next(json); // was: JSON.stringify(json)
    }

    sendNotification(notification: NotificationProtocol.Notification): void {
        if (this.isDebug) {
            console.log('Websocket -->', notification);
        }

        this.send(notification.toJson());
    }

    maybeStart(): void {
        if (this.reconnect && !this.connected$.getValue() && !this.isOpening) {
            this.start();
        }
    }

    start(): void {
        if (this.connected$.getValue()) {
            return;
        }

        this.isOpening = true;
        this.websocket$ = new WebSocketSubject(this.config);

        this.data$ = this.websocket$.subscribe({
            next: (data: MessageEvent) => {
                const notification = NotificationProtocol.Notification.fromJson(data as any);

                if (this.isDebug) {
                    console.log('Websocket <--', notification);
                }

                if (notification instanceof NotificationProtocol.Hello) {
                    this.sendNotification(new NotificationProtocol.Hello());
                }

                this.ngZone.run(() => this.recv(notification));
            },

            error: (error: any) => {
                if (this.isDebug) {
                    console.log('Websocket error: ', error);
                }

                this.restart();
            },
        });
    }
}

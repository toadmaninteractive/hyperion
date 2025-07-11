import { Injectable } from '@angular/core';
import { Observable, of } from 'rxjs';
import { SelectItem } from '../../../shared/interfaces/select-item';
import { PlayerEvent } from '../../../shared/interfaces/player-event';

@Injectable({
    providedIn: 'root'
})
export class FakeDataService {
    getData(key: string): number {
        switch (key) {
            case 'tuu':
                return Math.round(Math.random() * 10000);
            case 'asl':
                return Math.round(Math.random() * 1000) / 100;
            case 'nut':
                return Math.round(Math.random() * 100);
            case 'dau':
                return Math.round(Math.random() * 100);
            case 'mau':
                return Math.round(Math.random() * 1000);
            default:
                return 0;
        }
    }

    getUniqueUsersByPlatform(): Observable<SelectItem[]> {
        return of([
            {label: 'PC', value: Math.round(Math.random() * 1000000)},
            {label: 'Playstation 4', value: Math.round(Math.random() * 1000000)},
            {label: 'XBOX', value: Math.round(Math.random() * 1000000)},
            {label: 'Android', value: Math.round(Math.random() * 1000000)}
        ]);
    }

    getEvents(offset, limit): Observable<PlayerEvent[]> {
        const result = [];
        for (let index = 0; index < limit; index++) {
            result.push(this.eventFabric(offset, limit));
        }
        return of(result);
    }

    eventFabric(offset: number, limit: number): PlayerEvent {
        const eventsTitle = ['player_received_damage', 'player_deal_damage', 'character_created', 'character_deleted', 'session_start', 'session_finish'];
        const eventsStatusTitle = ['a', 'i', 'd', 't'];
        const statusArr = [];
        for (let index = 0; index < 3; index++) {
            statusArr.push({label: eventsStatusTitle[Math.floor(Math.random() * 4)], value: Math.random() >= 0.5});
        }
        const event: PlayerEvent = {
                id: Math.floor(Math.random() * limit) + offset,
                eventName: eventsTitle[Math.floor(Math.random() * eventsTitle.length )],
                status: statusArr,
                dateAdded: new Date(new Date(2017, 0, 1).getTime() + Math.random() * (new Date().getTime() - new Date(2017, 0, 1).getTime())).toLocaleDateString() ,
                parameterList: [{label: 'event_name', value: true}, {label: 'game_id', value: false}]
        };
        return event;
    }
}

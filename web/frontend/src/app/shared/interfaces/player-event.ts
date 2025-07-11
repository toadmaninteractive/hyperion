import { SelectItem } from './select-item';

export interface PlayerEvent {
    id: number;
    eventName: string;
    status: SelectItem[];
    dateAdded: string;
    parameterList: SelectItem[];
}

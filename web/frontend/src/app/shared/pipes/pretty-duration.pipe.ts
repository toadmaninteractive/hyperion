import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
    name: 'mPrettyDuration'
})
export class PrettyDurationPipe implements PipeTransform {
    transform(value: number | any, args?: any): string {
        if (!(typeof value === 'number' && value > 0)) {
            return 'invalid duration';
        }

        let duration = +value;
        const days = Math.floor(duration / 86400);
        duration = duration % 86400;
        const hours = Math.floor(duration / 3600);
        duration = duration % 3600;
        const minutes = Math.floor(duration / 60);
        const seconds = duration % 60;

        let result = days > 0 ? (days + ' days ') : '';
        result += (hours > 0) ? (hours + ' hours ') : '';
        result += (minutes > 0) ? (minutes + ' mins ') : '';
        result += (seconds > 0) ? (seconds + ' secs ') : '';

        return result;
    }
}

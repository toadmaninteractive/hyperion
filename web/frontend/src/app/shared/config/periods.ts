import { Period, PeriodKind } from '../interfaces/period';

export function defaultPeriods(): Period[] {
    return [
        { value: PeriodKind.Today, label: 'Today' },
        { value: PeriodKind.Week, label: 'Week' },
        { value: PeriodKind.Month, label: 'Month' },
    ];
}

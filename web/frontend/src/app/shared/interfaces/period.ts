export enum PeriodKind {
    Today,
    Week,
    Month,
}

export interface Period {
    value: PeriodKind;
    label: string;
}

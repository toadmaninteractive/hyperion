import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
    name: 'mPrettySize'
})
export class PrettySizePipe implements PipeTransform {
    private readonly KB = 1024;
    private readonly MB = 1024 * 1024;
    private readonly GB = 1024 * 1024 * 1024;
    private readonly TB = 1024 * 1024 * 1024 * 1024;

    private toPrecision(value: number, precision: number): number {
        return parseFloat(value.toPrecision(2));
    }

    transform(value: number | any, args?: any): string {
        if (!(typeof value === 'number' && value >= 0)) {
            return '???';
        }

        if (value >= this.TB) {
            return `${this.toPrecision(value / this.TB, 2)} TB`;
        } else if (value >= this.GB) {
            return `${this.toPrecision(value / this.GB, 2)} GB`;
        } else if (value >= this.MB) {
            return `${this.toPrecision(value / this.MB, 2)} MB`;
        } else if (value >= this.KB) {
            return `${this.toPrecision(value / this.KB, 2)} KB`;
        } else {
            return `${value} B`;
        }
    }
}

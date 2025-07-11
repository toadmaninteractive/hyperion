import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
    name: 'mNumeric'
})
export class NumericPipe implements PipeTransform {
    transform(value: number): string {
        return String(value).replace(/(\d)(?=(\d\d\d)+([^\d]|$))/g, '$1 ');
    }
}

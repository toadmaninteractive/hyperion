import { ChangeDetectionStrategy, Component, Input } from '@angular/core';
import { DomSanitizer, SafeUrl } from '@angular/platform-browser';
import { Md5 } from 'ts-md5';

@Component({
    selector: 'm-gravatar',
    templateUrl: './gravatar.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class GravatarComponent {
    @Input() email: string;
    @Input() defaultSet: '404' | 'mp' | 'identicon' | 'monsterid' | 'wavatar' = 'identicon';
    @Input() size = 32;
    @Input() radius = 4;

    constructor(private sanitizer: DomSanitizer) { }

    gravatarUrl(email: string, size: number): SafeUrl {
        const md5 = Md5.hashAsciiStr(email.toLowerCase().trim());
        return this.sanitizer.bypassSecurityTrustUrl(`https://www.gravatar.com/avatar/${md5}?s=${size}&d=${this.defaultSet}`);
    }
}

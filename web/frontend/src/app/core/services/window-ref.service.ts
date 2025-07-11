import { Injectable } from '@angular/core';

function getWindow (): any {
    return window;
}

@Injectable({
    providedIn: 'root',
})
export class WindowRefService {
    get nativeWindow (): any {
        return getWindow();
    }

    get width (): number {
        const w = this.nativeWindow,
            d = w.document,
            e = d.documentElement,
            g = d.getElementsByTagName('body')[0];

        return w.innerWidth || e.clientWidth || g.clientWidth;
    }

    get height (): number {
        const w = this.nativeWindow,
            d = w.document,
            e = d.documentElement,
            g = d.getElementsByTagName('body')[0];

        return w.innerHeight || e.clientHeight || g.clientHeight;
    }

    get scrollTop (): number {
        const w = this.nativeWindow,
            d = w.document,
            e = d.documentElement,
            g = d.getElementsByTagName('body')[0];

        return w.pageYOffset || e.scrollTop || g.scrollTop;
    }

    canvas: HTMLCanvasElement = this.nativeWindow.document.createElement('canvas');

    scrollTo(x: number, y: number): void {
        this.nativeWindow.scrollTo(x, y);
    }

    scrollToTop(duration: number): void {
        const cosParameter = this.nativeWindow.scrollY / 2;
        let scrollCount = 0,
            oldTimestamp = this.nativeWindow.performance.now();

        const stepFn = (newTimestamp: number) => {
            scrollCount += Math.PI / (duration / (newTimestamp - oldTimestamp));

            if (scrollCount >= Math.PI)
                this.nativeWindow.scrollTo(0, 0);

            if (this.nativeWindow.scrollY === 0)
                return;

            this.nativeWindow.scrollTo(0, Math.round(cosParameter + cosParameter * Math.cos(scrollCount)));
            oldTimestamp = newTimestamp;
            this.nativeWindow.requestAnimationFrame(stepFn);
        };

        this.nativeWindow.requestAnimationFrame(stepFn);
    }

    scrollElementIntoView(elementId: string, smooth = false): void {
        const d = this.nativeWindow.document,
            element = d.querySelector(`#${elementId}`),
            smoothOpts = { block: 'start', behavior: 'smooth' };

        if (element) {
            element.scrollIntoView(smooth ? smoothOpts : {});
        }
    }

    scrollToElementCenter(elementId: string, smooth = false): void {
        const d = this.nativeWindow.document,
            element = d.querySelector(`#${elementId}`),
            smoothOpts = { block: 'center', behavior: 'smooth' };

        if (element)
            element.scrollIntoView(smooth ? smoothOpts : {block: 'center'});
    }

    appendAndEvaluateContentToElement(content: string, element: any): void {
        try {
            const fragment = this.parsePartialHtml(content);
            this.executeScripts(fragment);

            if (element) {
                element.appendChild(fragment);
            }
        } catch (ex) {
            console.error('appendContentToDocument failed', ex);
        }
    }

    focusElementById(elemId: string, delay = 0): void {
        const d = this.nativeWindow.document,
            e = d.getElementById(elemId);

        if (e) {
            setTimeout(() => e.focus(), delay);
        }
    }

    private executeScripts(element: any): void {
        const w = this.nativeWindow,
            d = w.document,
            scripts = element.querySelectorAll('script');

        scripts.forEach((script: any) => {
            const fixedScript = d.createElement('script');
            fixedScript.type = script.type;

            if (script.innerHTML) {
                fixedScript.innerHTML = script.innerHTML;
            } else {
                fixedScript.src = script.src;
            }

            fixedScript.async = false;
            script.parentNode.replaceChild(fixedScript, script);
        });
    }

    private parsePartialHtml(html: string): void {
        const w = this.nativeWindow,
            d = w.document,
            doc = new DOMParser().parseFromString(html, 'text/html'),
            headNodes = doc.head.childNodes,
            bodyNodes = doc.body.childNodes,
            fragment = d.createDocumentFragment();

        while (headNodes.length) {
            fragment.appendChild(headNodes[0]);
        }

        while (bodyNodes.length) {
            fragment.appendChild(bodyNodes[0]);
        }

        return fragment;
    }
}

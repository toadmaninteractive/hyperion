import { Injectable } from '@angular/core';
import { IndividualConfig, ToastrService } from 'ngx-toastr';
import * as DataProtocol from '../../protocol/data-protocol.data';

export enum NotificationType {
    Success = 1,
    Info = 2,
    Warning = 3,
    Error = 4
}

@Injectable({
    providedIn: 'root',
})
export class NotificationService {
    constructor(private toastr: ToastrService) { }

    private defaultOptions(): Partial<IndividualConfig> {
        return {
            closeButton: true,
            positionClass: 'toast-top-right',
            progressAnimation: 'increasing',
            progressBar: true,
            timeOut: 5000,
        };
    }

    error(message: string | DataProtocol.BadRequestError<any> | DataProtocol.ForbiddenError | DataProtocol.NotFoundError | DataProtocol.InternalServerError): void {
        let actualMessage = typeof message === 'string' ? message : JSON.stringify(message);

        if (message instanceof DataProtocol.BadRequestError) {
            actualMessage = `Server cannot process your request. Reason: ${message.error}`;
        } else if (message instanceof DataProtocol.ForbiddenError) {
            actualMessage = `You are not authorized to perform requested action`;
        } else if (message instanceof DataProtocol.NotFoundError) {
            actualMessage = `Requested object does not exist on the server side`;
        } else if (message instanceof DataProtocol.InternalServerError) {
            actualMessage = `Internal server error. Reason: ${message.error}`;
        }

        setTimeout(() => this.toastr.error(actualMessage, null, this.defaultOptions()));
    }

    warning(message: string): void {
        setTimeout(() => this.toastr.warning(message, null, this.defaultOptions()));
    }

    info(message: string, title?: string): void {
        setTimeout(() => this.toastr.info(message ? message : null, title ? title : null, this.defaultOptions()));
    }

    success(message?: string, title?: string): void {
        setTimeout(() => this.toastr.success(message ? message : null, title ? title : null, this.defaultOptions()));
    }
}

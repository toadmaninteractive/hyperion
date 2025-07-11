import { Injectable } from '@angular/core';
import { HttpClient, HttpRequest } from '@angular/common/http';
import { Observable } from 'rxjs';
import * as DbProtocol from '../../protocol/db-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class UploadService {
    private readonly attachmentsPrefix = '/upload/attachments';

    constructor(private httpClient: HttpClient) { }

    public uploadAttachment(file: File, owner: DbProtocol.AttachmentOwner, linkedId: number): Observable<Object> {
        const formData = new FormData();
        formData.append('attachment', file, file.name);

        const url = `${this.attachmentsPrefix}/${DbProtocol.AttachmentOwner.toJsonKey(owner)}/${linkedId}`,
            req = new HttpRequest('POST', url, formData, { reportProgress: true });

        return this.httpClient.request(req);
    }
}

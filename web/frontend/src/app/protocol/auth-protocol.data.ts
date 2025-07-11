// Author: Igor compiler
// Compiler version: igorc 2.1.1
// DO NOT EDIT THIS FILE - it is machine generated

import * as Igor from './igor';
import * as DataProtocol from './data-protocol.data';

export class PersonnelStatusResponse {
    loggedIn!: boolean;
    userId?: number | null = null;
    email?: string | null = null;
    username?: string | null = null;

    static fromJson(json: Igor.Json.JsonValue): PersonnelStatusResponse {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new PersonnelStatusResponse();
        obj.loggedIn = jsonObject['logged_in'] as boolean;
        obj.userId = ('user_id' in jsonObject && jsonObject['user_id'] != null) ? jsonObject['user_id'] as number : null;
        obj.email = ('email' in jsonObject && jsonObject['email'] != null) ? jsonObject['email'] as string : null;
        obj.username = ('username' in jsonObject && jsonObject['username'] != null) ? jsonObject['username'] as string : null;
        return obj;
    }

    static toJson(value: PersonnelStatusResponse): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['logged_in'] = value.loggedIn;
        if (value.userId != null) result['user_id'] = value.userId;
        if (value.email != null) result['email'] = value.email;
        if (value.username != null) result['username'] = value.username;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return PersonnelStatusResponse.toJson(this);
    }
}

export class PersonnelLoginRequest {
    username!: string;
    password!: string;

    static fromJson(json: Igor.Json.JsonValue): PersonnelLoginRequest {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new PersonnelLoginRequest();
        obj.username = jsonObject['username'] as string;
        obj.password = jsonObject['password'] as string;
        return obj;
    }

    static toJson(value: PersonnelLoginRequest): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['username'] = value.username;
        result['password'] = value.password;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return PersonnelLoginRequest.toJson(this);
    }
}

export enum PersonnelLoginError {
    Failure = 1,
    AlreadyLoggedIn = 2,
    AccountNotExists = 3,
    AccountIsBlocked = 4,
    AccountIsDeleted = 5,
    InvalidPassword = 6,
}

export namespace PersonnelLoginError {
    export function toJson(value: PersonnelLoginError): Igor.Json.JsonValue {
        return toJsonKey(value);
    }

    export function fromJson(json: Igor.Json.JsonValue): PersonnelLoginError {
        return fromJsonKey(json);
    }

    export function toJsonKey(value: PersonnelLoginError): Igor.Json.JsonValue {
        switch (value) {
            case PersonnelLoginError.Failure: return 'failure';
            case PersonnelLoginError.AlreadyLoggedIn: return 'already_logged_in';
            case PersonnelLoginError.AccountNotExists: return 'account_not_exists';
            case PersonnelLoginError.AccountIsBlocked: return 'account_is_blocked';
            case PersonnelLoginError.AccountIsDeleted: return 'account_is_deleted';
            case PersonnelLoginError.InvalidPassword: return 'invalid_password';
            default: throw new Error(`Invalid PersonnelLoginError value: ${value}`);
        }
    }

    export function fromJsonKey(json: Igor.Json.JsonValue): PersonnelLoginError {
        switch (json) {
            case 'failure': return PersonnelLoginError.Failure;
            case 'already_logged_in': return PersonnelLoginError.AlreadyLoggedIn;
            case 'account_not_exists': return PersonnelLoginError.AccountNotExists;
            case 'account_is_blocked': return PersonnelLoginError.AccountIsBlocked;
            case 'account_is_deleted': return PersonnelLoginError.AccountIsDeleted;
            case 'invalid_password': return PersonnelLoginError.InvalidPassword;
            default: throw new Error(`Invalid PersonnelLoginError value: ${json}`);
        }
    }
}

export class PersonnelLoginResponse implements DataProtocol.IResult {
    result!: boolean;
    error?: PersonnelLoginError | null = null;
    sessionId?: string | null = null;
    userId?: number | null = null;
    email?: string | null = null;
    username?: string | null = null;

    static fromJson(json: Igor.Json.JsonValue): PersonnelLoginResponse {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new PersonnelLoginResponse();
        obj.result = jsonObject['result'] as boolean;
        obj.error = ('error' in jsonObject && jsonObject['error'] != null) ? PersonnelLoginError.fromJson(jsonObject['error']) : null;
        obj.sessionId = ('session_id' in jsonObject && jsonObject['session_id'] != null) ? jsonObject['session_id'] as string : null;
        obj.userId = ('user_id' in jsonObject && jsonObject['user_id'] != null) ? jsonObject['user_id'] as number : null;
        obj.email = ('email' in jsonObject && jsonObject['email'] != null) ? jsonObject['email'] as string : null;
        obj.username = ('username' in jsonObject && jsonObject['username'] != null) ? jsonObject['username'] as string : null;
        return obj;
    }

    static toJson(value: PersonnelLoginResponse): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['result'] = value.result;
        if (value.error != null) result['error'] = PersonnelLoginError.toJson(value.error);
        if (value.sessionId != null) result['session_id'] = value.sessionId;
        if (value.userId != null) result['user_id'] = value.userId;
        if (value.email != null) result['email'] = value.email;
        if (value.username != null) result['username'] = value.username;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return PersonnelLoginResponse.toJson(this);
    }
}

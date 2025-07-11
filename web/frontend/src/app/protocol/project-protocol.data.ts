// Author: Igor compiler
// Compiler version: igorc 2.1.1
// DO NOT EDIT THIS FILE - it is machine generated

import * as Igor from './igor';

export class CreateProjectRequest {
    title!: string;
    key!: string;
    jiraId?: number | null = null;
    jiraKey?: string | null = null;
    slackReceivers?: string | null = null;

    static fromJson(json: Igor.Json.JsonValue): CreateProjectRequest {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new CreateProjectRequest();
        obj.title = jsonObject['title'] as string;
        obj.key = jsonObject['key'] as string;
        obj.jiraId = ('jira_id' in jsonObject && jsonObject['jira_id'] != null) ? jsonObject['jira_id'] as number : null;
        obj.jiraKey = ('jira_key' in jsonObject && jsonObject['jira_key'] != null) ? jsonObject['jira_key'] as string : null;
        obj.slackReceivers = ('slack_receivers' in jsonObject && jsonObject['slack_receivers'] != null) ? jsonObject['slack_receivers'] as string : null;
        return obj;
    }

    static toJson(value: CreateProjectRequest): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['title'] = value.title;
        result['key'] = value.key;
        if (value.jiraId != null) result['jira_id'] = value.jiraId;
        if (value.jiraKey != null) result['jira_key'] = value.jiraKey;
        if (value.slackReceivers != null) result['slack_receivers'] = value.slackReceivers;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return CreateProjectRequest.toJson(this);
    }
}

export class UpdateProjectRequest {
    title!: string;
    key!: string;
    jiraId?: number | null;
    jiraKey?: string | null;
    slackReceivers?: string | null;

    static fromJson(json: Igor.Json.JsonValue): UpdateProjectRequest {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new UpdateProjectRequest();
        if (jsonObject['title'] !== undefined) obj.title = jsonObject['title'] as string;
        if (jsonObject['key'] !== undefined) obj.key = jsonObject['key'] as string;
        if (jsonObject['jira_id'] !== undefined) obj.jiraId = jsonObject['jira_id'] as number;
        if (jsonObject['jira_key'] !== undefined) obj.jiraKey = jsonObject['jira_key'] as string;
        if (jsonObject['slack_receivers'] !== undefined) obj.slackReceivers = jsonObject['slack_receivers'] as string;
        return obj;
    }

    static toJson(value: UpdateProjectRequest): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        if (value.title !== undefined) result['title'] = value.title;
        if (value.key !== undefined) result['key'] = value.key;
        if (value.jiraId !== undefined) result['jira_id'] = value.jiraId;
        if (value.jiraKey !== undefined) result['jira_key'] = value.jiraKey;
        if (value.slackReceivers !== undefined) result['slack_receivers'] = value.slackReceivers;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return UpdateProjectRequest.toJson(this);
    }
}

export enum ProjectError {
    InvalidTitle = 1,
    TitleAlreadyExists = 2,
    InvalidKey = 3,
    KeyAlreadyExists = 4,
    InvalidSlackReceivers = 5,
    OwnerNotExists = 6,
    JiraNotExists = 7,
}

export namespace ProjectError {
    export function toJson(value: ProjectError): Igor.Json.JsonValue {
        return toJsonKey(value);
    }

    export function fromJson(json: Igor.Json.JsonValue): ProjectError {
        return fromJsonKey(json);
    }

    export function toJsonKey(value: ProjectError): Igor.Json.JsonValue {
        switch (value) {
            case ProjectError.InvalidTitle: return 'invalid_title';
            case ProjectError.TitleAlreadyExists: return 'title_already_exists';
            case ProjectError.InvalidKey: return 'invalid_key';
            case ProjectError.KeyAlreadyExists: return 'key_already_exists';
            case ProjectError.InvalidSlackReceivers: return 'invalid_slack_receivers';
            case ProjectError.OwnerNotExists: return 'owner_not_exists';
            case ProjectError.JiraNotExists: return 'jira_not_exists';
            default: throw new Error(`Invalid ProjectError value: ${value}`);
        }
    }

    export function fromJsonKey(json: Igor.Json.JsonValue): ProjectError {
        switch (json) {
            case 'invalid_title': return ProjectError.InvalidTitle;
            case 'title_already_exists': return ProjectError.TitleAlreadyExists;
            case 'invalid_key': return ProjectError.InvalidKey;
            case 'key_already_exists': return ProjectError.KeyAlreadyExists;
            case 'invalid_slack_receivers': return ProjectError.InvalidSlackReceivers;
            case 'owner_not_exists': return ProjectError.OwnerNotExists;
            case 'jira_not_exists': return ProjectError.JiraNotExists;
            default: throw new Error(`Invalid ProjectError value: ${json}`);
        }
    }
}

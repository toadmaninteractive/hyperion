-module(jira).

%% Include files

-include("jira_protocol.hrl").

%% Exported functions

-export([
    get_user/3,
    create_issue/6
]).

%% API

-spec get_user(BaseUrl, AuthToken, Username) -> Result when
    BaseUrl :: binary(),
    AuthToken :: binary(),
    Username :: binary(),
    Result :: jira_protocol:get_user_response().

get_user(BaseUrl, AuthToken, Username) ->
    Authorization = <<"Basic ", AuthToken/binary>>,
    jira_api:get_user(BaseUrl, Username, Authorization).

-spec create_issue(BaseUrl, AuthToken, ProjectKey, IssueType, Summary, Description) -> Result when
    BaseUrl :: binary(),
    AuthToken :: binary(),
    ProjectKey :: binary(),
    IssueType :: jira_protocol:issue_type(),
    Summary :: binary(),
    Description :: binary(),
    Result :: jira_protocol:create_issue_response().

create_issue(BaseUrl, AuthToken, ProjectKey, IssueType, Summary, Description) ->
    Authorization = <<"Basic ", AuthToken/binary>>,
    Fields = #create_issue_request{
        project = #key_object{key = ProjectKey},
        summary = Summary,
        description = Description,
        issuetype = #name_object{name = IssueType}
    },
    jira_api:create_issue(BaseUrl, #fields{fields = Fields}, Authorization).

%% Local functions

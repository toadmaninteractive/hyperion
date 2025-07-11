-- Setup SQL
-- Revision: 27

-------------------
-- Custom types  --
-------------------

CREATE TYPE actor_t AS ENUM (
    'root',
    'personnel',
    'robot',
    'anonymous'
);

CREATE TYPE entity_t AS ENUM (
    'settings',
    'personnel'
);

CREATE TYPE operation_t AS ENUM (
    'create',
    'read',
    'update',
    'delete',
    'undelete',
    'block',
    'unblock',
    'login',
    'logout',
    'grant',
    'revoke'
);

CREATE TYPE test_run_status_t AS ENUM (
    'draft',
    'in_progress',
    'closed'
);

CREATE TYPE test_status_t AS ENUM (
    'pending',
    'in_progress',
    'passed',
    'failed',
    'blocked'
);

CREATE TYPE attachment_owner_t AS ENUM (
    'setup_step',
    'test_case',
    'test_run',
    'test_run_item'
);

CREATE TYPE spec_t AS ENUM (
    'value',
    'source',
    'any',
    'random'
);

CREATE TYPE role_t AS ENUM (
    'consumer',
    'maintainer',
    'admin'
);

------------------------
-- Tables and indexes --
------------------------

-- Properties
CREATE TABLE props
(
    name varchar PRIMARY KEY CHECK (TRIM(name) <> ''),
    value varchar
) WITHOUT OIDS;

-- Settings
CREATE TABLE settings
(
    param varchar PRIMARY KEY CHECK (TRIM(param) <> ''),
    type varchar NOT NULL CHECK (TRIM(type) <> ''),
    value text DEFAULT NULL
) WITHOUT OIDS;

INSERT INTO settings ("param", "type", "value") VALUES 
    ('personnel_session_duration', 'int', '31536000');

-- Logs
CREATE TABLE logs
(
    id bigserial PRIMARY KEY,
    actor actor_t NOT NULL,
    actor_id bigint DEFAULT NULL,
    entity entity_t NOT NULL,
    entity_id bigint DEFAULT NULL,
    operation operation_t NOT NULL,
    properties jsonb DEFAULT '{}',
    result boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE INDEX log_actor_index ON logs (actor);
CREATE INDEX log_actor_id_index ON logs (actor_id);
CREATE INDEX log_entity_index ON logs (entity);
CREATE INDEX log_entity_id_index ON logs (entity_id);
CREATE INDEX log_operation_index ON logs (operation);
CREATE INDEX log_result_index ON logs (result);
CREATE INDEX log_created_at_index ON logs (created_at);

-- Personnel accounts
CREATE TABLE personnel
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    username varchar UNIQUE NOT NULL CHECK (TRIM(username) <> ''),
    name varchar DEFAULT NULL,
    email varchar DEFAULT NULL CHECK (TRIM(email) <> ''),
    phone varchar DEFAULT NULL CHECK (TRIM(phone) <> ''),
    is_blocked boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX per_username_ult_index ON personnel (LOWER(TRIM(username)));
CREATE UNIQUE INDEX per_email_ult_index ON personnel (LOWER(TRIM(email)));
CREATE UNIQUE INDEX per_phone_ult_index ON personnel (LOWER(TRIM(phone)));
CREATE INDEX per_is_blocked_index ON personnel (is_blocked);
CREATE INDEX per_is_deleted_index ON personnel (is_deleted);
CREATE INDEX per_created_at_index ON personnel (created_at);
CREATE INDEX per_updated_at_index ON personnel (updated_at);

-- Personnel sessions
CREATE TABLE personnel_sessions
(
    id varchar PRIMARY KEY CHECK (TRIM(id) <> ''),
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    valid_thru timestamptz NOT NULL
) WITHOUT OIDS;

CREATE INDEX pers_personnel_id_index ON personnel_sessions (personnel_id);
CREATE INDEX pers_created_at_index ON personnel_sessions (created_at);
CREATE INDEX pers_valid_thru_index ON personnel_sessions (valid_thru);

-- Personnel groups
CREATE TABLE personnel_groups
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    name varchar UNIQUE NOT NULL CHECK (TRIM(name) <> ''),
    description varchar DEFAULT NULL,
    is_superadmin boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX perg_name_ult_index ON personnel_groups (LOWER(TRIM(name)));
CREATE INDEX perg_is_superadmin_index ON personnel_groups (is_superadmin);
CREATE INDEX perg_is_deleted_index ON personnel_groups (is_deleted);
CREATE INDEX perg_created_at_index ON personnel_groups (created_at);
CREATE INDEX perg_updated_at_index ON personnel_groups (updated_at);

-- Personnel group membership
CREATE TABLE personnel_group_membership
(
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    group_id bigint NOT NULL REFERENCES personnel_groups (id),
    PRIMARY KEY (personnel_id, group_id)
) WITHOUT OIDS;

CREATE INDEX pergm_personnel_id_index ON personnel_group_membership (personnel_id);
CREATE INDEX pergm_group_id_index ON personnel_group_membership (group_id);

-- JIRA instances
CREATE TABLE jira_instances
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    title varchar UNIQUE NOT NULL CHECK (TRIM(title) <> ''),
    url varchar NOT NULL CHECK (TRIM(url) <> ''),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX ji_title_ult_index ON jira_instances (LOWER(TRIM(title)));
CREATE INDEX ji_created_at_index ON jira_instances (created_at);
CREATE INDEX ji_updated_at_index ON jira_instances (updated_at);

-- JIRA authentications
CREATE TABLE jira_auth
(
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    jira_id bigint DEFAULT NULL REFERENCES jira_instances (id),
    username varchar NOT NULL CHECK (TRIM(username) <> ''),
    auth_token varchar NOT NULL CHECK (TRIM(auth_token) <> ''),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp,
    PRIMARY KEY (personnel_id, jira_id)
) WITHOUT OIDS;

CREATE INDEX ja_personnel_id_index ON jira_auth (personnel_id);
CREATE INDEX ja_jira_id_index ON jira_auth (jira_id);
CREATE INDEX ja_created_at_index ON jira_auth (created_at);
CREATE INDEX ja_updated_at_index ON jira_auth (updated_at);

-- Projects
CREATE TABLE projects
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    title varchar UNIQUE NOT NULL CHECK (TRIM(title) <> ''),
    "key" varchar UNIQUE NOT NULL CHECK (TRIM("key") <> ''),
    jira_id bigint DEFAULT NULL REFERENCES jira_instances (id),
    jira_key varchar DEFAULT NULL,
    slack_receivers varchar CHECK (TRIM(slack_receivers) <> ''),
    owner_id bigint NOT NULL REFERENCES personnel (id),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX proj_title_ult_index ON projects (LOWER(TRIM(title)));
CREATE UNIQUE INDEX proj_key_ult_index ON projects (LOWER(TRIM("key")));
CREATE INDEX proj_jira_id_index ON projects (jira_id);
CREATE INDEX proj_owner_id_index ON projects (owner_id);
CREATE INDEX proj_created_at_index ON projects (created_at);
CREATE INDEX proj_updated_at_index ON projects (updated_at);

-- Personnel roles
CREATE TABLE personnel_roles
(
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    project_id bigint NOT NULL REFERENCES projects (id),
    role role_t NOT NULL,
    PRIMARY KEY (personnel_id, project_id)
) WITHOUT OIDS;

CREATE INDEX perr_personnel_id_index ON personnel_roles (personnel_id);
CREATE INDEX perr_project_id_index ON personnel_roles (project_id);
CREATE INDEX perr_role_index ON personnel_roles (role);

-- Personnel group roles
CREATE TABLE personnel_group_roles
(
    group_id bigint NOT NULL REFERENCES personnel_groups (id),
    project_id bigint NOT NULL REFERENCES projects (id),
    role role_t NOT NULL,
    PRIMARY KEY (group_id, project_id)
) WITHOUT OIDS;

CREATE INDEX pergr_group_id_index ON personnel_group_roles (group_id);
CREATE INDEX pergr_project_id_index ON personnel_group_roles (project_id);
CREATE INDEX pergr_role_index ON personnel_group_roles (role);

-- Setup steps
CREATE TABLE setup_steps
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    parent_id bigint DEFAULT NULL REFERENCES setup_steps (id),
    project_id bigint NOT NULL REFERENCES projects (id),
    title varchar DEFAULT NULL,
    description varchar DEFAULT NULL,
    is_draft boolean NOT NULL DEFAULT FALSE,
    order_num integer NOT NULL DEFAULT 1000,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX sets_project_step_title_ult_index ON setup_steps (project_id, parent_id, LOWER(TRIM(title)));
CREATE UNIQUE INDEX sets_project_step_title_null_parent_index ON setup_steps (project_id, LOWER(TRIM(title))) WHERE parent_id IS NULL;
CREATE UNIQUE INDEX sets_project_step_title_null_title_index ON setup_steps (project_id, parent_id) WHERE title IS NULL;
CREATE INDEX sets_parent_id_index ON setup_steps (parent_id);
CREATE INDEX sets_project_id_index ON setup_steps (project_id);
CREATE INDEX sets_is_draft_index ON setup_steps (is_draft);
CREATE INDEX sets_order_num_index ON setup_steps (order_num);
CREATE INDEX sets_created_at_index ON setup_steps (created_at);
CREATE INDEX sets_updated_at_index ON setup_steps (updated_at);

-- Test cases
CREATE TABLE test_cases
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    parent_id bigint DEFAULT NULL REFERENCES test_cases (id),
    project_id bigint NOT NULL REFERENCES projects (id),
    precondition_id bigint DEFAULT NULL REFERENCES setup_steps (id),
    setup_id bigint DEFAULT NULL REFERENCES setup_steps (id),
    is_group boolean NOT NULL,
    title varchar DEFAULT NULL,
    description varchar DEFAULT NULL,
    test_steps varchar NOT NULL,
    expected_result varchar NOT NULL,
    is_draft boolean NOT NULL DEFAULT FALSE,
    order_num integer NOT NULL DEFAULT 1000,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX tc_project_case_title_ult_index ON test_cases (project_id, parent_id, LOWER(TRIM(title)));
CREATE UNIQUE INDEX tc_project_case_title_null_parent_index ON test_cases (project_id, LOWER(TRIM(title))) WHERE parent_id IS NULL;
CREATE UNIQUE INDEX tc_project_case_title_null_title_index ON test_cases (project_id, parent_id) WHERE title IS NULL;
CREATE INDEX tc_parent_id_index ON test_cases (parent_id);
CREATE INDEX tc_project_id_index ON test_cases (project_id);
CREATE INDEX tc_precondition_id_index ON test_cases (precondition_id);
CREATE INDEX tc_setup_id_index ON test_cases (setup_id);
CREATE INDEX tc_is_group_index ON test_cases (is_group);
CREATE INDEX tc_is_draft_index ON test_cases (is_draft);
CREATE INDEX tc_order_num_index ON test_cases (order_num);
CREATE INDEX tc_created_at_index ON test_cases (created_at);
CREATE INDEX tc_updated_at_index ON test_cases (updated_at);

-- Tags
CREATE TABLE tags
(
    id bigserial PRIMARY KEY,
    title varchar UNIQUE NOT NULL CHECK (TRIM(title) <> '')
) WITHOUT OIDS;

CREATE UNIQUE INDEX tag_title_ult_index ON tags (LOWER(TRIM(title)));

-- Project tag links
CREATE TABLE project_tag_links
(
    tag_id bigint NOT NULL REFERENCES tags (id),
    project_id bigint NOT NULL REFERENCES projects (id),
    PRIMARY KEY (tag_id, project_id)
) WITHOUT OIDS;

-- Setup tag links
CREATE TABLE setup_tag_links
(
    tag_id bigint NOT NULL REFERENCES tags (id),
    setup_id bigint NOT NULL REFERENCES setup_steps (id),
    PRIMARY KEY (tag_id, setup_id)
) WITHOUT OIDS;

-- Test case tag links
CREATE TABLE test_case_tag_links
(
    tag_id bigint NOT NULL REFERENCES tags (id),
    case_id bigint NOT NULL REFERENCES test_cases (id),
    PRIMARY KEY (tag_id, case_id)
) WITHOUT OIDS;

-- Test runs
CREATE TABLE test_runs
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    project_id bigint NOT NULL REFERENCES projects (id),
    title varchar DEFAULT NULL,
    status test_run_status_t NOT NULL DEFAULT 'draft',
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp,
    started_at timestamptz DEFAULT NULL,
    finished_at timestamptz DEFAULT NULL
) WITHOUT OIDS;

CREATE INDEX tr_project_id_index ON test_runs (project_id);
CREATE UNIQUE INDEX tr_project_title_index ON test_runs (project_id, LOWER(TRIM(title)));
CREATE INDEX tr_status_index ON test_runs (status);
CREATE INDEX tr_created_at_index ON test_runs (created_at);
CREATE INDEX tr_updated_at_index ON test_runs (updated_at);
CREATE INDEX tr_started_at_index ON test_runs (started_at);
CREATE INDEX tr_finished_at_index ON test_runs (finished_at);

-- Test run items
CREATE TABLE test_run_items
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    run_id bigint NOT NULL REFERENCES test_runs (id),
    case_id bigint NOT NULL REFERENCES test_cases (id),
    reporter_id bigint NOT NULL REFERENCES personnel (id),
    assignee_id bigint DEFAULT NULL REFERENCES personnel (id),
    status test_status_t NOT NULL DEFAULT 'pending',
    summary varchar DEFAULT NULL,
    failed_setup_id bigint DEFAULT NULL REFERENCES setup_steps (id),
    order_num integer NOT NULL DEFAULT 1000,
    params jsonb DEFAULT '{}',
    jira_issue_key varchar DEFAULT NULL,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp,
    started_at timestamptz DEFAULT NULL,
    finished_at timestamptz DEFAULT NULL
) WITHOUT OIDS;

CREATE INDEX tri_run_id_index ON test_run_items (run_id);
CREATE INDEX tri_case_id_index ON test_run_items (case_id);
CREATE INDEX tri_reporter_id_index ON test_run_items (reporter_id);
CREATE INDEX tri_assignee_id_index ON test_run_items (assignee_id);
CREATE INDEX tri_status_index ON test_run_items (status);
CREATE INDEX tri_order_num_index ON test_run_items (order_num);
CREATE INDEX tri_created_at_index ON test_run_items (created_at);
CREATE INDEX tri_updated_at_index ON test_run_items (updated_at);
CREATE INDEX tri_started_at_index ON test_run_items (started_at);
CREATE INDEX tri_finished_at_index ON test_run_items (finished_at);

-- File attachments
CREATE TABLE file_attachments
(
    id bigserial PRIMARY KEY,
    file_size bigint NOT NULL CHECK (file_size >= 0),
    file_sha varchar NOT NULL CHECK (TRIM(file_sha) <> ''),
    filename varchar NOT NULL CHECK (TRIM(filename) <> ''),
    thumb_filename varchar,
    original_filename varchar,
    content_type varchar,
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    created_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX fa_file_hash_index ON file_attachments (file_size, file_sha);
CREATE INDEX fa_personnel_id_index ON file_attachments (personnel_id);
CREATE INDEX fa_created_at_index ON file_attachments (created_at);

-- File attachment links
CREATE TABLE file_attachment_links
(
    attachment_id bigint NOT NULL REFERENCES file_attachments (id),
    owner attachment_owner_t NOT NULL,
    linked_id bigint NOT NULL CHECK (linked_id > 0),
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    PRIMARY KEY (attachment_id, owner, linked_id)
) WITHOUT OIDS;

CREATE INDEX fal_personnel_id_index ON file_attachment_links (personnel_id);
CREATE INDEX fal_created_at_index ON file_attachment_links (created_at);

-- Parameter sources
CREATE TABLE parameter_sources
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    project_id bigint NOT NULL REFERENCES projects (id),
    title varchar NOT NULL CHECK (TRIM(title) <> ''),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX pars_project_title_ult_index ON parameter_sources (project_id, LOWER(TRIM(title)));
CREATE INDEX pars_project_id_index ON parameter_sources (project_id);
CREATE INDEX pars_created_at_index ON parameter_sources (created_at);
CREATE INDEX pars_updated_at_index ON parameter_sources (updated_at);

-- Parameter source values
CREATE TABLE parameter_source_values
(
    id bigserial PRIMARY KEY,
    source_id bigint NOT NULL REFERENCES parameter_sources (id),
    value varchar NOT NULL
) WITHOUT OIDS;

CREATE UNIQUE INDEX parsv_unique_value_ult_index ON parameter_source_values (source_id, LOWER(TRIM("value")));
CREATE INDEX parsv_source_id_index ON parameter_source_values (source_id);

-- Parameters
CREATE TABLE parameters
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    project_id bigint NOT NULL REFERENCES projects (id),
    parent_id bigint DEFAULT NULL REFERENCES parameters (id),
    dependent_id bigint DEFAULT NULL REFERENCES parameters (id),
    title varchar NOT NULL CHECK (TRIM(title) <> ''),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX parm_project_title_ult_index ON parameters (project_id, LOWER(TRIM(title)));
CREATE INDEX parm_project_id_index ON parameters (project_id);
CREATE INDEX parm_parent_id_index ON parameters (parent_id);
CREATE INDEX parm_dependent_id_index ON parameters (dependent_id);
CREATE INDEX parm_created_at_index ON parameters (created_at);
CREATE INDEX parm_updated_at_index ON parameters (updated_at);

-- Parameter links
CREATE TABLE parameter_links
(
    parameter_id bigint PRIMARY KEY REFERENCES parameters (id),
    source_id bigint NOT NULL REFERENCES parameter_sources (id)
) WITHOUT OIDS;

CREATE INDEX parl_source_id_index ON parameter_links (source_id);

-- Parameter value links
CREATE TABLE parameter_value_links
(
    parameter_id bigint NOT NULL REFERENCES parameters (id),
    dependent_value_id bigint DEFAULT NULL REFERENCES parameter_source_values (id),
    value_id bigint NOT NULL REFERENCES parameter_source_values (id)
) WITHOUT OIDS;

CREATE UNIQUE INDEX parvl_dependent_value_index ON parameter_value_links (parameter_id, dependent_value_id, value_id);
CREATE UNIQUE INDEX parvl_independent_value_index ON parameter_value_links (parameter_id, value_id) WHERE dependent_value_id IS NULL;

-- Parameter setup links
CREATE TABLE parameter_setup_links
(
    setup_id bigint PRIMARY KEY REFERENCES setup_steps (id),
    parameter_id bigint NOT NULL REFERENCES parameters (id)
) WITHOUT OIDS;

CREATE INDEX parsl_parameter_id_index ON parameter_setup_links (parameter_id);

-- Parameter setup specialization
CREATE TABLE parameter_setup_specialization
(
    case_id bigint NOT NULL REFERENCES test_cases (id),
    setup_id bigint NOT NULL REFERENCES setup_steps (id),
    spec_type spec_t NOT NULL,
    value_id bigint DEFAULT NULL REFERENCES parameter_source_values (id),
    parameter_id bigint DEFAULT NULL REFERENCES parameters (id),
    PRIMARY KEY (case_id, setup_id)
) WITHOUT OIDS;

CREATE INDEX parss_case_id_index ON parameter_setup_specialization (case_id);
CREATE INDEX parss_setup_id_index ON parameter_setup_specialization (setup_id);
CREATE INDEX parss_value_id_index ON parameter_setup_specialization (value_id);
CREATE INDEX parss_parameter_id_index ON parameter_setup_specialization (parameter_id);

--------------------
-- SQL Functions  --
--------------------

-- Deactivate personnel account
CREATE OR REPLACE FUNCTION deactivate_personnel_account(_personnel_id bigint) RETURNS text AS $$
DECLARE
    p_personnel personnel;
BEGIN
    -- Get personnel account and check it
    SELECT * INTO p_personnel FROM personnel WHERE id = _personnel_id;
    IF (p_personnel.id IS NULL) THEN RETURN 'account_not_exists'; END IF;

   -- Delete account sessions
    DELETE FROM personnel_sessions WHERE personnel_id = _personnel_id;

    -- Mark account as deleted
    UPDATE personnel
    SET is_deleted = TRUE, updated_at = current_timestamp
    WHERE id = _personnel_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Set superadmin personnel group
CREATE OR REPLACE FUNCTION set_superadmin_personnel_group(_group_name text) RETURNS text AS $$
BEGIN
    -- Unset previous superadmin personnel groups
    UPDATE personnel_groups
    SET is_superadmin = FALSE, updated_at = current_timestamp
    WHERE LOWER(TRIM(name)) <> LOWER(TRIM(_group_name)) AND is_superadmin;
    
    -- Set current superadmin personnel group
    UPDATE personnel_groups
    SET is_superadmin = TRUE, updated_at = current_timestamp
    WHERE LOWER(TRIM(name)) = LOWER(TRIM(_group_name)) AND NOT is_superadmin;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Check if project is accessible by personnel account
CREATE OR REPLACE FUNCTION is_project_accessible_by_personnel(_project_id bigint, _personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_accessible boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND project_id = _project_id;

    IF (p_accessible) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.project_id = _project_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IS NOT NULL);

    RETURN p_accessible;
END;
$$ LANGUAGE plpgsql STABLE;

-- Check if personnel account is allowed to manage at least one project
CREATE OR REPLACE FUNCTION is_project_manager(_personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_managable boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND role IN ('maintainer', 'admin');

    IF (p_managable) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IN ('maintainer', 'admin'));

    RETURN p_managable;
END;
$$ LANGUAGE plpgsql STABLE;

-- Check if personnel account is allowed to manage a project
CREATE OR REPLACE FUNCTION is_project_manager(_project_id bigint, _personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_managable boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND project_id = _project_id AND role IN ('maintainer', 'admin');

    IF (p_managable) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.project_id = _project_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IN ('maintainer', 'admin'));

    RETURN p_managable;
END;
$$ LANGUAGE plpgsql STABLE;

-- Check if personnel account is superadmin
CREATE OR REPLACE FUNCTION is_superadmin(_personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_superadmin boolean;
BEGIN
    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_superadmin
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    WHERE pergm.personnel_id IS NOT NULL AND perg.is_superadmin;

    RETURN p_superadmin;
END;
$$ LANGUAGE plpgsql STABLE;

-- Get role numeric representation
CREATE OR REPLACE FUNCTION role_level(_role role_t) RETURNS integer AS $$
BEGIN
    CASE _role
        WHEN 'consumer' THEN RETURN 1;
        WHEN 'maintainer' THEN RETURN 2;
        WHEN 'admin' THEN RETURN 3;
        ELSE RETURN 0;
    END CASE;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Check if project access role is sufficient
CREATE OR REPLACE FUNCTION is_project_access_role_sufficient(_project_id bigint, _personnel_id bigint, _min_role role_t) RETURNS boolean AS $$
DECLARE
    p_accessible boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND project_id = _project_id AND role_level(role) >= role_level(_min_role);

    IF (p_accessible) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.project_id = _project_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR role_level(pergr.role) >= role_level(_min_role));

    RETURN p_accessible;
END;
$$ LANGUAGE plpgsql STABLE;

-- Create project parameter
CREATE OR REPLACE FUNCTION create_project_parameter(_project_id bigint, _parent_id bigint, _dependent_id bigint, _title text) RETURNS text AS $$
DECLARE
    p_exists boolean;
    p_param_id bigint;
    p_source_id bigint;
BEGIN
    -- Check if project exists
    SELECT COUNT(*) > 0 INTO p_exists FROM projects WHERE id = _project_id;
    IF (NOT p_exists) THEN RETURN 'project_not_exists'; END IF;

    -- Check if parameter already exists
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameters
    WHERE project_id = _project_id AND TRIM(LOWER(title)) = TRIM(LOWER(_title));

    IF (p_exists) THEN RETURN 'parameter_already_exists'; END IF;

    -- Check if parent parameter exists
    IF (_parent_id IS NOT NULL) THEN
        SELECT COUNT(*) > 0 INTO p_exists FROM parameters WHERE id = _parent_id;
        IF (NOT p_exists) THEN RETURN 'parent_not_exists'; END IF;
    END IF;

    -- Prevent to create top-level dependent parameter
    IF (_parent_id IS NULL AND _dependent_id IS NOT NULL) THEN RETURN 'parent_is_independent'; END IF;

    -- Check if parameter source already exists
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameter_sources
    WHERE project_id = _project_id AND TRIM(LOWER(title)) = TRIM(LOWER(_title));

    IF (p_exists) THEN RETURN 'parameter_source_already_exists'; END IF;

    -- Create new parameter
    INSERT INTO parameters ("project_id", "parent_id", "dependent_id", "title")
    VALUES (_project_id, _parent_id, _dependent_id, TRIM(_title))
    RETURNING id INTO p_param_id;

    -- Perform additional actions for top-level parameters
    IF (_parent_id IS NULL) THEN
        -- Create new parameter source
        INSERT INTO parameter_sources ("project_id", "title")
        VALUES (_project_id, TRIM(_title))
        RETURNING id INTO p_source_id;

        -- Create a link between a parameter source and a top-level parameter
        INSERT INTO parameter_links ("parameter_id", "source_id")
        VALUES (p_param_id, p_source_id);
    END IF;

    RETURN p_param_id::text;
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Remove project parameter
CREATE OR REPLACE FUNCTION remove_project_parameter(_param_id bigint) RETURNS text AS $$
DECLARE
    p_param parameters;
    p_param_link parameter_links;
    p_exists boolean;
    p_source_id bigint;
BEGIN
    -- Get project parameter
    SELECT * INTO p_param FROM parameters WHERE id = _param_id;
    IF (p_param.id IS NULL) THEN RETURN 'parameter_not_exists'; END IF;

    -- Check if parameter has children
    SELECT COUNT(*) > 0 INTO p_exists FROM parameters WHERE parent_id = _param_id;
    IF (p_exists) THEN RETURN 'has_children'; END IF;

    -- Check if parameter has dependants
    SELECT COUNT(*) > 0 INTO p_exists FROM parameters WHERE dependent_id = _param_id;
    IF (p_exists) THEN RETURN 'has_dependants'; END IF;

    -- Delete parameter setup links
    DELETE FROM parameter_setup_links WHERE parameter_id = _param_id;

    -- Delete parameter setup specialization
    DELETE FROM parameter_setup_specialization WHERE parameter_id = _param_id;

    -- Delete parameter value links
    DELETE FROM parameter_value_links WHERE parameter_id = _param_id;

    -- Check for top-level parameter
    IF (p_param.parent_id IS NULL) THEN
        -- Get linked parameter source
        SELECT source_id INTO p_source_id FROM parameter_links WHERE parameter_id = _param_id;
    
        -- Remove parameter source values
        DELETE FROM parameter_source_values WHERE source_id = p_source_id;
    
        -- Remove parameter link
        DELETE FROM parameter_links WHERE parameter_id = _param_id;
    
        -- Remove parameter source
        DELETE FROM parameter_sources WHERE id = p_source_id;
    END IF;

    -- Delete parameter itself
    DELETE FROM parameters WHERE id = _param_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Rename project parameter
CREATE OR REPLACE FUNCTION rename_project_parameter(_param_id bigint, _new_title text) RETURNS text AS $$
DECLARE
    p_param parameters;
    p_exists boolean;
    p_source_id bigint;
BEGIN
    -- Get project parameter
    SELECT * INTO p_param FROM parameters WHERE id = _param_id;
    IF (p_param.id IS NULL) THEN RETURN 'parameter_not_exists'; END IF;

    -- Check if parameter has children
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameters
    WHERE id <> _param_id AND project_id = p_param.project_id AND TRIM(LOWER(title)) = TRIM(LOWER(_new_title));

    IF (p_exists) THEN RETURN 'title_already_exists'; END IF;

    -- Get linked parameter source
    SELECT source_id INTO p_source_id FROM parameter_links WHERE parameter_id = _param_id;

    -- Rename parameter
    UPDATE parameters SET title = TRIM(_new_title), updated_at = NOW() WHERE id = _param_id;

    -- Rename parameter source
    UPDATE parameter_sources SET title = TRIM(_new_title), updated_at = NOW() WHERE id = p_source_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Add project parameter value
CREATE OR REPLACE FUNCTION add_project_parameter_value(_param_id bigint, _dependent_value text, _value text) RETURNS text AS $$
DECLARE
    p_param parameters;
    p_param_link parameter_links;
    p_exists boolean;
    p_value_id bigint;
    p_dependent_param parameters;
    p_dependent_link parameter_links;
    p_dependent_value_id bigint;
BEGIN
    -- Get project parameter
    SELECT * INTO p_param FROM parameters WHERE id = _param_id;
    IF (p_param.id IS NULL) THEN RETURN 'parameter_not_exists'; END IF;

    -- Check for top-level parameter
    IF (p_param.parent_id IS NULL) THEN
        -- Check linked parameter source
        SELECT * INTO p_param_link FROM parameter_links WHERE parameter_id = _param_id;
        IF (p_param_link.source_id IS NULL) THEN RETURN 'parameter_source_not_exists'; END IF;
   
        -- Check if supplied value already exists among parameter source values
        SELECT COUNT(*) > 0 INTO p_exists
        FROM parameter_source_values
        WHERE source_id = p_param_link.source_id AND TRIM(LOWER("value")) = TRIM(LOWER(_value));

        IF (p_exists) THEN RETURN 'value_already_exists'; END IF;

        -- Add new value to parameter source
        INSERT INTO parameter_source_values ("source_id", "value")
        VALUES (p_param_link.source_id, TRIM(_value))
        RETURNING id INTO p_value_id;

        -- Link created value to project parameter
        INSERT INTO parameter_value_links ("parameter_id", "value_id")
        VALUES (_param_id, p_value_id);
    ELSE
        -- Check if value is defined for parent parameter
        SELECT pvl.value_id INTO p_value_id
        FROM parameter_value_links AS pvl
        LEFT OUTER JOIN parameter_source_values AS psv ON (psv.id = pvl.value_id)
        WHERE pvl.parameter_id = p_param.parent_id AND TRIM(LOWER(psv.value)) = TRIM(LOWER(_value));

        IF (p_value_id IS NULL) THEN RETURN 'value_not_exists'; END IF;

        -- Check dependent project parameter
        IF (p_param.dependent_id IS NOT NULL) THEN
            -- Check if dependent project parameter exists
            SELECT * INTO p_dependent_param FROM parameters WHERE id = p_param.dependent_id;
            IF (p_dependent_param.id IS NULL) THEN RETURN 'dependent_parameter_not_exists'; END IF;

            -- Checked linked dependent project source
            SELECT * INTO p_dependent_link FROM parameter_links WHERE parameter_id = p_dependent_param.id;
            IF (p_dependent_link.source_id IS NULL) THEN RETURN 'dependent_source_not_exists'; END IF;

            -- Checked linked dependent project source value
            SELECT pvl.value_id INTO p_dependent_value_id
            FROM parameter_value_links AS pvl
            LEFT OUTER JOIN parameter_source_values AS psv ON (psv.id = pvl.value_id)
            WHERE pvl.parameter_id = p_dependent_param.id AND TRIM(LOWER(psv.value)) = TRIM(LOWER(_dependent_value));

            IF (p_dependent_value_id IS NULL) THEN RETURN 'dependent_value_not_exists'; END IF;

            -- Check if value already exists
            SELECT COUNT(*) > 0 INTO p_exists
            FROM parameter_value_links
            WHERE parameter_id = _param_id AND dependent_value_id = p_dependent_value_id AND value_id = p_value_id;

            IF (p_exists) THEN RETURN 'value_already_exists'; END IF;

            -- Link created value to project parameter
            INSERT INTO parameter_value_links ("parameter_id", "dependent_value_id", "value_id")
            VALUES (_param_id, p_dependent_value_id, p_value_id);
        ELSE
            -- Check if value already exists
            SELECT COUNT(*) > 0 INTO p_exists
            FROM parameter_value_links
            WHERE parameter_id = _param_id AND value_id = p_value_id;

            IF (p_exists) THEN RETURN 'value_already_exists'; END IF;

            -- Link created value to project parameter
            INSERT INTO parameter_value_links ("parameter_id", "value_id")
            VALUES (_param_id, p_value_id);
        END IF;
    END IF;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Remove project parameter value
CREATE OR REPLACE FUNCTION remove_project_parameter_value(_param_id bigint, _dependent_value text, _value text) RETURNS text AS $$
DECLARE
    p_param parameters;
    p_param_link parameter_links;
    p_exists boolean;
    p_value_id bigint;
    p_dependent_param parameters;
    p_dependent_link parameter_links;
    p_dependent_value_id bigint;
BEGIN
    -- Get project parameter
    SELECT * INTO p_param FROM parameters WHERE id = _param_id;
    IF (p_param.id IS NULL) THEN RETURN 'parameter_not_exists'; END IF;

    -- Get parameter value
    SELECT pvl.value_id INTO p_value_id
    FROM parameter_value_links AS pvl
    LEFT OUTER JOIN parameter_source_values AS psv ON (psv.id = pvl.value_id)
    WHERE pvl.parameter_id = _param_id AND TRIM(LOWER(psv.value)) = TRIM(LOWER(_value));

    IF (p_value_id IS NULL) THEN RETURN 'value_not_exists'; END IF;

    -- Check if child parameters are linked to the value
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameters AS p
    LEFT OUTER JOIN parameter_value_links AS pvl ON (pvl.parameter_id = p.id)
    WHERE p.parent_id = _param_id AND pvl.value_id = p_value_id;

    IF (p_exists) THEN RETURN 'linked_to_children'; END IF;

    -- Check if dependent parameters are linked to the value
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameter_value_links
    WHERE dependent_value_id = p_value_id;

    IF (p_exists) THEN RETURN 'linked_to_dependants'; END IF;

    -- Check dependent project parameter
    IF (p_param.dependent_id IS NOT NULL) THEN
        -- Check if dependent project parameter exists
        SELECT * INTO p_dependent_param FROM parameters WHERE id = p_param.dependent_id;
        IF (p_dependent_param.id IS NULL) THEN RETURN 'dependent_parameter_not_exists'; END IF;

        -- Checked linked dependent project source
        SELECT * INTO p_dependent_link FROM parameter_links WHERE parameter_id = p_dependent_param.id;
        IF (p_dependent_link.source_id IS NULL) THEN RETURN 'dependent_source_not_exists'; END IF;

        -- Checked linked dependent project source value
        SELECT pvl.value_id INTO p_dependent_value_id
        FROM parameter_value_links AS pvl
        LEFT OUTER JOIN parameter_source_values AS psv ON (psv.id = pvl.value_id)
        WHERE pvl.parameter_id = p_dependent_param.id AND TRIM(LOWER(psv.value)) = TRIM(LOWER(_dependent_value));

        IF (p_dependent_value_id IS NULL) THEN RETURN 'dependent_value_not_exists'; END IF;

        -- Remove parameter value link
        DELETE FROM parameter_value_links
        WHERE parameter_id = _param_id AND dependent_value_id = p_dependent_value_id AND value_id = p_value_id;
    ELSE
        -- Remove parameter value link
        DELETE FROM parameter_value_links
        WHERE parameter_id = _param_id AND value_id = p_value_id;
    END IF;

    -- Check for top-level parameter
    IF (p_param.parent_id IS NULL) THEN
        -- Remove parameter source value
        DELETE FROM parameter_source_values WHERE id = p_value_id;

        -- Delete parameter setup specialization
        DELETE FROM parameter_setup_specialization WHERE value_id = p_value_id;
    END IF;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Rename project parameter value
CREATE OR REPLACE FUNCTION rename_project_parameter_value(_param_id bigint, _old_value text, _new_value text) RETURNS text AS $$
DECLARE
    p_param parameters;
    p_exists boolean;
    p_source_id bigint;
    p_value_id bigint;
BEGIN
    -- Get project parameter
    SELECT * INTO p_param FROM parameters WHERE id = _param_id;
    IF (p_param.id IS NULL) THEN RETURN 'parameter_not_exists'; END IF;

    -- Check if parameter is a top-level
    IF (p_param.parent_id IS NOT NULL) THEN RETURN 'invalid_parameter'; END IF;

    -- Get linked parameter source
    SELECT source_id INTO p_source_id FROM parameter_links WHERE parameter_id = _param_id;

    -- Get parameter value
    SELECT id INTO p_value_id
    FROM parameter_source_values
    WHERE source_id = p_source_id AND TRIM(LOWER("value")) = TRIM(LOWER(_old_value));

    IF (p_value_id IS NULL) THEN RETURN 'value_not_exists'; END IF;

    -- Check if new parameter value already exists
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameter_source_values
    WHERE source_id = p_source_id AND id <> p_value_id AND TRIM(LOWER("value")) = TRIM(LOWER(_new_value));

    IF (p_exists) THEN RETURN 'value_already_exists'; END IF;

    -- Rename parameter source value
    UPDATE parameter_source_values SET "value" = TRIM(_new_value) WHERE id = p_value_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Link setup to project parameter
CREATE OR REPLACE FUNCTION link_setup_to_project_parameter(_setup_id bigint, _param_id bigint) RETURNS text AS $$
DECLARE
    p_setup setup_steps;
    p_param parameters;
BEGIN
    -- Get setup step
    SELECT * INTO p_setup FROM setup_steps WHERE id = _setup_id;
    IF (p_setup.id IS NULL) THEN RETURN 'setup_step_not_exists'; END IF;

    -- Get project parameter
    SELECT * INTO p_param FROM parameters WHERE id = _param_id;
    IF (p_param.id IS NULL) THEN RETURN 'parameter_not_exists'; END IF;

    -- Check if setup step and parameter belong to the same project
    IF (p_setup.project_id <> p_param.project_id) THEN RETURN 'project_mismatch'; END IF;

    -- Link setup step to project parameter
    INSERT INTO parameter_setup_links (setup_id, parameter_id)
    VALUES (_setup_id, _param_id)
    ON CONFLICT (setup_id) DO UPDATE SET parameter_id = _param_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Specialize test case setup
CREATE OR REPLACE FUNCTION specialize_test_case_setup(_case_id bigint, _setup_id bigint, _spec_type spec_t, _id bigint) RETURNS text AS $$
DECLARE
    p_case test_cases;
    p_setup_id bigint;
    p_setup setup_steps;
    p_exists boolean;
    p_param_id bigint;
    p_param parameters;
    p_target_value_id bigint;
    p_target_param_id bigint;
BEGIN
    -- Get test case
    SELECT * INTO p_case FROM test_cases WHERE id = _case_id;
    IF (p_case.id IS NULL) THEN RETURN 'test_case_not_exists'; END IF;

    -- Get closest test case with assigned setup step within test case tree
    WITH RECURSIVE test_tree AS (
        SELECT id, parent_id, setup_id, 1 AS depth FROM test_cases WHERE id = _case_id
        UNION
        SELECT tc.id, tc.parent_id, tc.setup_id, tt.depth + 1 FROM test_cases AS tc
        INNER JOIN test_tree AS tt ON tt.parent_id = tc.id
    )
    SELECT setup_id INTO p_setup_id
    FROM test_tree
    WHERE setup_id IS NOT NULL
    ORDER BY depth ASC LIMIT 1;

    -- Check if any valid setup exists
    IF (p_setup_id IS NULL) THEN RETURN 'setup_step_not_assigned'; END IF;

    -- Get setup step
    SELECT * INTO p_setup FROM setup_steps WHERE id = _setup_id;
    IF (p_setup.id IS NULL) THEN RETURN 'setup_step_not_exists'; END IF;

    -- Check if supplied setup step exists in the test case setup chain
    WITH RECURSIVE setup_tree AS (
        SELECT id, parent_id FROM setup_steps WHERE id = p_setup_id
        UNION
        SELECT ss.id, ss.parent_id FROM setup_steps AS ss
        INNER JOIN setup_tree AS st ON st.parent_id = ss.id
    )
    SELECT COUNT(*) > 0 INTO p_exists
    FROM setup_tree
    WHERE id = _setup_id;

    IF (NOT p_exists) THEN RETURN 'invalid_setup_step'; END IF;

    -- Check if supplied setup step has a linked parameter
    SELECT parameter_id INTO p_param_id FROM parameter_setup_links WHERE setup_id = _setup_id;
    IF (p_param_id IS NULL) THEN RETURN 'parameterless_setup_step'; END IF;

    -- Get project parameter assigned to setup step
    SELECT * INTO p_param FROM parameters WHERE id = p_param_id;

    -- Check additional specialization
    IF (_spec_type = 'value') THEN
        -- Check if supplied value is valid
        SELECT COUNT(*) > 0 INTO p_exists
        FROM parameter_value_links
        WHERE parameter_id = p_param.id AND value_id = _id;

        IF (NOT p_exists) THEN RETURN 'invalid_parameter_value'; END IF;
    ELSIF (_spec_type = 'source') THEN
        -- Check if valid child parameter is supplied
        WITH RECURSIVE param_tree AS (
            SELECT id, parent_id, 1 AS depth FROM parameters WHERE id = p_param.id
            UNION
            SELECT p.id, p.parent_id, pt.depth + 1 FROM parameters AS p
            INNER JOIN param_tree AS pt ON pt.id = p.parent_id
        )
        SELECT COUNT(*) > 0 INTO p_exists FROM param_tree WHERE id = _id;

        IF (NOT p_exists) THEN RETURN 'invalid_parameter_source'; END IF;
    END IF;

    -- Reset target value_id and parameter_id
    p_target_value_id := NULL;
    p_target_param_id := NULL;

    -- Evaluate target value_id and parameter_id
    CASE _spec_type
        WHEN 'value' THEN p_target_value_id := _id;
        WHEN 'source' THEN p_target_param_id := _id;
        ELSE
    END CASE;

    -- Link setup step to project parameter
    INSERT INTO parameter_setup_specialization (case_id, setup_id, spec_type, value_id, parameter_id)
    VALUES (_case_id, _setup_id, _spec_type, p_target_value_id, p_target_param_id)
    ON CONFLICT (case_id, setup_id)
    DO UPDATE SET spec_type = _spec_type, value_id = p_target_value_id, parameter_id = p_target_param_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Delete JIRA instance
CREATE OR REPLACE FUNCTION delete_jira_instance(_jira_id bigint) RETURNS boolean AS $$
DECLARE
    p_jira jira_instances;
BEGIN
    -- Get JIRA instance
    SELECT * INTO p_jira FROM jira_instances WHERE id = _jira_id;
    IF (p_jira.id IS NULL) THEN RETURN FALSE; END IF;

    -- Unbind JIRA instance from projects
    UPDATE projects SET jira_id = NULL, jira_key = NULL WHERE jira_id = _jira_id;

    -- Delete JIRA authentications
    DELETE FROM jira_auth WHERE jira_id = _jira_id;

    -- Delete JIRA instance itself
    DELETE FROM jira_instances WHERE id = _jira_id;

    RETURN TRUE;
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Retrieve file attachment IDs within project
CREATE OR REPLACE FUNCTION get_project_file_attachment_ids(_project_id bigint) RETURNS SETOF bigint AS $$
BEGIN
    RETURN QUERY SELECT DISTINCT attachment_id FROM (
        SELECT fal1.attachment_id FROM file_attachment_links AS fal1
        LEFT OUTER JOIN setup_steps AS ss ON (ss.id = fal1.linked_id)
        WHERE fal1.owner = 'setup_step' AND ss.project_id = _project_id
        UNION
        SELECT fal2.attachment_id FROM file_attachment_links AS fal2
        LEFT OUTER JOIN test_cases AS tc ON (tc.id = fal2.linked_id)
        WHERE fal2.owner = 'test_case' AND tc.project_id = _project_id
        UNION
        SELECT fal3.attachment_id FROM file_attachment_links AS fal3
        LEFT OUTER JOIN test_runs AS tr1 ON (tr1.id = fal3.linked_id)
        WHERE fal3.owner = 'test_run' AND tr1.project_id = _project_id
        UNION
        SELECT fal4.attachment_id FROM file_attachment_links AS fal4
        LEFT OUTER JOIN test_run_items AS tri ON (tri.id = fal4.linked_id)
        LEFT OUTER JOIN test_runs AS tr2 ON (tr2.id = tri.run_id)
        WHERE fal4.owner = 'test_run_item' AND tr2.project_id = _project_id
    ) AS qq;
END;
$$ LANGUAGE plpgsql STABLE;

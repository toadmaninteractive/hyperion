-- Patch SQL
-- Revision: 1 -> 2

-- Test status enum
CREATE TYPE test_status_t AS ENUM (
    'pending',
    'in_progress',
    'passed',
    'failed',
    'blocked'
);

-- Projects
CREATE TABLE projects
(
    id bigserial PRIMARY KEY,
    title varchar UNIQUE NOT NULL CHECK (TRIM(title) <> ''),
    "key" varchar UNIQUE NOT NULL CHECK (TRIM("key") <> ''),
    owner_id bigint NOT NULL REFERENCES personnel (id),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX proj_title_ult_index ON projects (LOWER(TRIM(title)));
CREATE UNIQUE INDEX proj_key_ult_index ON projects (LOWER(TRIM("key")));
CREATE INDEX proj_owner_id_index ON projects (owner_id);
CREATE INDEX proj_created_at_index ON projects (created_at);
CREATE INDEX proj_updated_at_index ON projects (updated_at);

-- Setup steps
CREATE TABLE setup_steps
(
    id bigserial PRIMARY KEY,
    parent_id bigint DEFAULT NULL REFERENCES setup_steps (id),
    project_id bigint NOT NULL REFERENCES projects (id),
    title varchar DEFAULT NULL,
    description varchar DEFAULT NULL,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX sets_project_step_title_ult_index ON setup_steps (project_id, parent_id, LOWER(TRIM(title)));
CREATE INDEX sets_parent_id_index ON setup_steps (parent_id);
CREATE INDEX sets_project_id_index ON setup_steps (project_id);
CREATE INDEX sets_created_at_index ON setup_steps (created_at);
CREATE INDEX sets_updated_at_index ON setup_steps (updated_at);

-- Test cases
CREATE TABLE test_cases
(
    id bigserial PRIMARY KEY,
    parent_id bigint DEFAULT NULL REFERENCES test_cases (id),
    project_id bigint NOT NULL REFERENCES projects (id),
    setup_id bigint DEFAULT NULL REFERENCES setup_steps (id),
    is_group boolean NOT NULL,
    title varchar DEFAULT NULL,
    test_steps varchar NOT NULL,
    expected_result varchar NOT NULL,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX tc_project_step_title_ult_index ON test_cases (project_id, parent_id, LOWER(TRIM(title)));
CREATE INDEX tc_parent_id_index ON test_cases (parent_id);
CREATE INDEX tc_project_id_index ON test_cases (project_id);
CREATE INDEX tc_setup_id_index ON test_cases (setup_id);
CREATE INDEX tc_is_group_index ON test_cases (is_group);
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
    project_id bigint NOT NULL REFERENCES projects (id),
    status test_status_t NOT NULL DEFAULT 'pending',
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp,
    started_at timestamptz DEFAULT NULL,
    finished_at timestamptz DEFAULT NULL
) WITHOUT OIDS;

CREATE INDEX tr_project_id_index ON test_runs (project_id);
CREATE INDEX tr_status_index ON test_runs (status);
CREATE INDEX tr_created_at_index ON test_runs (created_at);
CREATE INDEX tr_updated_at_index ON test_runs (updated_at);
CREATE INDEX tr_started_at_index ON test_runs (started_at);
CREATE INDEX tr_finished_at_index ON test_runs (finished_at);

-- Test run items
CREATE TABLE test_run_items
(
    id bigserial PRIMARY KEY,
    run_id bigint NOT NULL REFERENCES test_runs (id),
    case_id bigint NOT NULL REFERENCES test_cases (id),
    reporter_id bigint NOT NULL REFERENCES personnel (id),
    assignee_id bigint DEFAULT NULL REFERENCES personnel (id),
    status test_status_t NOT NULL DEFAULT 'pending',
    summary varchar DEFAULT NULL,
    failed_setup_id bigint DEFAULT NULL REFERENCES setup_steps (id),
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
CREATE INDEX tri_created_at_index ON test_run_items (created_at);
CREATE INDEX tri_updated_at_index ON test_run_items (updated_at);
CREATE INDEX tri_started_at_index ON test_run_items (started_at);
CREATE INDEX tri_finished_at_index ON test_run_items (finished_at);

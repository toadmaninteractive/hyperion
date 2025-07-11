-- Patch SQL
-- Revision: 21 -> 22

-- JIRA instances
CREATE TABLE jira_instances
(
    id bigserial PRIMARY KEY,
    title varchar UNIQUE NOT NULL CHECK (TRIM(title) <> ''),
    url varchar NOT NULL CHECK (TRIM(url) <> ''),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE INDEX ji_created_at_index ON jira_instances (created_at);
CREATE INDEX ji_updated_at_index ON jira_instances (updated_at);

-- Add jira_id and jira_key fields to projects table, create related index
ALTER TABLE projects ADD COLUMN jira_id bigint DEFAULT NULL REFERENCES jira_instances (id);
ALTER TABLE projects ADD COLUMN jira_key varchar DEFAULT NULL;
CREATE INDEX proj_jira_id_index ON projects (jira_id);

-- Add jira_issue_key field to test_run_items table
ALTER TABLE test_run_items ADD COLUMN ira_issue_key varchar DEFAULT NULL;

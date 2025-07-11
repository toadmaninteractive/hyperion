-- Patch SQL
-- Revision: 7 -> 8

ALTER TABLE test_runs ADD COLUMN title varchar DEFAULT NULL;

CREATE UNIQUE INDEX tr_project_title_index ON test_runs (project_id, LOWER(TRIM(title)));

-- Patch SQL
-- Revision: 4 -> 5

-- Add description column to test_cases table
ALTER TABLE test_cases ADD COLUMN description varchar DEFAULT NULL;

-- Drop invalid index
DROP INDEX IF EXISTS tc_project_step_title_ult_index;

-- Add new indexes to test_cases table
CREATE UNIQUE INDEX tc_project_case_title_ult_index ON test_cases (project_id, parent_id, LOWER(TRIM(title)));
CREATE UNIQUE INDEX tc_project_case_title_null_parent_index ON test_cases (project_id, LOWER(TRIM(title))) WHERE parent_id IS NULL;
CREATE UNIQUE INDEX tc_project_case_title_null_title_index ON test_cases (project_id, parent_id) WHERE title IS NULL;

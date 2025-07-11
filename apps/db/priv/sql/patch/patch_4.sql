-- Patch SQL
-- Revision: 3 -> 4

-- Add partial indexes to setup_steps table
CREATE UNIQUE INDEX sets_project_step_title_null_parent_index ON setup_steps (project_id, LOWER(TRIM(title))) WHERE parent_id IS NULL;
CREATE UNIQUE INDEX sets_project_step_title_null_title_index ON setup_steps (project_id, parent_id) WHERE title IS NULL;

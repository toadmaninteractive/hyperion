-- Patch SQL
-- Revision: 2 -> 3

-- Add rev field to related tables
ALTER TABLE projects ADD COLUMN rev integer NOT NULL DEFAULT 1;
ALTER TABLE setup_steps ADD COLUMN rev integer NOT NULL DEFAULT 1;
ALTER TABLE test_cases ADD COLUMN rev integer NOT NULL DEFAULT 1;
ALTER TABLE test_runs ADD COLUMN rev integer NOT NULL DEFAULT 1;
ALTER TABLE test_run_items ADD COLUMN rev integer NOT NULL DEFAULT 1;
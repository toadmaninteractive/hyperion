-- Patch SQL
-- Revision: 19 -> 20

-- Add params column to test_run_items table
ALTER TABLE test_run_items ADD COLUMN params jsonb DEFAULT '{}';

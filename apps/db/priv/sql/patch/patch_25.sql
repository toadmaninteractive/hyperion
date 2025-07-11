-- Patch SQL
-- Revision: 24 -> 25

-- Add rev field to jira_instances table
ALTER TABLE jira_instances ADD COLUMN rev integer NOT NULL DEFAULT 1;

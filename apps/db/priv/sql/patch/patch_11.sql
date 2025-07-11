-- Patch SQL
-- Revision: 10 -> 11

-- Add slack_receiver column to projects table
ALTER TABLE projects ADD COLUMN slack_receiver varchar CHECK (TRIM(slack_receiver) <> '');

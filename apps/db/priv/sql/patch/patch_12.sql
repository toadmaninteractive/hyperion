-- Patch SQL
-- Revision: 11 -> 12

-- Rename slack_receiver column to slack_receivers in projects table
ALTER TABLE projects RENAME COLUMN slack_receiver TO slack_receivers;
ALTER TABLE projects RENAME CONSTRAINT projects_slack_receiver_check TO projects_slack_receivers_check;

-- Patch SQL
-- Revision: 23 -> 24

ALTER TABLE test_run_items DROP COLUMN ira_issue_key;
ALTER TABLE test_run_items ADD COLUMN jira_issue_key varchar DEFAULT NULL;

-- Patch SQL
-- Revision: 25 -> 26

CREATE UNIQUE INDEX ji_title_ult_index ON jira_instances (LOWER(TRIM(title)));

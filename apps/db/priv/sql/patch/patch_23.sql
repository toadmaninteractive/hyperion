-- Patch SQL
-- Revision: 22 -> 23

-- JIRA authentications
CREATE TABLE jira_auth
(
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    jira_id bigint DEFAULT NULL REFERENCES jira_instances (id),
    username varchar NOT NULL CHECK (TRIM(username) <> ''),
    auth_token varchar NOT NULL CHECK (TRIM(auth_token) <> ''),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp,
    PRIMARY KEY (personnel_id, jira_id)
) WITHOUT OIDS;

CREATE INDEX ja_personnel_id_index ON jira_auth (personnel_id);
CREATE INDEX ja_jira_id_index ON jira_auth (jira_id);
CREATE INDEX ja_created_at_index ON jira_auth (created_at);
CREATE INDEX ja_updated_at_index ON jira_auth (updated_at);

-- Delete JIRA instance
CREATE OR REPLACE FUNCTION delete_jira_instance(_jira_id bigint) RETURNS boolean AS $$
DECLARE
    p_jira jira_instances;
BEGIN
    -- Get JIRA instance
    SELECT * INTO p_jira FROM jira_instances WHERE id = _jira_id;
    IF (p_jira.id IS NULL) THEN RETURN FALSE; END IF;

    -- Unbind JIRA instance from projects
    UPDATE projects SET jira_id = NULL, jira_key = NULL WHERE jira_id = _jira_id;

    -- Delete JIRA authentications
    DELETE FROM jira_auth WHERE jira_id = _jira_id;

    -- Delete JIRA instance itself
    DELETE FROM jira_instances WHERE id = _jira_id;

    RETURN TRUE;
END;
$$ LANGUAGE plpgsql VOLATILE;

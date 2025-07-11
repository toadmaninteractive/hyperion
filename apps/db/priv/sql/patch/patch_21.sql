-- Patch SQL
-- Revision: 20 -> 21

CREATE TYPE role_t AS ENUM (
    'consumer',
    'maintainer',
    'admin'
);

-- Add name field to personnel table
ALTER TABLE personnel ADD COLUMN name varchar DEFAULT NULL;

-- Personnel groups
CREATE TABLE personnel_groups
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    name varchar UNIQUE NOT NULL CHECK (TRIM(name) <> ''),
    description varchar DEFAULT NULL,
    is_superadmin boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX perg_name_ult_index ON personnel_groups (LOWER(TRIM(name)));
CREATE INDEX perg_is_superadmin_index ON personnel_groups (is_superadmin);
CREATE INDEX perg_is_deleted_index ON personnel_groups (is_deleted);
CREATE INDEX perg_created_at_index ON personnel_groups (created_at);
CREATE INDEX perg_updated_at_index ON personnel_groups (updated_at);

-- Personnel group membership
CREATE TABLE personnel_group_membership
(
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    group_id bigint NOT NULL REFERENCES personnel_groups (id),
    PRIMARY KEY (personnel_id, group_id)
) WITHOUT OIDS;

CREATE INDEX pergm_personnel_id_index ON personnel_group_membership (personnel_id);
CREATE INDEX pergm_group_id_index ON personnel_group_membership (group_id);

-- Personnel roles
CREATE TABLE personnel_roles
(
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    project_id bigint NOT NULL REFERENCES projects (id),
    role role_t NOT NULL,
    PRIMARY KEY (personnel_id, project_id)
) WITHOUT OIDS;

CREATE INDEX perr_personnel_id_index ON personnel_roles (personnel_id);
CREATE INDEX perr_project_id_index ON personnel_roles (project_id);
CREATE INDEX perr_role_index ON personnel_roles (role);

-- Personnel group roles
CREATE TABLE personnel_group_roles
(
    group_id bigint NOT NULL REFERENCES personnel_groups (id),
    project_id bigint NOT NULL REFERENCES projects (id),
    role role_t NOT NULL,
    PRIMARY KEY (group_id, project_id)
) WITHOUT OIDS;

CREATE INDEX pergr_group_id_index ON personnel_group_roles (group_id);
CREATE INDEX pergr_project_id_index ON personnel_group_roles (project_id);
CREATE INDEX pergr_role_index ON personnel_group_roles (role);

-- Deactivate personnel account
CREATE OR REPLACE FUNCTION deactivate_personnel_account(_personnel_id bigint) RETURNS text AS $$
DECLARE
    p_personnel personnel;
BEGIN
    -- Get personnel account and check it
    SELECT * INTO p_personnel FROM personnel WHERE id = _personnel_id;
    IF (p_personnel.id IS NULL) THEN RETURN 'account_not_exists'; END IF;

   -- Delete account sessions
    DELETE FROM personnel_sessions WHERE personnel_id = _personnel_id;

    -- Mark account as deleted
    UPDATE personnel
    SET is_deleted = TRUE, updated_at = current_timestamp
    WHERE id = _personnel_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Set superadmin personnel group
CREATE OR REPLACE FUNCTION set_superadmin_personnel_group(_group_name text) RETURNS text AS $$
BEGIN
    -- Unset previous superadmin personnel groups
    UPDATE personnel_groups
    SET is_superadmin = FALSE, updated_at = current_timestamp
    WHERE LOWER(TRIM(name)) <> LOWER(TRIM(_group_name)) AND is_superadmin;
    
    -- Set current superadmin personnel group
    UPDATE personnel_groups
    SET is_superadmin = TRUE, updated_at = current_timestamp
    WHERE LOWER(TRIM(name)) = LOWER(TRIM(_group_name)) AND NOT is_superadmin;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Check if project is accessible by personnel account
CREATE OR REPLACE FUNCTION is_project_accessible_by_personnel(_project_id bigint, _personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_accessible boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND project_id = _project_id;

    IF (p_accessible) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.project_id = _project_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IS NOT NULL);

    RETURN p_accessible;
END;
$$ LANGUAGE plpgsql STABLE;

-- Check if personnel account is allowed to manage at least one project
CREATE OR REPLACE FUNCTION is_project_manager(_personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_managable boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND role IN ('maintainer', 'admin');

    IF (p_managable) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IN ('maintainer', 'admin'));

    RETURN p_managable;
END;
$$ LANGUAGE plpgsql STABLE;

-- Check if personnel account is allowed to manage a project
CREATE OR REPLACE FUNCTION is_project_manager(_project_id bigint, _personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_managable boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND project_id = _project_id AND role IN ('maintainer', 'admin');

    IF (p_managable) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.project_id = _project_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IN ('maintainer', 'admin'));

    RETURN p_managable;
END;
$$ LANGUAGE plpgsql STABLE;

-- Check if personnel account is superadmin
CREATE OR REPLACE FUNCTION is_superadmin(_personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_superadmin boolean;
BEGIN
    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_superadmin
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    WHERE pergm.personnel_id IS NOT NULL AND perg.is_superadmin;

    RETURN p_superadmin;
END;
$$ LANGUAGE plpgsql STABLE;

-- Get role numeric representation
CREATE OR REPLACE FUNCTION role_level(_role role_t) RETURNS integer AS $$
BEGIN
    CASE _role
        WHEN 'consumer' THEN RETURN 1;
        WHEN 'maintainer' THEN RETURN 2;
        WHEN 'admin' THEN RETURN 3;
        ELSE RETURN 0;
    END CASE;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Check if project access role is sufficient
CREATE OR REPLACE FUNCTION is_project_access_role_sufficient(_project_id bigint, _personnel_id bigint, _min_role role_t) RETURNS boolean AS $$
DECLARE
    p_accessible boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND project_id = _project_id AND role_level(role) >= role_level(_min_role);

    IF (p_accessible) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.project_id = _project_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR role_level(pergr.role) >= role_level(_min_role));

    RETURN p_accessible;
END;
$$ LANGUAGE plpgsql STABLE;

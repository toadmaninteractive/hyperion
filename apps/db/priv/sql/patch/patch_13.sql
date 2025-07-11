-- Patch SQL
-- Revision: 12 -> 13

-- Parameter sources
CREATE TABLE parameter_sources
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    project_id bigint NOT NULL REFERENCES projects (id),
    title varchar NOT NULL CHECK (TRIM(title) <> ''),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX pars_project_title_ult_index ON parameter_sources (project_id, LOWER(TRIM(title)));
CREATE INDEX pars_project_id_index ON parameter_sources (project_id);
CREATE INDEX pars_created_at_index ON parameter_sources (created_at);
CREATE INDEX pars_updated_at_index ON parameter_sources (updated_at);

-- Parameter source values
CREATE TABLE parameter_source_values
(
    id bigserial PRIMARY KEY,
    source_id bigint NOT NULL REFERENCES parameter_sources (id),
    value varchar NOT NULL
) WITHOUT OIDS;

CREATE UNIQUE INDEX parsv_unique_value_ult_index ON parameter_source_values (source_id, LOWER(TRIM("value")));
CREATE INDEX parsv_source_id_index ON parameter_source_values (source_id);

-- Parameters
CREATE TABLE parameters
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    project_id bigint NOT NULL REFERENCES projects (id),
    parent_id bigint DEFAULT NULL REFERENCES parameters (id),
    dependent_id bigint DEFAULT NULL REFERENCES parameters (id),
    title varchar NOT NULL CHECK (TRIM(title) <> ''),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX parm_project_title_ult_index ON parameters (project_id, LOWER(TRIM(title)));
CREATE INDEX parm_project_id_index ON parameters (project_id);
CREATE INDEX parm_parent_id_index ON parameters (parent_id);
CREATE INDEX parm_dependent_id_index ON parameters (dependent_id);
CREATE INDEX parm_created_at_index ON parameters (created_at);
CREATE INDEX parm_updated_at_index ON parameters (updated_at);

-- Parameter links
CREATE TABLE parameter_links
(
    parameter_id bigint PRIMARY KEY REFERENCES parameters (id),
    source_id bigint NOT NULL REFERENCES parameter_sources (id)
) WITHOUT OIDS;

CREATE INDEX parl_source_id_index ON parameter_links (source_id);

-- Parameter value links
CREATE TABLE parameter_value_links
(
    parameter_id bigint NOT NULL REFERENCES parameters (id),
    dependent_value_id bigint DEFAULT NULL REFERENCES parameter_source_values (id),
    value_id bigint NOT NULL REFERENCES parameter_source_values (id)
) WITHOUT OIDS;

CREATE UNIQUE INDEX parvl_dependent_value_index ON parameter_value_links (parameter_id, dependent_value_id, value_id);
CREATE UNIQUE INDEX parvl_independent_value_index ON parameter_value_links (parameter_id, value_id) WHERE dependent_value_id IS NULL;

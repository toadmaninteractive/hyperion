-- Patch SQL
-- Revision: 14 -> 15

-- Specialization
CREATE TYPE spec_t AS ENUM (
    'value',
    'source',
    'any',
    'random'
);

-- Parameter setup links
CREATE TABLE parameter_setup_links
(
    setup_id bigint PRIMARY KEY REFERENCES setup_steps (id),
    parameter_id bigint NOT NULL REFERENCES parameters (id)
) WITHOUT OIDS;

CREATE INDEX parsl_parameter_id_index ON parameter_setup_links (parameter_id);

-- Parameter setup specialization
CREATE TABLE parameter_setup_specialization
(
    сase_id bigint NOT NULL REFERENCES test_cases (id),
    setup_id bigint NOT NULL REFERENCES setup_steps (id),
    spec_type spec_t NOT NULL,
    value_id bigint DEFAULT NULL REFERENCES parameter_source_values (id),
    parameter_id bigint DEFAULT NULL REFERENCES parameters (id),
    PRIMARY KEY (сase_id, setup_id)
) WITHOUT OIDS;

CREATE INDEX parss_сase_id_index ON parameter_setup_specialization (сase_id);
CREATE INDEX parss_setup_id_index ON parameter_setup_specialization (setup_id);
CREATE INDEX parss_value_id_index ON parameter_setup_specialization (value_id);
CREATE INDEX parss_parameter_id_index ON parameter_setup_specialization (parameter_id);

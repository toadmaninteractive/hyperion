-- Patch SQL
-- Revision: 18 -> 19

-- Drop parameter_setup_specialization table
DROP TABLE IF EXISTS parameter_setup_specialization;

-- Parameter setup specialization
CREATE TABLE parameter_setup_specialization
(
    case_id bigint NOT NULL REFERENCES test_cases (id),
    setup_id bigint NOT NULL REFERENCES setup_steps (id),
    spec_type spec_t NOT NULL,
    value_id bigint DEFAULT NULL REFERENCES parameter_source_values (id),
    parameter_id bigint DEFAULT NULL REFERENCES parameters (id),
    PRIMARY KEY (case_id, setup_id)
) WITHOUT OIDS;

CREATE INDEX parss_case_id_index ON parameter_setup_specialization (case_id);
CREATE INDEX parss_setup_id_index ON parameter_setup_specialization (setup_id);
CREATE INDEX parss_value_id_index ON parameter_setup_specialization (value_id);
CREATE INDEX parss_parameter_id_index ON parameter_setup_specialization (parameter_id);

-- Specialize test case setup
CREATE OR REPLACE FUNCTION specialize_test_case_setup(_case_id bigint, _setup_id bigint, _spec_type spec_t, _id bigint) RETURNS text AS $$
DECLARE
    p_case test_cases;
    p_setup_id bigint;
    p_setup setup_steps;
    p_exists boolean;
    p_param_id bigint;
    p_param parameters;
    p_target_value_id bigint;
    p_target_param_id bigint;
BEGIN
    -- Get test case
    SELECT * INTO p_case FROM test_cases WHERE id = _case_id;
    IF (p_case.id IS NULL) THEN RETURN 'test_case_not_exists'; END IF;

    -- Get closest test case with assigned setup step within test case tree
    WITH RECURSIVE test_tree AS (
        SELECT id, parent_id, setup_id, 1 AS depth FROM test_cases WHERE id = _case_id
        UNION
        SELECT tc.id, tc.parent_id, tc.setup_id, tt.depth + 1 FROM test_cases AS tc
        INNER JOIN test_tree AS tt ON tt.parent_id = tc.id
    )
    SELECT setup_id INTO p_setup_id
    FROM test_tree
    WHERE setup_id IS NOT NULL
    ORDER BY depth ASC LIMIT 1;

    -- Check if any valid setup exists
    IF (p_setup_id IS NULL) THEN RETURN 'setup_step_not_assigned'; END IF;

    -- Get setup step
    SELECT * INTO p_setup FROM setup_steps WHERE id = _setup_id;
    IF (p_setup.id IS NULL) THEN RETURN 'setup_step_not_exists'; END IF;

    -- Check if supplied setup step exists in the test case setup chain
    WITH RECURSIVE setup_tree AS (
        SELECT id, parent_id FROM setup_steps WHERE id = p_setup_id
        UNION
        SELECT ss.id, ss.parent_id FROM setup_steps AS ss
        INNER JOIN setup_tree AS st ON st.parent_id = ss.id
    )
    SELECT COUNT(*) > 0 INTO p_exists
    FROM setup_tree
    WHERE id = _setup_id;

    IF (NOT p_exists) THEN RETURN 'invalid_setup_step'; END IF;

    -- Check if supplied setup step has a linked parameter
    SELECT parameter_id INTO p_param_id FROM parameter_setup_links WHERE setup_id = _setup_id;
    IF (p_param_id IS NULL) THEN RETURN 'parameterless_setup_step'; END IF;

    -- Get project parameter assigned to setup step
    SELECT * INTO p_param FROM parameters WHERE id = p_param_id;

    -- Check additional specialization
    IF (_spec_type = 'value') THEN
        -- Check if supplied value is valid
        SELECT COUNT(*) > 0 INTO p_exists
        FROM parameter_value_links
        WHERE parameter_id = p_param.id AND value_id = _id;

        IF (NOT p_exists) THEN RETURN 'invalid_parameter_value'; END IF;
    ELSIF (_spec_type = 'source') THEN
        -- Check if valid child parameter is supplied
        WITH RECURSIVE param_tree AS (
            SELECT id, parent_id, 1 AS depth FROM parameters WHERE id = p_param.id
            UNION
            SELECT p.id, p.parent_id, pt.depth + 1 FROM parameters AS p
            INNER JOIN param_tree AS pt ON pt.id = p.parent_id
        )
        SELECT COUNT(*) > 0 INTO p_exists FROM param_tree WHERE id = _id;

        IF (NOT p_exists) THEN RETURN 'invalid_parameter_source'; END IF;
    END IF;

    -- Reset target value_id and parameter_id
    p_target_value_id := NULL;
    p_target_param_id := NULL;

    -- Evaluate target value_id and parameter_id
    CASE _spec_type
        WHEN 'value' THEN p_target_value_id := _id;
        WHEN 'source' THEN p_target_param_id := _id;
        ELSE
    END CASE;

    -- Link setup step to project parameter
    INSERT INTO parameter_setup_specialization (case_id, setup_id, spec_type, value_id, parameter_id)
    VALUES (_case_id, _setup_id, _spec_type, p_target_value_id, p_target_param_id)
    ON CONFLICT (case_id, setup_id)
    DO UPDATE SET spec_type = _spec_type, value_id = p_target_value_id, parameter_id = p_target_param_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

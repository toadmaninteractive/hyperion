-- Patch SQL
-- Revision: 15 -> 16

-- Remove project parameter
CREATE OR REPLACE FUNCTION remove_project_parameter(_param_id bigint) RETURNS text AS $$
DECLARE
    p_param parameters;
    p_param_link parameter_links;
    p_exists boolean;
    p_source_id bigint;
BEGIN
    -- Get project parameter
    SELECT * INTO p_param FROM parameters WHERE id = _param_id;
    IF (p_param.id IS NULL) THEN RETURN 'parameter_not_exists'; END IF;

    -- Check if parameter has children
    SELECT COUNT(*) > 0 INTO p_exists FROM parameters WHERE parent_id = _param_id;
    IF (p_exists) THEN RETURN 'has_children'; END IF;

    -- Check if parameter has dependants
    SELECT COUNT(*) > 0 INTO p_exists FROM parameters WHERE dependent_id = _param_id;
    IF (p_exists) THEN RETURN 'has_dependants'; END IF;

    -- Delete parameter setup links
    DELETE FROM parameter_setup_links WHERE parameter_id = _param_id;

    -- Delete parameter setup specialization
    DELETE FROM parameter_setup_specialization WHERE parameter_id = _param_id;

    -- Delete parameter value links
    DELETE FROM parameter_value_links WHERE parameter_id = _param_id;

    -- Check for top-level parameter
    IF (p_param.parent_id IS NULL) THEN
        -- Get linked parameter source
        SELECT source_id INTO p_source_id FROM parameter_links WHERE parameter_id = _param_id;
    
        -- Remove parameter source values
        DELETE FROM parameter_source_values WHERE source_id = p_source_id;
    
        -- Remove parameter link
        DELETE FROM parameter_links WHERE parameter_id = _param_id;
    
        -- Remove parameter source
        DELETE FROM parameter_sources WHERE id = p_source_id;
    END IF;

    -- Delete parameter itself
    DELETE FROM parameters WHERE id = _param_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Remove project parameter value
CREATE OR REPLACE FUNCTION remove_project_parameter_value(_param_id bigint, _dependent_value text, _value text) RETURNS text AS $$
DECLARE
    p_param parameters;
    p_param_link parameter_links;
    p_exists boolean;
    p_value_id bigint;
    p_dependent_param parameters;
    p_dependent_link parameter_links;
    p_dependent_value_id bigint;
BEGIN
    -- Get project parameter
    SELECT * INTO p_param FROM parameters WHERE id = _param_id;
    IF (p_param.id IS NULL) THEN RETURN 'parameter_not_exists'; END IF;

    -- Get parameter value
    SELECT pvl.value_id INTO p_value_id
    FROM parameter_value_links AS pvl
    LEFT OUTER JOIN parameter_source_values AS psv ON (psv.id = pvl.value_id)
    WHERE pvl.parameter_id = _param_id AND TRIM(LOWER(psv.value)) = TRIM(LOWER(_value));

    IF (p_value_id IS NULL) THEN RETURN 'value_not_exists'; END IF;

    -- Check if child parameters are linked to the value
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameters AS p
    LEFT OUTER JOIN parameter_value_links AS pvl ON (pvl.parameter_id = p.id)
    WHERE p.parent_id = _param_id AND pvl.value_id = p_value_id;

    IF (p_exists) THEN RETURN 'linked_to_children'; END IF;

    -- Check if dependent parameters are linked to the value
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameter_value_links
    WHERE dependent_value_id = p_value_id;

    IF (p_exists) THEN RETURN 'linked_to_dependants'; END IF;

    -- Check dependent project parameter
    IF (p_param.dependent_id IS NOT NULL) THEN
        -- Check if dependent project parameter exists
        SELECT * INTO p_dependent_param FROM parameters WHERE id = p_param.dependent_id;
        IF (p_dependent_param.id IS NULL) THEN RETURN 'dependent_parameter_not_exists'; END IF;

        -- Checked linked dependent project source
        SELECT * INTO p_dependent_link FROM parameter_links WHERE parameter_id = p_dependent_param.id;
        IF (p_dependent_link.source_id IS NULL) THEN RETURN 'dependent_source_not_exists'; END IF;

        -- Checked linked dependent project source value
        SELECT pvl.value_id INTO p_dependent_value_id
        FROM parameter_value_links AS pvl
        LEFT OUTER JOIN parameter_source_values AS psv ON (psv.id = pvl.value_id)
        WHERE pvl.parameter_id = p_dependent_param.id AND TRIM(LOWER(psv.value)) = TRIM(LOWER(_dependent_value));

        IF (p_dependent_value_id IS NULL) THEN RETURN 'dependent_value_not_exists'; END IF;

        -- Remove parameter value link
        DELETE FROM parameter_value_links
        WHERE parameter_id = _param_id AND dependent_value_id = p_dependent_value_id AND value_id = p_value_id;
    ELSE
        -- Remove parameter value link
        DELETE FROM parameter_value_links
        WHERE parameter_id = _param_id AND value_id = p_value_id;
    END IF;

    -- Check for top-level parameter
    IF (p_param.parent_id IS NULL) THEN
        -- Remove parameter source value
        DELETE FROM parameter_source_values WHERE id = p_value_id;

        -- Delete parameter setup specialization
        DELETE FROM parameter_setup_specialization WHERE value_id = p_value_id;
    END IF;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

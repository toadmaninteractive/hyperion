-- Patch SQL
-- Revision: 13 -> 14

-- Create project parameter
CREATE OR REPLACE FUNCTION create_project_parameter(_project_id bigint, _parent_id bigint, _dependent_id bigint, _title text) RETURNS text AS $$
DECLARE
    p_exists boolean;
    p_param_id bigint;
    p_source_id bigint;
BEGIN
    -- Check if project exists
    SELECT COUNT(*) > 0 INTO p_exists FROM projects WHERE id = _project_id;
    IF (NOT p_exists) THEN RETURN 'project_not_exists'; END IF;

    -- Check if parameter already exists
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameters
    WHERE project_id = _project_id AND TRIM(LOWER(title)) = TRIM(LOWER(_title));

    IF (p_exists) THEN RETURN 'parameter_already_exists'; END IF;

    -- Check if parent parameter exists
    IF (_parent_id IS NOT NULL) THEN
        SELECT COUNT(*) > 0 INTO p_exists FROM parameters WHERE id = _parent_id;
        IF (NOT p_exists) THEN RETURN 'parent_not_exists'; END IF;
    END IF;

    -- Prevent to create top-level dependent parameter
    IF (_parent_id IS NULL AND _dependent_id IS NOT NULL) THEN RETURN 'parent_is_independent'; END IF;

    -- Check if parameter source already exists
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameter_sources
    WHERE project_id = _project_id AND TRIM(LOWER(title)) = TRIM(LOWER(_title));

    IF (p_exists) THEN RETURN 'parameter_source_already_exists'; END IF;

    -- Create new parameter
    INSERT INTO parameters ("project_id", "parent_id", "dependent_id", "title")
    VALUES (_project_id, _parent_id, _dependent_id, TRIM(_title))
    RETURNING id INTO p_param_id;

    -- Perform additional actions for top-level parameters
    IF (_parent_id IS NULL) THEN
        -- Create new parameter source
        INSERT INTO parameter_sources ("project_id", "title")
        VALUES (_project_id, TRIM(_title))
        RETURNING id INTO p_source_id;

        -- Create a link between a parameter source and a top-level parameter
        INSERT INTO parameter_links ("parameter_id", "source_id")
        VALUES (p_param_id, p_source_id);
    END IF;

    RETURN p_param_id::text;
END;
$$ LANGUAGE plpgsql VOLATILE;

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

-- Rename project parameter
CREATE OR REPLACE FUNCTION rename_project_parameter(_param_id bigint, _new_title text) RETURNS text AS $$
DECLARE
    p_param parameters;
    p_exists boolean;
    p_source_id bigint;
BEGIN
    -- Get project parameter
    SELECT * INTO p_param FROM parameters WHERE id = _param_id;
    IF (p_param.id IS NULL) THEN RETURN 'parameter_not_exists'; END IF;

    -- Check if parameter has children
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameters
    WHERE id <> _param_id AND project_id = p_param.project_id AND TRIM(LOWER(title)) = TRIM(LOWER(_new_title));

    IF (p_exists) THEN RETURN 'title_already_exists'; END IF;

    -- Get linked parameter source
    SELECT source_id INTO p_source_id FROM parameter_links WHERE parameter_id = _param_id;

    -- Rename parameter
    UPDATE parameters SET title = TRIM(_new_title), updated_at = NOW() WHERE id = _param_id;

    -- Rename parameter source
    UPDATE parameter_sources SET title = TRIM(_new_title), updated_at = NOW() WHERE id = p_source_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Add project parameter value
CREATE OR REPLACE FUNCTION add_project_parameter_value(_param_id bigint, _dependent_value text, _value text) RETURNS text AS $$
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

    -- Check for top-level parameter
    IF (p_param.parent_id IS NULL) THEN
        -- Check linked parameter source
        SELECT * INTO p_param_link FROM parameter_links WHERE parameter_id = _param_id;
        IF (p_param_link.source_id IS NULL) THEN RETURN 'parameter_source_not_exists'; END IF;
   
        -- Check if supplied value already exists among parameter source values
        SELECT COUNT(*) > 0 INTO p_exists
        FROM parameter_source_values
        WHERE source_id = p_param_link.source_id AND TRIM(LOWER("value")) = TRIM(LOWER(_value));

        IF (p_exists) THEN RETURN 'value_already_exists'; END IF;

        -- Add new value to parameter source
        INSERT INTO parameter_source_values ("source_id", "value")
        VALUES (p_param_link.source_id, TRIM(_value))
        RETURNING id INTO p_value_id;

        -- Link created value to project parameter
        INSERT INTO parameter_value_links ("parameter_id", "value_id")
        VALUES (_param_id, p_value_id);
    ELSE
        -- Check if value is defined for parent parameter
        SELECT pvl.value_id INTO p_value_id
        FROM parameter_value_links AS pvl
        LEFT OUTER JOIN parameter_source_values AS psv ON (psv.id = pvl.value_id)
        WHERE pvl.parameter_id = p_param.parent_id AND TRIM(LOWER(psv.value)) = TRIM(LOWER(_value));

        IF (p_value_id IS NULL) THEN RETURN 'value_not_exists'; END IF;

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

            -- Check if value already exists
            SELECT COUNT(*) > 0 INTO p_exists
            FROM parameter_value_links
            WHERE parameter_id = _param_id AND dependent_value_id = p_dependent_value_id AND value_id = p_value_id;

            IF (p_exists) THEN RETURN 'value_already_exists'; END IF;

            -- Link created value to project parameter
            INSERT INTO parameter_value_links ("parameter_id", "dependent_value_id", "value_id")
            VALUES (_param_id, p_dependent_value_id, p_value_id);
        ELSE
            -- Check if value already exists
            SELECT COUNT(*) > 0 INTO p_exists
            FROM parameter_value_links
            WHERE parameter_id = _param_id AND value_id = p_value_id;

            IF (p_exists) THEN RETURN 'value_already_exists'; END IF;

            -- Link created value to project parameter
            INSERT INTO parameter_value_links ("parameter_id", "value_id")
            VALUES (_param_id, p_value_id);
        END IF;
    END IF;

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
    END IF;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Rename project parameter value
CREATE OR REPLACE FUNCTION rename_project_parameter_value(_param_id bigint, _old_value text, _new_value text) RETURNS text AS $$
DECLARE
    p_param parameters;
    p_exists boolean;
    p_source_id bigint;
    p_value_id bigint;
BEGIN
    -- Get project parameter
    SELECT * INTO p_param FROM parameters WHERE id = _param_id;
    IF (p_param.id IS NULL) THEN RETURN 'parameter_not_exists'; END IF;

    -- Check if parameter is a top-level
    IF (p_param.parent_id IS NOT NULL) THEN RETURN 'invalid_parameter'; END IF;

    -- Get linked parameter source
    SELECT source_id INTO p_source_id FROM parameter_links WHERE parameter_id = _param_id;

    -- Get parameter value
    SELECT id INTO p_value_id
    FROM parameter_source_values
    WHERE source_id = p_source_id AND TRIM(LOWER("value")) = TRIM(LOWER(_old_value));

    IF (p_value_id IS NULL) THEN RETURN 'value_not_exists'; END IF;

    -- Check if new parameter value already exists
    SELECT COUNT(*) > 0 INTO p_exists
    FROM parameter_source_values
    WHERE source_id = p_source_id AND id <> p_value_id AND TRIM(LOWER("value")) = TRIM(LOWER(_new_value));

    IF (p_exists) THEN RETURN 'value_already_exists'; END IF;

    -- Rename parameter source value
    UPDATE parameter_source_values SET "value" = TRIM(_new_value) WHERE id = p_value_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

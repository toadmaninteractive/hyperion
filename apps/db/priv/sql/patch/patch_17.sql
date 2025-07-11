-- Patch SQL
-- Revision: 16 -> 17

-- Link setup to project parameter
CREATE OR REPLACE FUNCTION link_setup_to_project_parameter(_setup_id bigint, _param_id bigint) RETURNS text AS $$
DECLARE
    p_setup setup_steps;
    p_param parameters;
BEGIN
    -- Get setup step
    SELECT * INTO p_setup FROM setup_steps WHERE id = _setup_id;
    IF (p_setup.id IS NULL) THEN RETURN 'setup_step_not_exists'; END IF;

    -- Get project parameter
    SELECT * INTO p_param FROM parameters WHERE id = _param_id;
    IF (p_param.id IS NULL) THEN RETURN 'parameter_not_exists'; END IF;

    -- Check if setup step and parameter belong to the same project
    IF (p_setup.project_id <> p_param.project_id) THEN RETURN 'project_mismatch'; END IF;

    -- Link setup step to project parameter
    INSERT INTO parameter_setup_links (setup_id, parameter_id)
    VALUES (_setup_id, _param_id)
    ON CONFLICT (setup_id) DO UPDATE SET parameter_id = _param_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

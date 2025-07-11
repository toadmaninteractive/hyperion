-- Patch SQL
-- Revision: 26 -> 27

-- Retrieve file attachment IDs within project
CREATE OR REPLACE FUNCTION get_project_file_attachment_ids(_project_id bigint) RETURNS SETOF bigint AS $$
BEGIN
    RETURN QUERY SELECT DISTINCT attachment_id FROM (
        SELECT fal1.attachment_id FROM file_attachment_links AS fal1
        LEFT OUTER JOIN setup_steps AS ss ON (ss.id = fal1.linked_id)
        WHERE fal1.owner = 'setup_step' AND ss.project_id = _project_id
        UNION
        SELECT fal2.attachment_id FROM file_attachment_links AS fal2
        LEFT OUTER JOIN test_cases AS tc ON (tc.id = fal2.linked_id)
        WHERE fal2.owner = 'test_case' AND tc.project_id = _project_id
        UNION
        SELECT fal3.attachment_id FROM file_attachment_links AS fal3
        LEFT OUTER JOIN test_runs AS tr1 ON (tr1.id = fal3.linked_id)
        WHERE fal3.owner = 'test_run' AND tr1.project_id = _project_id
        UNION
        SELECT fal4.attachment_id FROM file_attachment_links AS fal4
        LEFT OUTER JOIN test_run_items AS tri ON (tri.id = fal4.linked_id)
        LEFT OUTER JOIN test_runs AS tr2 ON (tr2.id = tri.run_id)
        WHERE fal4.owner = 'test_run_item' AND tr2.project_id = _project_id
    ) AS qq;
END;
$$ LANGUAGE plpgsql STABLE;

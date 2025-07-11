-- Patch SQL
-- Revision: 8 -> 9

-- Add is_draft and order_num columns and related indexes to setup_steps table
ALTER TABLE setup_steps ADD COLUMN is_draft boolean NOT NULL DEFAULT FALSE;
ALTER TABLE setup_steps ADD COLUMN order_num integer NOT NULL DEFAULT 1000;

CREATE INDEX sets_is_draft_index ON setup_steps (is_draft);
CREATE INDEX sets_order_num_index ON setup_steps (order_num);

-- Add precondition_id, is_draft and order_num columns and related indexes to test_cases table
ALTER TABLE test_cases ADD COLUMN precondition_id bigint DEFAULT NULL REFERENCES setup_steps (id);
ALTER TABLE test_cases ADD COLUMN is_draft boolean NOT NULL DEFAULT FALSE;
ALTER TABLE test_cases ADD COLUMN order_num integer NOT NULL DEFAULT 1000;

CREATE INDEX tc_precondition_id_index ON test_cases (precondition_id);
CREATE INDEX tc_is_draft_index ON test_cases (is_draft);
CREATE INDEX tc_order_num_index ON test_cases (order_num);

-- Add order_num column and related index to test_run_items table
ALTER TABLE test_run_items ADD COLUMN order_num integer NOT NULL DEFAULT 1000;

CREATE INDEX tri_order_num_index ON test_run_items (order_num);
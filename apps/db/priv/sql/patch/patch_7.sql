-- Patch SQL
-- Revision: 6 -> 7

-- Problem: new values cannot be added to enum type in transactions
-- Solution: create new enum, switch to it, drop old enum, rename new enum into old

-- Create test run status enum
CREATE TYPE test_run_status_t AS ENUM (
    'draft',
    'in_progress',
    'closed'
);

-- Create temporary enum
CREATE TYPE test_status_t_new AS ENUM (
    'pending',
    'in_progress',
    'passed',
    'failed',
    'blocked'
);

-- Set all non-draft test runs' status to draft
UPDATE test_runs SET status = 'in_progress' WHERE status <> 'draft';

-- Switch status column in test_runs table
ALTER TABLE test_runs ALTER COLUMN status DROP DEFAULT;
ALTER TABLE test_runs ALTER COLUMN status SET DATA TYPE test_run_status_t USING status::text::test_run_status_t;
ALTER TABLE test_runs ALTER COLUMN status SET DEFAULT 'draft';

-- Set all draft test run items' status to draft
UPDATE test_run_items SET status = 'pending' WHERE status = 'draft';

-- Switch status column in test_run_items table
ALTER TABLE test_run_items ALTER COLUMN status DROP DEFAULT;
ALTER TABLE test_run_items ALTER COLUMN status SET DATA TYPE test_status_t_new USING status::text::test_status_t_new;
ALTER TABLE test_run_items ALTER COLUMN status SET DEFAULT 'pending';

-- Replace existing test_status_t enum with test_status_t_new
DROP TYPE IF EXISTS test_status_t;
ALTER TYPE test_status_t_new RENAME TO test_status_t;

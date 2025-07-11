-- Patch SQL
-- Revision: 5 -> 6

-- Problem: new values cannot be added to enum type in transactions
-- Solution: create new enum, switch to it, drop old enum, rename new enum into old

-- Create temporary enum
CREATE TYPE test_status_t_new AS ENUM (
    'draft',
    'pending',
    'in_progress',
    'passed',
    'failed',
    'blocked'
);

-- Switch status column in test_runs table to test_status_t_new enum
ALTER TABLE test_runs ALTER COLUMN status DROP DEFAULT;
ALTER TABLE test_runs ALTER COLUMN status SET DATA TYPE test_status_t_new USING status::text::test_status_t_new;
ALTER TABLE test_runs ALTER COLUMN status SET DEFAULT 'draft';

-- Switch status column in test_run_items table to test_status_t_new enum
ALTER TABLE test_run_items ALTER COLUMN status DROP DEFAULT;
ALTER TABLE test_run_items ALTER COLUMN status SET DATA TYPE test_status_t_new USING status::text::test_status_t_new;
ALTER TABLE test_run_items ALTER COLUMN status SET DEFAULT 'draft';

-- Replace existing test_status_t enum with test_status_t_new
DROP TYPE IF EXISTS test_status_t;
ALTER TYPE test_status_t_new RENAME TO test_status_t;

-- Patch SQL
-- Revision: 9 -> 10

-- Create attachment owner enum
CREATE TYPE attachment_owner_t AS ENUM (
    'setup_step',
    'test_case',
    'test_run',
    'test_run_item'
);

-- File attachments
CREATE TABLE file_attachments
(
    id bigserial PRIMARY KEY,
    file_size bigint NOT NULL CHECK (file_size >= 0),
    file_sha varchar NOT NULL CHECK (TRIM(file_sha) <> ''),
    filename varchar NOT NULL CHECK (TRIM(filename) <> ''),
    thumb_filename varchar,
    original_filename varchar,
    content_type varchar,
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    created_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX fa_file_hash_index ON file_attachments (file_size, file_sha);
CREATE INDEX fa_personnel_id_index ON file_attachments (personnel_id);
CREATE INDEX fa_created_at_index ON file_attachments (created_at);

-- File attachment links
CREATE TABLE file_attachment_links
(
    attachment_id bigint NOT NULL REFERENCES file_attachments (id),
    owner attachment_owner_t NOT NULL,
    linked_id bigint NOT NULL CHECK (linked_id > 0),
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    PRIMARY KEY (attachment_id, owner, linked_id)
) WITHOUT OIDS;

CREATE INDEX fal_personnel_id_index ON file_attachment_links (personnel_id);
CREATE INDEX fal_created_at_index ON file_attachment_links (created_at);

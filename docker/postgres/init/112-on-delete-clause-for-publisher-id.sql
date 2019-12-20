ALTER TABLE gigs
DROP CONSTRAINT gigs_publisher_id_fkey;

ALTER TABLE gigs
ADD CONSTRAINT gigs_publisher_id_fkey
FOREIGN KEY (publisher_id)
REFERENCES users(id)
ON DELETE SET NULL;

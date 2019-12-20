ALTER TABLE gigs ADD COLUMN date DATE;

UPDATE gigs SET date = gig_datetime::date;

ALTER TABLE gigs DROP COLUMN gig_datetime;
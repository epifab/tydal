ALTER TABLE venues ADD COLUMN geo_location geography NULL;
UPDATE venues set geo_location = coordinates::geography;

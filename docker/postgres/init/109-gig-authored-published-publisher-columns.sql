ALTER TABLE gigs
ADD COLUMN authored timestamp without time zone not null default '2019-01-01 00:00:00',
ADD COLUMN published timestamp without time zone null,
ADD COLUMN publisher_id varchar(36) references users(id) null;

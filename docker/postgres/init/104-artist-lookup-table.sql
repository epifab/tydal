CREATE TABLE artist_lookups (
  lookup varchar(512) not null primary key,
  id varchar(36) references artists(id) on delete cascade
);

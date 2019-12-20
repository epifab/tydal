CREATE TABLE spotify_artists (
  artist_id varchar(36) primary key references artists(id) on delete cascade,
  external_id varchar(128) null,
  tags varchar(128)[] null,
  picture_url varchar(256) null
);

CREATE TABLE lastfm_artists (
  artist_id varchar(36) primary key references artists(id) on delete cascade,
  external_id varchar(128) null,
  tags varchar(128)[] null,
  picture_url varchar(256) null
);

CREATE VIEW artists_with_metadata AS
SELECT
a.id AS id,
a.name AS name,
la.external_id AS lastfm_id,
sa.external_id AS spotify_id,
ARRAY(SELECT DISTINCT UNNEST(
  (
    case when sa.tags is null then '{}'::varchar[]
    else sa.tags end
  ) || (
    case when la.tags is null then '{}'::varchar[]
    else la.tags end
  )
)) AS tags,
case when sa.picture_url is not null then sa.picture_url
when la.picture_url is not null then la.picture_url
else null end as picture_url
FROM
artists a
LEFT JOIN lastfm_artists la ON la.artist_id = a.id
LEFT JOIN spotify_artists sa ON sa.artist_id = a.id;

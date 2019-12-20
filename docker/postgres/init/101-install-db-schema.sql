CREATE TYPE import_status as ENUM('SCHEDULED', 'ABORTED', 'FAILED', 'SUCCESSFUL');
CREATE TYPE log_level as ENUM('DEBUG', 'INFO', 'WARNING', 'ERROR');

CREATE TABLE imports (
  id varchar(36) primary key,
  import_datetime timestamp without time zone not null,
  location varchar(256) not null,
  gigs_date timestamp without time zone not null,
  status import_status not null
);

CREATE TABLE import_logs (
  id varchar(36) primary key,
  import_id varchar(36) not null references imports(id) on delete cascade,
  log_datetime timestamp without time zone not null,
  level log_level not null,
  message text not null,
  mdc json
);

CREATE TABLE users (
  id varchar(36) primary key,
  idp varchar(64),
  idp_id varchar(256),
  first_name varchar(256),
  middle_name varchar(256),
  last_name varchar(256),
  email varchar(256),
  picture_url varchar(256),
  spotify_id varchar(128),
  unique (idp, idp_id)
);

CREATE TABLE venues (
  id varchar(36) primary key,
  location varchar(256) not null,
  name varchar(256) not null,
  address varchar(512) null,
  coordinates geometry null
);

CREATE TABLE venue_lookups (
  lookup varchar(512) not null primary key,
  id varchar(36) references venues(id) on delete cascade
);

CREATE TABLE artists (
  id varchar(36) primary key,
  name varchar(256) not null,
  tags varchar(256)[] not null,
  image_url varchar(512) null,
  spotify_id varchar(128) null,
  lastfm_id varchar(128) null
);

CREATE TABLE gigs (
  id varchar(36) primary key,
  uri varchar(256) not null,
  venue_id varchar(36) references venues(id) not null,
  gig_datetime timestamp without time zone not null,
  unique (uri)
);

CREATE TABLE gigs_artists (
  gig_id varchar(36) references gigs(id) on delete cascade,
  artist_id varchar(36) references artists(id) on delete cascade,
  index int,
  primary key (gig_id, artist_id)
);

CREATE TABLE user_gig_reaction_events (
  user_id varchar(36) references users(id) on delete cascade,
  gig_id varchar(36) references gigs(id) on delete cascade,
  reaction varchar(64),
  reaction_datetime timestamp without time zone default (now() at time zone 'utc'),
  primary key (user_id, gig_id, reaction_datetime)
);

CREATE VIEW user_gig_reactions AS
SELECT r.*
FROM user_gig_reaction_events r
INNER JOIN (
  SELECT
    user_id,
    gig_id,
    MAX(reaction_datetime) AS last_datetime
  FROM user_gig_reaction_events
  GROUP BY user_id, gig_id
) as mr
ON mr.user_id = r.user_id
  AND mr.gig_id = r.gig_id
  AND mr.last_datetime = r.reaction_datetime;

CREATE VIEW gig_tags AS
SELECT
   g.id AS gig_id,
   toc.tag AS tag,
   toc.occurrences::float / tc.total::float AS relevance
 FROM gigs g
 INNER JOIN (SELECT
    ga.gig_id AS gig_id,
    COUNT(atags.tag) AS total
   FROM gigs_artists ga
   INNER JOIN (SELECT id, UNNEST(tags) AS tag FROM artists) atags ON atags.id = ga.artist_id
   GROUP BY ga.gig_id) AS tc ON tc.gig_id = g.id
 INNER JOIN (SELECT
    ga.gig_id AS gig_id,
    atags.tag AS tag,
    COUNT(atags.tag) as occurrences
   FROM gigs_artists ga
   INNER JOIN (SELECT id, UNNEST(tags) AS tag FROM artists) atags ON atags.id = ga.artist_id
   GROUP BY ga.gig_id, atags.tag) AS toc ON toc.gig_id = g.id;

CREATE VIEW user_tag_reactions AS
SELECT
  r.user_id AS user_id,
  gt.tag AS tag,
  r.reaction as reaction,
  SUM(gt.relevance) as score
FROM user_gig_reactions r
INNER JOIN gig_tags gt ON gt.gig_id = r.gig_id
GROUP BY r.user_id, gt.tag, r.reaction;

CREATE VIEW user_tag_scores AS
SELECT
  tr.user_id AS user_id,
  tr.tag AS tag,
  SUM(case
    when tr.reaction = 'LOVE' then tr.score
    when tr.reaction = 'HATE' then -(tr.score)
    else 0.0
  end) AS score
FROM user_tag_reactions tr
GROUP BY tr.user_id, tr.tag;

CREATE MATERIALIZED VIEW user_gig_likeability AS
SELECT
  ut1.user_id,
  gt1.gig_id,
  SUM(ut1.score * gt1.relevance) as score
FROM user_tag_scores ut1
INNER JOIN gig_tags gt1
  ON gt1.tag = ut1.tag
GROUP BY ut1.user_id, gt1.gig_id;

CREATE VIEW gig_users_meta AS
SELECT
 g1.id AS gig_id,
 u1.id AS user_id,
 (case when ugl.score is null then 0 else ugl.score end) AS score,
 ugr.reaction AS reaction
FROM gigs g1 CROSS JOIN users u1
-- likeability
LEFT JOIN user_gig_likeability ugl
  ON ugl.gig_id = g1.id
  AND ugl.user_id = u1.id
-- reaction
LEFT JOIN user_gig_reactions ugr
  ON ugr.gig_id = g1.id
  AND ugr.user_id = u1.id;

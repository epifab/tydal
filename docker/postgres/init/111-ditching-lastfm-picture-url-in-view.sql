DROP VIEW artists_with_metadata CASCADE;

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
-- when la.picture_url is not null then la.picture_url
else null end as picture_url
FROM
artists a
LEFT JOIN lastfm_artists la ON la.artist_id = a.id
LEFT JOIN spotify_artists sa ON sa.artist_id = a.id;

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
   INNER JOIN (SELECT id, UNNEST(tags) AS tag FROM artists_with_metadata) atags ON atags.id = ga.artist_id
   GROUP BY ga.gig_id) AS tc ON tc.gig_id = g.id
 INNER JOIN (SELECT
    ga.gig_id AS gig_id,
    atags.tag AS tag,
    COUNT(atags.tag) as occurrences
   FROM gigs_artists ga
   INNER JOIN (SELECT id, UNNEST(tags) AS tag FROM artists_with_metadata) atags ON atags.id = ga.artist_id
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

CREATE TABLE users_idp (
  idp_type varchar(64),
  idp_id varchar(256),
  user_id varchar(36) REFERENCES users(id) ON DELETE CASCADE,
  UNIQUE(user_id, idp_type),
  PRIMARY KEY(idp_type, idp_id)
);

INSERT INTO users_idp (user_id, idp_type, idp_id)
SELECT id, idp, idp_id FROM users;

ALTER TABLE users
DROP CONSTRAINT users_idp_idp_id_key,
DROP COLUMN idp,
DROP COLUMN idp_id;

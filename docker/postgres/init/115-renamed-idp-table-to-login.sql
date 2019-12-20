CREATE TABLE users_login(
  provider varchar(64),
  external_id varchar(256),
  user_id varchar(36) REFERENCES users(id) ON DELETE CASCADE,
  UNIQUE(user_id, provider),
  PRIMARY KEY(provider, external_id)
);

INSERT INTO users_login (provider, external_id, user_id)
SELECT idp_type, idp_id, user_id FROM users_idp;

DROP TABLE users_idps;
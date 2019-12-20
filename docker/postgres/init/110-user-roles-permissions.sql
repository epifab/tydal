CREATE TABLE permissions (
  id VARCHAR(128) PRIMARY KEY,
  description VARCHAR(256)
);

CREATE TABLE roles (
  id VARCHAR(128) PRIMARY KEY,
  description VARCHAR(256)
);

CREATE TABLE roles_permissions(
  role_id VARCHAR(128) REFERENCES roles(id) ON DELETE CASCADE,
  permission_id VARCHAR(128) REFERENCES permissions(id) ON DELETE CASCADE,
  PRIMARY KEY(role_id, permission_id)
);

CREATE TABLE users_roles(
  user_id VARCHAR(36) REFERENCES users(id) ON DELETE CASCADE,
  role_id VARCHAR(128) REFERENCES roles(id) ON DELETE CASCADE,
  PRIMARY KEY(user_id, role_id)
);

INSERT INTO permissions VALUES
('gigs-create', 'Can create a new gig'),
('gigs-view-all', 'Can view any existing gig'),
('gigs-publish', 'Can publish any existing gig'),
('gigs-unpublish', 'Can unpublish any existing gig');

INSERT INTO roles VALUES
('authors', 'Authors'),
('publishers', 'Publishers');

INSERT INTO roles_permissions VALUES
('authors', 'gigs-create'),
('publishers', 'gigs-view-all'),
('publishers', 'gigs-publish'),
('publishers', 'gigs-unpublish');

-- INSERT INTO users_roles VALUES
-- ('b423e4ed-b7ca-4767-9523-f420656e4a5f', 'authors'),
-- ('b423e4ed-b7ca-4767-9523-f420656e4a5f', 'publishers');

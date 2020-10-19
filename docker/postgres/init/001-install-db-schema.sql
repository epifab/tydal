CREATE TYPE interest as ENUM('music', 'art', 'history', 'math');

create table students(
  id uuid primary key,
  name varchar(128) not null,
  email varchar(128),
  date_of_birth date not null,
  interests interest[] not null,
  address json
);

create table courses(
  id uuid primary key,
  name varchar(128) not null
);

create table exams(
  student_id uuid references students(id),
  course_id uuid references courses(id),
  score int,
  exam_timestamp timestamp without time zone,
  registration_timestamp timestamp without time zone,
  primary key (student_id, course_id)
);

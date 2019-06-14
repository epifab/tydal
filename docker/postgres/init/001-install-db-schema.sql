CREATE TYPE interest as ENUM('music', 'art', 'history', 'math');

create table students(
  id int primary key,
  name varchar(128) not null,
  email varchar(128),
  date_of_birth date not null,
  interests interest[] not null,
  address json
);

create table exams(
  student_id int,
  course_id int,
  score int,
  exam_timestamp timestamp without time zone,
  registration_timestamp timestamp without time zone,
  primary key (student_id, course_id)
);

create table courses(
  id int primary key,
  name varchar(128) not null
);

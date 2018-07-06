create table students(
  id int primary key,
  name varchar(128) not null,
  email varchar(128),
  interests varchar(128)[] not null,
  address json
);

create table exams(
  student_id int,
  course_id int,
  rate int,
  exam_timestamp timestamp without time zone,
  primary key (student_id, course_id)
);

create table courses(
  id int primary key,
  name varchar(128) not null
);

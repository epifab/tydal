create table students(
  id int primary key,
  name varchar(128) not null,
  email varchar(128),
  interests varchar(128)[] not null,
  address varchar(1024) not null
);

create table exams(
  student_id int,
  course_id int,
  rate int,
  primary key (student_id, course_id)
);

create table course(
  course_id int,
  name varchar(128) not null
);

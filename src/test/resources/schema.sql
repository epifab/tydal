create table hd_students(
  id int primary key,
  name varchar(128) not null,
  email varchar(128)
);

create table hd_exams(
  student_id int,
  course_id int,
  rate int,
  primary key (student_id, course_id)
);

create table hd_course(
  course_id int,
  name varchar(128) not null
);

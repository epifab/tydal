FROM hseeberger/scala-sbt:11.0.6_1.3.10_2.13.1

WORKDIR /tmp
ADD . .
RUN sbt update

WORKDIR /user/root

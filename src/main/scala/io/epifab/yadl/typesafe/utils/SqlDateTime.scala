package io.epifab.yadl.typesafe.utils

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

object SqlDateTime {
  val formatter: DateTimeFormatter = new DateTimeFormatterBuilder()
    .appendPattern("yyyy-MM-dd")
    .appendLiteral("T")
    .appendPattern("HH:mm:ss.SSSSSSSSS")
    .toFormatter()

  val parser: DateTimeFormatter = new DateTimeFormatterBuilder()
    .appendPattern("yyyy-MM-dd")
    .appendLiteral(" ")
    .appendPattern("HH:mm:ss")
    // optional nanos, with 9, 6 or 3 digits
    .appendPattern("[.SSSSSSSSS][.SSSSSSSS][.SSSSSSS][.SSSSSS][.SSSSS][.SSSS][.SSS][.SS][.S]")
    .toFormatter()
}
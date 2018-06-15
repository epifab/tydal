package io.epifab.yadl.domain

sealed trait DALError extends Throwable

final case class ExtractorError(msg: String) extends Error(msg) with DALError
final case class DriverError(cause: Throwable) extends Error(cause) with DALError

package io.epifab.yadl.domain

sealed trait DALError extends Throwable

case class ExtractorError private(msg: String, cause: Option[Throwable])
  extends RuntimeException(msg, cause.orNull) with DALError

case class DriverError private(cause: Throwable, querySql: Option[String])
  extends RuntimeException(s"Database error: ${cause.getMessage}", cause) with DALError

object ExtractorError {
  def apply(msg: String): ExtractorError = new ExtractorError(msg, None)
  def apply(msg: String, cause: Throwable): ExtractorError = new ExtractorError(msg, Some(cause))
}

object DriverError {
  def apply(cause: Throwable): DriverError = new DriverError(cause, None)
  def apply(cause: Throwable, querySql: String) = new DriverError(cause, Some(querySql))
}

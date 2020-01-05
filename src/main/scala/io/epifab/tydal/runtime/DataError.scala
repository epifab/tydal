package io.epifab.tydal.runtime

trait DataError extends RuntimeException

case class DecoderError(reason: String) extends RuntimeException(reason) with DataError
case class DriverError(reason: String) extends RuntimeException(reason) with DataError
case class MultipleResultsError(reason: String) extends RuntimeException(reason) with DataError
case class NoResultsError(reason: String) extends RuntimeException(reason) with DataError
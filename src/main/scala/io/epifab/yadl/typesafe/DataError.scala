package io.epifab.yadl.typesafe

trait DataError extends RuntimeException

case class DecoderError(reason: String) extends RuntimeException(reason) with DataError
case class DriverError(reason: String) extends RuntimeException(reason) with DataError

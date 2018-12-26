package io.epifab.yadl.utils

import org.slf4j.{Logger, LoggerFactory, MDC}

trait LoggingSupport {
  protected def log: Logger = LoggerFactory.getLogger(getClass)

  def withMdc(mdc: Map[String, String])(block: => Unit): Unit = {
    val mdcCopy: Map[String, Option[String]] = mdc.keys.map(key => key -> Option(MDC.get(key))).toMap
    try {
      mdc.foreach { case (k, v) => MDC.put(k, v) }
      block
    } finally {
      mdcCopy.foreach {
        case (k, Some(v)) => MDC.put(k, v)
        case (k, None) => MDC.remove(k)
      }
    }
  }
}

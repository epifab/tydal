package io.epifab.yadl.utils

import org.slf4j.{Logger, LoggerFactory, MDC}

trait LoggingSupport {
  protected def log: Logger = LoggerFactory.getLogger(getClass)

  def withMdc(mdc: Map[String, String])(block: => Unit): Unit = {
    val context = MDC.getCopyOfContextMap
    mdc.foreach { case (k, v) => MDC.put(k, v) }
    block
    MDC.setContextMap(context)
  }
}

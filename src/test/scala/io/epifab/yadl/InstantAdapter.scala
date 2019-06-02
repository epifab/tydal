package io.epifab.yadl

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneOffset}

import io.epifab.yadl.domain.FieldAdapter
import org.scalatest.{FlatSpec, Matchers}

class InstantAdapter extends FlatSpec with Matchers {
  private val baseDate = LocalDateTime.of(2020, 3, 29, 15, 24, 32, 0)
  private val baseDateStr = baseDate.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
  private val instantAdapter = FieldAdapter.instant

  private def toInstant(date: LocalDateTime): Instant = date.toInstant(ZoneOffset.UTC)

  it should "parse dates with any number of digits for nanoseconds" in {
    instantAdapter.read(baseDateStr) shouldBe Right(toInstant(baseDate))
    instantAdapter.read(s"$baseDateStr.1") shouldBe Right(toInstant(baseDate.withNano(100000000)))
    instantAdapter.read(s"$baseDateStr.11") shouldBe Right(toInstant(baseDate.withNano(110000000)))
    instantAdapter.read(s"$baseDateStr.111") shouldBe Right(toInstant(baseDate.withNano(111000000)))
    instantAdapter.read(s"$baseDateStr.1111") shouldBe Right(toInstant(baseDate.withNano(111100000)))
    instantAdapter.read(s"$baseDateStr.11111") shouldBe Right(toInstant(baseDate.withNano(111110000)))
    instantAdapter.read(s"$baseDateStr.111111") shouldBe Right(toInstant(baseDate.withNano(111111000)))
    instantAdapter.read(s"$baseDateStr.1111111") shouldBe Right(toInstant(baseDate.withNano(111111100)))
    instantAdapter.read(s"$baseDateStr.11111111") shouldBe Right(toInstant(baseDate.withNano(111111110)))
    instantAdapter.read(s"$baseDateStr.111111111") shouldBe Right(toInstant(baseDate.withNano(111111111)))
  }
}

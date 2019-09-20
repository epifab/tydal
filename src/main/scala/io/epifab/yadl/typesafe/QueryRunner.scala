package io.epifab.yadl.typesafe

import java.sql.{Connection, PreparedStatement, ResultSet}

import io.epifab.yadl.typesafe.AsyncQueryRunner.FutureEither
import io.epifab.yadl.typesafe.fields._
import io.epifab.yadl.typesafe.runner.DataExtractor

import scala.concurrent.{ExecutionContext, Future}
import scala.language.reflectiveCalls

//class TaggedList[L <: HList] private(val placeholders: L) extends FindContext[L] {
//  def add[X, NAME <: String](tagged: X with Tag[NAME]): TaggedList[X with Tag[NAME] :: L] =
//    new TaggedList(tagged :: placeholders)
//
//  override def apply[TAG <: String](implicit tag: ValueOf[TAG]): FindByTag[TAG, L] =
//    new FindByTag(placeholders)
//}
//
//object TaggedList {
//  val empty: TaggedList[HNil] = new TaggedList(HNil)
//}

trait QueryRunner[F[+_, +_], -INPUT, +ERROR, +OUTPUT] {
  def run(input: INPUT): F[ERROR, Iterable[OUTPUT]]
}

object AsyncQueryRunner {
  type FutureEither[+ERROR, +OUTPUT] = Future[Either[ERROR, OUTPUT]]
}

class AsyncJdbcQueryRunner[FIELDS, -INPUT, +OUTPUT]
    (connection: Connection, query: String, fields: FIELDS)
    (implicit
     dataExtractor: DataExtractor[ResultSet, FIELDS, OUTPUT],
     executionContext: ExecutionContext) extends QueryRunner[FutureEither, INPUT, DecoderError, OUTPUT] {

  private lazy val preparedStatement: PreparedStatement = connection.prepareStatement(query)

  private def extract(resultSet: ResultSet): Either[DecoderError, Seq[OUTPUT]] = {
    import io.epifab.yadl.utils.MonadicOps._

    val iterator = new Iterator[Either[DecoderError, OUTPUT]] {
      override def hasNext: Boolean = resultSet.next()
      override def next(): Either[DecoderError, OUTPUT] = dataExtractor.extract(resultSet, fields)
    }

    iterator.toList.getOrFirstError
  }

  override def run(input: INPUT): FutureEither[DecoderError, Iterable[OUTPUT]] =
    Future(extract(preparedStatement.executeQuery()))
}

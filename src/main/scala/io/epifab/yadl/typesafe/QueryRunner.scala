package io.epifab.yadl.typesafe

import java.sql.{Connection, PreparedStatement, ResultSet}

import io.epifab.yadl.typesafe.AsyncQueryRunner.FutureEither
import io.epifab.yadl.typesafe.fields._
import io.epifab.yadl.typesafe.runner.DataExtractor
import shapeless.HList

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

class Statement[PLACEHOLDERS <: HList](query: String, placeholders: PLACEHOLDERS)

trait QueryRunner[F[+_, +_], -INPUT, +ERROR, +OUTPUT] {
  def run(input: INPUT): F[ERROR, Iterable[OUTPUT]]
}

object QueryRunner {
  def async[FIELDS <: HList, INPUT, OUTPUT]
    (select: Select[FIELDS, _ <: HList, _ <: HList, _ <: BinaryExpr])
    (implicit
     connection: Connection,
     dataExtractor: DataExtractor[ResultSet, FIELDS, OUTPUT],
     executionContext: ExecutionContext): QueryRunner[FutureEither, INPUT, DecoderError, OUTPUT] =
    new AsyncJdbcQueryRunner(select.query, select.fields)
}

object AsyncQueryRunner {
  type FutureEither[+ERROR, +OUTPUT] = Future[Either[ERROR, OUTPUT]]
}

class AsyncJdbcQueryRunner[FIELDS, -INPUT, +OUTPUT]
    (query: Query, fields: FIELDS)
    (implicit
     connection: Connection,
     dataExtractor: DataExtractor[ResultSet, FIELDS, OUTPUT],
     executionContext: ExecutionContext) extends QueryRunner[FutureEither, INPUT, DecoderError, OUTPUT] {

  private def preparedStatement: PreparedStatement = connection.prepareStatement(query.sql)

  private def extract(resultSet: ResultSet): Either[DecoderError, Seq[OUTPUT]] = {
    import io.epifab.yadl.utils.MonadicOps._

    val iterator = new Iterator[Either[DecoderError, OUTPUT]] {
      override def hasNext: Boolean = resultSet.next()
      override def next(): Either[DecoderError, OUTPUT] = dataExtractor.extract(resultSet, fields)
    }

    iterator.toList.getOrFirstError
  }

  def set[U](statement: PreparedStatement, paramIndex: Int, dbType: FieldType[U], value: U): Unit = {
    dbType match {
      case TypeString | TypeDate | TypeDateTime | TypeJson | TypeEnum(_) | TypeGeography | TypeGeometry =>
        statement.setObject(paramIndex, value)

      case TypeInt =>
        statement.setInt(paramIndex, value)

      case TypeDouble =>
        statement.setDouble(paramIndex, value)

      case TypeSeq(innerType) =>
//        setSeq(statement, paramIndex, innerType, value)

      case TypeOption(innerDbType) =>
//        Option(statement.getObject(fieldName))
//          .map(_ => get(innerDbType, fieldName, statement))
    }
  }

  override def run(input: INPUT): FutureEither[DecoderError, Iterable[OUTPUT]] =
    ???
//    Future {
//      val statement = connection.prepareCall(query.sql)
//
//      val placeholderValues = tagged.toMap(input)
//      query.placeholders.zipWithIndex.foreach { case (placeholder, index) =>
//        set(statement, index, placeholder.encoder.dbType, placeholderValues.get(placeholder.name))
//      }
//
//      extract(preparedStatement.executeQuery())
//    }
}

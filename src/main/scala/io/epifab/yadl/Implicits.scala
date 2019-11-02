package io.epifab.yadl

import io.epifab.yadl.fields._

object Implicits {
  implicit class ExtendedTag[A <: String with Singleton](tag: A)(implicit valueOf: ValueOf[A]) {
    def ~~>[T](value: T)(implicit fieldEncoder: FieldEncoder[T]): Value[T] with Tag[A] = Value(tag, value)
    def ?[T](implicit fieldEncoder: FieldEncoder[T], fieldDecoder: FieldDecoder[T]): Placeholder[T, T] with Tag[A] = Placeholder[T, A]
  }

  implicit class ExtendedField[F1 <: Field[_]](field1: F1) {
    def ===[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): Equals[F1, F2] =
      Equals(field1, field2)

    def !==[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): NotEquals[F1, F2] =
      NotEquals(field1, field2)

    def <[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): LessThan[F1, F2] =
      LessThan(field1, field2)

    def >[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): GreaterThan[F1, F2] =
      GreaterThan(field1, field2)

    def <=[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): LessThanOrEqual[F1, F2] =
      LessThanOrEqual(field1, field2)

    def >=[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): GreaterThanOrEqual[F1, F2] =
      GreaterThanOrEqual(field1, field2)

    def subsetOf[F2 <: Field[_]](field2: F2)(implicit canBeSubset: CanBeSubset[F1, F2]): IsSubset[F1, F2] =
      IsSubset(field1, field2)

    def supersetOf[F2 <: Field[_]](field2: F2)(implicit canBeSuperset: CanBeSuperset[F1, F2]): IsSuperset[F1, F2] =
      IsSuperset(field1, field2)

    def overlaps[F2 <: Field[_]](field2: F2)(implicit canOverlap: CanOverlap[F1, F2]): Overlaps[F1, F2] =
      Overlaps(field1, field2)

    def in[F2 <: Field[_]](field2: F2)(implicit canBeIncluded: CanBeIncluded[F1, F2]): IsIncluded[F1, F2] =
      IsIncluded(field1, field2)

    def ===[NAME <: String with Singleton, T]
        (placeholderName: NAME)
        (implicit
         valueOf: ValueOf[NAME],
         fieldT: FieldT[F1, T],
         fieldEncoder: FieldEncoder[T],
         fieldDecoder: FieldDecoder[T],
         comparable: AreComparable[F1, Placeholder[T, T] with Tag[NAME]]): Equals[F1, Placeholder[T, T] with Tag[NAME]] =
      Equals(field1, Placeholder[T, NAME])

    def !==[NAME <: String with Singleton, T]
        (placeholderName: NAME)
        (implicit
         valueOf: ValueOf[NAME],
         fieldT: FieldT[F1, T],
         fieldEncoder: FieldEncoder[T],
         fieldDecoder: FieldDecoder[T],
         comparable: AreComparable[F1, Placeholder[T, T] with Tag[NAME]]): NotEquals[F1, Placeholder[T, T] with Tag[NAME]] =
      NotEquals(field1, Placeholder[T, NAME])

    def <[NAME <: String with Singleton, T]
        (placeholderName: NAME)
        (implicit
         valueOf: ValueOf[NAME],
         fieldT: FieldT[F1, T],
         fieldEncoder: FieldEncoder[T],
         fieldDecoder: FieldDecoder[T],
         comparable: AreComparable[F1, Placeholder[T, T] with Tag[NAME]]): LessThan[F1, Placeholder[T, T] with Tag[NAME]] =
      LessThan(field1, Placeholder[T, NAME])

    def >[NAME <: String with Singleton, T]
        (placeholderName: NAME)
        (implicit
         valueOf: ValueOf[NAME],
         fieldT: FieldT[F1, T],
         fieldEncoder: FieldEncoder[T],
         fieldDecoder: FieldDecoder[T],
         comparable: AreComparable[F1, Placeholder[T, T] with Tag[NAME]]): GreaterThan[F1, Placeholder[T, T] with Tag[NAME]] =
      GreaterThan(field1, Placeholder[T, NAME])

    def <=[NAME <: String with Singleton, T]
        (placeholderName: NAME)
        (implicit
         valueOf: ValueOf[NAME],
         fieldT: FieldT[F1, T],
         fieldEncoder: FieldEncoder[T],
         fieldDecoder: FieldDecoder[T],
         comparable: AreComparable[F1, Placeholder[T, T] with Tag[NAME]]): LessThanOrEqual[F1, Placeholder[T, T] with Tag[NAME]] =
      LessThanOrEqual(field1, Placeholder[T, NAME])

    def >=[NAME <: String with Singleton, T]
        (placeholderName: NAME)
        (implicit
         valueOf: ValueOf[NAME],
         fieldT: FieldT[F1, T],
         fieldEncoder: FieldEncoder[T],
         fieldDecoder: FieldDecoder[T],
         comparable: AreComparable[F1, Placeholder[T, T] with Tag[NAME]]): GreaterThanOrEqual[F1, Placeholder[T, T] with Tag[NAME]] = GreaterThanOrEqual(field1, Placeholder[T, NAME])

    def subsetOf[NAME <: String with Singleton, T]
        (placeholderName: NAME)
        (implicit
         valueOf: ValueOf[NAME],
         fieldT: FieldT[F1, T],
         fieldEncoder: FieldEncoder[T],
         fieldDecoder: FieldDecoder[T],
         canBeSubset: CanBeSubset[F1, Placeholder[T, T] with Tag[NAME]]): IsSubset[F1, Placeholder[T, T] with Tag[NAME]] =
      IsSubset(field1, Placeholder[T, NAME])

    def supersetOf[NAME <: String with Singleton, T]
        (placeholderName: NAME)
        (implicit
         valueOf: ValueOf[NAME],
         fieldT: FieldT[F1, T],
         fieldEncoder: FieldEncoder[T],
         fieldDecoder: FieldDecoder[T],
         canBeSuperset: CanBeSuperset[F1, Placeholder[T, T] with Tag[NAME]]): IsSuperset[F1, Placeholder[T, T] with Tag[NAME]] =
      IsSuperset(field1, Placeholder[T, NAME])

    def overlaps[NAME <: String with Singleton, T]
        (placeholderName: NAME)
        (implicit
         valueOf: ValueOf[NAME],
         fieldT: FieldT[F1, T],
         fieldEncoder: FieldEncoder[T],
         fieldDecoder: FieldDecoder[T],
         canOverlap: CanOverlap[F1, Placeholder[T, T] with Tag[NAME]]): Overlaps[F1, Placeholder[T, T] with Tag[NAME]] =
      Overlaps(field1, Placeholder[T, NAME])

    def in[NAME <: String with Singleton, T]
        (placeholderName: NAME)
        (implicit
         valueOf: ValueOf[NAME],
         fieldT: FieldT[F1, T],
         fieldEncoder: FieldEncoder[Seq[T]],
         fieldDecoder: FieldDecoder[Seq[T]],
         canBeIncluded: CanBeIncluded[F1, Placeholder[Seq[T], Seq[T]] with Tag[NAME]]): IsIncluded[F1, Placeholder[Seq[T], Seq[T]] with Tag[NAME]] =
      IsIncluded(field1, Placeholder[Seq[T], NAME])
  }
}

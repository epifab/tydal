import tydal.schema._

package object tydal {

  type Tag = String with Singleton

  type As[+T, A <: String with Singleton] = T with Tagging[A]

  type :=:[A <: String with Singleton, T] = Column[T] with Tagging[A]
  type ~~>[A <: String with Singleton, T] = KeyVal[A, T]

  implicit class ExtendedTag[A <: String with Singleton](tag: A)(implicit valueOf: ValueOf[A]) {
    def ~~>[T](value: T): KeyVal[A, T] = new KeyVal(tag, value)
  }

  implicit class ExtendedFilterOption[+E <: Filter](val option: Option[E]) {
    def toFilter: FilterOption[E] = new FilterOption[E] {
      override def filter: Option[E] = option
    }
  }
}

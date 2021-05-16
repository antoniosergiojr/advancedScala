package exercises

import lectures.part4implicits.TypeClasses.User

object EqualityPlayground extends App {

  /**
   * Equality
   */
  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  implicit object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  object FullEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.email == b.email
  }

  /*
  Exercise: implement the TC pattern for the Equality tc.
 */
  object Equal {
    def apply[T](a: T, b: T)(implicit equalizer: Equal[T]): Boolean =
      equalizer.apply(a, b)
  }

  val test = User("Test", 32, "test@test.com")
  val anotherTest = User("John", 45, "anotherTest@test.com")
  println(Equal(test, anotherTest))
  // AD-HOC polymorphism

  /*
    Exercise - improve the Equal TC with an implicit conversion class
    ===(anotherValue: T)
    !==(anotherValue: T)
   */
  implicit class TypeSafeEqual[T](value: T) {
    def ===(other: T)(implicit equalizer: Equal[T]): Boolean = equalizer.apply(value, other)

    def !==(other: T)(implicit equalizer: Equal[T]): Boolean = !equalizer.apply(value, other)
  }

  println(test === anotherTest)
  /*
    john.===(anotherJohn)
    new TypeSafeEqual[User](john).===(anotherJohn)
    new TypeSafeEqual[User](john).===(anotherJohn)(NameEquality)
   */
  /*
    TYPE SAFE
   */
  println(test == 43)
  //  println(john === 43) // TYPE SAFE
}

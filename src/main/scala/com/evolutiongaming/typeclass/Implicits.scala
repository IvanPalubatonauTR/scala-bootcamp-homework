package com.evolutiongaming.typeclass

object Implicits {

  object TypeclassTask {

    trait HashCode[A] {
      def hash(a: A): Int
    }

    object HashCode {
      def apply[A](implicit instance: HashCode[A]): HashCode[A] = instance
    }

    implicit class HashCodeSyntax[A](x: A) {
      def hash(implicit j: HashCode[A]): Int = {
        j.hash(x)
      }
    }

    implicit val HashString: HashCode[String] = string => string.hashCode

    "abc".hash
  }

  object Task1 {

    final case class Money(amount: BigDecimal)

    implicit val moneyOrdering: Ordering[Money] = (x: Money, y: Money) => x.amount.toInt - y.amount.toInt
  }

  object Task2 {

    trait Show[T] {
      def show(entity: T): String
    }

    final case class User(id: String, name: String)

    object Show {
      def apply[A](implicit instance: Show[A]): Show[A] = instance
    }

    implicit class ShowSyntax[A](x: A) {
      def show(implicit j: Show[A]): String = {
        j.show(x)
      }
    }

    implicit val HashString: Show[User] = user => user.toString

    User("1", "Oleg").show
  }

  object Task3 {
    type Error = String

    trait Parse[T] {
      def parse(entity: String): Either[Error, T]
    }

    final case class User(id: String, name: String)

    object Parse {
      def apply[T](implicit instance: Parse[T]): Parse[T] = instance
    }

    implicit class ParseSyntax[T](x: String) {
      def parse(implicit j: Parse[T]): Either[Error, T] = {
        j.parse(x)
      }
    }

    implicit val HashString: Parse[User] = string => string.split("\\s+").toList match {
      case Nil => Left("Error")
      case x :: _ :: id :: _ :: name :: Nil if x == "User" => Right(User(id, name))
      case _ => Left("Error")
    }

    "lalala".parse
  }

  object Task4 {

    trait Equal[A] {
      def eq(a: A, b: A): Boolean
    }

    object Equal {
      def apply[A](implicit instance: Equal[A]): Equal[A] = instance
    }

    implicit class EqualSyntax[A](x: A) {
      def ===(y: A)(implicit j: Equal[A]): Boolean = {
        j eq(x, y)
      }
    }

    implicit val stringEq: Equal[String] = (string1: String, string2: String) => string1 == string2
    "1" === "2"
  }

  object AdvancedHomework {

    trait Task[F[_]] {
      def flatMap[A, B](x: F[A])(f: A => F[B]): F[B]
    }

  }

}

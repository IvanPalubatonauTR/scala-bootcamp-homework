package com.evolutiongaming.cats


object CatsHW {

  import cats.Monad

  object Monads {

    val optionMonad: Monad[Option] = new Monad[Option] {
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
        case None => None
        case Some(a) => f(a)
      }

      override def pure[A](x: A): Option[A] = Option(x)

      override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???
    }

    def eitherMonad[T]: Monad[Either[T, *]] = new Monad[Either[T, *]] {

      override def flatMap[A, B](fa: Either[T, A])(f: A => Either[T, B]): Either[T, B] = fa match {
        case Left(t) => Left(t)
        case Right(a) => f(a)
      }

      override def pure[A](x: A): Either[T, A] = x match {
        case t: T => Left(t)
        case a: A => Right(a)
      }

      override def tailRecM[A, B](a: A)(f: A => Either[T, Either[A, B]]): Either[T, B] = ???
    }

    def functionMonad[T]: Monad[T => *] = new Monad[T => *] {

      override def flatMap[A, B](fa: T => A)(f: A => T => B): T => B = a => f(fa(a))(a)

      override def pure[A](x: A): T => A = (t: T) => x

      override def tailRecM[A, B](a: A)(f: A => T => Either[A, B]): T => B = ???
    }
  }

  object TypeHierarchy extends App {

    import cats.Functor

    trait Applicative[F[_]] extends Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B]

      def unit[A](a: => A): F[A]

      def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

      def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a, f) => f(a))

      def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

      def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))
    }

    trait Monad[M[_]] extends Functor[M] {
      def unit[A](a: => A): M[A]

      def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

      def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

      def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

      def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = join(map(ma)(a => map(mb)(b => f(a, b))))
    }

  }

}

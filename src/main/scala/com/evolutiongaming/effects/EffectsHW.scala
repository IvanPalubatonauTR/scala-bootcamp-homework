package com.evolutiongaming.effects

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object EffectsHW extends App {

  final class IO[A](run: () => A) {
    def map[B](f: A => B): IO[B] = new IO(() => f(run()))

    def flatMap[B](f: A => IO[B]): IO[B] = f(run())

    def *>[B](another: IO[B]): IO[B] = another

    def as[B](newValue: => B): IO[B] = new IO(() => newValue)

    def void: IO[Unit] = new IO(() => ())

    def attempt: IO[Either[Throwable, A]] = run() match {
      case a: A => new IO(() => Right(a))
      case throwable: Throwable => new IO(() => Left(throwable))
    }

    def option: IO[Option[A]] = new IO(() => Option(run()))

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = ???

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = ???

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = ???

    def unsafeRunSync(): A = run()

    def unsafeToFuture(): Future[A] = ???
  }

  object IO {
    def apply[A](body: => A): IO[A] = new IO(() => body)

    def suspend[A](thunk: => IO[A]): IO[A] = thunk

    def delay[A](body: => A): IO[A] = IO(body)

    def pure[A](a: A): IO[A] = IO(a)

    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Right(a) => pure(a)
      case Left(b) => raiseError(b)
    }

    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case None => raiseError(orElse)
      case Some(value) => pure(value)
    }

    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Success(value) => pure(value)
      case Failure(value) => raiseError(value)
    }

    def none[A]: IO[Option[A]] = pure(None)

    def raiseError[A](e: Throwable): IO[A] = new IO(() => throw e)

    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = raiseWhen(!cond)(e)

    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) raiseError(e) else unit

    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = whenA(!cond)(action)

    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit

    val unit: IO[Unit] = pure(())
  }

}

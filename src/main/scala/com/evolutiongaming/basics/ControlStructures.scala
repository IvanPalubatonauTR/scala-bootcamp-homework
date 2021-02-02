package com.evolutiongaming.basics

import com.evolutiongaming.basics.ControlStructures.Command._
import com.evolutiongaming.basics.ControlStructures.CommandConstants._
import com.evolutiongaming.basics.ControlStructures.Error._

import scala.io.Source

object ControlStructures {
  val ErrorPrefix = "Error: "

  object CommandConstants {
    type CommandConstants = String
    val DIVIDE = "divide"
    val SUM = "sum"
    val AVERAGE = "average"
    val MIN = "min"
    val MAX = "max"
  }

  sealed trait Command

  object Command {

    final case class Divide(dividend: Option[Double], divisor: Option[Double]) extends Command

    final case class Sum(numbers: List[Option[Double]]) extends Command

    final case class Average(numbers: List[Option[Double]]) extends Command

    final case class Min(numbers: List[Option[Double]]) extends Command

    final case class Max(numbers: List[Option[Double]]) extends Command

  }

  sealed trait ErrorMessage {
    def value: String
  }

  object Error {

    final case class InvalidCommandError(value: String) extends ErrorMessage

    final case class DivisionByZeroError(value: String) extends ErrorMessage

    final case class CalculationError(value: String) extends ErrorMessage

  }

  final case class DataToRender(command: String, numbers: List[Option[Double]], calculatedValue: Double)

  def parseCommand(x: String): Either[ErrorMessage, Command] = {

    val listOfStrings = x.toLowerCase.split("\\s+").toList.filter(_ != " ")
    val tupleToParse = (listOfStrings.head, listOfStrings.tail.map(_.toDoubleOption))

    tupleToParse match {
      case (DIVIDE, dividend :: divisor) => Right(Divide(dividend, divisor.head))
      case (SUM, x :: xs) => Right(Sum(x :: xs))
      case (AVERAGE, x :: xs) => Right(Average(x :: xs))
      case (MIN, x :: xs) => Right(Min(x :: xs))
      case (MAX, x :: xs) => Right(Max(x :: xs))
      case _ => Left(InvalidCommandError(ErrorPrefix + "Invalid command"))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, DataToRender] = {
    x match {
      case Divide(_, Some(0)) =>
        Left(DivisionByZeroError(ErrorPrefix + "Division by zero"))

      case Divide(Some(dividend), Some(divisor)) =>
        Right(DataToRender(DIVIDE, List(Some(dividend), Some(divisor)), dividend / divisor))

      case Sum(list) if list.forall(_.isDefined) =>
        Right(DataToRender(SUM, list, list.map(_.get).sum))

      case Average(list) if list.forall(_.isDefined) =>
        Right(DataToRender(AVERAGE, list, list.map(_.get).sum / list.length))

      case Min(list) if list.forall(_.isDefined) =>
        Right(DataToRender(MIN, list, list.map(_.get).min))

      case Max(list) if list.forall(_.isDefined) =>
        Right(DataToRender(MAX, list, list.map(_.get).max))

      case _ => Left(CalculationError(ErrorPrefix + "Can not perform calculation on provided input"))
    }
  }

  def renderResult(x: DataToRender): String = {
    x.command match {
      case DIVIDE => s"${x.numbers.head.get} ${x.command}d by ${x.numbers.tail.head.get} is ${x.calculatedValue}"
      case _ => s"the ${x.command} of ${x.numbers.map(_.get).mkString(" ")} is ${x.calculatedValue}"
    }
  }

  def process(x: String): String = {
    (for {
      y <- parseCommand(x)
      z <- calculate(y)
    } yield renderResult(z)) match {
      case Right(value) => value
      case Left(error) => error.value
    }
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}

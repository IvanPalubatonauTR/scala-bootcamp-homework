package com.evolutiongaming.basics

import com.evolutiongaming.basics.ControlStructures.Command._
import com.evolutiongaming.basics.ControlStructures.Error._

import scala.io.Source

object ControlStructures {
  val ErrorPrefix = "Error: "

  object CommandConstants {
    type CommandConstants = String
    val Divide = "divide"
    val Sum = "sum"
    val Average = "average"
    val Min = "min"
    val Max = "max"
  }

  sealed trait Command

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  sealed trait ErrorMessage {
    def value: String
  }

  object Error {
    final case class InvalidCommandError(value: String) extends ErrorMessage
    final case class DivisionByZeroError(value: String) extends ErrorMessage
    final case class CalculationError(value: String) extends ErrorMessage
  }

  final case class DataToRender(command: String, numbers: List[Double], calculatedValue: Double)

  final case class ValidatedInputData(command: String, arguments: List[Double])

  def prepareInput(x: String): Either[ErrorMessage, ValidatedInputData] = {
    x.toLowerCase.split("\\s+").toList match {
      case Nil                                                     => Left(InvalidCommandError(ErrorPrefix + "Invalid command"))
      case x :: xs if xs.map(_.toDoubleOption).forall(_.isDefined) => Right(ValidatedInputData(x, xs.map(_.toDouble)))
      case _                                                       => Left(InvalidCommandError(ErrorPrefix + "Invalid arguments"))
    }
  }

  def parseCommand(x: ValidatedInputData): Either[ErrorMessage, Command] = {
    x match {
      case ValidatedInputData(CommandConstants.Divide, dividend :: divisor :: Nil) => Right(Divide(dividend, divisor))
      case ValidatedInputData(CommandConstants.Sum, list@_ :: _ :: _)              => Right(Sum(list))
      case ValidatedInputData(CommandConstants.Average, list@_ :: _ :: _)          => Right(Average(list))
      case ValidatedInputData(CommandConstants.Min, list@_ :: _ :: _)              => Right(Min(list))
      case ValidatedInputData(CommandConstants.Max, list@_ :: _ :: _)              => Right(Max(list))
      case _                                                                       => Left(InvalidCommandError(ErrorPrefix + "Invalid command"))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, DataToRender] = {
    x match {
      case Divide(_, 0)              => Left(DivisionByZeroError(ErrorPrefix + "Division by zero"))
      case Divide(dividend, divisor) => Right(DataToRender(CommandConstants.Divide, List(dividend, divisor), dividend / divisor))
      case Sum(list)                 => Right(DataToRender(CommandConstants.Sum, list, list.sum))
      case Average(list)             => Right(DataToRender(CommandConstants.Average, list, list.sum / list.length))
      case Min(list)                 => Right(DataToRender(CommandConstants.Min, list, list.min))
      case Max(list)                 => Right(DataToRender(CommandConstants.Max, list, list.max))
      case _                         => Left(CalculationError(ErrorPrefix + "Can not perform calculation on provided input"))
    }
  }

  def renderResult(x: DataToRender): String = {
    x.command match {
      case CommandConstants.Divide => s"${x.numbers.head} ${x.command}d by ${x.numbers.tail.head} is ${x.calculatedValue}"
      case _                       => s"the ${x.command} of ${x.numbers.mkString(" ")} is ${x.calculatedValue}"
    }
  }

  def process(x: String): String = {
    (for {
      input <- prepareInput(x)
      command <- parseCommand(input)
      data <- calculate(command)
    } yield renderResult(data)) match {
      case Right(value) => value
      case Left(error)  => error.value
    }
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}

package com.evolutiongaming.testing

import com.evolutiongaming.testing.CalculatorExercise.BuggyCalculator.Operation
import com.evolutiongaming.testing.CalculatorExercise.Error.DigitOutOfRange

object CalculatorExercise {

  sealed abstract class ErrorMessage(value: String) {
    def message: String = "Error: " + value
  }

  object Error {

    final case object DigitOutOfRange extends ErrorMessage("Digit out of range")

  }

  case class BuggyCalculator(memory: Int = 0, screen: Int = 0, operation: Option[Operation] = None) {

    def enter(digit: Int): Either[ErrorMessage, BuggyCalculator] =
      if (digit >= 0 && digit <= 9) {
        Right(this.copy(memory = memory + digit))
      } else {
        Left(DigitOutOfRange)
      }

    def plus(digit: Int): BuggyCalculator = this.copy(screen = digit, operation = Some(Operation.Plus))

    def minus(digit: Int): BuggyCalculator = this.copy(screen = digit, operation = Some(Operation.Minus))

    def multiply(digit: Int): BuggyCalculator = this.copy(screen = digit, operation = Some(Operation.Multiply))

    def calculate: BuggyCalculator = operation.fold(this) {
      case Operation.Plus => BuggyCalculator(screen = screen + memory)
      case Operation.Minus => BuggyCalculator(screen = memory - screen)
      case Operation.Multiply => BuggyCalculator(screen = screen * memory)
    }

  }

  object BuggyCalculator {

    sealed trait Operation

    object Operation {

      object Plus extends Operation

      object Minus extends Operation

      object Multiply extends Operation

    }

  }

}


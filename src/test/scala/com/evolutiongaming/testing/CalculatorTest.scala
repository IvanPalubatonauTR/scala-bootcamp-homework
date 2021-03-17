package com.evolutiongaming.testing

import com.evolutiongaming.testing.CalculatorExercise.BuggyCalculator
import com.evolutiongaming.testing.CalculatorExercise.Error.DigitOutOfRange
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CalculatorTest extends AnyWordSpec with Matchers with EitherValues {

  "Calculator" when {
    "enters the number correctly" in {
      val calculator = BuggyCalculator()
      (0 to 9).foreach(integer => calculator.enter(integer).right.value should be(BuggyCalculator(integer, 0, None)))
    }
    "enters the number incorrectly" in {
      val calculator = BuggyCalculator()
      calculator.enter(-1).left.value should be(DigitOutOfRange)
      calculator.enter(10).left.value should be(DigitOutOfRange)
    }
    "check plus operation" in {
      val calculator = BuggyCalculator(memory = 33)
      calculator.plus(1).calculate should be(BuggyCalculator(0, 34, None))
    }
    "check minus operation" in {
      val calculator = BuggyCalculator(memory = 33)
      calculator.minus(1).calculate should be(BuggyCalculator(0, 32, None))
    }
    "check multiply operation" in {
      val calculator = BuggyCalculator(memory = 33)
      calculator.multiply(2).calculate should be(BuggyCalculator(0, 66, None))
    }
    "does nothing when you just repeat pressing `=`" in {
      val calculator = BuggyCalculator()
      calculator.calculate.calculate.calculate.calculate should be(calculator)
    }
  }


}
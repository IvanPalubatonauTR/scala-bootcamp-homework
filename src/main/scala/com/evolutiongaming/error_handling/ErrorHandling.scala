package com.evolutiongaming.error_handling

import cats.data.ValidatedNec
import cats.syntax.all._
import com.evolutiongaming.error_handling.ErrorHandling.PaymentCardValidator.{AllErrorsOr, validateCardNumber, validateDate, validateUser}

object ErrorHandling {

  final case class CardNumber(value: String) extends AnyVal

  sealed trait DateAttribute

  object DateAttribute {

    final case class Month(month: Int) extends AnyVal

    final case class Year(year: Int) extends AnyVal

  }

  final case class Name(name: String) extends AnyVal

  import DateAttribute._

  final case class ValidityDate(month: Month, year: Year)

  final case class User(username: Name)

  sealed abstract case class PaymentCard private(user: User, cardNumber: CardNumber, validityDate: ValidityDate)

  object PaymentCard {
    def create(user: User, cardNumber: CardNumber, validityDate: ValidityDate): AllErrorsOr[PaymentCard] =
      (validateUser(user), validateCardNumber(cardNumber), validateDate(validityDate)).mapN {
        case (user, cardNumber, date) => new PaymentCard(user, cardNumber, date) {}
      }
  }

  sealed trait ValidationError

  object ValidationError {

    final case object NameLengthIsInvalid extends ValidationError {
      override def toString: String = "Name must be between 3 and 30 characters"
    }

    final case object NameHasSpecialCharacters extends ValidationError {
      override def toString: String = "Name cannot contain special characters"
    }

    final case object cardNumberIsNotNumeric extends ValidationError {
      override def toString: String = "Card number must be a number"
    }

    final case object cardNumberLengthIsInvalid extends ValidationError {
      override def toString: String = "Card number must be equals to 16"
    }

    final case object DateLengthIsInvalid extends ValidationError {
      override def toString: String = "Month must be in MM format and year in YYYY"
    }

    final case object DateIsNotNumeric extends ValidationError {
      override def toString: String = "Date must be a number"
    }

  }

  object PaymentCardValidator {

    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validateUser(user: User): AllErrorsOr[User] = {
      val username = user.username.name

      def validateNameLength(userName: String): AllErrorsOr[User] =
        if (userName.length >= 3 && userName.length <= 30) User(Name(userName)).validNec
        else NameLengthIsInvalid.invalidNec

      def validateNameContents(userName: String): AllErrorsOr[User] =
        if (userName.matches("^[a-zA-Z0-9]+$")) User(Name(userName)).validNec
        else NameHasSpecialCharacters.invalidNec

      validateNameLength(username)
        .productR(validateNameContents(username))
    }


    def validateCardNumber(cardNumber: CardNumber): AllErrorsOr[CardNumber] = {
      val card = cardNumber.value

      def validateCardLength(card: String): AllErrorsOr[CardNumber] = {
        if (card.length == 16) CardNumber(card).validNec
        else cardNumberLengthIsInvalid.invalidNec
      }

      def validateCardContents(card: String): AllErrorsOr[CardNumber] = {
        if (card.matches("\\d+")) CardNumber(card).validNec
        else cardNumberIsNotNumeric.invalidNec
      }

      validateCardLength(card) *> validateCardContents(card)

    }

    def validateDate(validityDate: ValidityDate): AllErrorsOr[ValidityDate] = {
      val month = validityDate.month.month
      val year = validityDate.year.year

      def validateDateLength(month: Int, year: Int): AllErrorsOr[ValidityDate] =
        if (month == 2 && year == 4) ValidityDate(Month(month), Year(year)).validNec
        else DateLengthIsInvalid.invalidNec

      validateDateLength(month, year)
    }

  }

}



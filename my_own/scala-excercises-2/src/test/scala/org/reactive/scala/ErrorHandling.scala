package org.reactive.scala

import org.reactive.error.UserValidationEither.{AgeValidationError,NameValidationError}
import org.reactive.error.UserValidationNonFunctional.{AgeValidation, NameValidation}
import org.reactive.error.{User, UserValidationEither, UserValidationNonFunctional, UserValidationTry}
import org.scalatest.{FlatSpecLike, Matchers}

import scala.util.{Failure, Success, Try}

class ErrorHandling extends FlatSpecLike with Matchers {

  "The student" should "see that exceptions are not very usefull" in {
    val validUser = User("alf", 21)
    UserValidationNonFunctional.verifyUser(validUser) shouldBe validUser

    assertThrows[AgeValidation] {
      val yourUser = User("alf", 15)
      UserValidationNonFunctional.verifyUser(yourUser) shouldBe validUser
    }

    assertThrows[NameValidation] {
      val tooShortUser = User("al", 21)
      UserValidationNonFunctional.verifyUser(tooShortUser) shouldBe validUser
    }
  }

  it should "make a version using try" in {
    //you can wrap the previous functions
    val validUser = User("alf", 21)
    UserValidationTry.verifyUser2(validUser) shouldBe Success(validUser)

    val yourUser = User("alf", 15)
    UserValidationTry.verifyUser2(yourUser) shouldBe Failure(AgeValidation("you cant be under 18 to learn functional"))


    val tooShortUser = User("al", 21)
    UserValidationTry.verifyUser2(tooShortUser) shouldBe Failure(NameValidation("that's a name? don't mess with me"))

  }

  it should "make a version using either" in {
    //dont reuse, make it from scrach
    val validUser = User("alf", 21)
    UserValidationEither.verifyUser(validUser) shouldBe Right(validUser)

    val yourUser = User("alf", 15)
    UserValidationEither.verifyUser(yourUser) shouldBe Left(AgeValidationError("you cant be under 18 to learn functional"))


    val tooShortUser = User("al", 21)
    UserValidationEither.verifyUser(tooShortUser) shouldBe Left(NameValidationError("that's a name? don't mess with me"))
  }
}

package org.reactive.error

object UserValidationEither {
  trait UserValidationError {
    val message:String
  }
  case class AgeValidationError(message:String) extends UserValidationError
  case class NameValidationError(message:String) extends UserValidationError

  def itCantBeUnderaged(age:Int):Either[UserValidationError,Unit] =
    Either.cond(age >= 18, (), AgeValidationError("you cant be under 18 to learn functional"))

  def itCantBeMatusalen(age:Int):Either[UserValidationError,Unit] =
    Either.cond(age <= 100, (), AgeValidationError("too old, get out of here"))

  def minimunNameLength(name:String):Either[UserValidationError,Unit] =
    Either.cond(name.length >= 3, (), NameValidationError("that's a name? don't mess with me"))

  def verifyUser(user: User):Either[UserValidationError,User] = for {
    _ <- itCantBeUnderaged(user.age)
    _ <- itCantBeMatusalen(user.age)
    _ <- minimunNameLength(user.name)
  } yield user
}

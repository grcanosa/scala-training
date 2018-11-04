package org.reactive.error

case class User(name:String, age:Int)

object UserValidationNonFunctional {

  case class AgeValidation(message:String) extends Throwable
  case class NameValidation(message:String) extends Throwable

  def itCantBeUnderaged(age:Int):Unit = {
    if (age < 18) {
      throw AgeValidation("you cant be under 18 to learn functional")
    } else {
      ()
    }
  }

  def itCantBeMatusalen(age:Int):Unit = {
    if (age > 100) {
      throw AgeValidation("too old, get out of here")
    } else {
      ()
    }
  }

  def minimunNameLength(name:String):Unit = {
    if (name.length < 3) {
      throw NameValidation("that's a name? don't mess with me")
    }
  }

  def verifyUser(user: User):User = {
    itCantBeUnderaged(user.age)
    itCantBeMatusalen(user.age)
    minimunNameLength(user.name)
    user
  }
}

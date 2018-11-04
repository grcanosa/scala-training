package org.reactive.error

import scala.util.Try

object UserValidationTry {

  def itCantBeUnderaged(age:Int):Try[Unit] = Try{UserValidationNonFunctional.itCantBeUnderaged(age)}

  def itCantBeMatusalen(age:Int):Try[Unit] = Try{ UserValidationNonFunctional.itCantBeMatusalen(age)}

  def minimumNameLength(name:String):Try[Unit] = Try{UserValidationNonFunctional.minimunNameLength(name)}

  def verifyUser(user: User):Try[User] = for {
    _ <- itCantBeUnderaged(user.age)
    _ <- itCantBeMatusalen(user.age)
    _ <- minimumNameLength(user.name)
  } yield user

  def verifyUser3(user: User):Try[User] = itCantBeUnderaged(user.age)
    .flatMap { case _ =>
      itCantBeMatusalen(user.age)
        .flatMap { case _ =>
          minimumNameLength(user.name)
            .map { case _ => user }
        }
    }

  def verifyUser2(user:User): Try[User] = itCantBeUnderaged(user.age)
    .flatMap(_ => itCantBeMatusalen(user.age))
    .flatMap(_ => minimumNameLength(user.name))
    .map(_ => user)


//      Try{UserValidationNonFunctional.itCantBeUnderaged(user.age)}
//          .flatMap(_ => Try{ UserValidationNonFunctional.itCantBeMatusalen(user.age)}
//                            .flatMap(_ => Try{minimumNameLength(user.name)}
//                                         .map(_ => user)))
}

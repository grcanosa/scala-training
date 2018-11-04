package org.datahack

import cats.Monad

import scala.util.{Failure, Success, Try}

/**
 * @author ${user.name}
 */
object App {



  def main(args : Array[String]) {
    import cats.implicits._
    import cats.syntax._
    import cats.instances.list._
    //    import cats.instances.option._

    //    val result = Functor[List].map(List(1,2,3,4))(_.toString()+"!")
    //    println(result)
    //    val list = List(Success(1), Success(3), Failure(new Exception("error")))
    //
    //    val result2 = Functor[List]
    //                    .compose[Try]
    //                    .map(list)(_.toString()+"!")
    //    println(result2)
    //    val monada1 = Monad[List].compose[Try]
    //    val res3 =  monada1.map(list)(_.toString()+"!")
    //    println(res3)
    //
    //    //Creamos un nuevo elemento.
    //    val pur = monada1.pure("todo ok")
    //    println(pur)
    //    //Saldría Succes(Some(Some("TODO VA BIEN")))
    //    val monada2 = monada1.compose[Option].compose[Option]
    //    println(monada2.pure("TODO VA BIEN"))
    //
    //    val monada3 = Monad[Try].compose[Option].compose[Option]
    //
    //    val l_ints = List(1, 2, 3, 4)
    //    println(l_ints.map(monada3.pure))

    //    val lfat: List[Try[Option[Option[Int]]]] = List(1,2,3,4).map(monada2.pure)
    //
    //    //Otro método que nos da las mónadas es el flatten
    //    println(monada2.flatten(List(1,2,3,4).map(monada3.pure)))

    val someInt1: Option[Int] = Some(3)
    val someInt2: Option[Int] = Some(1)
    val res5 = someInt1 |+| someInt2
//    println(res5)
//    println(1.some |+| 5.some)
//    println("1".some |+| "4".some)
//    println(List(1, 2, 3).some |+| List(4, 5, 6).some)
//    println(1.asRight[Throwable] |+| 6.asRight[Throwable])
//    println(none[Int] |+| 3.some)
//    println(none[Int] |+| none[Int])


    val divide: Int => Try[Int] = n => Try {
      100 / n
    }
    val res6 = (-10 to 10).map(divide)
    println(res6)
    //traverse nos devuelve el primer error.
    //val res7 = (-10 to 10).toList.traverse(divide)
    //println(res7)
    //    val res8 = (-10 to 10).toList.reduceMap(_ + 11)
    //    println(res8)

  }

}

case class User(id:String,name:String, age:Int)


//Con F dejamos la gestión de errores para luego
trait Application[F[_]] {
  def getUser(id:String):F[User]
  def changeAge(user:User,x: Int => Int):F[User]
  def writeUser(user:User):F[Unit]

  //Con esta interfaz es como que el error te da igual, si se puede bien y sino pues devuelvo
  //el F (Option, Try, etc...)
}

trait ChangeUser[F[_]] {
  import cats.implicits._
  val app:Application[F]
  def updateUserAge(id:String,x:Int => Int)(implicit m:Monad[F]):F[User] = {
    for {
      user <-app.getUser(id)
      userWithNewAge <- app.changeAge(user,x)
      _ <- app.writeUser(userWithNewAge)
    } yield userWithNewAge
  }


}

class ApplicationOption extends Application[Option] {

  import cats.implicits._

  var users:Map[String,User] = Map.empty[String,User]


  override def getUser(id: String): Option[User] = users.get(id) //Try{users(id)}.toOption -> esto seria otra opcion

  override def changeAge(user: User, x: Int => Int): Option[User] = {
    val newAge = x(user.age)
    if (newAge < 0)
      None
    else
      user.copy(age=newAge).some  //Sin la sintaxis de Cats sería Some(user.copy(age=newAge))

  }

  override def writeUser(user: User): Option[Unit] = {
    users = users + (user.id -> user)  //el + sabe concatenar mapas y si la key es igual reemplaza
    //users = users + ((user.id,  user))  // otra manera
    Some(Unit)
    //Some(())  // Otra opcion de sintaxis
    //().some // sintaxis de Cats

  }
}

class ApplicationTry extends Application[Try] {

  import cats.implicits._

  var users:Map[String,User] = Map.empty[String,User]


  override def getUser(id: String): Try[User] = Try{users(id)} //Try{users(id)}.toOption -> esto seria otra opcion

  override def changeAge(user: User, x: Int => Int): Try[User] = {
    val newAge = x(user.age)
    if (newAge < 0)
      Failure(new Exception("age cannot be negative"))
    else
      Success(user.copy(age=newAge))  //Sin la sintaxis de Cats sería Some(user.copy(age=newAge))

  }

  override def writeUser(user: User): Try[Unit] = {
    users = users + (user.id -> user)  //el + sabe concatenar mapas y si la key es igual reemplaza
    //users = users + ((user.id,  user))  // otra manera
    Success(())
    //Some(())  // Otra opcion de sintaxis
    //().some // sintaxis de Cats

  }
}

//class ApplicationOption extends ChangeUser[Option]{
//  override val monad: Monad[Option] = _
//
//  override def getUser(id: String): Option[User] = ???
//
//  override def changeAge(user: User, x: Int => Int): Option[User] = ???
//
//  override def writeUser(user: User): Option[Unit] = ???
//}
//

object Main {
  import cats.instances.option._
  def main2(args: Array[String]): Unit = {
    val changer = new ChangeUser[Option] {
      override val app: Application[Option] = new ApplicationOption
      app.writeUser(User("123","pepe",10))
    }
    val u = changer.updateUserAge("123", _ + 10)
    println(u)
  }

  def main(args: Array[String]): Unit = {
    import cats.instances.try_._
    val changer = new ChangeUser[Try] {
      override val app: Application[Try] = new ApplicationTry
      app.writeUser(User("123","pepe",10))
    }
    val u = changer.updateUserAge("123", _ - 100)
    println(u)
  }
}


package org.datahack

import monocle._

object Main {

  //http://julien-truffaut.github.io/Monocle/ Implementacion de lentes en scala

  //Mirar doobie slick para hacer Marshalling a una tabla de base de datos.
  //slick esta mas usado
  //doobie queda más legible

  //Scala-js te convierte codigo scala a codigo javascript.

  //Shapeless es una librería de programacion genérica.
  //Circle, librería de parse de JSON.

  //Monix (la 2.3.3 funciona muy bien) -> como hacer programacion lazy

  //cats effect: composicion de tareas lazy.

  //https://neo4j.com/

  //http://h2o-release.s3.amazonaws.com/h2o/master/3344/docs-website/h2o-py/docs/intro.html

  import monocle.Lens
  import monocle.macros.GenLens
  import scalaz.std.list._

  val user2address   : Lens[User, Address]   = GenLens[User](_.address)
  val user2name      : Lens[User, String]    = GenLens[User](_.name)
  val address2street : Lens[Address,Street]  = GenLens[Address](_.street)
  val address2city   : Lens[Address, String] = GenLens[Address](_.city)
  val street2num     : Lens[Street, Int]     = GenLens[Street](_.number)
  val street2name    : Lens[Street, String]  = GenLens[Street](_.name)


  def add1ToStreeNumLens(user:User):User = {
    (user2address composeLens address2street composeLens street2num).modify(_ + 1 )(user)
  }


  def add1ToStreetNum(user:User):User = {
    user.copy(address=user.address.copy(street=user.address.street.copy(number = user.address.street.number+1)))
  }

  def main(args: Array[String]): Unit = {
    val user = User("user1",Address(Street("street1",2),"Madrid"),Nil)
    val user2 = add1ToStreetNum(user)
    val user3 = add1ToStreeNumLens(user)


    println(user)
    println(user2)
    println(user3)
    //Con las lentes podermos hacer get directamente
    println(user2address.get(user))
    println(user2address.composeLens(address2street).composeLens(street2num).get(user))
    //Las lentes tb nos dan metodos para modificar
    println(user2address.composeLens(address2street).composeLens(street2num).modify(_ + 1 )(user))
    //La forma comoda sería
    val user2address2street2number: Lens[User, Int] = user2address.composeLens(address2street).composeLens(street2num)
    println(user2address2street2number.modify(_+1)(user))
    val user2address2street2name = user2address.composeLens(address2street).composeLens(street2name)
    //También se puede guarda la operacion como val
    val userToUserWithStreetInUppercase = user2address2street2name.modify(_.toUpperCase)
    println(userToUserWithStreetInUppercase(user))
    //También se puede usar un set
    val setStringName = user2address2street2name.set("Pepito")


    //Se pueden hacer ISOs

    val streetAsTuple:Iso[Street,(String,Int)] = Iso[Street,(String,Int)](s => (s.name,s.number))(t => Street(t._1,t._2))
    val l = user2address composeLens address2street composeIso streetAsTuple

    println(l.set("streets of rage",3)(user))

    //Lentes opcionales
    val firstContact: Optional[User,User] =
      Optional[User,User](_.contacts.headOption)(
        newContact =>
          user =>
           user.copy(contacts=newContact :: user.contacts.tail))

    println(firstContact.getOption(user))
    println(firstContact.modifyOption(_ => user2)(user))

    val friend1 = User("friend1",Address(Street("street4",55),"Madrid"),List())
    val friend2 = User("friend2",Address(Street("street4",55),"Madrid"),List(friend1))
    val friend3 = User("friend3",Address(Street("street4",55),"Madrid"),List(friend2,friend1))
    println(firstContact.getOption(friend1))
    //También puedo saber el primer amigo de mi primer amigo
    println(firstContact.composeOptional(firstContact).getOption(friend3))


    //Transversales
    val myFriends: Lens[User,List[User]] = GenLens[User](_.contacts)
    val eachL: Traversal[List[User], User] = Traversal.fromTraverse[List, User]

    val traverseMyFriends: Traversal[User, User] = myFriends composeTraversal eachL

    println(traverseMyFriends.exist(user2address2street2number.get(_) == 55)(friend3))
    println(traverseMyFriends.exist(user2address2street2number.get(_) == 52)(friend3))
    //Podemos hacer que todos vivan en el 32
    println(traverseMyFriends.modify(user2address2street2number.set(32))(friend3))
    //Puedo sacar todos los primeros amigos de mis amigos
    println(traverseMyFriends.composeOptional(firstContact).getAll(friend3))
    val superLens = traverseMyFriends composeTraversal traverseMyFriends composeLens l
    println(superLens.set("under a bridge",-1)(friend3))

  }
}

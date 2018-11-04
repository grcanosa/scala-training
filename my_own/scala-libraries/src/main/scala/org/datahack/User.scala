package org.datahack

//Mirar diferencia entre private y selaed

case class Street(name:String,number:Int)
case class Address(street:Street,city:String)
case class User(name:String,address:Address,contacts:List[User])


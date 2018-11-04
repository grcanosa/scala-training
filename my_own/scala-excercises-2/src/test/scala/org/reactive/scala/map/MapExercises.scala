package org.reactive.scala.map

import org.scalatest.{FlatSpecLike, Matchers}

import scala.collection.immutable

class MapExercises extends FlatSpecLike with Matchers{
  "Map" should "transform the contained elements" in {
    List(1,2,3).map(_ + 1) shouldBe List(2,3,4)
    Option("Reactive").map(_.toUpperCase()) shouldBe Option("REACTIVE")
  }

  it should "do the same if multiple maps are applied" in{

    val addExclamation:String => String = _ + "!"
    val addQuestion:String => String = _ + "?"

    Option("hola").map(addExclamation).map(addQuestion) shouldBe Option("hola").map(x =>addQuestion(addExclamation(x)))
  }

  "The student" should "do the first exercise" in {
    //If i give you the list l
    val l: Seq[Option[Int]] = List(Option(1), None, Option(3), Option(4), None)
    //Keep all the elements in the list, but add 10 to all defined values
    val lTransformed: Seq[Option[Int]] = l.map(v => v.map(_ + 10))

    lTransformed shouldBe List(Option(11),None, Option(13),Option(14),None)
  }

  it should "do the second" in {
    //Given the list tuple of names and ages
    val l:List[(String, Int)] = List(("Alf",22), ("Fran",23), ("Juan", 40), ("María", 26))
    //And a case class definition
    case class Person(name:String, age:Int)
    //transform the list into a list of Persons
    val lTransformed:List[Person] = l.map(t => Person(t._1,t._2))
    val lTransformed2:List[Person] = l.map(Person.tupled)

    lTransformed  shouldBe List(Person("Alf",22), Person("Fran",23), Person("Juan", 40), Person("María", 26))
    lTransformed2 shouldBe List(Person("Alf",22), Person("Fran",23), Person("Juan", 40), Person("María", 26))
  }
}

package io.github.antonkw

import java.time.ZonedDateTime

import cats.implicits._
import eu.timepit.refined._
import eu.timepit.refined.api.{RefType, Refined}
import eu.timepit.refined.collection.Contains
import eu.timepit.refined.numeric.Greater
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.{Transformer, TransformerF}

import scala.util.Random

object Main extends App {
  case class MakeCoffee(id: Int, kind: String, addict: String)
  case class CoffeeMade(id: Int, kind: String, forAddict: String, at: ZonedDateTime)

  val command = MakeCoffee(id = Random.nextInt(),
    kind = "Espresso",
    addict = "Piotr"
  )

  val event1 = CoffeeMade(
    id = command.id,
    kind = command.kind,
    forAddict = command.addict,
    at = ZonedDateTime.now
  )

  val event2 = command.into[CoffeeMade]
    .withFieldComputed(_.at, _ => ZonedDateTime.now)
//    .withFieldConst(_.at, ZonedDateTime.now) -- just the same
    .withFieldRenamed(_.addict, _.forAddict)
    .transform

  //withFieldRenamed actually has one significant "power"

  case class Person(name: String)

  case class Name(value: String) extends AnyVal
  case class Employee(firstName: Name)

  val employee1 = Person("Anton")
    .into[Employee]
    .withFieldRenamed(_.name, _.firstName)
    //withFieldRenamed lift firstName to Name
    .transform

//  val employee2 = Person("Anton")
//    .into[Employee]
//    .withFieldComputed(_.firstName, _.name)
//    .transform
  //Type mismatch!
  //Function passed to `withFieldComputed` returns type: String
  //Type required by 'firstName' field: io.github.antonkw.Main.Name
  //

  //Using method accessors

  sealed trait Color
  object Color {
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color
  }

  sealed trait Channel
  object Channel {
    case object Alpha extends Channel
    case object Blue extends Channel
    case object Green extends Channel
    case object Red extends Channel
  }

  val colRed: Color = Color.Red
  val chanRed = colRed.transformInto[Channel]
//   chanRed: Channel = Red


//  chanRed.transformInto[Color]
  // error: Chimney can't derive transformation from Channel to Color
  //
  // Color
  //   can't transform coproduct instance Channel.Alpha to Color


 val copruductInstance = (_: Channel.Alpha.type) => Color.Blue
  val red = chanRed.into[Color]
    .withCoproductInstance(copruductInstance)
    .transform
  // red: Color = Red

  val alpha: Channel = Channel.Alpha
  val blue = alpha.into[Color]
    .withCoproductInstance(copruductInstance)
    .transform
  // blue: Color = Blue

  case class RegistrationForm(
                               email: String,
                               username: String,
                               password: String,
                               age: String
                             )

  case class RegisteredUser(
                             email: String,
                             username: String,
                             passwordHash: String,
                             age: Int
                           )

  val okForm = RegistrationForm("john@example.com", "John", "s3cr3t", "40")

  val registered: Option[RegisteredUser] = okForm
    .intoF[Option, RegisteredUser] //Option type will be used for handling partial transformations.
    .withFieldComputed(_.passwordHash, form => form.password.length.toString)
    .withFieldComputedF(_.age, _.age.toIntOption) //nstead of withFieldComputed, we use withFieldComputedF, where second parameter is a function that wraps result into a type constructor provided
    .transform

  println(registered) //Some(RegisteredUser(john@example.com,John,6,40))

  val badForm = RegistrationForm("john@example.com", "John", "s3cr3t", "not an int")

  val invalid = badForm
    .intoF[Option, RegisteredUser]
    .withFieldComputed(_.passwordHash, _.password.length.toString)
    .withFieldComputedF(_.age, _.age.toIntOption)
    .transform

  println(invalid) //None

  type EitherVecStr[+X] = Either[Vector[String], X]
  type Email = String Refined Contains['@']
  type AdultAge = Int Refined Greater[W.`18`.T]

  case class RegForm(
                      email: String,
                      username: String,
                      password: String,
                      age: String
                    )

  case class User(
                   email: Email,
                   username: String,
                   passwordHash: String,
                   age: AdultAge
                 )

  implicit val transformer: TransformerF[EitherVecStr, RegForm, User] = {
    Transformer.defineF[EitherVecStr, RegForm, User]
      .withFieldComputedF(_.email, form => RefType.applyRef[Email](form.email).leftMap(_.pure[Vector]))
      .withFieldComputed(_.passwordHash, form => form.password.length.toString)
      .withFieldComputedF(_.age, _.age.toIntOption.toRight("Invalid int value").flatMap(RefType.applyRef[AdultAge](_)).leftMap(_.pure[Vector]))
      .buildTransformer
  }

  val nonSuccess = Array(
    RegForm("john_example.com", "John", "s3cr3t", "10"),
    RegForm("alice@example.com", "Alice", "s3cr3t", "19"),
    RegForm("bob@example.com", "Bob", "s3cr3t", "21.5")
  ).transformIntoF[EitherVecStr, List[User]]

  println(nonSuccess) //left with list of errors

  val success = Array(
    RegForm("john@example.com", "John", "s3cr3t", "133"),
    RegForm("alice@example.com", "Alice", "s3cr3t", "19"),
    RegForm("bob@example.com", "Bob", "s3cr3t", "21")
  ).transformIntoF[EitherVecStr, List[User]] // right with list of users

  println(success)
  println(s"${Console.RED}lol${Console.RESET} hi")
}

package com.example.catstest

import cats._
import cats.data._
import cats.implicits._

object App {

  def eitherExample: Unit = {
    type Result[A] = Either[String, A]
    val l: Result[Int] = Either.left("Failed")
    val r: Result[Int] = Either.right(1)
    println(l map (_ + 1))
    println(r map (_ + 1))
  }

  def validatedExample: Unit = {
    type Error = List[String]
    type EitherErrInt = Either[Error, Int]
    type ValidatedR = Validated[Error, Int]
    val x1: EitherErrInt = Either.right(1)
    val x2: EitherErrInt = Either.left(List("Stops here"))
    val x3: EitherErrInt = Either.left(List("This will be ignored"))
    val v1: ValidatedR = Validated.valid(1)
    val v2: ValidatedR = Validated.invalid(List("Accumulates this"))
    val v3: ValidatedR = Validated.invalid(List("And this"))
    println(
      for {
        x <- x1
        y <- x2
        z <- x3
      } yield x + y + z
    )
    println((v1 |@| v2 |@| v3) map { _ + _ + _ })
    println((v1 |@| v1 |@| v1) map { _ + _ + _ })
  }

  def main(args: Array[String]) {
    println("eitherExample:")
    eitherExample
    println("validatedExample:")
    validatedExample
  }
}

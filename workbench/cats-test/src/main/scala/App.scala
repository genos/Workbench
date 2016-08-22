package com.qf.catstest

import cats.data._
import cats.std.all._
import cats.syntax.all._

object App {

  def xorExample: Unit = {
    type Result[A] = String Xor A
    val l: Result[Int] = "Failed".left
    val r: Result[Int] = 1.right
    println(l map (_ + 1))
    println(r map (_ + 1))
  }

  def validatedExample: Unit = {
    type Error = List[String]
    type XorR = Xor[Error, Int]
    type ValidatedR = Validated[Error, Int]
    val x1: XorR = 1.right
    val x2: XorR = List("Stops here").left
    val x3: XorR = List("This will be ignored").left
    val v1: ValidatedR = 1.valid
    val v2: ValidatedR = List("Accumulates this").invalid
    val v3: ValidatedR = List("And this").invalid
    println(
      for {
        x <- x1
        y <- x2
        z <- x3
      } yield x + y + z
    )
    println((v1 |@| v2 |@| v3) map { _ + _ + _ })
  }

  def main(args: Array[String]) {
    println("xorExample:")
    xorExample
    println("validatedExample:")
    validatedExample
  }
}

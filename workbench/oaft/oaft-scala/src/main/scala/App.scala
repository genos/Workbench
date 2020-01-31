package com.qf.oaft

object ExpAlg {
  trait Repr[T]

  case class Eval[T](value: T) extends Repr[T]
  def eval[T]: Eval[T] => T = _.value

  case class View[T](view: String) extends Repr[T]
  def view[T]: View[T] => String = _.view

  trait ExpAlg[R[_]] {
    def lit: Int => R[Int]
    def add: R[Int] => R[Int] => R[Int]
  }

  implicit object ExpAlgEval extends ExpAlg[Eval] {
    def lit = Eval(_)
    def add = x => y => Eval(x.value + y.value)
  }

  implicit object ExpAlgView extends ExpAlg[View] {
    def lit = x => View(x.toString)
    def add = x => y => View(s"(${x.view} + ${y.view})")
  }

  trait MulAlg[R[_]] {
    def mul: R[Int] => R[Int] => R[Int]
  }

  implicit object MulAlgEval extends MulAlg[Eval] {
    def mul = x => y => Eval(x.value * y.value)
  }
  implicit object MulAlgView extends MulAlg[View] {
    def mul = x => y => View(s"(${x.view} * ${y.view})")
  }

  def e1[R[_]](implicit ea: ExpAlg[R]): R[Int] = {
    import ea._
    add(lit(1))(add(lit(2))(lit(3)))
  }
  val v1: Int    = eval(e1[Eval])
  val s1: String = view(e1[View])

  def e2[R[_]](implicit ea: ExpAlg[R], ma: MulAlg[R]): R[Int] = {
    import ea._, ma._
    mul(lit(4))(add(lit(5))(lit(6)))
  }
  val v2: Int    = eval(e2[Eval])
  val s2: String = view(e2[View])
}

object App {
  import ExpAlg._
  def main(args: Array[String]): Unit = {
    println(v1)
    println(s1)
    println(v2)
    println(s2)
  }
}

package ttkv

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}
import cats.effect.{Clock, IO, Sync}

object TTKVSpec extends Properties("TTKV[IO]") {

  implicit val sync = Sync[IO]
  implicit val clock = Clock.create[IO]

  property("initially empty") = forAll { (a: Char) =>
    TTKV.empty.get(a) == None
  }

  property("single get") = forAll { (a: Char, x: Int) =>
    TTKV.empty.put(a, x).map(_.get(a) == Some(x)).unsafeRunSync
  }

  property("two gets, different keys") = forAll {
    (a: Char, b: Char, x: Int, y: Int) =>
      (a != b) ==>
        TTKV.empty
          .put(a, x)
          .flatMap(_.put(b, y))
          .map { m =>
            (m.get(a) == Some(x)) && (m.get(b) == Some(y))
          }
          .unsafeRunSync
  }

  property("two puts, same key") = forAll { (a: Char, x: Int, y: Int) =>
    TTKV.empty
      .put(a, x)
      .flatMap(_.put(a, y))
      .map(_.get(a) == Some(y))
      .unsafeRunSync
  }

  property("two puts same key => two times") = forAll {
    (a: Char, x: Int, y: Int) =>
      TTKV.empty
        .put(a, x)
        .flatMap(_.put(a, y))
        .map(_.times(a).length == 2)
        .unsafeRunSync
  }

  property("two puts => two times") = forAll {
    (a: Char, b: Char, x: Int, y: Int) =>
      TTKV.empty
        .put(a, x)
        .flatMap(_.put(b, y))
        .map(_.times.length == 2)
        .unsafeRunSync
  }

  property("middle get") = forAll { (a: Char, x: Int, y: Int, d: Double) =>
    ((0 <= d) && (d < 1)) ==>
      TTKV.empty
        .put(a, x)
        .flatMap(_.put(a, y))
        .map { m =>
          val ts = m.times(a)
          val t0 = ts.head
          val t1 = ts.last
          val δ = (t1 - t0) * d
          m.get(a, Some(t0 + δ.toLong)) == Some(x)
        }
        .unsafeRunSync
  }

  property("get before time") = forAll { (a: Char, x: Int, t: Long) =>
    (t > 0) ==>
      TTKV.empty
        .put(a, x)
        .map { m =>
          val t0 = m.times(a).head
          m.get(a, Some(t0 - t)) == None
        }
        .unsafeRunSync
  }

}

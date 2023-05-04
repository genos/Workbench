package ttkv

import cats.effect.{Clock, Sync}
import cats.implicits._
import alleycats.std.iterable._
import scala.concurrent.duration.NANOSECONDS
import scala.collection.immutable.LongMap

case class TTKV[K, V](inner: Map[K, LongMap[V]]) extends AnyVal {

  def put[F[_]](key: K, value: V)
               (implicit sync: Sync[F], clock: Clock[F]): F[TTKV[K, V]] =
    clock.monotonic(NANOSECONDS).map { t =>
      val m = inner.getOrElse(key, LongMap.empty) + (t -> value)
      TTKV(inner + (key -> m))
    }

  def get(key: K, time: Option[Long] = None): Option[V] =
    for {
      m <- inner.get(key)
      keys = m.keys.filter(_ <= time.getOrElse(Long.MaxValue))
      t0 <- keys.maximumOption
      value <- m.get(t0)
    } yield value

  def times(key: K): List[Long] =
    inner.get(key).map(_.keys.toList.sorted).getOrElse(Nil)

  def times: List[Long] =
    inner.keys.toList.flatMap(times).sorted

}

object TTKV {

  def empty[K, V]: TTKV[K, V] = TTKV(Map.empty)

}

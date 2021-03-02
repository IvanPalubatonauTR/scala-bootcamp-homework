package com.evolutiongaming.typeclass

import com.evolutiongaming.typeclass.MutableBoundedCache.MyTwitter.{FbiNote, Twit}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object MutableBoundedCache {

  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[-T] {
      def apply(value: T): SizeScore
    }

    object syntax {

      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner)
      }

    }

    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {

      import syntax._

      private val map = mutable.LinkedHashMap.empty[K, V]

      private def actualSize(map: mutable.Map[K, V]): Int = (for {
        (x, y) <- map
      } yield x.sizeScore + y.sizeScore).sum

      def put(key: K, value: V): Unit = {
        val sizeCurrentElement = key.sizeScore + value.sizeScore

        @tailrec
        def removeOversizeElement(innerMap: mutable.Map[K, V]): mutable.Map[K, V] = {
          sizeCurrentElement match {
            case size if actualSize(innerMap) + size <= maxSizeScore => innerMap.addOne((key, value))
            case _ => removeOversizeElement(map.subtractOne(innerMap.head._1))
          }
        }

        removeOversizeElement(map)
      }

      def get(key: K): Option[V] = {
        map.get(key)
      }
    }

    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])

    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()

      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]

      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {

      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }

      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      implicit val mapIterate: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.view.keys.iterator

        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.view.values.iterator
      }

      implicit val packedMapIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = (for {
          (a, _) <- f.inner
        } yield a).iterator

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = (for {
          (_, b) <- f.inner
        } yield b).iterator
      }

      implicit val byteGetSizeScore: GetSizeScore[Byte] = _ => 1
      implicit val charGetSizeScore: GetSizeScore[Char] = _ => 2
      implicit val intSizeScore: GetSizeScore[Int] = _ => 4
      implicit val longSizeScore: GetSizeScore[Long] = _ => 8
      implicit val stringSizeScore: GetSizeScore[String] = str => 12 + str.length * 2
      implicit val twitSizeScore: GetSizeScore[Twit] = v => 12 + v.id.sizeScore + v.attributes.sizeScore + v.hashTags.sizeScore + v.userId.sizeScore
      implicit val fbiSizeScore: GetSizeScore[FbiNote] = v => 12 + v.month.sizeScore + v.favouriteChar.sizeScore + v.watchedPewDiePieTimes.sizeScore

      implicit def seqSizeScore[T: GetSizeScore]: GetSizeScore[Seq[T]] = v => 12 + v.map(_.sizeScore).sum

      implicit def arraySizeScore[T: GetSizeScore]: GetSizeScore[Array[T]] = v => 12 + v.map(_.sizeScore).sum

      implicit def mapSizeScore[T: GetSizeScore, H: GetSizeScore]: GetSizeScore[Map[T, H]] = v => (for {
        (a, b) <- v
      } yield a.sizeScore + b.sizeScore).sum + 12

      implicit def packedMapSizeScore[T: GetSizeScore, H: GetSizeScore]: GetSizeScore[PackedMultiMap[T, H]] = v => (for {
        (a, b) <- v.inner
      } yield a.sizeScore + b.sizeScore).sum + 12
    }


  }

  object MyTwitter {

    import SuperVipCollections4s._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )

    trait TwitCache {
      def put(twit: Twit): Unit

      def get(id: Long): Option[Twit]
    }

    //TODO: Wrong result for last test
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = {
      import instances._
      val cache = new MutableBoundedCache[Long, Twit](maxSizeScore)
      new TwitCache {
        override def put(twit: Twit): Unit = cache.put(twit.id, twit)

        override def get(id: Long): Option[Twit] = cache.get(id)
      }
    }
  }

}

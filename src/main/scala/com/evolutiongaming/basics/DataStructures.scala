package com.evolutiongaming.basics

object DataStructures {
  val vegetableWeights = Map(
    ("pumpkins", 10),
    ("cucumbers", 20),
    ("olives", 2),
  )

  val vegetablePrices = Map(
    "tomatoes" -> 4,
    "peppers" -> 5,
    "olives" -> 17,
  )

  val vegetableAmounts = Map(
    "tomatoes" -> 17,
    "peppers" -> 234,
    "olives" -> 32,
    "cucumbers" -> 323,
  )

  val totalVegetableWeights: Map[String, Int] = {
    for {
      (vegetable, weight) <- vegetableWeights
      count <- vegetableAmounts.get(vegetable)
    } yield (vegetable, count * weight)
  }

  val totalVegetableCost: Int = {
    (for {
      (vegetable, amount) <- vegetableAmounts
      price = vegetablePrices.getOrElse(vegetable, 10) * amount
    } yield price).sum
  }

  //TODO:implementation has a bug :)
  def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] = {
    n match {
      case 1 => for {y <- set} yield Set(y)

      case n if n > 0 => for {
        y <- set
        z <- (set - y).sliding(n - 1)
      } yield z + y
    }
  }

  def sortConsideringEqualValues[T](map: Map[T, Int]): Seq[(Set[T], Int)] = {
    (for {
      (index, innerMap) <- map.groupBy { case (_, index) => index }
      string = innerMap.keySet
    } yield string -> index).toSeq.sortBy { case (_, index) => index }
  }

  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0)(_ + _).tail
  }

  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    (nums take n) zip (nums drop n) flatMap { case (a, b) => Array(a, b) }
  }

  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    for {
      candy <- candies
      result = candies.forall(_ <= candy + extraCandies)
    } yield result
  }

  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val x = points.map(_.head).sorted
    x.zip(x.tail).flatMap { case (a, b) => List(b - a) }.max
  }

  def balancedStringSplit(s: String): Int = {
    val isStringBalanced: (String, String) => Boolean = (a, b) => (a.sorted diff b.sorted).length == 0 ||
      (a + b).replaceAll("L", "").length != a.length

    @scala.annotation.tailrec
    def helpFunction(s1: String, iterator: Int, acc: Int): Int = {
      (s1.take(iterator), s1.slice(iterator, iterator + iterator)) match {
        case ("", _) => acc
        case (a, b) if isStringBalanced(a, b) => helpFunction(s1, iterator + 1, acc + 0)
        case (a, b) => helpFunction(s1.replaceFirst(a, "").replaceFirst(b, ""), 1, acc + 1)
      }
    }

    helpFunction(s, 1, 0)
  }

  //looked at the solution of balancedStringSplit by Aleksander Sorokin and implement maxDepth with scanLeft approach
  def maxDepth(s: String): Int = {
    s.scanLeft(0) {
      case (balance, '(') => balance + 1
      case (balance, ')') => balance - 1
      case (balance, _) => balance
    }.max
  }

}
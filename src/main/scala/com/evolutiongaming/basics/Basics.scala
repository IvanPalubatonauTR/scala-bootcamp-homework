package com.evolutiongaming.basics

object Basics {
  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) Math.abs(a) else gcd(b, a % b)

  def lcm(a: Int, b: Int): Int = Math.abs(a * b) / gcd(a, b)
}

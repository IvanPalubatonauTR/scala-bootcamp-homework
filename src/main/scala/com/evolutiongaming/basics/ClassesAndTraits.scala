package com.evolutiongaming.basics

object ClassesAndTraits {

  sealed trait Shape2D extends Located2D with Bounded with Movable {
    def area: Option[Double]
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Shape2D
  }

  sealed trait Located2D {
    def x: Double

    def y: Double
  }

  sealed trait Bounded {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double
  }

  final case class Point2D(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point2D = Point2D(x, y)

    override def area: Option[Double] = None
  }

  case class Square(topLeftPoint: Point2D, bottomRightPoint: Point2D) extends Shape2D {

    override def minX: Double = topLeftPoint.x

    override def maxX: Double = bottomRightPoint.x

    override def minY: Double = bottomRightPoint.y

    override def maxY: Double = topLeftPoint.y

    override def move(dx: Double, dy: Double): Square = ???

    override def x: Double = (bottomRightPoint.x - topLeftPoint.x) / 2 + topLeftPoint.x

    override def y: Double = (topLeftPoint.y - bottomRightPoint.y) / 2 + bottomRightPoint.y

    override def area: Option[Double] = Some((bottomRightPoint.x - topLeftPoint.x) * (topLeftPoint.y - bottomRightPoint.y))
  }

  final case class Triangle(firstPoint: Point2D, secondPoint: Point2D, thirdPoint: Point2D) extends Shape2D {

    val bounded: Bounded = minimumBoundingRectangle(Set(firstPoint, secondPoint, thirdPoint))

    override def minX: Double = bounded.minX

    override def maxX: Double = bounded.maxY

    override def minY: Double = bounded.minY

    override def maxY: Double = bounded.maxY

    override def move(dx: Double, dy: Double): Triangle = ???

    override def x: Double = (firstPoint.x + secondPoint.x + thirdPoint.x) / 3

    override def y: Double = (firstPoint.y + secondPoint.y + thirdPoint.y) / 3

    override def area: Option[Double] = Some((firstPoint.x * (secondPoint.y - thirdPoint.y)
      + secondPoint.x * (firstPoint.y - thirdPoint.y)
      + thirdPoint.x * (firstPoint.y - thirdPoint.y)) / 2)
  }

  def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = {
    new Bounded {
      override def minX: Double = objects.map(_.minX).min

      override def maxX: Double = objects.map(_.maxX).max

      override def minY: Double = objects.map(_.minY).min

      override def maxY: Double = objects.map(_.maxY).max
    }
  }

  sealed trait Shape3D extends Located3D {
    def volume: Option[Double]

    def surfaceArea: Option[Double]
  }

  sealed trait Located3D extends Located2D {
    def z: Double
  }

  final case object Origin3D extends Located3D {
    override def x: Double = 0

    override def y: Double = 0

    override def z: Double = 0
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def volume: Option[Double] = None

    override def surfaceArea: Option[Double] = None
  }

  abstract class RightAnglesShape3D(topLeftPoint: Point3D, bottomRightPoint: Point3D) extends Shape3D {
    val length: Double = bottomRightPoint.x - topLeftPoint.x
    val width: Double = topLeftPoint.y - bottomRightPoint.y
    val height: Double = topLeftPoint.z - bottomRightPoint.z

    override def volume: Option[Double] = Some(length * width * height)

    override def surfaceArea: Option[Double] = Some(2 * (length * width + width * height + height * length))

    override def z: Double = (bottomRightPoint.z - topLeftPoint.z) / 2 + topLeftPoint.z

    override def x: Double = (bottomRightPoint.x - topLeftPoint.x) / 2 + topLeftPoint.x

    override def y: Double = (topLeftPoint.y - bottomRightPoint.y) / 2 + bottomRightPoint.y
  }

  final case class Cube(topLeftPoint: Point3D, bottomRightPoint: Point3D) extends RightAnglesShape3D(topLeftPoint, bottomRightPoint)

  final case class Cuboid(topLeftPoint: Point3D, bottomRightPoint: Point3D) extends RightAnglesShape3D(topLeftPoint, bottomRightPoint)

  final case class Sphere(x: Double, y: Double, z: Double, radius: Double) extends Shape3D {
    override def volume: Option[Double] = Some(4 / 3 * Math.PI * Math.pow(radius, 3))

    override def surfaceArea: Option[Double] = Some(4 * Math.PI * Math.pow(radius, 2))
  }

  final case class Tetrahedron(firstPoint: Point3D, secondPoint: Point3D, thirdPoint: Point3D, fourthPoint: Point3D) extends Shape3D {
    override def volume: Option[Double] = ???

    override def surfaceArea: Option[Double] = ???

    override def z: Double = ???

    override def x: Double = ???

    override def y: Double = ???
  }

}
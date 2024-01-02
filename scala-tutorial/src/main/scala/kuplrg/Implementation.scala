package kuplrg

object Implementation extends Template {

  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a * b * c

  def concat(x: String, y: String): String = x + y

  def mulN(n: Int): Int => Int = (x: Int) => n * x

  def twice(f: Int => Int): Int => Int = (x: Int) => f(f(x))

  def compose(f: Int => Int, g: Int => Int): Int => Int = (x: Int) => g(f(x))

  def double(l: List[Int]): List[Int] = l.map(_ * 2)

  def product(l: List[Int]): Int = l.foldLeft(1)(_ * _)

  def getOrNotFound(m: Map[String, Int], s: String): Int = m.getOrElse(s, error("Not Found"))

  def depth(t: Tree): Int = t match
    case Leaf(_) => 0
    case Branch(_, children) => 1 + (children map depth).max

  def sum(t: Tree): Int = t match
    case Leaf(value) => value
    case Branch (value, children) => value + (children map sum).sum

  def countLeaves(t: Tree): Int = t match
    case Leaf(_) => 1
    case Branch(_, children) => (children map countLeaves).sum

  def flatten(t: Tree): List[Int] = t match
    case Leaf(value) => List(value)
    case Branch(value, children) => value :: (children flatMap flatten)
}

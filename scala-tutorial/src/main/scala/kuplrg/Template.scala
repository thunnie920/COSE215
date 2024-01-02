package kuplrg

trait Template {

  def volumeOfCuboid(a: Int, b: Int, c: Int): Int

  def concat(x: String, y: String): String

  def mulN(n: Int): Int => Int

  def twice(f: Int => Int): Int => Int

  def compose(f: Int => Int, g: Int => Int): Int => Int

  def double(l: List[Int]): List[Int]

  def product(l: List[Int]): Int

  def getOrNotFound(m: Map[String, Int], s: String): Int

  sealed trait Tree
  case class Branch(value: Int, children: List[Tree]) extends Tree
  case class Leaf(value: Int) extends Tree

  def depth(t: Tree): Int

  def sum(t: Tree): Int

  def countLeaves(t: Tree): Int

  def flatten(t: Tree): List[Int]
}

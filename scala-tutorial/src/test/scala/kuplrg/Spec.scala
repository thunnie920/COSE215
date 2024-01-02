package kuplrg

import Implementation.*

class Spec extends SpecBase {
  // tests for `volumeOfCuboid`
  test(volumeOfCuboid(1, 3, 5), 15)
  test(volumeOfCuboid(2, 3, 4), 24)

  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a * b * c

  // tests for `concat`
  test(concat("x", "y"), "xy")
  test(concat("abc", "def"), "abcdef")
  
  def concat(x: String, y: String): String = x + y

  // tests for `mulN`
  test(mulN(3)(5), 15)
  test(mulN(4)(5), 20)
  
  def mulN(n: Int): Int => Int = (x: Int) => n * x

  // tests for `twice`
  test(twice(mulN(3))(5), 45)
  test(twice(mulN(4))(5), 80)

  def twice(f: Int => Int): Int => Int = (x: Int) => f(f(x))

  // tests for `compose`
  test(compose(mulN(3), mulN(4))(5), 60)
  test(compose(mulN(4), mulN(5))(5), 100)

  def compose(f: Int => Int, g: Int => Int): Int => Int = (x: Int) => g(f(x))

  // tests for `double`
  test(double(List(1, 2, 3)), List(2, 4, 6))
  test(double(double(List(1, 2, 3, 4, 5))), List(4, 8, 12, 16, 20))
  
  def double(l: List[Int]): List[Int] = l.map(_ * 2)

  // tests for `product`
  test(product(List(1, 2, 3)), 6)
  test(product(List(4, 2, 3, 7, 5)), 840)

  def product(l: List[Int]): Int = l.foldLeft(1)(_ * _)

  // tests for `getOrNotFound`
  val m: Map[String, Int] = Map("Park" -> 1, "Kim" -> 2)
  test(getOrNotFound(m, "Park"), 1)
  test(getOrNotFound(m, "Kim"), 2)
  testExc(getOrNotFound(m, "Ryu"), "Not Found")
  testExc(getOrNotFound(m, "Hong"), "Not Found")

  def getOrNotFound(m: Map[String, Int], s: String): Int = m.getOrElse(s, error("Not Found"))

  

  // example trees

  //     1
  //   / | \
  //  2  3  4
  val t1: Tree = Branch(1, List(Leaf(2), Leaf(3), Leaf(4)))

  //    1
  //   / \
  //  2   3
  //     / \
  //    4   5
  val t2: Tree = Branch(1, List(Leaf(2), Branch(3, List(Leaf(4), Leaf(5)))))

  //    5
  //    |
  //    4
  //    |
  //    3
  //   / \
  //  2   1
  val t3: Tree = Branch(5, List(Branch(4, List(Branch(3, List(Leaf(2), Leaf(1)))))))

  // tests for `depth`
  test(depth(t1), 1)
  test(depth(t2), 2)
  test(depth(t3), 3)

  def depth(t: Tree): Int = t match
    case Leaf(_) => 0
    case Branch(_, children) => 1 + (children map depth).max


  // tests for `sum`
  test(sum(t1), 10)
  test(sum(t2), 15)
  test(sum(t3), 15)

  def sum(t: Tree): Int = t match
    case Leaf(value) => value
    case Branch (value, children) => value + (children map sum).sum
  

  // tests for `countLeaves`
  test(countLeaves(t1), 3)
  test(countLeaves(t2), 3)
  test(countLeaves(t3), 2)

  def countLeaves(t: Tree): Int = t match
    case Leaf(_) => 1
    case Branch(_, children) => (children map countLeaves).sum

  // tests for `flatten`
  test(flatten(t1), List(1, 2, 3, 4))
  test(flatten(t2), List(1, 2, 3, 4, 5))
  test(flatten(t3), List(5, 4, 3, 2, 1))

  def flatten(t: Tree): List[Int] = t match
    case Leaf(value) => List(value)
    case Branch(value, children) => value :: (children flatMap flatten)

  /* Write your own tests */
}

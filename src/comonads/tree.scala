package comonads

import scalaz._

class Tree[A]
case class Leaf[E, A](attr: A, value: E) extends Tree[A]
case class Fork[E, A](attr: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object treeComonad extends Comonad[Tree] with Cobind.FromCojoin[Tree] {
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(attr, value) => Leaf(f(attr), value)
    case Fork(attr, l, r) => Fork(f(attr), map(l)(f), map(r)(f))
  }

  def copoint[A](t: Tree[A]) = t match {
    case Leaf(attr, _) => attr
    case Fork(attr, _, _) => attr
  }

  def cojoin[A](t: Tree[A]): Tree[Tree[A]] = t match {
    case Leaf(attr, value) => Leaf(t, value)
    case Fork(attr, l, r) => Fork(t, cojoin(l), cojoin(r))
  }

}

object treeExample {

  def stringize[A](t: Tree[A]): String = t match {
    case Leaf(a, v) => v.toString
    case Fork(a, l, r) => stringize(l) + stringize(r)
  }

  def main(args: Array[String]) {
    val treeEx = Fork("root", Leaf("A", 1), Leaf("B", 2))
    println(treeComonad.copoint(treeEx))
    println(treeComonad.cobind(treeEx)(stringize))

  }
}
package comonads

import scalaz._

case class Store[A, I](peek: I => A, index: I)

class storeComonad[I] extends Comonad[({ type f[x] = Store[x, I] })#f] {
  def map[A, B](s: Store[A, I])(f: A => B): Store[B, I] = Store({ i => f(s.peek(i)) }, s.index)
  def copoint[A](s: Store[A, I]): A = s.peek(s.index)
  def cojoin[A](s: Store[A, I]): Store[Store[A, I], I] = Store({ i => Store(s.peek, i) }, s.index)
  def cobind[A, B](s: Store[A, I])(f: Store[A, I] => B): Store[B, I] = Store({ i => f(Store(s.peek, i)) }, s.index)
}

object storeExample {
  def main(args: Array[String]) {
    val a = Array(1, 2, 3, 4, 5)
    val exstore = Store({ i: Int => a(i) }, 1)

    val stcom = new storeComonad[Int]
    println(stcom.copoint(exstore))
    println(stcom.copoint(stcom.cobind(exstore) { case Store(peek, index) => peek(index).toString + "s" }))
  }

}
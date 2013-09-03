package comonads

import scalaz._

case class Store[A, I](peek: I => A, index: I) {
  override def toString = (peek(index), index).toString
}

class storeComonad[I] extends Comonad[({ type f[x] = Store[x, I] })#f] {
  def >>-[A, B, C](a: Store[A, I] => Store[B, I], b: Store[B, I] => Store[C, I]): Store[A, I] => Store[C, I] = {
    a andThen b
  }
  def >>--[A, B, C](a: Store[A, I] => Store[B, I], b: B => Store[B, I] => Store[C, I]): Store[A, I] => Store[C, I] =
    co =>
      {
        val x = a(co)
        b(copoint(x))(x)
      }
  def coreturn[A, B](v: B)(s: Store[A, I]): Store[B, I] = cobind(s) { _ => v }

  def map[A, B](s: Store[A, I])(f: A => B): Store[B, I] = Store({ i => f(s.peek(i)) }, s.index)
  def copoint[A](s: Store[A, I]): A = s.peek(s.index)
  def cojoin[A](s: Store[A, I]): Store[Store[A, I], I] = Store({ i => Store(s.peek, i) }, s.index)
  def cobind[A, B](s: Store[A, I])(f: Store[A, I] => B): Store[B, I] = Store({ i => f(Store(s.peek, i)) }, s.index)

  def get[A](s: Store[A, I]): Store[I, I] = Store({ x => x }, s.index)
  def set[A](newI: I)(s: Store[A, I]): Store[A, I] = Store(s.peek, newI)
  def modify[A](mutator: I => I, s: Store[A, I]): Store[A, I] = Store({ mutator andThen s.peek }, s.index)
  def runStore[A, B](fun: Store[I, I] => Store[B, I], v0: I) = {
    val a = fun(Store({ x: I => x }, v0))
    (a.peek(a.index), a.index)
  }

}

object storeExample {
  def main(args: Array[String]) {
    val sc = new storeComonad[Int]
    def foobar[A] = sc.>>--({ s: Store[A, Int] => sc.get(s) }, { x: Int => { s: Store[Int, Int] => sc.coreturn(3 * x)(s) } })

    val start = Store({ x: Int => x }, 4)
    println(start)
    val s1 = sc.get(start)
    println(s1)
    val s2 = sc.set(15)(s1)
    println(s2)
    val s3 = foobar(s2)
    println(s3)
    val s4 = sc.set(sc.copoint(s1))(s3)
    println(s4)
    val s5 = sc.coreturn(sc.copoint(s3).toString)(s4)
    println(s5)

    /*val a = Array(1, 2, 3, 4, 5)
    val exstore = Store({ i: Int => a(i) }, 1)

    println(sc.copoint(exstore))
    println(sc.copoint(sc.set(2, sc.cobind(exstore) { case Store(peek, index) => peek(index).toString + "s" })))*/
  }

}
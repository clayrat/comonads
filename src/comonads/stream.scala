package comonads

import scalaz._

object streamC extends Comonad[Stream] {
  def cobind[A, B](fa: Stream[A])(f: Stream[A] => B): Stream[B] =
    if (fa.isEmpty)
      Nil.toStream
    else
      f(fa) #:: cobind(fa.tail)(f)
  def copoint[A](p: Stream[A]): A = p.head
  def map[A, B](fa: Stream[A])(f: A => B): Stream[B] = fa.map(f)
  def cojoin[A](a: Stream[A]): Stream[Stream[A]] = cobind(a) { x: Stream[A] => x }

  def next[A](p: Stream[A]) = p.tail
  def produce[A](f: A => A, init: A): Stream[A] = {
    val x = f(init)
    x #:: produce(f, x)
  }
}

object test {
  def main(args: Array[String]) = {
    val a = (1 to 10).toStream
    println(streamC.cobind(a)(streamC.copoint).toList) // identity
  }

}
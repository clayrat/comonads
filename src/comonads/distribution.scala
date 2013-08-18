package comonads

import scala.util.Random
import scala.annotation.tailrec
import scalaz._

case class Distribution[A](get: () => A)

object distComonad extends Comonad[Distribution] {
  def copoint[A](p: Distribution[A]): A = p.get()
  def map[A, B](a: Distribution[A])(f: A => B): Distribution[B] = new Distribution[B](get = () => f(a.get()))
  def cobind[A, B](fa: Distribution[A])(f: Distribution[A] => B): Distribution[B] = new Distribution[B](get = () => f(fa))
  def cojoin[A](a: Distribution[A]): Distribution[Distribution[A]] = cobind(a) { x: Distribution[A] => x }

  @tailrec
  final def coiterate[A](a: Distribution[A])(n: Int, f: Distribution[A] => A): Distribution[A] = {
    if (n == 0) a
    else coiterate(cobind(a)(f))(n - 1, f)
  }

}

object dist {
  private val rand = new Random()
  def always[A](value: A) = new Distribution[A](get = () => value)
  def discreteUniform[A](values: Iterable[A]): Distribution[A] = new Distribution[A](get = () => {
    val vec = Vector() ++ values
    vec(rand.nextInt(vec.length))
  })
  sealed abstract class Coin
  case object H extends Coin
  case object T extends Coin
  def coin: Distribution[Coin] = discreteUniform(List(H, T))

  object uniform extends Distribution[Double](get = () => rand.nextDouble())

  def comarkov[A](init: Distribution[A], steps: Int)(transition: Distribution[A] => A): Distribution[A] = {
    distComonad.coiterate(init)(steps, transition)
  }

}

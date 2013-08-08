package comonads

import scalaz._

case class Relative[A](f: Double => A)

object relativeComonad extends Comonad[Relative] {
  def map[A,B](r: Relative[A])(f: A => B): Relative[B] = Relative({ w=> f(r.f(w))})
  def copoint[A](r: Relative[A]): A = r.f(0.0)
  def cojoin[A](r: Relative[A]): Relative[Relative[A]] = Relative({ w1 => Relative({ w2 => r.f(w1 + w2) }) })
  def cobind[A, B](r: Relative[A])(f: Relative[A] => B): Relative[B] = Relative({ w1 => f(Relative({ w2 => r.f(w1 + w2) })) })
}

object relExample {
  def main(args: Array[String]) {
    def differentiate(r: Relative[Double]) = (r.f(0.01) - r.f(0)) / 0.01
    def fun = Relative(math.sin)
    val dfun = relativeComonad.cobind(fun)(differentiate)
    
    println(fun.f(3.1415))
    println(dfun.f(3.1415))

    
  }
  
}


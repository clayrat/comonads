package comonads

import scalaz._

case class CArray[I, A](arr: Map[I, A], i: I)

class CAcomonad[I] extends Comonad[({ type f[x] = CArray[I, x] })#f] {
  def copoint[A](a: CArray[I, A]): A = a.arr(a.i)
  def cobind[A, B](a: CArray[I, A])(f: CArray[I, A] => B): CArray[I, B] =
    {
      val es = a.arr.keys.map { j => (j, f(CArray(a.arr, j))) }.toMap
      CArray(es, a.i)
    }

  def cojoin[A](a: CArray[I, A]): CArray[I, CArray[I, A]] = ??? // Members declared in 
  def map[A, B](fa: CArray[I, A])(f: A => B): CArray[I, B] = cobind(fa)(FA => f(copoint(FA)))

}

object array {

  def laplace2D(a: CArray[(Int, Int), Float]): Float =
    relInd(a, (-1, 0)) +
      relInd(a, (1, 0)) +
      relInd(a, (0, -1)) +
      relInd(a, (0, 1)) - 4 * relInd(a, (0, 0))

  def plus(a: (Int, Int), b: (Int, Int)) = (a._1 + b._1, a._2 + b._2)
  def inRange(a: (Int, Int), minmax: ((Int, Int), (Int, Int))) = {
    def min = minmax._1
    def max = minmax._2
    (min._1 < a._1) && (max._1 > a._1) && (min._2 < a._2) && (max._2 > a._2)
  }
  def bounds(a: CArray[(Int, Int), Float]): ((Int, Int), (Int, Int)) = {
    def indices = a.arr.keys
    (indices.min, indices.max)
  }

  def relInd(a: CArray[(Int, Int), Float], i: (Int, Int)): Float =
    if (inRange(plus(a.i, i), bounds(a))) a.arr(plus(a.i, i)) else 0
    
  def main(args: Array[String]) {
      
    }
}


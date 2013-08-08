package comonads

import scalaz._
import scala.collection.immutable.Stream.consWrapper

case class StreamZipper[X](left: Stream[X], center: X, right: Stream[X]) {
  def shiftRight = this match {
    case StreamZipper(a, b, c #:: cs) => StreamZipper(b #:: a, c, cs)
  }
  def shiftLeft = this match {
    case StreamZipper(a #:: as, b, c) => StreamZipper(as, a, b #:: c)
  }
  def shift(i: Int) =
    Stream.iterate(this)(x => if (i < 0) x.shiftLeft else x.shiftRight).apply(i.abs)

  def half = this match {
    case StreamZipper(_, b, c) => Stream(b) ++ c
  }

  def toStream(i: Int, j: Int) = this.shift(i).half.take(j - i)
}

case class PlaneZipper[X](plane: StreamZipper[StreamZipper[X]]) {
  def up = PlaneZipper(plane.shiftLeft)
  def down = PlaneZipper(plane.shiftRight)
  def left = PlaneZipper(szComonad.map(plane)(_.shiftLeft))
  def right = PlaneZipper(szComonad.map(plane)(_.shiftRight))
  def horizontal = StreamZipper(Stream.iterate(this)(_.left).tail, this, Stream.iterate(this)(_.right).tail)
  def vertical = StreamZipper(Stream.iterate(this)(_.up).tail, this, Stream.iterate(this)(_.down).tail)
}

object szComonad extends Comonad[StreamZipper] with Cobind.FromCojoin[StreamZipper] {
  def map[A, B](x: StreamZipper[A])(f: A => B): StreamZipper[B] =
    StreamZipper(x.left.map(f), f(x.center), x.right.map(f))
  def copoint[A](p: StreamZipper[A]): A = p.center
  def cojoin[A](a: StreamZipper[A]): StreamZipper[StreamZipper[A]] =
    StreamZipper(Stream.iterate(a)(_.shiftLeft).tail, a, Stream.iterate(a)(_.shiftRight).tail)
}

object pzComonad extends Comonad[PlaneZipper] with Cobind.FromCojoin[PlaneZipper] {
  def map[A, B](x: PlaneZipper[A])(f: A => B): PlaneZipper[B] =
    PlaneZipper(szComonad.map(x.plane)(szComonad.map(_)(f)))
  def copoint[A](p: PlaneZipper[A]): A = p.plane.center.center
  def cojoin[A](a: PlaneZipper[A]): PlaneZipper[PlaneZipper[A]] =
    PlaneZipper(szComonad.map(a.vertical)(_.horizontal))
}

object CA {

  val falseStream = Stream continually false

  def drawLine(x: Stream[Boolean]) = x.map(c => if (c) '#' else ' ').mkString("|")

  def rule60(a: Boolean, b: Boolean, c: Boolean) = a != b
  
  def zipperRule(rule: (Boolean, Boolean, Boolean) => Boolean)(u: StreamZipper[Boolean]) = u match {
    case StreamZipper(a #:: _, b, c #:: _) => rule(a, b, c)
  }
  
  def conway(pz: PlaneZipper[Boolean]): Boolean = {
    def aliveNeighbours(pz: PlaneZipper[Boolean]): Int =
      List(pz.left, pz.right, pz.up, pz.down, pz.left.up, pz.left.down, pz.right.up, pz.right.down)
        .map { pzComonad.copoint(_) }.filter(a => a).size

    aliveNeighbours(pz) match {
      case 2 => pzComonad.copoint(pz)
      case 3 => true
      case _ => false
    }
  }

  def CA1D(l: Int, r: Int, t: Int, rule: StreamZipper[Boolean] => Boolean): String = {
    val seed = StreamZipper(falseStream, true, falseStream)
    val ss = Stream.iterate(seed)(szComonad.cobind(_)(rule))
    ss.map(s => drawLine(s.toStream(l, r))).take(t).force.mkString("\n")
  }

  def CA2D(l: Int, r: Int, u: Int, d: Int, t: Int, rule: PlaneZipper[Boolean] => Boolean): String = {
    def falseZipper = StreamZipper(falseStream, false, falseStream)
    def falseZipperStream = Stream continually falseZipper
    def line(l: List[Boolean]) = StreamZipper(falseStream, false, l.toStream ++ falseStream)
    val rs = List(line(List(false, true, false)),
      line(List(false, false, true)),
      line(List(true, true, true)))
      .toStream ++ falseZipperStream
    val glider = PlaneZipper(StreamZipper(falseZipperStream, falseZipper, rs))
    val sss = Stream.iterate(glider)(pzComonad.cobind(_)(rule))
    ///sss.map(s => draw(s.toStream(l, r))).mkString("\n").take(t).force
    sss.map(
      _.plane.toStream(u, d).map(
        x => drawLine(x.toStream(l, r))).force.mkString("\n")).take(t).mkString("\n\n")
  }

  def main(args: Array[String]) {
    println(CA1D(-20, 20, 20, zipperRule(rule60)))
    println(CA2D(-10, 10, -10, 10, 5, conway))

  }
}
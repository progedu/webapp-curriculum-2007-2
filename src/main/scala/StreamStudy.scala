trait StreamStudy[+A] {

  def headOption: Option[A] = {
    this match {
      case EmptyStream => None
      case a: Cons[A] => Option(a.h())
    }
  }

  def tail: StreamStudy[A] = {
    this match {
      case EmptyStream => EmptyStream
      case a: Cons[A] => a.t()
    }
  }
}

case object EmptyStream extends StreamStudy[Nothing]

case class Cons[+A](h: () => A, t: () => StreamStudy[A]) extends StreamStudy[A]

object StreamStudy {

  def cons[A](h: => A, t: => StreamStudy[A]): StreamStudy[A] = {
    Cons(() => h, () => t)
  }

  def empty[A]: StreamStudy[A] = EmptyStream

}

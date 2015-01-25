package fpinscala.laziness

import Stream._
trait Stream[+A] {
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def map[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def take(n: Int): Stream[A] = unfold((n, this)) {
    case (i, Cons(a, as)) if i > 0 => Some((a(), (i - 1, as())))
    case _ => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n -1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(a, as) if p(a()) => Some((a(), as()))
    case _ => None
  }

  def zipWith[B,C](ys: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, ys)){
      case (Cons(a, as), Cons(b, bs)) => Some((f(a(), b()), (as(), bs())))
      case _ => None
    }

  def zip[B](ys: Stream[B]): Stream[(A,B)] =
    zipWith(ys)((a, b) => (a, b))

  def zipAll[B,C](ys: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, ys)){
      case (Cons(a, as), Cons(b, bs)) => Some(((Some(a()), Some(b())), (as(), bs())))
      case (Cons(a, as), Empty) => Some(((Some(a()), None), (as(), empty)))
      case (Empty, Cons(b, bs)) => Some(((None, Some(b())), (empty, bs())))
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def startsWith[B](s: Stream[B]): Boolean = (zipAll(s) foldRight true) {
    case ((Some(a), Some(b)), true) if a == b => true
    case ((_, None), true) => true
    case _ => false
  }

  def tails: Stream[Stream[A]] = cons(this,
    unfold(this) {
      case Cons(_, as) => Some((as(), as()))
      case _ => None
    }
  )
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  val fibs2 = unfold((0, 1)){ case (x, y) => Some((x, (y, x + y))) }

  def from(n: Int): Stream[Int] = unfold(n)(x => Some((x, x + 1)))

  def constant[A](a : A): Stream[A] =
    unfold(Nil)( _ => Some((a, Nil)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Stream.empty
  }
}
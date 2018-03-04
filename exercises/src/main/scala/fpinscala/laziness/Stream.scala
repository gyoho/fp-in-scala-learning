package fpinscala.laziness

import Stream._
trait Stream[+A] {
  def toListNonTailRecursive: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toListNonTailRecursive
  }

  def toListTailRecursive: List[A] = {
    @annotation.tailrec
    // to finish with itself invocation and to proceed to next, pass the remaining as an argument
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
//      case Cons(h, t) =>  go(t(), acc :+ h())  // appending to tail has O(n) time complexity
      case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, List()).reverse
  }

  def toList: List[A] = {
    val buffer = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => buffer.toList
      case Cons(h, t) =>
        buffer += h()
        go(t())
    }
    go(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _                   => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _                    => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def forAllVerbose(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => t().forAllVerbose(p)
    case _                    => true
  }

  def headOption: Option[A] = foldRight(None[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)


  def startsWith[B](s: Stream[B]): Boolean = ???
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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // fib: 0, 1, 1, 2, 3, 5, ... -> next = prev + curr
  def fib: Stream[Int] = {
    def fibHelper(prev: Int, curr: Int): Stream[Int] = {
      cons(prev, fibHelper(curr, prev + curr))
    }
    fibHelper(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}
package ch05

sealed trait Stream[+A] {
    def headOption(): Option[A] = this match {
        case Empty => None
        case Cons(head, _) => Some(head())
    }

    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(head, tail) => head() :: tail().toList
    }

    def take(n: Int): Stream[A] = this match {
        case Cons(head, tail) if n > 0 => Stream.cons(head(), tail().take(n - 1))
        case _ => Stream.empty
    }

    def drop(n: Int): Stream[A] = this match {
        case Cons(_, tail) if n > 0 => tail().drop(n-1)
        case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Cons(head, tail) if (p(head())) => Stream.cons(head(), tail() takeWhile p)
        case _ => Stream.empty
    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
package ch02;

sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(get) => Some(f(get))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(get) => get
    }

    // TODO : not yet implemented
    def orElse[B >: A](obj: => Option[B]): Option[B] = this

    def filter(f: A => Boolean): Option[A] = if (map(f).getOrElse(false)) this else None
}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]
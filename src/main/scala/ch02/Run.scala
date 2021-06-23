package ch02

object Ch02 {
    def run(): Unit = {
        println(s"""
################ Chapter 4: Handling errors without exceptions""")

        val seq = List(Some(1), Some(2), Some(3))
        println(s"""
        Exercise 4.4: List[Option[A]] to Option[List[A]] using sequence
        ${sequence(seq)}""")

    }

    // def variance(xs: Seq[Double]): Option[Double] = {
    //     ???
    // }

    // def mean(xs: Seq[Double]): Double = xs.sum() / xs.length

    def sequence[A](ls: List[Option[A]]): Option[List[A]] = ls match {
        case Nil => Some(Nil)
        case head :: tail => head flatMap(hd => sequence(tail) map (hd :: _))
    }
}
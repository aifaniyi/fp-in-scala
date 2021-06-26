package ch05

object Ch05 {
    def run(): Unit = {
        val stream: Stream[Int] = Stream(1,2,3,4,5,6,7,8,9)
        println(s"""
################ Chapter 5: strict and non-strict functions
        stream = ${stream.toList}""")

        println(s"""
        Exercise 5.1: convert stream to list
        ${stream.toList}""")

        println(s"""
        Exercise 5.2: take n elements from stream
        take(5) = ${stream.take(5).toList}
        drop(5) = ${stream.drop(5).toList}""")

        println(s"""
        Exercise 5.3: takeWhile predicate function is true
        takeWhile(_<5) = ${stream.takeWhile(_ < 5).toList}""")

        println(s"""
        Exercise 5.4: test if all elements satisfy condition 
        forAll(_<10) = ${stream.forAll(_ < 10)}
        forAll(_<5) = ${stream.forAll(_ < 5)}""")
    }
}
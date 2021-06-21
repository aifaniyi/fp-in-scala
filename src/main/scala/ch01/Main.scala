package ch01;

object Main {
  
    def main(args: Array[String]) = {
        val list = List(1,2,3,4,5,6,7,8,9)

        println(s"""
        Exercise 3.4: Drop n elements from a List
        ${List.drop(list, 3)}""")

        println(s"""
        Exercise 3.5: Dropwhile List prefix satisfies a condition
        ${List.dropWhile(list, (x: Int) => x <= 6)}""")

        println(s"""
        Exercise 3.6: init to return all but last list element
        ${List.init(list)}""")

        println(s"""
        Exercise 3.9: len of list
        ${List.length(list)}""")
    }
}

package ch01;

object Ch01 {
    def run(): Unit = {

        println("#################### CHAPTER 03: Functional Data structures")
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

        println(s"""
        Exercise 3.11: sum product and length using fold left
        Sum = ${
            List.foldLeft(list, 0)((x, y) => x + y)
        }        
        Product = ${
            List.foldLeft(list, 1)((x, y) => x*y)
        }        
        Length = ${
            List.foldLeft(list, 0)((x, y) => x+1)
        }""")

        println(s"""
        Exercise 3.12: reverse a list
            ${List.reverse(list)}
        """)

        println(s"""
        Exercise 3.14: append to a list
            ${List.append(list, List(10,11,12,13,14,15,16,17))}
        """)

        println(s"""
        Exercise 3.18: map a list to squares
            ${List.map(list)(x => x*x)}
        """)

        println(s"""
        Exercise 3.19: filter a list
            ${List.filter(list)(_%2 == 0)}
        """)

        println(s"""
        Exercise 3.20: flatMap
            ${List.flatMap(list)(x => List(x, x))}
        """)

        println(s"""
        Exercise 3.21: filter using flatMap
            ${List.filterUsingFlatMap(list)(_ > 5)}
        """)

        println(s"""
        Exercise 3.23: zip
            ${List.zip(list, List(9,8,7,6,5,4,3,2,1))(_ + _)}
        """)
    }
}

def sumIf(in: List[Int], cmp: Int => Boolean): Int = in match {
    case Nil => 0
    case x :: rest if cmp(x) => x + sumIf(rest, cmp)
    case _ :: rest => sumIf(rest, cmp)
}

val l = (1 to 200).toList
println(sumIf(l, { n => n % 2 == 1 }).toString)

def noPairs[T](in: List[T]): List[T] = in match {
    case Nil => Nil
    case a :: b :: rest if a == b => noPairs(a :: rest)
    case a :: rest => a :: noPairs(rest)
}

val m = ((1 to 10).toList ::: (1 to 10).toList).sorted
println(m)
println(noPairs(m))

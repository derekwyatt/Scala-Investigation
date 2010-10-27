
// This file needs to be compiled and then run

class MList[+T] {
    def ?:[B >: T](x: B): MList[B] = new ?:(x, this)
}

case object MNil extends MList[Nothing]

case class ?:[T](hd: T, tail: MList[T]) extends MList[T]

object Main {

    def tryMList(in: MList[Any]) = in match {
        case 1 ?: MNil => "foo"
        case 1 ?: _ => "bar"
        case _ => "baz"
    }

    def main(args: Array[String]) {
        println(tryMList(1 ?: "Two" ?: MNil))
        println(tryMList(1 ?: MNil))
        println(tryMList(MNil))
    }
}

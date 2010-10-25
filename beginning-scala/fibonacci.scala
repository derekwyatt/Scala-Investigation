def fibonacci(i: Int): BigInt = i match {
    case 0 => 0
    case 1 => 1
    case n => fibonacci(n - 1) + fibonacci(n - 2)
}

(1 to 40).foreach(i => println("[" + i + "]: " + fibonacci(i)))


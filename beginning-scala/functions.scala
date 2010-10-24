def identity(i: Int) = i

def funcParam1(i: Int, func: Int => Int) = "Func returns: " + func(i)

def funcParam2(i: Int)(func: Int => Int) = "Func returns: " + func(i)

def forPartialFunc1(i: Int, j: Int, k: Int) = i + j + k

def forPartialFunc2(i: Int, j: Int)(k: Int) = i + j + k

println("identity(5) = " + identity(5))
println("funcParam1(5, r) = " + funcParam1(5, identity))
println("funcParam1(5, {code}) = " + funcParam1(5, { i => i * i }))
println("funcParam2(5, {code}) = " + funcParam2(5)({ i => i * i }))
println("funcParam1(5, forPartialFunc1(1, 2, _)) = " + funcParam1(5, forPartialFunc1(1, 2, _)))

def partial = forPartialFunc2(1, 2) _

println("funcParam1(5, partial) = " + funcParam1(5, partial))

def typeIdentity[T](t: T): T = t

println("typeIdentity[String]('Hithere') = " + typeIdentity("Hithere"))

class MySomeClass(val s: String) { override def toString = "\"" + s + "\"" }

println("typeIdentity[MySomeClass](...) = " + typeIdentity(new MySomeClass("MySomeClass Right Here")))

def typeFunction[T, U](i: T, f: T => U) = {
    println("Running typeFunction: Calling f(i)")
    println(f(i).toString)
    println("Running typeFunction: Done f(i)")
}

def toBeCalled(i: Int) = "toBeCalled returning " + i

typeFunction(200, toBeCalled)

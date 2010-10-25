sealed trait Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Val(value: Int) extends Expr
case class Var(name: String) extends Expr

def calc(expr: Expr, vars: Map[String, Int]): Int = {
    expr match {
        case Add(left, right) => calc(left, vars) + calc(right, vars)
        case Mul(left, right) => calc(left, vars) * calc(right, vars)
        case Val(v) => v
        case Var(name) => vars(name)
    }
}

println(calc(Mul(Val(11), Add(Val(6), Val(5))), Map.empty))

def buildCalc(expr: Expr): Map[String, Int] => Int = {
    expr match {
        case Add(left, right) => {
            val lf = buildCalc(left)
            val rf = buildCalc(right)
            m => lf(m) + rf(m)
        }
        case Mul(left, right) => {
            val lf = buildCalc(left)
            val rf = buildCalc(right)
            m => lf(m) * rf(m)
        }
        case Val(v) => m => v
        case Var(name) => m => m(name)
    }
}

val f = buildCalc(Mul(Val(12), Add(Val(9), Val(3))))
println(f(Map.empty))

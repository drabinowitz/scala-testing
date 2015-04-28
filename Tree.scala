object TreeTesting {
  type Environment = String => Int

  abstract class Tree
  case class Sum(l: Tree, r: Tree) extends Tree
  case class Var(n: String) extends Tree
  case class Const(v: Int) extends Tree

  def eval(t: Tree, env: Environment): Int = t match {
    case Sum(l, r) => eval(l, env) + eval(r, env)
    case Var(n)    => env(n)
    case Const(v)  => v
  }

  def simplify(t: Tree): Tree = t match {
    case Sum(l, r) if (l != null || r != null) =>
      val simplifiedL = simplify(l)
      val simplifiedR = simplify(r)
      if (simplifiedR == null && simplifiedL == null)
        null
      else if (simplifiedL == null)
        simplifiedR
      else if (simplifiedR == null)
        simplifiedL
      else
        Sum(simplifiedL, simplifiedR)

    case Const(v) if (v != 0) => Const(v)
    case _ => null
  }

  def derive(t: Tree, v: String): Tree = t match {
    case Sum(l, r) => Sum(derive(l, v), derive(r, v))
    case Var(n) if (v == n) => Const(1)
    case _ => Const(0)
  }

  def main(args: Array[String]) {
    val exp: Tree = Sum(Sum(Var("x"), Var("x")), Sum(Const(7), Var("y")))
    val env: Environment = { case "x" => 5 case "y" => 7 }
    println("Expression: " + exp)
    println("Evaluation with x=5, y=7: " + eval(exp, env))
    println("Derivative relative to x:\n " + derive(exp, "x"))
    println("Derivative relative to y:\n " + derive(exp, "y"))
    println("Simplified derivative relative to x:\n " + simplify(derive(exp, "x")))
    println("Simplified derivative relative to y:\n " + simplify(derive(exp, "y")))
  }
}

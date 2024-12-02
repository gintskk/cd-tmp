object Uniquify {

  import GenSym._
  import Lint.{Expr, Prim}
  import Lvar._

  def uniquifyProgram(p: Lvar): Lvar = p match {
    case Program(body) => Program(uniquifyBody(body))
    case _ => throw new RuntimeException()
  }

  private def uniquifyBody(body: Expr): Expr = body match {
    case Let(name, e1, e2) => {
      val newName = gensym(name)
      Let(newName, uniquifyBody(e1), replaceName(e2, name, newName))
    }
    case Prim(operator, args) => Prim(operator, args.map(e => uniquifyBody(e)))
    case expr => expr
  }

  private def replaceName(e2: Expr, oldName: String, newName: String): Expr = {
    e2 match {
      case Prim(op, args) => Prim(op, args.map(arg => replaceName(arg, oldName, newName)))
      case Var(x) => {
        if (x == oldName) {
          Var(newName)
        } else {
          Var(x)
        }
      }
      case Let(x, e1, e2) => {
        //Name collision: shadowing fix
        val newNameInner = gensym(x)
        val retVal = Let(newNameInner, replaceName(e1, oldName, newName), replaceName(e2, x, newNameInner))
        Let(retVal.x, retVal.e1, replaceName(retVal.e2, oldName, newName))
      }
      case expr => expr
    }
  }
}

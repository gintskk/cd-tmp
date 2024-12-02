object Select {

  import Cvar._
  import Lint.Expr
  import Lint.Minus
  import Lint.Num
  import Lint.Plus
  import Lint.Prim
  import Lint.Read
  import Lvar._
  import X86int._
  import X86var._

  def selectProgram(p: Cvar): X86var = p match {
    case CProgram(blocks) => X86VarProgram(new Info, blocks.map { case (name, t) => (name, Blk(new BlockInfo, selectTail(t)))
    })
  }

  def selectTail(t: Tail): List[Instruction] = t match {
    case Seq(s, t) => selectStmt(s) ++ selectTail(t)
    case Return(e) => selectStmt(Assign("return_temp", e)) ++ List(Instr(Movq(), List(XVar("return_temp"), Reg(RAX()))))
  }

  def selectStmt(s: Stmt): List[Instruction] = s match {
    case Assign(x, e) => e match {
      case Num(i) => List(Instr(Movq(), List(Imm(i), XVar(x))))
      case Var(v) => List(Instr(Movq(), List(XVar(v), XVar(x))))
      case Prim(op, es) => op match {
        case Plus() => es(1) match {
          case value: Var if value.x == x => // var = (+ a1 var)
            List(Instr(Addq(), List(selectAtm(es(0)), XVar(x))))
          case _ => //var = (+ a1 a2)
            List(Instr(Movq(), List(selectAtm(es(0)), XVar(x))), Instr(Addq(), List(selectAtm(es(1)), XVar(x))))
        }
        case Minus() => if (es.length == 2) {
          List(Instr(Movq(), List(selectAtm(es(0)), XVar(x))), Instr(Subq(), List(selectAtm(es(1)), XVar(x))))
        } else {
          List(Instr(Movq(), List(selectAtm(es(0)), XVar(x))), Instr(Negq(), List(XVar(x))))
        }
        case Read() => List(Callq("_read_int", 0), Instr(Movq(), List(Reg(RAX()), XVar(x))))
        case Print() => List(Instr(Movq(), List(selectAtm(es(0)), XVar(x))), Instr(Movq(), List(selectAtm(es(0)), Reg(RDI()))), Callq("_print_int", 1))
      }
    }
  }

  def selectAtm(e: Expr): Arg = e match {
    case Num(i) => Imm(i)
    case Var(x) => XVar(x)
    case _ => throw new RuntimeException()
  }
}

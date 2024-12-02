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

  private def selectTail(t: Tail): List[Instruction] = t match {
    case Seq(s, t) => selectStatement(XVar(s.asInstanceOf[Assign].x), s.asInstanceOf[Assign].e) ++ selectTail(t)
    case Return(e) => selectStatement(Reg(RAX()), e)
  }

  private def selectStatement(x: Arg, e: Expr): List[Instruction] = e match {
    case Num(i) => List(Instr(Movq(), List(Imm(i), x)))
    case Var(v) => List(Instr(Movq(), List(XVar(v), x)))
    case Prim(op, es) => op match {
      case Plus() => es(1) match {
        case value: Var if x.isInstanceOf[XVar] && value.x == x.asInstanceOf[XVar].x =>
          // var = (+ a1 var)
          List(Instr(Addq(), List(selectAtm(es(0)), x)))
        case _ => //var = (+ a1 a2)
          List(Instr(Movq(), List(selectAtm(es(0)), x)), Instr(Addq(), List(selectAtm(es(1)), x)))
      }
      case Minus() => if (es.length == 2) {
        List(Instr(Movq(), List(selectAtm(es(0)), x)), Instr(Subq(), List(selectAtm(es(1)), x)))
      } else {
        List(Instr(Movq(), List(selectAtm(es(0)), x)), Instr(Negq(), List(x)))
      }
      case Read() => List(Callq("_read_int", 0), Instr(Movq(), List(Reg(RAX()), x)))
      case Print() => List(Instr(Movq(), List(selectAtm(es(0)), x)), Instr(Movq(), List(selectAtm(es(0)), Reg(RDI()))), Callq("_print_int", 1))
    }
  }

  private def selectAtm(e: Expr): Arg = e match {
    case Num(i) => Imm(i)
    case Var(x) => XVar(x)
    case _ => throw new RuntimeException()
  }
}

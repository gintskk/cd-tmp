object PatchInstructions {

  import X86int._

  def patchProgram(p: X86int): X86int = p match {
    case X86IntProgram(info, blocks) => blocks match {
      case Nil => throw new RuntimeException()
      case es => X86IntProgram(info, es.map(e => processBlock(e)))
    }
  }

  private def processBlock(e: (String, Block)): (String, Block) = e match {
    case (str, block) =>
      block match {
        case Blk(info, instrs) => {
          val patchedInstructions = instrs.flatMap(i => processInstr(i))
          (str, Blk(info, patchedInstructions))
        }
      }
  }

  private def processInstr(i: Instruction): List[Instruction] = i match {
    case Instr(c, args) => c match {
      case Addq() | Subq() | Movq() => checkMaxOneMemory(c, args)
      case otherCommand => List(Instr(otherCommand, args))
    }
    case otherInstr => List(otherInstr)
  }

  private def checkMaxOneMemory(c: Cmd, args: List[Arg]): List[Instruction] = args match {
    case List(fst, snd) => (fst, snd) match {
      case (a@Deref(_, _), b@Deref(_, _)) => {
        List(
          Instr(Movq(), List(a, Reg(RAX()))),
          Instr(c, List(Reg(RAX()), b))
        )
      }
      case other => List(Instr(c, List(other._1, other._2)))
    }
  }
}

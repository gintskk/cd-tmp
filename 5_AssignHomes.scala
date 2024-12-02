object AssignHomes {

  import X86int._
  import X86var._

  def assignProgram(p: X86var): X86int = p match {
    case X86var.X86VarProgram(_, blocks) => {
      val stackLayout = allocateVarsOnStack(blocks)
      val newBlocks = replaceVarsWithDerefs(blocks, stackLayout)
      println(stackLayout)
      println(newBlocks)
      X86int.X86IntProgram(ProgramInfo(stackLayout.keys.toList, stackLayout.count(_ => true) * 8), newBlocks)
    }
  }

  private def replaceVarsWithDerefs(blocks: List[(String, Block)], vars: Map[String, Int]): List[(String, Block)] = {
    blocks match {
      case Nil => Nil
      case head :: tail => processBlock(head, vars) :: replaceVarsWithDerefs(tail, vars)
    }
  }

  private def processBlock(head: (String, Block), vars: Map[String, Int]): (String, Block) = head._2 match {
    case Blk(info, instrs) => {
      val newInstrs = instrs.map(i => assignHomes(i, vars))
      (head._1, Blk(info, newInstrs))
    }
  }

  private def assignHomes(i: Instruction, vars: Map[String, Int]): Instruction = i match {
    case Instr(c, args) => Instr(c, args.map { case XVar(x) => {
      vars.get(x) match {
        case None => throw new IllegalArgumentException(s"Variable $x not found in variables list: $vars")
        case Some(address) => println(s"offset: $address")
          Deref(RBP(), address)
      }
    }
    case other => other
    })
    case i@(Callq(_, _) | Retq() | Jmp(_)) => i
  }

  private def allocateVarsOnStack(blocks: List[(String, Block)]): Map[String, Int] = {
    val vars = blocks.flatMap(b => b._2 match {
      case Blk(_, instrs) => findVarsInBlock(instrs)
    }).distinct
    val varMap: Map[String, Int] = vars.zipWithIndex.map { case (str, index) => str -> ((index + 1) * -8)
    }.toMap
    varMap
  }

  private def findVarsInBlock(instrs: List[Instruction]): List[String] = instrs match {
    case Nil => Nil
    case head :: tl => varNames(head) ++ findVarsInBlock(tl)
  }

  private def varNames(head: Instruction): List[String] = head match {
    case Instr(_, args) => args.filter(a => isVariable(a)).map(a => extractVariableName(a))
    case _ => Nil
  }

  private def extractVariableName(a: Arg): String = a match {
    case XVar(x) => x
  }

  private def isVariable(a: Arg): Boolean = a match {
    case XVar(_) => true
    case _ => false
  }
}

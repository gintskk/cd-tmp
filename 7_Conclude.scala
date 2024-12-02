object Conclude {

  import X86int._

  def concludeProgram(p: X86int): X86int = p match {
    case X86IntProgram(info@ProgramInfo(_, stackSpace), ("start", Blk(inf, startInstrs)) :: Nil) => {
      val main = Blk(new BlockInfo, instrs = List(
        Instr(Pushq(), List(Reg(RBP()))),
        Instr(Movq(), List(Reg(RSP()), Reg(RBP()))),
        Instr(Subq(), List(Imm(stackSpace), Reg(RSP()))),
        Jmp("start")))

      val startInstrsWithJmp = startInstrs ++ List(Jmp("conclusion"))

      val conclusion = Blk(new BlockInfo, instrs = List(
        Instr(Addq(), List(Imm(stackSpace), Reg(RSP()))),
        Instr(Popq(), List(Reg(RBP()))),
        Retq()))

      X86IntProgram(info, List(
        ("main", main),
        ("start", Blk(inf, startInstrsWithJmp)),
        ("conclusion", conclusion)))
    }
  }
}


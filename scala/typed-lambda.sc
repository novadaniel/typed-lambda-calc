sealed trait ULTerm
case class ULVar(index: Int) extends ULTerm {
  override def toString() = index.toString()
}
case class ULAbs(t: ULTerm) extends ULTerm {
  override def toString() = "lambda . " + t.toString()
}
case class ULApp(t1: ULTerm, t2: ULTerm) extends ULTerm {
  override def toString() = "(" + t1.toString() + ") (" + t2.toString() + ")"
}

sealed trait STTerm
case class STVar(index: Int) extends STTerm {
  override def toString() = index.toString()
}
case class STApp(x: STTerm, y: STTerm) extends STTerm {
  override def toString() = "(" + x.toString() + ") (" + y.toString() + ")"
}
case class STAbs(t: STType, x: STTerm) extends STTerm {
  override def toString() = "lambda : " + t.toString() + " . " + x.toString()
}
case object STZero extends STTerm {
  override def toString() = "zero"
}
case class STSuc(x: STTerm) extends STTerm {
  override def toString() = "suc(" + x.toString() + ")"
}
case class STIsZero(x: STTerm) extends STTerm {
  override def toString() = "iszero(" + x.toString() + ")"
}
case object STTrue extends STTerm {
  override def toString() = "true"
}
case object STFalse extends STTerm {
  override def toString() = "false"
}
case class STTest(x: STTerm, y: STTerm, z: STTerm) extends STTerm {
  override def toString() = "test (" + x.toString() + ") (" + y.toString() + ") (" + z.toString() + ")"
}

sealed trait STType
case object STNat extends STType {
  override def toString() = "nat"
}
case object STBool extends STType {
  override def toString() = "bool"
}
case class STFun(dom: STType, codom: STType) extends STType {
  override def toString() = "(" + dom.toString + ") -> (" + codom.toString + ")"
}

def typecheck(x: STTerm): Boolean = x match {
  case STTrue => true
  case STFalse => true
  case STZero => true
  case STVar(_) => false
  case other => typeOf(other, List()).isDefined
}

def typeOf(x: STTerm, context: List[STType]): Option[STType] = x match {
  case STVar(_) => context.lastOption
  case STTrue => Some(STBool)
  case STFalse => Some(STBool)
  case STZero => Some(STNat)
  case STAbs(t, x) => try {
    Some(STFun(t, typeOf(x, context :+ t).get))
  } catch {
    case _: NoSuchElementException =>
      None
  }
  case STApp(x, y) =>
    val t1 = typeOf(x, context)
    val t2 = typeOf(y, context)
    t1 match {
      case Some(STFun(dom, codom)) => if (t2.contains(dom)) Some(codom) else None
      case _ => None
    }
  case STSuc(x) =>
    typeOf(x, context) match {
      case Some(STNat) => Some(STNat)
      case _ => None
    }
  case STIsZero(x) =>
    typeOf(x, context) match {
      case Some(STNat) => Some(STNat)
      case _ => None
    }
  case STTest(x, _, _) =>
    typeOf(x, context) match {
      case Some(STBool) => Some(STBool)
      case _ => None
    }
}

def eraseTypes(x: STTerm): ULTerm = x match {
  case STVar(i) => ULVar(i)
  case STTrue => ULAbs(ULAbs(ULVar(1)))
  case STFalse => ULAbs(ULAbs(ULVar(0)))
  case STZero => ULAbs(ULAbs(ULVar(0)))
  case STAbs(_, x) => ULAbs(eraseTypes(x))
  case STApp(x, y) => ULApp(eraseTypes(x), eraseTypes(y))
  case STSuc(x) =>
    ULApp(ULAbs(ULAbs(ULAbs(
        ULApp(
          ULVar(1),
          ULApp(
            ULApp(ULVar(2),ULVar(1)),
            ULVar(0)
          )
        )
    ))), eraseTypes(x))
  case STIsZero(x) =>
    ULAbs(
      ULApp(
        ULApp(eraseTypes(x), ULAbs(eraseTypes(STFalse))),
        eraseTypes(STTrue)
      )
    )
  case STTest(x, y, z) =>
    ULAbs(ULAbs(ULAbs(
      ULApp(
        ULApp(eraseTypes(x), eraseTypes(y)),
        eraseTypes(z)
      )
    )))
}

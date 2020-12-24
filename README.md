#Assignment 2 - Typing a &#955;-calculus
## Scala

Begin with defining `ULTerm`, `STTerm` and `STType`. This was fairly routine, no real design decisions had to be made here. I redefined `toString()` for everything as it's good practice and allowed for easier troubleshooting.
```scala worksheet
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
```
For part 2, I defined the `typecheck(x)` function where for every singleton class, it acts as specified. If it is not singleton, we throw it into `typeOf(x, context)` which returns `None` if no type is defined. Then we just verify that the type is defined and return the correct boolean value with the built in `.isDefined` method.
```scala worksheet
def typecheck(x: STTerm): Boolean = x match {
  case STTrue => true
  case STFalse => true
  case STZero => true
  case STVar(_) => false
  case other => typeOf(other, List()).isDefined
}
```
Continuing with part 2, `typeOf(x, context)` returns either the type of a `STTerm` (`STType`) if found, or `None`. As seen here, all of the specified lambda rules are defined here,
 Rule 1 is applied to `STVar`, Rule 2 to `STAbs`, Rule 3 to `STApp`, and the rest to their respective `STTerm`'s. I found that using the `Option[STType]`,
 led to the cleanest and simplest solution.
```scala worksheet
def typeOf(x: STTerm, context: List[STType]): Option[STType] = x match {
  case STVar(_) => context.lastOption
  case STTrue => Some(STBool)
  case STFalse => Some(STBool)
  case STZero => Some(STNat)
```
Because there is still a chance that `None` can be returned, we have to account for that for every case class.
Because we use `.get` in STAbs, it must be wrapped in a `try/catch`, otherwise just checking for the right type using `match` is enough.
```scala worksheet
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
```
Now for part 3, for the first 4 classes we deal with constants, so I did what was specified. For `STVar`, `STApp` and `STAbs`,
translating them is also relatively trivial as they have direct translations. All that's necessary is removing the type.
```scala worksheet
def eraseTypes(x: STTerm): ULTerm = x match {
  case STVar(i) => ULVar(i)
  case STTrue => ULAbs(ULAbs(ULVar(1)))
  case STFalse => ULAbs(ULAbs(ULVar(0)))
  case STZero => ULAbs(ULAbs(ULVar(0)))
  case STAbs(_, x) => ULAbs(eraseTypes(x))
  case STApp(x, y) => ULApp(eraseTypes(x), eraseTypes(y))
```
For the less trivial ones, I just went according to the specified definitions.
```scala worksheet
  case STSuc(x) =>
    ULApp(ULAbs(ULAbs(ULAbs(ULApp(ULVar(1),ULApp(ULApp(ULVar(2),ULVar(1)),ULVar(0)))))), eraseTypes(x))
  case STIsZero(x) => ULAbs(ULApp(ULApp(eraseTypes(x), ULAbs(eraseTypes(STFalse))), eraseTypes(STTrue)))
  case STTest(x, y, z) => ULAbs(ULAbs(ULAbs(ULApp(ULApp(eraseTypes(x), eraseTypes(y)), eraseTypes(z)))))
}
```

##Ruby

For Ruby, I felt that the subclass approach was the easiest, as I worked on Scala first as it came more naturally to me,
 and I translated the Scala code to Ruby code by throwing each case class into its respective subclass. I copied down the
 contents of `ULTerm` into this file as well to avoid confusion.
```ruby
class ULTerm; end

class ULVar < ULTerm
  attr_reader :index

  def initialize(i)
    unless i.is_a?(Integer)
      throw "Constructing a lambda term out of non-lambda terms"
    end
    @index = i
  end

  def ==(type); type.is_a?(ULVar) && @index == type.index end
end
class ULAbs < ULTerm
  attr_reader :t

  def initialize(t)
    unless t.is_a?(ULTerm)
      throw "Constructing a lambda term out of a non-lambda term"
    end
    @t = t
  end

  def ==(type); type.is_a?(ULAbs) && @t == type.t end
end

class ULApp < ULTerm
  attr_reader :t1
  attr_reader :t2

  def initialize(t1,t2)
    unless t1.is_a?(ULTerm) && t2.is_a?(ULTerm)
      throw "Constructing a lambda term out of non-lambda terms"
    end
    @t1 = t1; @t2 = t2
  end

  def ==(type); type.is_a?(ULApp) && @t1 == type.t1 && @t2 == type.t2 end
end
```
Each STTerm needs a `typecheck`, `typeOf(context)`, `eraseTypes` and `==(type)` function and any class that isn't singleton needs a 
`initialize` method.
```ruby
class STTerm; end
```
`STVar` does as was specified . For the `context` datatype that was recommended, the plan was to use a list object similar to a stack
where we only push to the back, cause we only ever need to take the most recent `STType` which is what is being done here with `STVar`. 
```ruby
class STVar < STTerm
  attr_reader :index

  def initialize(i)
    unless i.is_a?(Integer)
      throw "Constructing a lambda term out of non-lambda terms"
    end
    @index = i
  end

  def ==(type); type.is_a?(STVar) && @index == type.index end

  def typecheck
    false
  end

  def typeOf(context)
    context[-1]
  end

  def eraseTypes
    ULVar.new(@index)
  end
end
```
`STTrue`, `STFalse` and `STZero` all more or less do the same thing, and return constant values for everything as defined.
```ruby
class STTrue < STTerm
  def typecheck
    true
  end

  def ==(type); type.is_a?(STTrue) end

  def typeOf(_)
    STBool.new
  end

  def eraseTypes
    ULAbs.new(ULAbs.new(ULVar.new(1)))
  end
end

class STFalse < STTerm
  def typecheck
    true
  end

  def ==(type); type.is_a?(STFalse) end

  def typeOf(_)
    STBool.new
  end

  def eraseTypes
    ULAbs.new(ULAbs.new(ULVar.new(0)))
  end
end

class STZero < STTerm
  def typecheck
    true
  end

  def ==(type); type.is_a?(STZero) end

  def typeOf(_)
    STNat.new
  end

  def eraseTypes
    ULAbs.new(ULAbs.new(ULVar.new(0)))
  end
end
```
For `typecheck` with any of the following classes, it does exactly as the Scala function does wherein it sends it to `typeOf(context)`
and sees if it returns anything. `typeOf(context)` is where the specified rules are applied. The logic for everything is very similar
to Scala except Ruby isn't as strict with typing and such which allows for simpler code here.
```ruby
class STApp < STTerm
  attr_reader :t1
  attr_reader :t2

  def initialize(t1, t2)
    unless t1.is_a?(STTerm) && t2.is_a?(STTerm)
      throw "Constructing a lambda term out of non-lambda terms"
    end
    @t1 = t1
    @t2 = t2
  end

  def ==(type); type.is_a?(STApp) && @t1 == type.t1 && @t2 == type.t2 end

  def typecheck
    typeOf([]) != nil
  end

  def typeOf(context)
    x = @t1.typeOf(context)
    y = @t2.typeOf(context)
    if x.is_a?(STFun) && y == x.dom
        x.codom
    else
      nil
    end
  end

  def eraseTypes
    ULApp.new(@t1.eraseTypes, @t2.eraseTypes)
  end
end

class STAbs < STTerm
  attr_reader :type
  attr_reader :term

  def initialize(type, term)
    unless type.is_a?(STType) && term.is_a?(STTerm)
      throw "Constructing a lambda term out of non-lambda terms"
    end
    @type = type
    @term = term
  end

  def ==(type); type.is_a?(STAbs) && @type == type.type && @term == type.term end

  def typecheck
    typeOf([]) != nil
  end

  def typeOf(context)
    v = @term.typeOf(context.append(@type))
    if v == nil
      nil
    else
      STFun.new(@type, v)
    end
  end

  def eraseTypes
    ULAbs.new(@term.eraseTypes)
  end
end
```
`eraseTypes` gets a little more complex, but everything here remains relatively consistent to the specifications.
I didn't define `toString` here because my code didn't need as much troubleshooting given I had completed the Scala part
prior to starting this.
```ruby
class STSuc < STTerm
  attr_reader :term

  def initialize(term)
    unless term.is_a?(STTerm)
      throw "Constructing a lambda term out of non-lambda terms"
    end
    @term = term
  end

  def ==(type); type.is_a?(STSuc) && @term == type.term end

  def typecheck
    typeOf([]) != nil
  end

  def typeOf(context)
    t = @term.typeOf(context)
    if t.is_a?(STNat)
      t
    else
      nil
    end
  end

  def eraseTypes
    ULApp.new(
        ULAbs.new(
            ULAbs.new(
                ULAbs.new(
                    ULApp.new(ULVar.new(1),ULApp.new(ULApp.new(ULVar.new(2),ULVar.new(1)), ULVar.new(0)))))),
        @term.eraseTypes)
  end
end

class STIsZero < STTerm
  attr_reader :term

  def initialize(term)
    unless term.is_a?(STTerm)
      throw "Constructing a lambda term out of non-lambda terms"
    end
    @term = term
  end

  def ==(type); type.is_a?(STIsZero) && @term == type.term end

  def typecheck
    typeOf([]) != nil
  end

  def typeOf(context)
    t = @term.typeOf(context)
    if t.is_a?(STNat)
      t
    else
      nil
    end
  end

  def eraseTypes
    ULAbs.new(ULApp.new(ULApp.new(@term.eraseTypes, ULAbs.new(STFalse.new.eraseTypes)), STTrue.new.eraseTypes))
  end
end

class STTest < STTerm
  attr_reader :t1
  attr_reader :t2
  attr_reader :t3

  def initialize(t1, t2, t3)
    unless t1.is_a?(STTerm) && t2.is_a?(STTerm) && t3.is_a?(STTerm)
      throw "Constructing a lambda term out of non-lambda terms"
    end
    @t1 = t1
    @t2 = t2
    @t3 = t3
  end

  def ==(type); type.is_a?(STTest) && @t1 == type.t1 && @t2 == type.t2 && @t3 == type.t3 end

  def typecheck
    typeOf([]) != nil
  end

  def typeOf(context)
    t = @t1.typeOf(context)
    if t.is_a?(STBool)
      t
    else
      nil
    end
  end

  def eraseTypes
    ULAbs.new(ULAbs.new(ULAbs.new(ULApp.new(ULApp.new(@t1.eraseTypes, @t2.eraseTypes), @t3.eraseTypes))))
  end
end

class STType; end

class STNat < STType
  def ==(type); type.is_a?(STNat) end
  def to_s; "nat" end
end

class STBool < STType
  def ==(type); type.is_a?(STBool) end
  def to_s; "bool" end
end

class STFun < STType
  attr_reader :dom
  attr_reader :codom

  def initialize(dom, codom)
    unless dom.is_a?(STType) && dom.is_a?(STType)
      throw "Constructing a type out of non-types"
    end
    @dom = dom; @codom = codom
  end

  def ==(type); type.is_a?(STFun) && type.dom == @dom && type.codom == @codom end
  def to_s; "(" + dom.to_s + ") -> (" + codom.to_s + ")" end
end
```

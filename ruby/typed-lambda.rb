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

class STTerm; end

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

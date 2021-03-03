* Ultimately, the compiler should produce LLVM IR. Or short-term, just translate
  to C. This leads to extraordinarily easy C interop.
* D's _ranges_ are awesome. CoTE is one of the few langauges that can implement
  them cleanly.
* Go's _channels_ are awesome.
* Type classes (ala Haskell?) AND interfaces together! May require the ML/OcaML 
  notion of signatures and Functors.
* RAII is a killer feature, how do we incorporate that?

Do we use garbage collection?
-----------------------------
Yes. Otherwise we have to accept all the safety shortcoming of C, or the
complex type-system of Rust. I don't think CoTE needs to fit the niche of AAA
game-dev, so we can afford to use a garbage collector like most other general-purpose languages.







Let's say there's an "Ord" and an "Eq" typeclass...

sig Ord<T> {
  fn <=> bool a:T b:T {
    $fail "unimplemented!";
  }
}

sig Eq<T> {
  fn == bool a:T b:T {
    $fail "unimplemented!";
  }

  fn != bool a:T b:T {
    return !(a == b);
  }
}

-- modules + sigs are "objects" and "interfaces"

module ReflectiveEq for Eq<T> {
  fn == bool a:T b:T {
    return $case T.type 
      | struct -> structEq a b
      | enum   -> enumEq a b
    ;
  }
}

module OrdInt for Ord<int> {
  fn <=> bool a:int b:int {
    return case 
      | a < b => LT
      | a > b => GT
      | else  => EQ
    ;
  }
}



fn sort<T> void xs:Array<T> using m:Ord<T> {
  let middle = middle(xs);
  let left = all(xs, _ m.<= middle);
  left right = all(xs, _ m.> middle);
  return combine ++ left ++ right;
}

fn ==<T> bool a:T b:T using ord:Ord<T> {
  return case ord.<=> a b
    | Ord.GT => false
    | Ord.EQ => true
    | Ord.LT => false
  ;
}

fn ==<int> bool a:int b:int {
  return a prim.== b;
}



// This way there's no strictly-defined typeclasses, but we seem to get most the
// benefits? Doesn't appear to handle the "different types of Monoids" problem,
// however. This also requires "template specialization" a bit like C++, with some
// way of signifying that some implementations "overrule" others, some way of
// defining specificity.
fn ==<T> bool a:T b:T {
  $if defined(<=><T>) {
    $warning fallback "Falling back to default implementation of '==', which relies on '<=>'."
    return case <=><T> a b
      | Ord.GT => false
      | Ord.EQ => true
      | Ord.LT => false
    ;
  } $else {
    $error "Not implemented!"
  }
}

fn !=<T> bool a:T b:T {
  $if defined(==<T>) {
    ! (==<T> a b)
  } $else {
    $error "Not implemented!"
  }
}  

// e.g.
fn ==<int> bool a:int b:int {
  return prim_int_== a b;
}


fn map<F: Type->Type> F<U> f:T->U c:F<T> {
  $error "map<$F> not implemented!"
}

fn map<Vector> Vector<U> f:T->U c:Vector<T> {
  let result = Vector::new(c.size);
  for let i = 0; i < c.size; ++i {
    append! result (f (@ i c))
    append!(&result, f (@(i c)));
  }
  return result;
}

fn format<FMT: string> FmtType(FMT) {
  return FmtValue(string)
}

fn FmtType Type fmt:string {
  case fmt {
    "%d...rest" -> double->FmtType(rest)
  }
}

format<"%d is awesome %d %d!">(2)(3)(4);

struct Vector<T> {
  data:T*
  size:u32
  capacity:u32
}

fn ==<Vector<t>> bool a:Vector<t> b:Vector<t> {
  if a.size != b.size {
    return false;
  }
  let ap = a.data
  let bp = b.data
  for i = 0; i < a.size; ++i {
    if (*ap != *bp) {
      return false;
    }
    ap++; // pointer arithmetic ala C?
    bp++; // pointer arithmetic ala C?    
  }
  return true;
}
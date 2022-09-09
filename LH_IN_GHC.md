# LiquidHaskell in GHC

The goal of this proposal is to integrate LiquidHaskell more tightly with GHC,
by incrementally enhancing GHC with LiquidHaskell-related features, culminating
with a full porting of LiquidHaskell inside GHC. This proposal has been broken down in
levels of increasing level of difficulty, such that each level can be reviewed
and tackled independently. Furthermore, each level is self-contained, and its goal is to
provide added benefits independently to whether or not the next one is implemented.

## Level 1, "Simple Extension"

**Goal**: Introduce a new language extension called `LiquidHaskell` that, when turned on, builds
the module (or the whole project) using the existing LiquidHaskell plugin, which needs to be available
in the project. This level implies that LiquidHaskell refinements will need to be written the usual way,
using comments. Example:

```hs
{-@ one :: {v: Int | v == 1 @-}
one :: Int
one = 1
```

**Possible Execution**: Modify the `Extension` type inside module `GHC.LanguageExtensions.Type` and add a new
data constructor `LiquidHaskell`. A possible way forward would be to extend the parser over at
`GHC/Parser/Header.hs` so that `{-# LANGUAGE LiquidHaskell #-}` is turned into `{-# LANGUAGE -fplugin=LiquidHaskell #-}`.
Alternatively we would need to modify the main GHC compilation entrypoint and remove from the `DynFlags`
the `LiquidHaskell` extension in favour of `{-# LANGUAGE -fplugin=LiquidHaskell #-}`.

**Open question(s):** 

* Should this language extension be recycled in the last
  level to have GHC check LiquidHaskell's refinements or should this be only for
  the plugin? If we recycle it it's more elegant due to the non-proliferation of
  extensions, but code written using the LH's comment-style annotation will now
  silently stop being checked.

## Level 2, "Syntax & ANN desugaring"

**Goal**: Introduce a new language extension called `LiquidHaskellSyntax` (implied by `LiquidHaskell`) that enables 
LH-syntax in type signatures. GHC will desugar those into Annotations, so that they can be checked by the existing plugin.

### Level 2.a 

At this level we would focus only on the refinement syntax on function signatures. Example:

```hs
one :: {v:Int | v == 1}
one = 1
```

Internally, GHC would ignore `{v:` and ` | v == 1}` and effectively treat the above as `one :: Int`. 

**Possible Execution**: We would need to modify the Parser. This lenient parsing
succeeds only if `LiquidHaskellSyntax` is enabled. Then, we need to turn that
into a [source
annotation](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#source-annotations)
for the `one` function that contains (more or less) the string `one :: { v: Int
| v == 1}`, so that the existing LH plugin can "pick it up". Currently, the
plugin expects LH refinements to be found on comments starting with the special
syntax `{-@`. We might have to extend the plugin so that it can look at any
existing annotation, and try to turn those into a `BareSpec`.

### Level 2.b

**Goal:** At this level we would focus on the type synonyms syntax, starting with defining a potential
syntax for type synonyms, example:

```hs
type Nat = {v:Int | v >= 0}
```

**Execution:** TDB. Probably it entails modifying the Parser for a `ty_decl`:

```
-- Type declarations (toplevel)
--
ty_decl :: { LTyClDecl GhcPs }
           -- ordinary type synonyms
        : 'type' type '=' ktype
                -- Note ktype, not sigtype, on the right of '='
                -- We allow an explicit for-all but we don't insert one
                -- in   type Foo a = (b,b)
                -- Instead we just say b is out of scope
                --
                -- Note the use of type for the head; this allows
                -- infix type constructors to be declared
                {% mkTySynonym (comb2A $1 $4) $2 $4 [mj AnnType $1,mj AnnEqual $3] }
```

(cfr. `mkTySynonym`). 

**Open Questions:**

* Then the question becomes what to do with such
  `refinement` -- should we translate it into yet another annotation that is
  merged with a `BareSpec` for a given module, so that we can, once again,
  piggyback on the existing plugin?

### Level 2.c

At this level we would focus on the measures syntax, starting with defining a potential syntax, and once
again making this available as a source annotation in the GHC AST.

**Open questions:** Keywords or pragmas? i.e.

```hs
notEmpty       :: [a] -> Bool
notEmpty []    = False
notEmpty (_:_) = True
{-# MEASURE "nonEmpty" #-}
```

vs

```hs
measure notEmpty :: [a] -> Bool
notEmpty []    = False
notEmpty (_:_) = True
```

**Open Questions:** 

* Measures appears in data types -- should this level only define the syntax
  for them, or should this be deferred later?

### Level 2.d

**Goal**: At this level we would focus on the datatype refinements syntax, starting with defining a
potential syntax for data type refinements, and once again making this available at the AST level.
Possible examples:

```hs
data IncList a
  = Nil
  | Cons { hd :: a
         , tl :: IncList {v:a | hd <= v}
         }

data IncList a
  = Nil
  | Cons {hd:a} ( IncList {v:a | hd <= v} )

data IncList a where
  Nil  :: IncList a
  Cons :: {hd:a} -> IncList {v:a | hd <= v} -> IncList a
```

This also introduces an explicit scoping problem for variables, i.e. `{hd:a}` exists only such that `hd`
can be used in the second refinement. The syntax needs to be figured out. The GADT case is also interesting;
should the refinement go on the second type argument of `Cons` or on the final `IncList a`? Should GHC
support both formats? Also observe how the LH syntax is fairly ad-hoc here. From the tutorial:

> The Haskell type above does not state that the elements are in order of
> course, but we can specify that requirement by refining _every element in tl_
> to be greater than hd:

So in the example `{v:a | hd <= v}` is checking the predicate for every element.

## Level 3 "From ANN to proper types"

### Level 3.a

**Goal**: Have GHC's HsExpr AST modified to attach refinements to datatypes or
signatures, instead of source annotations. Then, we can "pretty print" those refinements into something which
could be placed, once again, into source annotations, to be picked by the
plugin.

### Level 3.b

**Goal**: Implement well-formedness checking for refinements. This level open
up for "external contributions" scenario; being the refinements embedded
"first-class" into a `HsExpr` means that plugin authors could read and
potentially manipulate them as part of the type-checking phase. In particular,
it's conceivable that in the near future GHC might provide first-class support
for LH's core functionalities (see level 4), but the rest could still be
implemented using the plugin. This way, experimental or research-driven
features could be outsourced outside GHC, which could focus on LH's
foundations.

**Possible Execution:** To be defined.

## Level 4

**Goal**: Have the LH engine _within GHC_ support the following features:

* basic refinements
* type synonyms
* datatype refinements
* measures (both as automatic refinements on datatypes and as uninterpreted functions)

This goal differs from Level 3 because now the whole refinement engine would be
implemented inside GHC, without the need for an extra plugin, at least for
those 4 bullet points (see Level 3 for considerations about a plugin-driven
architecture for more exotic LH's features).

**Possible execution:** To be defined.

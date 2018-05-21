# YOW! LJ 2018

* Brief history of lenses. DB and JSON update. Also partial updates (with 0 or 1): if the row exists, update it.

* Lets-lens takes us through the history, including all the pain. We only have one day so will skip through some of this to just get the key points. (and avoid some of the pain)

* [Lens derivation](https://github.com/ekmett/lens/wiki/Derivation). Composing functions, fmap, foldMap, traverse.
    - e.g. `traverse . traverse` makes a new `traverse`

# Lens.hs

* use `:m + Lets.Lens` in GHCi to bring this stuff into scope.
* Ed realised could generalise `fmapT` by passing in `traverse` instance (giving us `over`).
* Other `over` example: `over _1 length ("hello", 2) = (5,2)` where `_1 f (a,b) = (\a' -> (a', b)) <$> f a`

```
λ> let _1 f (a,b) = (\a' -> (a', b)) <$> f a
*Lets Lets.Lens
λ> over _1 length ("hello", 2)
(5,2)
```

* `over t f`: given a `t` that goes into a structure, modify it with `f`. Also called "modify". "Over some `t`, do `f`".

* Solving `sets`: expand type alias, add lambda for each arg, and take notes about what we have in comments. Include the goal we are aiming for (`?` in notes).

```
sets ::
  ((a -> b) -> s -> t)
  -> ((a->b) -> s -> t) -- expand type alias
  -> (a -> Identity b)
  -> s
  -> Identity t
sets =
    -- f :: (a -> b) -> s -> t
    -- i :: a -> Identity b
    -- s :: s
    -- getIdentity :: Identity x -> x
    -- ? :: Identity (GOAL)
    \f i s -> 
```

* Ed's approach:
    - Uses type-level hungrarian notation (`afb`).
    - Works backwards from Tony's way

```
    -- step 1: Need identity, so start with the type constructor
    \f afb s -> Identity $ f _ s
```

* Aim of `sets` is to provide an inverse of `over`. `over l f` means given a structure `l` return a function that will update `l` with `f`. `sets f` means given a change `f` return a structure.

```

λ> :t over
over :: Set s t a b -> (a -> b) -> s -> t
λ> :t sets
sets :: ((a -> b) -> s -> t) -> Set s t a b
```

```
over (sets f) = f
sets (over f) = f
```

```
> over mapped (+1) [ [1,2,3], [4,5,6] ]
[[2,3,4],[5,6,7]]
```

* `over` ~ `fmap`, traverse using `Identity`, `Set`
* `foldMapOf` ~ `foldMap`, traverse using `Const`, `Fold`




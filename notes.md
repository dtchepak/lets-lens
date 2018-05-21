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

* `over` ~ `fmap`, traverse using `Identity`, `Set`. Inverse: `sets`
* `foldMapOf` ~ `foldMap`, traverse using `Const`, `Fold`. Inverse: `folds` (see `folding` in lens lib)
* `sets`/`fmap`/`mapped`
* `folds`/`foldMap`/`folded`
* Ed: if here was a combinator that worked in base (e.g. `fmap`, `foldMap`), worked that into lens.
* `foldMapOf l f`: at `l`, do `f` and fold result using monoid instance.

```
λ> over traverse (+1) [1,2,3]
[2,3,4]
λ> foldMapOf traverse Sum [1,2,3]
Sum {getSum = 6}
```

* `Get` is `Fold` without the `Monoid` constraint.
* In lens library, `Get` = `View` (to avoid naming clash with other libs (store/state?)). This is also why `put` became `over`.
* `s t a b` ~ source target input_value_type output_value_type

* `over` is "go down a structure, run a function, and modify the structure". Can `over (traverse.traverse) (+1)` to add 1 to a list of list. `set` is go down the structure and replace a value. `fold` is go through a structure using a monoid and append as we go. `get` is go down the structure and get the value once we've found it. `traversal` is go down the structure like a `fold`, but also do modifications. 
* `over` gives us a view of one thing, `traversal` gives a view of many things.

* Ed on lens and lens laws:

```
get :: Lens s t a b -> s -> a
set :: Lens s t a b -> s -> b -> t

get :: Lens' s a -> (s -> a) -- getter
set :: Lens' s a -> (s -> a -> s) -- setter
```

Goal: rather than imperative `foo.bar.baz += 10`, we want a functional version for immutable code. In Haskell/lens, can do `foo.bar.baz +~ 10`. Reclaiming imperative notation, but keeping FP. Nice thing is the `.` worked well as field accessor using funtion composition.

Laws:

```
-- fmap laws:
fmap id = id
fmap f . fmap g = fmap (f . g)

-- same for over:
over l id = id
over l f . over l g = over l (f . g)

-- lens laws:
set l s (get l s) = s -- set the value i just got, nothing changes
get l (set l s a) = a -- if i set something, i'll get that out when i read it
set l (set l s a) b = set l s b -- setting twice the same as setting once; repeatable, no side effect
```

* `over` / `Lens`: one thing; `Traversal`: zero or more things (many things); `Prism`: zero or one thing.
* Ed: wrote this for linear alg, surprised that everyone wanted it for CRUD apps.
* `Traversal` is generalised to any `Applicative`. `Set` and `Fold` used `Identity` and `Const` applicatives, now we're talking about any `Applicative`.

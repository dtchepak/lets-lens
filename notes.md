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
* Using `both` traversal: `over both length ("hello", "world!") = (5, 6)`
* Pattern for traversals: go into structure, and map into the value we're interested in. Otherwise just pass through and ignore (rebuild). See `traverseLeft`/`traverseRight`.
* See `personStrings` example in `Lens.hs`.
* Lens is a slightly better traversal. Everywhere you can use a traversal you can use a lens.
* `mapL`: aim is to make `Map.lookup` return whatever the user is asking for. So if we `set` key `3` to `Just "X"`, then future `lookup 3`  should return `Just "X"`. If we set it to `Nothing`, then future `lookup 3` should return `Nothing`.

```
λ> set (mapL 3) (Map.fromList [(1, "a"), (2, "b"), (3,"c")]) (Just "X")
fromList [(1,"a"),(2,"b"),(3,"X")]
λ> set (mapL 3) (Map.fromList [(1, "a"), (2, "b"), (3,"c")]) Nothing
fromList [(1,"a"),(2,"b")]
```

## Ed lecture

After afternoon tea. Translating this to the lens library.

```
-- idiomatic in lens lib: flipped fmap:
<&> :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

p (Map.lookup k m) <&> \case ->
    Just v -> Map.insert k v m
    Nothing -> Map.delete k m
```

Functions tend to be big, so `<$>` lets us flow those out to the right hand side.

```
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- We currently have this in the exercises:
type Getter r s a = (a -> Const r a) -> s -> Const r s
-- In Lens lib, we would like to move this to a constraint like the others.
class Contravariant f where
    -- Laws:
    -- contramap id = id
    -- contramap f . contramap g = contramap (g . f)
    contramap :: (a -> b) -> f b -> f a
-- example Contravariant functor:
data Predicate a => Predicate (a -> Bool)
-- So we can do:
type Getter s a = forall f. (Functor f, Contravariant f) => (a -> f a) -> s -> f s
-- functor and contravariant functor means we can't use the arg, can only be Const
-- Similarly:
type Fold s a = forall f. (Applicative f, Contravariant f) => (a -> f a) -> s -> f s


-- This lets us be a bit more generic
type Getter s a = forall f. (Functor f, Contravariant f)   => (a -> f a) -> s -> f s
type Lens s t a b = forall f. Functor f                    => (a -> f b) -> s -> f t
type Fold s a = forall f. (Applicative f, Contravariant f) => (a -> f a) -> s -> f s
type Traversal s t a b = forall f. Applicative f           => (a -> f b) -> s -> f t

-- We wanted opposite of Lens (Prism). This wasn't enough to get them. Needed to
-- "mangle" the `a -> f b` arrow, using profunctor.
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Iso s t a b = forall p f. (Profunctor p, Functor f)   => p a (f b) -> p s (f t)

class Profunctor p where
    -- Laws:
    -- dimap id f . dimap id g = dimap id (f . g)       <- like functor law
    -- dimap f id . dimap g id = dimap (g . f) id       <- like contravariant law
    -- dimap id id = id
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    -- can map forward on the result (c->d), backwards on arg (a->b)  (contravariant)

instance Profunctor (->) where
    -- For p = (->):
    --  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    --  dimap :: (a -> b) -> (c -> d) -> ((->) b c) -> ((->) a d)
    dimap :: (a -> b) -> (c -> d) -> (b -> c) -> a -> d
    dimap ab cd bc = cd . bc . ab

newtype Kleisli m a b = Kleisli (a -> m b)

-- For Isomorphism, wanted:
type Iso s t a b = (s -> a, b -> t)
-- But have to fit into `Iso s t a b` mold we're using to get lens compatibility:
type Iso s t a b = forall p f. (Profunctor p, Functor f)   => p a (f b) -> p s (f t)

-- Now we can do:
iso :: (s -> a) -> (b -> t) -> Iso s t a b
-- sa :: s -> a
-- bt :: b -> t
-- pafb :: p a (f b)
-- ? :: p s (f t)
iso sa bt pafb = dimap sa (fmap bt) pafb
-- Reduces to: iso sa bt = dimap sa (fmap bt)


--       getter         setter            lens
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt = sbt s <$>  f (sa s)

-- compare with iso
iso ::  (s -> a) -> (b -> t)      -> Iso s t a b
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
-- do not need the `s` in the `s -> b -> t` update

-- Another way to view a lens:
--   Lens' s a = exists c. s <-> (a, c)
-- There exists something c so that s can be split into the a parts i'm interested in
-- and the c stuff that i'm not, and then can reassemble back into a t.
--   Lens' s a = exists c. s <-> (a, c), (b, c) -> t
-- "iso view" of lens?

-- 3 ways to view a lens: our functor view, the getter/setter view, and the iso view
-- With Iso we're saying c is trivial, we don't need it.

-- Now for prisms:
-- Lens' s a = exists c. s <-> (a, c)               <- this is for products, a * c
-- Prism' s a = exists c. s <-> Either a c          <- this is for sums, a + c
-- could use maybe here ^, but if we want `t` then need either
-- Prism s t a b = exists c. (s -> Either a c, Either b c -> t)
```

Aside: this is a bit simpler in Purescript, because for GHC we wanted to be able to define lens stuff without importing all the packages. Things like Choice are coming in later GHC so we can improve this. (?)

Digression into positive and negative position of args.
```
newtype Cont r a = Cont ((a -> r) -> r) deriving Functor
    -- can define fmap for this
    -- because a in positive position.
    -- not for r, r is in positive (output) and negative position (input)
```

Prisms are really good for JSON docs.

See photo (21 May 2018) of lens lattice for how these things compose.

Plated lets us define specific traversals over a structure (?). (Plate comes from trying to eliminate "boilerPLATE". E.g. uniplate, biplate)

Names in lens are "pun-driven development". Example: started with View which is our version of Get. First item is `preview`. Should be able to build a traversal of first item, so `pre . view` gives us `preview`. Also: `fusing` function to eliminate multiple fmap calls, then traversal version is `confusing` (suits the horrifying type signature :) ).


Quick tour of `makePrisms`, `makeLenses` and `makeClassy` for template haskell. e.g. `makeClassy ''Persan` which generates `HasPerson` typeclass, including delegating for hierarchies. There's also `makeFields` (class for each field, `HasFirstName` for example) but Ed doesn't recommend this (clashes with `makeClassy`, is a bit messy).



# Goals and non-goals

Bottom line: let's make the best fucking calculator ever.

## Goals

* First class closures 
    * That means that you can have functions as elements of an array.
    * You should also be able to mark functions under some form of gradual-typing. I'm not putting this as a separate goal because I think this is a secondary concern.
        * Functions should be marked with their input shape, output shape, and application rank. Note that "shape" here necessarily includes the extra dimension described in "First class semi-heterogeneous arrays".
    * Some syntax for marking captures would be cool?
* First class ADT support
    * Arrays containing ADTs have an implicit dimension added at the end of their shape. 
    * This dimension contains the tag for the ADT, along with any extra information carried with the constructor
    * This would allow us to have gerund construction as the main control flow (pattern matching + function composition on steroids, see J docs)
        * The structure of a verb would naturally be a list of patterns and actions for if the patterns match, where in the simplest case it's just the bare pattern.
            * Along with a list of captures, in the case of the closures above
* First class semi-heterogeneous arrays
    * Now I'm not suggesting we get rid of boxing -- I'm a big fan of J's boxing model and will probably rip it -- but it needs to be less perversive in the language to make it easier to write.
    * Hypothesis: there are only TWO situations where heterogeneous arrays are actually useful in an APL.
        * The first situation is when you have header or trailer data on the array. Think column names in a spreadsheet.
        * The second situation is where you are running a computation that may fail or produce sidechannel data on every element of an array.
            * This is handled via ADT support -- failure or sidechannel cases should be returned using a dedicated ADT.
    * So that leaves the case in which indivudual dimensions (like a header) take on a different type than the rest of the array.
    * Traditionally, the shape of an array has shape `N`, where `N` is the rank of the array. We change this, such that the shape is an array of shape `2 N`.
        * The top row is the traditional shape of the array as given above.
        * The second row is a bunch of boxed arrays with elements given by the Type ADT:
            ```hs
            -- Haskell syntax, but you get it
            data Type = Int | Str | Fill Type | Any
            ```
        * The `i`'th boxed array should have a shape `M`, where `M` is at most the `i`'th element of the shape of the original array.
        * Then you can constrain each dimensional slice with types. Example:
            ```
                NB. Pretend that literally any of this is real syntax.
                NB. Let's make an array x.
                  x.= 1_2_3 \
                      4_5_6 \\

                      2_3_4 \
                      5_6_7

                NB. The first row of Shape x is just the shape you expect.
                  Head Shape x
                NB. We get back 2 2 3

                NB. The second row of Shape x is the type constraints on x.
                  Last Shape x 
                NB. We get back the following arrays:
                    +----------+-+-+
                    | Fill Int | | |
                    +----------+-+-+

                NB. But what if we had a heterogeneous array? Let's try it
                  y.= 1_2_3 \
                      "hello"_"world"_"aaa"
                  Head Shape y
                NB. We get back 2 3
                  Last Shape y
                NB. We get back the following:
                    +---------+-+
                    | Int Str | |
                    +---------+-+
                NB. The compiler will calculate the most general (farthest left) type constraint possible for a given array.
            ```
        * TL;DR the idea is that you can conditionally constrain certain slices of the array without affecting other slices of the array. Then you can use the `Fill` constructor to write function signatures that match all fancy.
* Applicative-order tacit-explicit homoiconicity
    * Other languages (Uiua, [Vern](https://github.com/voidwyrm-2/vern)) have achived tacit-explicit homoiconicity by switching away from applicative functions and to a stack-based model.
    * Most (all?) other applicative-order APLs interpret VV sentences as hooks, and VVV+ sentences as trains. I would like to see what would happen if we interpreted these sentences as partial applications instead.
    * There are a few tradeoffs that I believe are immediately apparent:
        * Stranding (i.e. NN sentences that turn into arrays) becomes impossible (see discussion below).
        * We can do functional programming! That means that adjectives and adverbs are just higher order functions
            * Where you might see a sentence like VAV, the compiler just sees VVV!
            * Silly solution: maybe you can strand functions together to make them apply like that. so VAV is written V_VV. this looks like subscripting a function in LaTeX which is an extra plus.
        * Monadic-dyadic choice would need to be marked explicitly (?)
            * This would be something that is highly contested but may not be that bad imo
            * With partial application, even sentences like VNV become ambiguous. Is the second V a monad missing one argument, or a dyad missing two arguments? What does putting a dyad and a monad next to each other even mean in this context?
            * We need convention: we cannot think about monads and dyads as separate, we must think of them as regular old 1-adic and 2-adic functions.
            * Idea 1: So this language must support two application orders: the traditional application order and a special-cased (>1)-adic order.
                * VNN must be interpreted as calling V with 2 arguments (so we lose stranding!)
                * NVN must be interpreted the same, as calling V with 2 arguments
                * Then we run into weird edge cases. NV should clearly be a dyadic V with 1 argument applied. But what about VN -- is this a dyadic V with the second argument applied? or a monadic V with all its arguments applied?
            * Idea 2: verbs are greedily grouped together RTL and interpreted in a way that produces the fewest unbound arguments as possible
                * Imagine a sentence:
                ```
                    Part of speech: VAVNNVVA
                    Index:          01234567
                ```
                * We start by doing a scanning pass, grouping all verbs and adverbs together:
                ```
                    Part of speech: (VAV)NN(VVA)
                    Index:           012 34 567
                ```
                * We then parse the verb phrases (VPs):
                ```
                    Part of speech:  VP NN VP
                    Index:           0  12 3
                ```
                * We then ask: for VP 3... i guess minimizing unbound arguments would just mean "always choosing the monadic case" for V5 and V6. So maybe we stick with that.
            * Idea 3: Right-to-left argument filling
                * We actually do away with applicative order in the monadic case and apply arguments in lojban order:
                    > x1 V x2 x3 x4 ...
                * So the examples above would not be `Head Shape x` but `x Shape Head`. Then it would be parsed RTL: 
                    * The parser consumes `Head`. It has no `x2 x3 ...`, so those arguments are not filled. The parser consumes `x Shape` to fill `x1` and it recurses.
                    * The parser consumes `Shape`. It has no `y2 y3 ...`, so those arguments are not filled. The parser consumes `x` to fill `y1` and it recurses.
                    * The parser consumes `x` as a noun, sees the EOL and ends.
                * So it's very left associative -- the phrase above becomes `(x Shape) Head`. Good!
                * What if there's a 2-adic version of `Shape` we want to invoke instead? would it be crazy to just tack it onto the end of the name...?
                    * so `x Shape2 Head` gets parsed the same way at first, `Head` ingests `x Shape2` as its `x1` and recurses
                    * then we parse `Shape2`, which needs 2 arguments but only got 1 (`x`). so it's partially applied. 
                    * What do we do now that we're gonna pass a partial function back to `Head`, which expects a noun?
                        * We automatically lift the unbound argument to the top level!
                    * So we parse it as `(x Shape2) Head` where `x Shape2` is a verb that expects a noun. And as such, so is `x Shape2 Head`.
                        * We'd go on a FIFO stack type beat and `x Shape2 Head2` would be a verb that takes 2 arguments. the first one would go to `Head2`, the second one would go to `Shape2`.
                    * This DOES mean that we will need to explicitly mark verb arguments as noun or verbs.
                        * More like, we'll implicitly assume that most verbs will take noun arguments and introduce special syntax for taking a verb argument
* Typeability and compiler unicode rendering
    * Capitalized terms should be compiled to unicode names, these names should be extendable in code.
    * I.e. `Plus` should become `+` at tokenization time. Unique prefixes (like `Plu`) should also be understood.
    * When users define nouns or verbs, they should be able to provide many synonyms that are handled by this system.
    * The compiler should be able to be invoked on a file to parse these names
    * MVP is OK to use a static list of unicode expansions
* It's gonna be a [query based compiler](https://ollef.github.io/blog/posts/query-based-compilers.html)
    * This idea is cool as shit. I think the idea of rolling my own cache layer sounds like a lot of fun. And this provides a really natural place to embed the token expansion pass, as a queryable in the compiler.

## Non-goals

* Namespaces, modules, imports
    * The assignment logic is already going to be very complicated -- we'll need special cases for verbs, adjectives (higher order verbs), and nouns. 
    * The compiler's token expansion pass needs to be able to declaratively determine names to expand them
* Stability, compatiblity with other APLs
* Ergonomics over performance
    * Running on GHC is an explicit choice -- we have access to a very fast GC and RTS, we can afford to abuse it.
    * Things like array specialization, SSE (and other computation kernels), and multithreading are currently soft-out-of-scope.
        * Even though the type info to do this stuff would exist
    * This language should be fast to write, easy to write, and provide immediate feedback before all else.
* Experimentation over stability.
    * This is a CALCULATOR first and foremost. I want to iterate fast and play with new featuresets. I don't want to get trapped like other young APL projects in trying to support a fledgling codebase.

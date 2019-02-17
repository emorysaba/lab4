(*
                              CS51 Lab 4
	       Error Handling, Options, and Exceptions
 *)

(*======================================================================
Part 1: Currying and uncurrying

Before getting into the main topic of this lab, how to handle
anomalous conditions using option types and exceptions, we continue
with some exercises about polymorphism.

........................................................................
Exercise 1: In this exercise, you'll define polymorphic higher-order
functions curry and uncurry for currying and uncurrying binary
functions (functions of two arguments). The functions are named after
mathematician Haskell Curry '1920. (By way of reminder, a curried
function takes its arguments one at a time. An uncurried function
takes them all at once in a tuple.)

To think about before you start coding:

  * What should the types of curry and uncurry be?

  * What is an example of a function that curry could apply to?
    Uncurry?

  * What are some tests that you could try to verify that your
    implementations of curry and uncurry work properly?

Now implement the two functions curry and uncurry.
......................................................................*)

(* In order to think through this problem, it helps to start with the
types of the functions. The curry function is a *function*; it has a
function type, of the form _ -> _. It is intended to take an uncurried
binary function as its argument, and return the corresponding curried
function. An uncurried binary function is a function that takes its
two arguments both "at the same time", that is, as a
pair. Generically, the type of such a function is thus 'a * 'b ->
'c. A curried binary function takes its two arguments "one at a
time". Its type is 'a -> ('b -> 'c). Putting these together, the type
of curry should be

    (('a * 'b) -> 'c) -> ('a -> ('b -> 'c))      .

(Dropping extraneous parentheses since the -> type operator is right
associative, we can also right this as

    (('a * 'b) -> 'c) -> 'a -> 'b -> 'c      .)

This type information already gives us a big hint as to how to write
the curry function. We start with the first line giving the argument
structure:

    let curry (uncurried : ('a * 'b) -> 'c) : 'a -> 'b -> 'c = ...

We call the argument function "uncurried" to emphasize that it is an
uncurried function, and indeed, its type is consistent with that.

The return type is a function type, so we'll want to build a function
value to return. We use the "fun _ -> _" anonymous function
construction to do so, carefully labeling the type of the function's
argument as a reminder of what's going on:

    let curry (uncurried : ('a * 'b) -> 'c) : 'a -> 'b -> 'c =
      fun (x : 'a) -> ...

The type of the argument of this anonymous function is 'a because its
type as a whole -- the return type of curry itself -- is 'a -> ('b ->
'c). This function should return a function of type 'b -> 'c. We'll
construct that as an anonymous function as well:

    let curry (uncurried : ('a * 'b) -> 'c) : 'a -> 'b -> 'c =
      fun (x : 'a) ->
        fun (y : 'b) -> ...

Now, how should we construct the value (of type 'c) that this inner
function should return? Remember that curry should return a curried
function whose value is the same as the uncurried function would have
delivered on arguments x and y. So we can simply apply uncurried to x
and y (in an uncurried fashion, of course), to obtain the value of
type 'c:

    let curry (uncurried : ('a * 'b) -> 'c) : 'a -> 'b -> 'c =
      fun (x : 'a) ->
        fun (y : 'b) -> uncurried (x, y) ;;

You'll note that all of these anonymous functions are a bit
cumbersome, and we have a nicer notation for defining functions in let
expressions incorporating the arguments in the definition part
itself. We've already done so for the argument uncurried. Let's use
that notation for the x and y arguments as well.

    let curry (uncurried : ('a * 'b) -> 'c) (x : 'a) (y : 'b) : 'c =
      uncurried (x, y) ;;

To make clearer what's going on, we can even drop the explicit types
to show the structure of the computation:

    let curry uncurried x y = uncurried (x, y) ;;

Here, we see what's really going on: "curry uncurried" when applied to
x and y in curried fashion gives the same value that uncurried gives
when applied to x and y in uncurried fashion.

By a similar argument (which it might be useful to carry out
yourself), uncurry is implemented as

    let uncurry curried (x, y) = curried x y ;;

Below, we use the version with explicit types, as we generally want to
do to make our typing intentions known to the compiler/interpreter.
 *)

let curry (uncurried : ('a * 'b) -> 'c) (x : 'a) (y : 'b) : 'c =
  uncurried (x, y) ;;

let uncurry (curried : 'a -> 'b -> 'c) ((x, y) : ('a * 'b)) : 'c =
  curried x y ;;

(*......................................................................
Exercise 2: OCaml's built in binary operators, like ( + ) and ( * ) are
curried:

# ( + ) ;;
- : int -> int -> int = <fun>
# ( * ) ;;
- : int -> int -> int = <fun>

Using your uncurry function, define uncurried versions of the plus and
times functions.
......................................................................*)

let plus = uncurry ( + ) ;;


let times = uncurry ( * ) ;;

(*......................................................................
Exercise 3: Recall the prods function from Lab 1:

let rec prods (lst : (int * int) list) : int list =
  match lst with
  | [] -> []
  | (x, y) :: tail -> (x * y) :: (prods tail) ;;

Now reimplement prods using map and your uncurried times function. Why
do you need the uncurried times function?
......................................................................*)


let prods = List.map times ;;

(*======================================================================
Part 2: Option types

In Lab 2, you implemented a function max_list that returns the maximum
element in a non-empty integer list. Here's a possible implementation
for max_list:

let rec max_list (lst : int list) : int =
  match lst with
  | [elt] -> elt
  | head :: tail -> max head (max_list tail) ;;

(This implementation makes use of the polymorphic max function from
the Pervasives module.)

As written, this function generates a warning that the match is not
exhaustive. Why? What's an example of the missing case? Try entering
the function in ocaml and see what information you can glean from the
warning message.

The problem is that there is no reasonable value for the maximum
element in an empty list. This is an ideal application for option
types.

........................................................................
Exercise 4:

Reimplement max_list, but this time, it should return an int option
instead of an int. Call it max_list_opt. The None return value should
be used when called on an empty list.
......................................................................*)

let rec max_list_opt (lst : int list) : int option =
  match lst with
  | [] -> None
  | head :: tail ->
     match (max_list_opt tail) with
     | None -> Some head
     | Some max_tail -> Some (max head max_tail) ;;

(*......................................................................
Exercise 5: Alternatively, we could have max_list raise an exception
upon discovering the error condition. Reimplement max_list so that it
does so. What exception should it raise? (See Section 10.2 in the
textbook for some advice.)
......................................................................*)

let rec max_list (lst : int list) : int =
  match lst with
  | [] -> raise (Invalid_argument "max_list: empty list")
  | [elt] -> elt
  | head :: tail -> max head (max_list tail) ;;

(*......................................................................
Exercise 6: Write a function min_option to return the smaller of two
int options, or None if both are None. If exactly one argument is
None, return the other. The built-in function min from the Pervasives
module may be useful. You'll want to make sure that all possible cases
are handled; no nonexhaustive match warnings!
......................................................................*)

let min_option (x : int option) (y : int option) : int option =
  match x, y with
  | None,      None       -> None
  | None,      Some right -> Some right
  | Some left, None       -> Some left
  | Some left, Some right -> Some (min left right) ;;

(*......................................................................
Exercise 7: Write a function plus_option to return the sum of two int
options, or None if both are None. If exactly one argument is None,
return the other.
......................................................................*)

let plus_option (x : int option) (y : int option) : int option =
  match x, y with
  | None,      None       -> None
  | None,      Some right -> Some right
  | Some left, None       -> Some left
  | Some left, Some right -> Some (left + right) ;;

(*======================================================================
Part 3: Polymorphism practice

........................................................................
Exercise 8: Do you see a pattern in your implementations of min_option
and plus_option? How can we factor out similar code?

Write a polymorphic higher-order function calc_option to extend binary
operations to operate on option type values, taking three arguments in
order: the binary operation (a curried function) and its first and
second argument. If both arguments are None, return None.  If one
argument is (Some x) and the other argument is None, the function should
return (Some x). If neither argument is none, the binary operation
should be applied to the argument values and the result appropriately
returned.

What is calc_option's function type signature?

Now implement calc_option.
......................................................................*)

let calc_option (f : 'a -> 'a -> 'a) (x : 'a option) (y : 'a option)
              : 'a option =
  match x, y with
  | None,      None       -> None
  | None,      Some right -> Some right
  | Some left, None       -> Some left
  | Some left, Some right -> Some (f left right) ;;

(*......................................................................
Exercise 9: Now rewrite min_option and plus_option using the higher-order
function calc_option. Call them min_option_2 and plus_option_2.
......................................................................*)

let min_option_2 : int option -> int option -> int option =
  calc_option min ;;

(* You may have not added in the specific type information in your
definition of min_option_2, and received an inscrutable warning
involving "weak type variables", and type problems when submitting
your code. Here's an example of that behavior:

  # let min_option_2 =
      calc_option min ;;
  val min_option_2 : '_weak1 option -> '_weak1 option -> '_weak1 option = <fun>
  # min_option_2 (Some 3) (Some 4) ;;
  - : int option = Some 3
  # min_option_2 (Some 4.2) (Some 4.1) ;;
  Error: This expression [namely, the 4.2] has type float but an expression
         was expected of type int

The type variables like '_weak1 (with the underscore) are "weak type
variables", not true type variables. Weak type variables are discussed
briefly in as discussed in Section 9.6 of the textbook. They arise
because in certain situations OCaml's type inference can't figure out
how to express the most general types and must resort to this weak
type variable approach.

When a function with these weak type variables is applied to arguments
with a specific type, the polymorphism of the function
disappears. Notice that the first time we apply min_option_2 above to
int options, things work fine. But the second time, applied to float
options, there's a type clash because the first use of min_option_2
fixed the weak type variables to be ints. Since our unit tests try
using min_option_2 in certain ways inconsistent with weak type
variables, you'll get an error message saying that "The type of this
expression, '_weak1 option -> '_weak1 option -> '_weak1 option,
contains type variables that cannot be generalized."

To correct the problem, you can add in specific typing information (as
we've done in the solution below) or, even better, make explicit the
full application of calc_option:

  let min_option_2 x y =
    calc_option min x y ;;

rather than the partial application we use below. Either of these
approaches gives OCaml sufficient hints to infer types more
accurately.

For the curious, if you want to see what's going on in detail, you can
check out the discussion in the section "A function obtained through
partial application is not polymorphic enough" at
https://ocaml.org/learn/faq.html#Typing.
 *)

let plus_option_2 : int option -> int option -> int option =
  calc_option (+) ;;

(*......................................................................
Exercise 10: Now that we have calc_option, we can use it in other
ways. Because calc_option is polymorphic, it can work on things other
than int options. Define a function and_option to return the boolean
AND of two bool options, or None if both are None. If exactly one is
None, return the other.
......................................................................*)

let and_option : bool option -> bool option -> bool option =
  calc_option (&&) ;;

(*......................................................................
Exercise 11: In Lab 3, you implemented a polymorphic function zip that
takes two lists and "zips" them together into a list of pairs. Here's
a possible implementation of zip:

let rec zip (x : 'a list) (y : 'b list) : ('a * 'b) list =
  match x, y with
  | [], [] -> []
  | xhd :: xtl, yhd :: ytl -> (xhd, yhd) :: (zip xtl ytl) ;;

A problem with the implementation of zip is that, once again, its
match is not exhaustive and it raises an exception when given lists of
unequal length. How can you use option types to generate an alternate
solution without this property?

Do so below in a new definition of zip, called zip_opt to make clear
that its signature has changed, which returns an appropriate option
type in case it is called with lists of unequal length.
......................................................................*)

let rec zip_opt (x : 'a list) (y : 'b list) : (('a * 'b) list) option =
  match (x, y) with
  | [], [] -> Some []
  | xhd :: xtl, yhd :: ytl ->
     (match zip_opt xtl ytl with
      | None -> None
      | Some ztl -> Some ((xhd, yhd) :: ztl))
  | _, _ -> None ;;

(*====================================================================
Part 4: Factoring out None-handling

Recall the definition of dotprod from Lab 2. Here it is adjusted to
an option type:

    let dotprod (a : int list) (b : int list) : int option =
      let pairsopt = zip_opt a b in
      match pairsopt with
      | None -> None
      | Some pairs -> Some (sum (prods pairs)) ;;

It uses zip_opt from Exercise 10 and prods from Exercise 3. The sum
function is simply *)

let sum : int list -> int =
  List.fold_left (+) 0 ;;

(* Notice how in dotprod and other option-manipulating functions we
frequently and annoyingly have to test if a value of option type is
None, requiring a separate match, and passing on the None value in the
"bad" branch or introducing the Some in the "good" branch. This is
something we're likely to be doing a lot of. Let's factor that out to
simplify the implementation.

........................................................................
Exercise 12: Define a function called maybe that takes a function of
type 'a -> 'b and an argument of type 'a option, and "maybe"
(depending on whether its argument is a None or a Some) applies the
function to the argument. The maybe function either passes on the None
if its first argument is None, or if its first argument is Some v, it
applies its second argument to that v and returns the result,
appropriately adjusted for the result type.

What should the type of the maybe function be?

Now implement the maybe function.
......................................................................*)

let maybe (f : 'a -> 'b) (x : 'a option) : 'b option =
  match x with
  | None -> None
  | Some v -> Some (f v) ;;

(*......................................................................
Exercise 13: Now reimplement dotprod to use the maybe function. (The
previous implementation makes use of functions sum and prods. You've
already (re)implemented prods in Exercise 3. We've provided sum for
you above.)  Your new solution for dotprod should be much simpler than
the version we provided above at the top of Part 4.
......................................................................*)

let dotprod (a : int list) (b : int list) : int option =
  maybe (fun pairs -> sum (prods pairs))
        (zip_opt a b) ;;

(*......................................................................
Exercise 14: Reimplement zip_opt along the same lines, in zip_opt_2
below.
......................................................................*)

(* We remove the embedded match using a maybe: *)

let rec zip_opt_2 (x : 'a list) (y : 'b list) : (('a * 'b) list) option =
  match (x, y) with
  | [], [] -> Some []
  | xhd :: xtl, yhd :: ytl ->
     maybe (fun ztl -> ((xhd, yhd) :: ztl))
           (zip_opt_2 xtl ytl)
  | _, _ -> None ;;

(*......................................................................
Exercise 15: For the energetic, reimplement max_list_opt along the
same lines. There's likely to be a subtle issue here, since the maybe
function always passes along the None.
......................................................................*)

let rec max_list_opt_2 (lst : int list) : int option =
  match lst with
  | [] -> None
  | [single] -> Some single
  | head :: tail ->
     maybe (fun max_tail -> max head max_tail)
           (max_list_opt_2 tail) ;;

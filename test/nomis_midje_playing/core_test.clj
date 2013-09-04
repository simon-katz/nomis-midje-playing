(ns nomis-midje-playing.core-test
  (:require [midje.sweet :refer :all]
            [nomis-midje-playing.core :refer :all]))

;;;; Most of these facts evaluate to `true`. A few examples fail and
;;;; evaluate to `false`. I only show results when false.

;;;; Many/most of these examples are direct copies of examples in the
;;;; Midje documentation.

;;;; ___________________________________________________________________________
;;;; Basic syntax

(fact 3 => 3)

(fact "Description" 3 => 3)

(fact "Description"
      3 => 3
      4 => 4
      5 => 5)

;;;; ___________________________________________________________________________
;;;; Some Terminology

;;;; /x => y/ is a /checkable/.

;;;; ___________________________________________________________________________
;;;; What's the exact syntax?

;;;; See https://github.com/marick/Midje/wiki/Syntax-and-a-little-semantics

;;; Comment this out because it messes things up when I run tests.
;; (fact
;;  "The descriptions go with the fact, not with the checkables." 3 => 3
;;  "Not a description, just a form somewhere" 3 => 4)
;; ;; FAIL "The descriptions go with the fact, not with the checkables."
;; ;; => false

(fact :x :y)
;; Ah! I guess without the `=>` Midje just passes stuff on to be evaluated:
(fact)
(fact (println 1) (println 2))
;; 1
;; 2

;;;; ___________________________________________________________________________
;;;; Future facts

(future-fact "Description" (plop 6) => 7)
;; WORK TO DO "Description" at (NO_SOURCE_FILE:1)
;; => nil

(fact "Description" (plop 6) =future=> 7)
;; WORK TO DO "Description - on `(plop 6)`" at (NO_SOURCE_FILE:1)


;;;; ___________________________________________________________________________

;;;; You cannot test Midje using Midje (or at least using the features
;;;; of Midje that are described to a beginner). The fact that `fact`
;;;; returns values can't be tested. And (not surprisingly) the inner
;;;; failure gets reported. (Maybe there's something less simplistic
;;;; you can do. You should take a look at how Midje is tested.)

;;;; Ah! But there's `=test=>`.  I think that might be what you'd need.

;;; Comment this out because it messes things up when I run tests.
;; (fact (fact 3 => 4)
;;       => false)
;; ;; => false

;;;; ___________________________________________________________________________
;;;; Functions on the RHS

(fact 3 => odd?)
(fact 3 => (if true odd? even?))
(fact 3 =not=> (if false odd? even?))

;;;; ___________________________________________________________________________
;;;; Terminology:

;;;; In the above, the RHS of the `=>` is called a /checker/.
;;;; - Q. Is this true when the RHS is a simple value?

;;;; ___________________________________________________________________________
;;;; Regular expressions on the RHS

(fact "aabbcc" => #"a+b+c+")

;;;; ___________________________________________________________________________
;;;; =not=> and =deny=> (synonyms, I think, though hard to tell by looking at
;;;; their definitions)

(fact 3 =not=> 4)
(fact 3 =deny=> 4)

;;;; ___________________________________________________________________________
;;;; Example from docs

(defn each-element-is-one-of [expected-elements]
  (fn [actual]
    (every? (set expected-elements) actual)))

(fact [1 2] => (each-element-is-one-of [1 2 3 4]))

;;;; I think we can do this more naturally like this:

;;;; - Now a more natural way of doing the `each-element-is-one-of`
;;;;   example above:

(fact [1 2] => #(clojure.set/subset? % [1 2 3 4]))

;;;; Perhaps what you say is true for subset, but you have `details=`
;;;; in Tea Knots (tea-knots.domain.tasks-test) and that seems useful.

;;;; ___________________________________________________________________________
;;;; `truthy` and `falsey`

(fact 3 =not=> true)
(fact 3 => truthy)

(fact 3 =not=> falsey)
(fact false => falsey)
(fact nil => falsey)

;;;; ___________________________________________________________________________
;;;; `anything` or `irrelevant`

;;;; These are synonyms.
;;;; Used for "don't care" arguments in prerequisites.
;;;; Could also use it to say that you make no assertions about
;;;; the result of a function.
;;;; jsk: I'm not sure when you'd use the latter.

(fact 3 => anything)

;;;; ___________________________________________________________________________
;;;; `exactly'

;;;; Avoid the special interpretation of functions on the RHS of checkables.
;;;; jsk: Seems like a bad name to me.
;;;;      In fact I'd prefer a special syntax like `(satisfies ...)`
;;;;      rather than the special interpretaion of functions and regexs.

(fact odd? =not=> odd?)
;; (even though you might expect
;; java.lang.IllegalArgumentException: Argument must be an integer)
;; as you get for (odd? odd?) -- I think Midje catches exceptions)

(fact odd? => (exactly odd?))

;;;; ___________________________________________________________________________
;;;; `roughly`

;;;; True if the actual value is within number ± range.
;;;; If range is not given, it's taken to be 1/1000th of number.

(fact 3.003 => (roughly 3))
(fact 3.004 =not=> (roughly 3))
(fact 3.004 => (roughly 3 0.004))

;;;; jsk: There's imprecise representation to be taken into account here.

;;;;___________________________________________________________________________
;;;; `throws`

;;;; Can check for exception class, for error message using string or regex,
;;;; and using a predicate.

;;;; **** Documentation error: predicates apply to the exception, not
;;;;      the message.

(defn my-throw []
  (throw (java.io.IOException. "My error message.")))

(fact 3 =not=> (throws))

(fact (my-throw) => (throws))
(fact (my-throw) => (throws java.io.IOException))
(fact (my-throw) => (throws "My error message."))
(fact (my-throw) => (throws #"error"))
(fact (my-throw) => (throws #(instance? java.io.IOException %)))

(fact (my-throw) => (throws "My error message."
                            #"error"
                            #(instance? java.io.IOException %)
                            java.io.IOException))

(fact (my-throw) => (throws "My error message."
                            "My error message."
                            #"error"
                            #"error"
                            #(instance? java.io.IOException %)
                            #(instance? java.io.IOException %)
                            java.io.IOException))

;;;; **** Documentation error: It says there can be a most one class
;;;;      argument, but (**** and this might be useful with to specify
;;;;      a superclass and a subclass)...

(fact (my-throw) => (throws Exception java.io.IOException))

;;;; ___________________________________________________________________________
;;;; Checking sequential collections

;;;; just
;;;; Use extended equaility.
;;;; **** Why is it called "just"?  It makes no sense to me.

(fact
 [1 2 3] => (just [odd? even? odd?]))

(fact
 ["a" "aa" "aaa"] (just [#"a+" #"a+" #"a+"]))

(fact
 "You can omit the brackets (**** jsk: yeuch!)"
 [1 2 3] => (just odd? even? odd?))

(fact
 "extended equality only applies to the top level"
 [[[1]]] =not=> (just [[[odd?]]])
 [[[1]]] =not=> [[(just odd?)]])

(fact
 "you have to do this"
 [[[1]]] => (just (just (just odd?))))


;;;; just and not caring about order

(fact
 [2 1 3] => (just #{1 2 3})
 [2 1 3] => (just [1 2 3] :in-any-order)
 [2 1 3] => (just 1 2 3 :in-any-order) ; **** jsk: yeuch++!
 [1 3] => (just [odd? 1] :in-any-order) ; doesn't over-commit to matches
 )

;;;; contains
;;;; For matching subsequences.

(facts
 (letfn [(a? [x] (#{:a "a"} x))
         (b? [x] (#{:b "b"} x))]
   ;; match a subsequence
   (fact [0 :a :b "a" 0] =>     (contains [b? a?]))
   (fact [0 :a :b "a" 0] =not=> (contains [a? a?]))
   (fact [0 :a :b "a" 0] =>     (contains [b?]))
   (fact [0 :a :b "a" 0] =>     (contains b?))
   ;; :in-any-order
   (fact [0 :a :b 0]   =>     (contains #{b? a?}))
   (fact [0 :a :b 0]   =>     (contains [b? a?] :in-any-order))
   (fact [0 :a 0 :b 0] =not=> (contains [b? a?] :in-any-order))
   ;; :gaps-ok
   (fact [0 :a :b "a" 0] => (contains [a? a?] :gaps-ok))
   ;; :in-any-order and :gaps-ok
   (fact [0 :a 0 :b 0] => (contains [b? a?] :in-any-order :gaps-ok))
   ;; Marick: "Note: the algorithm used for this complicated case is
   ;; inefficient and also won't work on longer sequences. I keep
   ;; hoping someone will replace it."
   ))

;;; Only for use with extended equality.
;;; So, unlike `has-prefix` and `has-suffix`.
;;; **** Feels wrong.
;;; This raises an exception:
;;;   (fact [1 2 3] => (contains? 2 3))

;;;; **** The word "contains" feels wrong because it's usually used
;;;;      for elements rather than subsequences.
;;;;      See examples below.

(facts 
 (letfn [(a? [x] (#{:a "a"} x))
         (b? [x] (#{:b "b"} x))]
   (fact "Leaving off brackets with a single item has the meaning I expect"
         [0 :a 0] => (contains a?))
   (facts "**** What I'd expect the word 'contains' to mean, and
                what you have to do instead"
          (fact "I'd expect this to succeed with `=>`"
                [0 :a [:b "a"] 0] =not=> (contains [b? a?]))
          (fact "What you have to do instead"
                [0 :a [:b "a"] 0] => (contains (just [b? a?]))))))

;;;; has-prefix

;;; **** Marick says "Whereas contains finds matches anywhere within the
;;; sequence, has-prefix forces the match to be at the beginning". But
;;; there's also the fact that `contains` uses extended equality and
;;; `has-prefix` does not.

(fact "has-prefix is anchored to the left"
      [1 2 3] =not=> (has-prefix [2 3]) ; it's not a prefix
      [1 2 3] =>     (has-prefix [1 2])     
      [1 2 3] =not=> (has-prefix [2 1]) ; order matters
      [1 2 3] =>     (has-prefix [2 1] :in-any-order)
      [1 2 3] =>     (has-prefix #{2 1}))

;;;; has-suffix

;;; Like `has-prefix`.

;;;; n-of Marick: "These are used to avoid giving just a sequence of n
;;;; identical elements"

(fact "one checker to match exactly N elements."
      ["a"] => (one-of "a")
      [:k :w] => (two-of keyword?)
      ["a" "aa" "aaa"] => (three-of #"a+")
      ;; ...
      [1 3 5 7 9 11 13 15 17] => (nine-of odd?)
      ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j"] => (ten-of string?)
      ;; to go above ten-of
      (repeat 100 "a") => (n-of "a" 100)
      ;; nesting
      [[2 4] [6 8]] => (two-of (two-of even?)))

;;;; has

(facts
 (fact "You can do this"
       [2 3 4] =>     (partial some odd?)
       [2 4 6] =not=> (partial some odd?))
 (fact "Some more elaborate uses of partial will not work as you expect,
        because of a mistake in Midje's design (which will be fixed)."
       [[1] [2]] => (partial every? (just [1])))
 (fact "Use `has` instead"
       [[1] [2]] =not=> (has every? (just [1])))
 (fact "`has` allows regular expressions"
       ["ab" "aab" "aaab"] => (has every? #"a+b")
       [1 2 3] => (has not-any? #"a+b")))

;;;; ___________________________________________________________________________
;;;; Collection checkers and strings

;;;; Marick: "Because Clojure treats strings as a sequence of
;;;; characters, the sequence checkers checkers apply. Given regular
;;;; expressions, they're seldom useful."

;;;; See https://github.com/marick/Midje/wiki/Collection-checkers-and-strings.

;;;; ___________________________________________________________________________
;;;; Checking maps and records

(defrecord R [x y])
(defrecord NotR [x y])

(fact
 ;; That the left-hand side below is a record is irrelevant:
 (R. 1 2) => {:x 1, :y 2}
 ;; The contents of the left are compared to the right in exactly
 ;; the same way as if both sides were maps:
 {:x 1, :y 2} => {:x 1, :y 2})

(fact
 ;; (**** The example has an extra right parenthesis.)
 "using a record on the right implies that you care about *both* contents and type"
 ;; **** bug: The following two fail; thay also fails with `=>`.
 ;;           (Hmmm, so how well tested is Midje?)
 ;; {:x 1, :y 2} =not=> (R. 1 2)
 ;; (NotR. 1 2)  =not=> (R. 1 2)
 (R. 1 2)     =>     (R. 1 2))

;;;; You can use `just` for extended equality.
;;;; Use `contains` when you don't care about parts of a map or record

(facts
 (fact "`just` provides extended equality"
       {:a 1, :b 2, :c "some text"} => (just {:a odd?, :b 2, :c #"text"}))
 (fact "`contains` ignores unmentioned keys"
       (R. 1 2) => (contains {:x 1}))
 (fact "`contains` provides extended equality"
       (R. 1 2) => (contains {:x odd?})))

;;;; In both of the previous examples, `contains` will ignore the type
;;;; of the left-hand side. If you want to say specifically that the
;;;; left-hand side is an R that contains an odd :x, use a combining
;;;; checker:

(fact
 (R. 1 2) => (every-checker #(instance? R %)
                            (contains {:x 1})))

;;;; `has` works with quantifiers and values

(fact "`has` quantifies over values"
      {:a 1, :b 3} => (has every? odd?)
      {:a 1, :b 3} => (has some #(= 1 %)))

;;;; Making claims about keys is more awkward

(fact
 "ways to make claims about keys"
 (fact "Contains every key -- two ways"
       (keys {:x 1, :y 1}) => (just #{:x :y})
       {:x 1, :y 1} => (just {:x anything, :y anything}))
 (fact "Contains some of the keys -- two ways"
       (keys {:x 1, :y 1}) => (contains #{:x})
       {:x 1, :y 1} => (contains {:x anything})))

;;;; Map entries
;;;; The just and contains right-hand sides can take arrays of pairs
;;;; (or Java MapEntry objects) instead of a map or record:

(fact
 "a sequence of key/value pairs is OK on the right-hand side"
 {:a 1, :b 2} => (just [[:a 1] [:b 2]])
 (R. 1 nil) => (contains [[:x 1]]))

;;;; ___________________________________________________________________________
;;;; Checking sets

;;;; Checking the whole set

(fact "`just` provides extended equality for sets"
      #{3 8 1} => (just odd? 3 even?))

(fact "checking properties of known number of elements"
      #{1 3 5} => (three-of odd?))

(fact "number irrelevant"
      #{1 3 5} => (has every? odd?))

;;;; Checking a subset

(fact "subsets of literal values"
      #{1 2 3} => (contains 3))

(fact "subsets of checkers"
      #{1 2 3} => (contains odd? even?)) ; **** example has extra right parenthesis.

;;;; ___________________________________________________________________________
;;;; Combining checkers

;;;; `every-checker` can take any argument that can appear on the
;;;; right-hand side of a checkable, including values to be compared
;;;; with ordinary equality.

(fact 5 => (every-checker 5 odd? (roughly 5)))

(fact "aaab" => (every-checker #"a+b" #(= 4 (count %))))

;;;; `some-checker` fails only if all of its constituent checkers
;;;; fail. If any succeeds, some-checker stops checking and succeeds
;;;; itself.

(fact 4 =>     (some-checker odd? (roughly 4)))
(fact 4 =not=> (some-checker odd? (roughly 3)))

;;;; ___________________________________________________________________________
;;;; Setup and teardown

;;;; Marick: "Testing code that works with mutable state often
;;;; requires that you change that state before or after a fact or
;;;; checkable. Midje has two ways to do that, one baroque and
;;;; deprecated, one simpler and encouraged."

;;;; Here's the simpler approach.

;;; (with-state-changes [(before :facts ...)]
;;;   (fact ...)
;;;   (fact ...)
;;;   ...)
;;; 
;;; (with-state-changes [(after :facts ...)]
;;;   (fact ...)
;;;   (fact ...))
;;; 
;;; (with-state-changes [(around :facts (... ?form ...))]
;;;   (fact ...)
;;;   (fact ...))

;;;; Nesting currently gets things a bit wrong.

;;;; REPL and whole-file support
;;;; For if you want to test individual facts in the REPL.
;;;; I think it means "namespace", not "whole file".

;;;; ___________________________________________________________________________
;;;; Tabular facts

;;;; ___________________________________________________________________________
;;;; Chatty checkers
;;;; https://github.com/marick/Midje/wiki/Chatty-checkers

;;;; Chatty checkers provide extra information about a failure.

;;;; There are three ways to create them. Two depend on a checker
;;;; function having the right "shape". The other is less convenient,
;;;; but more general.

;;;; (1) `every-checker` and `some-checker`.

;;;; (2) `chatty-checker` converts each top-level function application
;;;;     in its body into a form that reports its results.

;;;; (3) Adding notes to checker failures
;;;;     Marick: "Note: this implementation is nicely functional and
;;;;     all that, but it's actually awkward for the Midje
;;;;     implementation and prone to error. It may be replaced in
;;;;     later versions of Midje."

;;;; ___________________________________________________________________________
;;;; Testing private functions

;;;; Three ways:

;;;; (1) Can use `#'` to get any var.
;;;;     This is ugly, but only this way works in prerequisites (which
;;;;     is perhaps an argument for always using this way).  That's
;;;;     because the other ways copy vars.

;;;  (fact (#'scratch.core/super-secret-function 1) => 6)

;;;; (2) `testable-privates`

;;; (use '[midje.util :only [testable-privates]])
;;; (testable-privates scratch.core super-secret-function)
;;; (fact (super-secret-function 4) => 4)

;;;; (3) `expose-testables`

;;; Code under test:
;;;   (defn- ^{:testable true}
;;;     not-so-secret-function [n] ...)
;;; Tests:
;;;   (use '[midje.util :only [expose-testables]])
;;;   (expose-testables scratch.core2)
;;;   (fact (not-so-secret-function 4) => 4)

;;;; ___________________________________________________________________________
;;;; Top-down

;;;; ____
;;;; Basics

;;; Note the use of metaconstants (..lump.. etc).

(unfinished lump-pieces decode-piece)

(defn decode-lump [lump]
  (apply str (map decode-piece (lump-pieces lump))))

(fact
 (decode-lump ..lump..) => "01"
 (provided
  (lump-pieces ..lump..) => [..0-lump.. ..1-lump..]
  (decode-piece ..0-lump..) => 0
  (decode-piece ..1-lump..) => 1))

;;;; ____
;;;; Throwing

(unfinished throw-if-4)

(defn call-throw-if-4 [x]
  (try (throw-if-4 x)
       (catch Error
           e
         :throw-happened)))

(fact
 (call-throw-if-4 4) => :throw-happened
 (provided
  (throw-if-4 4) =throws=> (Error. "boom!")))

;;;; ____
;;;; Multiple checkables with pre-requisites -- **** horrible syntax

;;;; **** Did I mention that the /x => y/ syntax is horrible? In Lisp
;;;;      you want to make good use of forms, for nice indentation and
;;;;      for simpler editing.
;;;;      Here you have four separate things that go together but that
;;;;      aren't together.
;;;;      (fact a1 => e1 (provided ...) a2 => e2 (provided ...))

(fact
 "Nasty syntax; don't do this! Have separate facts instead."
 (call-throw-if-4 4) => :throw-happened
 (provided
  (throw-if-4 4) =throws=> (Error. "boom!"))
 (call-throw-if-4 5) => 5
 (provided
  (throw-if-4 5) => 5))

(fact
 "Nicer"
 (fact
  (call-throw-if-4 4) => :throw-happened
  (provided
   (throw-if-4 4) =throws=> (Error. "boom!")))
 (fact
  (call-throw-if-4 5) => 5
  (provided
   (throw-if-4 5) => 5)))

;;;; ____
;;;; The GPA exercises

(unfinished child-of-wealthy-alumnus?)

(defn gpa-1 [student coursework]
  (let [total-hours (reduce + (map :credit-hours
                                   coursework))
        total-grads (reduce + (map #(* (:credit-hours %) (:grade %))
                                   coursework))
        extras (if (child-of-wealthy-alumnus? student) 0.5 0)]
    (min 5.0
         (+ (/ total-grads total-hours)
            extras))))

(fact
 (let [correct-gpa 3.66
       tolerance 0.01
       coursework [{:credit-hours 1, :grade 5}
                   {:credit-hours 2, :grade 3}]]
   (fact "without extras"
         (gpa-1 ..student.. coursework) => (roughly correct-gpa tolerance)
         (provided (child-of-wealthy-alumnus? ..student..) => false))
   (fact "with extras"
         (gpa-1 ..student.. coursework) => (roughly (+ correct-gpa 0.5) tolerance)
         (provided (child-of-wealthy-alumnus? ..student..) => true)))
 (let [max-gpa 5.0
       coursework [{:credit-hours 1, :grade 4.7}]]
   (fact "max"
         (gpa-1 ..student.. coursework) => max-gpa
         (provided (child-of-wealthy-alumnus? ..student..) => true))))

;;;; ____

(defn fair-gpa [coursework]
  (let [total-hours (reduce + (map :credit-hours
                                   coursework))
        total-grads (reduce + (map #(* (:credit-hours %) (:grade %))
                                   coursework))]
    (/ total-grads total-hours)))

(defn gpa-2 [student coursework]
  (let [extras (if (child-of-wealthy-alumnus? student) 0.5 0)]
    (min 5.0
         (+ (fair-gpa coursework)
            extras))))

(fact
 (let [correct-gpa 3.66
       tolerance 0.01
       coursework [{:credit-hours 1, :grade 5}
                   {:credit-hours 2, :grade 3}]]
   (fact "fair-gpa"
         (fair-gpa coursework) => (roughly correct-gpa tolerance))
   (fact "extras"
         (fact "without extras"
               (gpa-2 ..student.. ..coursework..) => (roughly correct-gpa tolerance)
               (provided (child-of-wealthy-alumnus? ..student..) => false
                         (fair-gpa ..coursework..) => correct-gpa))
         (fact "with extras"
               (gpa-2 ..student.. ..coursework..) => (roughly (+ correct-gpa 0.5) tolerance)
               (provided (child-of-wealthy-alumnus? ..student..) => true
                         (fair-gpa ..coursework..) => correct-gpa))
         (fact "max"
               (let [max-gpa 5.0]
                 (fact
                  (gpa-2 ..student.. ..coursework..) => max-gpa
                  (provided (child-of-wealthy-alumnus? ..student..) => true
                            (fair-gpa ..coursework..) => 4.7)))))))

;;;; ____
;;;; Prerequisites use a variant of extended equality to match
;;;; arguments
;;;; https://github.com/marick/Midje/wiki/Describing-one-checkable%27s-prerequisites#prerequisites-use-a-variant-of-extended-equality-to-match-arguments

;; (provided 
;;  (g #"hello.*world") => 12)

;; (provided 
;;  (g (roughly 5.0 0.01)) => 89)

;; (provided 
;;  (g anything) => 89)

;;; Argument matching isn't quite the same as extended equality,
;;; though. It differs in the handling of functions other than
;;; predefined checkers. Functions are treated just as values.
;;; You can use `as-checker`, which means /satisfies?/.
;;; You can define your own checkers for use in prerequisites.
;;; (**** I think use of /satisfies?/ everywhere would have been nicer.)

;;;; ____
;;;; Call counts

(unfinished call-me maybe-call-me)

(defn do-two-calls []
  (call-me)
  (call-me)
  42)

(facts
 (fact "By default, expect at least one call"
       (do-two-calls) => 42
       (provided
        (call-me) => anything))
 (fact "Can specify call counts"
       (do-two-calls) => 42
       (provided
        (call-me) => anything :times 2))
 (fact "Can specify call counts as a sequence of permitted values"
       (do-two-calls) => 42
       (provided
        (call-me) => anything :times [2 4]))
 (fact "Can specify call counts as a lazy sequence of permitted values"
       (do-two-calls) => 42
       (provided
        (call-me) => anything :times (range 2 5)))
 (fact "To say this call is optional, use /:times (range)/"
       (do-two-calls) => 42
       (provided
        (call-me) => anything
        (maybe-call-me) => anything :times (range)))
 (fact "Can specify call counts using checkers"
       (do-two-calls) => 42
       (provided
        (call-me) => anything :times even?))
 (fact "To say not called, must specify args (if any) and result.
        This will be improved later."
       (do-two-calls) => 42
       (provided
        (call-me) => anything
        (maybe-call-me) => anything :times 0)))

;;;; ____
;;;; Shorthand for prerequisite chaining

(unfinished happens-first happens-second)

(defn function-under-test [n]
  (inc (happens-second (happens-first 1 n))))

(fact "Not using prerequisite chaining"
      (function-under-test 5) => 101
      (provided
       (happens-first 1 5) => ..some-result..
       (happens-second ..some-result..) => 100))

(fact "Using prerequisite chaining"
      (function-under-test 5) => 101
      (provided
       (happens-second (happens-first 1 5)) => 100))

;;;; ____
;;;; Variant prerequisite arrows

;;;; Streaming values from impure prerequisites
;;;; `=streams=>`

;;;; Prerequisites that throw exceptions
;;;; `=throws=>` -- we've seen this already.

;;;; ____
;;;; Prerequisites and protocols

;;;; Because protocol functions are compiled so they're called by the
;;;; JVM, not by the Clojure dispatching mechanism, using them in
;;;; prerequisites requires a little extra work.

;; use midje.open-protocols in addition to midje.sweet
;; 
;; Use `defrecord-openly`, `deftype-openly`.
;; 
;; Other protocol functions like reify don't have -openly variants
;; yet.

;;;; ____
;;;; Using private functions in prerequisites

;;;; "Earlier, you saw three ways of using private functions in
;;;; checkables."  But only referring to them via their var works
;;;; in prerequisites, because the other ways copy vars.

;;;; ____
;;;; Establishing fact-wide prerequisites

(unfinished fwp-1 fwp-2)

(defn fwp-1+2 [] (+ (fwp-1) (fwp-2)))
(defn fwp-1*2 [] (* (fwp-1) (fwp-2)))

(fact
 (prerequisites (fwp-1) => 3
                (fwp-2) => 4)
 (fwp-1+2) => 7
 (fwp-1*2) => 12)

;;;; "Fact-wide prerequisites do not have to be used."

;;;; Nesting overrides as you'd expect.

;;;; "`provided` prerequisites override any from a `prerequisites` form."

;;;; "When more than one prerequisite at the same nesting level match,
;;;; it's the latest one that's chosen. That allows for "catch all" or
;;;; default prerequisites."

;;;; "prerequisite expressions have access to lexically-scoped symbols."

;;;; ___________________________________________________________________________
;;;; Defining checkers for use in prerequisites

;;;; https://github.com/marick/Midje/wiki/Defining-checkers-for-use-in-prerequisites

;;;; ___________________________________________________________________________
;;;; =expands-to=>
;;;; The left-hand side is macroexpanded and compared to the right-hand side:

(fact (when true 3) =expands-to=> (if true (do 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types

;; self type (all terms can type themselves, tautology)
(: x x)

;; types of terms
(: (=> 1 0)                 (-> 1 0))              ;; function
(: (=> (Set 1 0) (Set 0 1)) (? (-> 1 0) (-> 0 1))) ;; function of sets
(: (Set (=> 1 0) (=> 0 1))  (? (-> 1 0) (-> 0 1))) ;; set of functions
(: (Set 1 0)                1)                     ;; set
(: (Do String)              (do x read read))      ;; IO binding
(: 1                        (= x 1 x))             ;; let binding
(: 0                        ((-> 1 0) 1))          ;; application
(: [Int Float]              [42 3.14])             ;; product
(: [Char Char nil]          "hi")                  ;; string
(: Float                    3.14)                  ;; float
(: Int                      -7)                    ;; int
(: Nat                      42)                    ;; nat
(: Char                     'a')                   ;; char
(: nil                      nil)                   ;; unit
(: 1                        one)                   ;; variable

;; polymorphism
(A a Type (: (=> a a) (-> x x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lambda cube

;; simple typing (terms depend on terms)

(= id
   (: (=> Nat Nat)
      (-> x x)))

(id 1) ;; (: Nat 1)

;; polymorphism (terms depend on types)

(= id
   (A a Type
      (: (=> a a)
         (-> x x))))

(id 'a') ;; (: Char 'a')

;; type constructors (types depend on types)

(= List
   (: (=> Type Type)
      (-> a (Set nil [a (List a)]))))

;; this type is infinite! but evaluation is lazy
(List Nat) ;; (Set nil [Nat (Set nil [Nat ...])])

;; dependent types (types depend on terms)

(= Vec
   (: (=> Nat Type Type)
      (? (-> 0 a nil)
         (-> n a [a (Vec (- n 1) a)]))))

(Vec 3 Char) ;; [Char Char Char nil]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structural control flow

;; the idea of structural control flow is that control flow is
;; achieved through sets of functions, and sets are structurally
;; typed, so control flow is also structurally typed.

;; this set of functions
(? (-> 1 2) (-> 3 4))
;; has the type
(Set (=> 1 2) (=> 3 4))

;; in the same way that a function can be applied, so can a set of functions.
;; we select the first function from the set that is applicable.
;; here, 1 matches 1, so the result is 2
((? (-> 1 2) (-> n (+ n 1))) 1)
;; here, 1 does not match 1, but n (wildcard) does match 42, so we get (+ 42 1)
((? (-> 1 2) (-> n (+ n 1))) 42)

;; the most precise type of the set above is
(Set (=> 1 2) (=> Nat Nat))
;; however, (=> 1 2) is a subtype of (=> Nat Nat), so we can also write:
(Set (=> Nat Nat) (=> Nat Nat))
;; which is equivalent to:
(=> Nat Nat)

;; the return type of `if` is allowed to vary
(= if
   (A a b
      (: (Set (=> "true" a b a)
              (=> "false" a b b))
         (? (-> "true" x y x)
            (-> "false" x y y)))))

;; argument type "true", so result is Float
(: Float (if "true" 3.14 42))

;; argument type (Set "true" "false"), so result is (Set Float Nat)
(: (Set Float Nat) (if x 3.14 42))

;; structurally aware `not`
(= not
   (: (Set (=> "true" "false")
           (=> "false" "true"))
      (? (-> "true" "false")
         (-> "false" "true"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dependent functions

;; we use universal quantification to create dependent functions
;; the return type depends on the argument type

(= paired
   (A a Bool
      (: (=> a (if a [Nat Nat] Nat))
         (-> x (if x [1 2] 0)))))

(paired "true") ;; (: [Nat Nat] [1 2])
(paired "false") ;; (: Nat 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subtyping

;; structural typing of sets enables subtyping

;; User supertype
(= User
   (Set (=> "name" String)
        (=> "id" Nat)
        (=> "ask" Nat Nat)))

;; Alice subtype with no variance, so it's actually invariant (aka type synonym)
(= Alice User)

;; Alice object
(= alice
   (: Alice
      (? (-> "name" "Alice")
         (-> "id" 1)
         (-> "ask" n (+ n 42)))))

;; Bob subytpe with invariance, covariance, contravariance and extension
(= Bob
   (Set (=> "name" String)    ;; same as supertype (invariance)
        (=> "id" 2)           ;; output type 2 is a subtype of Nat (covariance)
        (=> "bob" String)     ;; extension
        (=> String Nat Nat))) ;; input String is a supertype of "ask" (contravariance)

;; Bob object
(= bob
   (: Bob
      (? (-> "name" "Bob")
         (-> "id" 2)
         (-> "bob" "Yes, this is Bob.")
         (-> s n 0))))

(alice "ask" 1) ;; 43
(bob "ask" 1)   ;; 0
(bob "bob")     ;; "Yes, this is Bob."

(= users
   (: (List User)
      [alice bob nil]))

(map (-> user (user "ask" 1)) users) ;; (: (List Nat) [43 0 nil])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modules

;; global definition, across files
(<- a b)

;; local definition, within file
(= a b)

;; util.langa
(= id (-> x x))
(= const (-> x y x))
(<- util
    (? (-> "id" id)
       (-> "const" const)))

;; main.langa
(= const (util "const"))
(= one (const 1 2))

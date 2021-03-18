#lang racket/base

;------------------------------------------------------------------------------
; CS 330 Fall 2020 Programming Exercise 4 - Symbolic Differentiation
;
; author:  Hampton Ford
;
; date:  10/31/2020
;
; Documentation:
;   Course slides and videos/lectures
;   EI w/ Dr. Hadfield to work through Lab25 functions
;   Rewrote make-product to simplify more based on "Comp Sci 330 - Lesson 27 Lab" instructions
;   Tested derivatives w/
;    --https://www.symbolab.com/solver/ordinary-differential-equation-calculator/%5Cfrac%7Bd%7D%7Bdx%7D%5Cleft(5%5E%7Bx%7D%5Cright)
;   EI w/ Dr. Hadfield to work on deriv structure
;   Referenced **A LOT** of Racket Documentation on their official website
;   Special shoutout to Piano Covers by Music Design Group on Spotify for the soul soothing piano music
;
; NOTES:
;  The n-1th level compute simplyfing is pretty cool I think.
; 
;  I ommitted some functions from the template on purpose in favor of built-ins or base provided functions.
; 
;  deriv works as far as I could tell
;
;  derivative, eval-deriv, and eval-derivative don't work
;
; Welp, I honestly can say I tried ʕ ͡°' ͜ʖ ͡°'ʔ
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; MISCELLANEOUS SUPPORT and WRAPPER FUNCTIONS
;------------------------------------------------------------------------------

; Test data types:
; number? built-in
; variable? -- see below
; atom? listed in official documentation but nowhere to be found... see below


; (variable? E) - Is E a variable (single symbol and not a number)?
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t or #f (no side-effects)
; EXAMPLE USE:  (variable? 'x) returns #t
(define (variable? E)
  (if (and (not (number? E)) (not (list? E))) #t #f))

; (atom? E) - Is E a single symbol?
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t or #f (no side-effects)
; EXAMPLE USE:  (atom? 5) returns #t
(define (atom? E)
  (if (or (number? E) (variable? E)) #t #f))
;------------------------------------------------------------------------------

; Test if specifically two numbers are equal:
; =number?

; (=number? EXP NUM) - checks if EXP is a number and equal to NUM
;
; PRECONDITIONS: EXP is an expression list, NUM is a number
; POSTCONDITIONS: #t returned if EXP is a number and equal to NUM,
;         otherwise #f
; EXAMPLE USE: (=number? 5 5) returns #t
(define (=number? EXP NUM)
  (if (and (number? EXP) (number? NUM)) (equal? EXP NUM) #f))

;------------------------------------------------------------------------------

; Test command validity for calculator:
; sum?
; difference?
; product?
; quotient?
; **?

; For functions in section:
; PRECONDITIONS: none
; POSTCONDITIONS: returs #t or #f depending on if E fits data operation 
; EXAMPLE USE: (sum? 'x (**? (+ 5 a) (- c a)))) returns #t while (quotient? '/) returns #f

(define (sum? E)
  (if (not (list? E)) #f
      (if (not (equal? (length E) 3)) #f    ; until here generic test for E as list w/ 3 expressions
          (if (equal? (car E) '+) #t #f))))

(define (difference? E)
  (if (not (list? E)) #f
      (if (not (equal? (length E) 3)) #f    ; until here generic test for E as list w/ 3 expressions
          (if (equal? (car E) '-) #t #f))))

(define (product? E)
    (if (not (list? E)) #f
      (if (not (equal? (length E) 3)) #f    ; until here generic test for E as list w/ 3 expressions
          (if (equal? (car E) '*) #t #f))))

(define (quotient? E)
    (if (not (list? E)) #f
      (if (not (equal? (length E) 3)) #f    ; until here generic test for E as list w/ 3 expressions
          (if (equal? (car E) '/) #t #f)))) ; chose to let language handle divide by zero since I would just throw the same error anyways

(define (**? E)
    (if (not (list? E)) #f
      (if (not (equal? (length E) 3)) #f    ; until here generic test for E as list w/ 3 expressions
          (if (equal? (car E) '**) #t #f))))

;------------------------------------------------------------------------------

; Wrapper functions to simplify std operations - why print '(a + 0) when you can simplify that to 'a
; NOTE: These functions should never be called directly, but instead only be called recursively or by compute
; make-sum
; make-difference
; make-product
; make-quotient
; **

; (make-NAME A B) - returns a level n-1 simplified sum for A + B
;
; PRECONDITIONS: corresponding sum?, difference?, quotient?, etc has been called and is true
; POSTCONDITIONS: list expression (+ A B) or reduced form
;        if either A or B is zero or both are numbers
; EXAMPLE USE: (make-sum '(+ 5 6) 5) returns '(+ 11 5)
(define (make-sum A B)
  (cond
    ((and (list? A) (list? B)) (list '+ (compute A) (compute B)))            ; two lists
    ((and (atom? A) (list? B)) (list '+ A (compute B)))                      ; 1 atom, 1 list
    ((and (atom? B) (list? A)) (list '+ (compute A) B))                      ; 1 atom, 1 list
    ; all atoms now - test variables and numbers;
    ((and (variable? A) (variable? B)) (list '+ A B))                        ; 2 variables
    ((and (number? A) (variable? B))                                         ; 1 number, 1 variable
     (if (=number? A 0) B (list '+ B A)))
    ((and (number? B) (variable? A))                                         ; 1 number, 1 variable
     (if (=number? B 0) A (list '+ A B)))
    ((and (number? A) (number? B)) (+ A B))                                  ; 2 numbers - all possibilities exhausted
    (else (error "Invalid expression. Please try again."))))

(define (make-difference A B)
  (cond
    ((and (list? A) (list? B)) (list '- (compute A) (compute B)))            ; two lists
    ((and (atom? A) (list? B)) (list '- A (compute B)))                      ; 1 atom, 1 list
    ((and (atom? B) (list? A)) (list '- (compute A) B))                      ; 1 atom, 1 list
    ; all atoms now - test variables and numbers;
    ((and (variable? A) (variable? B)) (list '- A B))                        ; 2 variables
    ((and (number? A) (variable? B))                                         ; 1 number, 1 variable
     (if (=number? A 0) B (list '- B A)))
    ((and (number? B) (variable? A))                                         ; 1 number, 1 variable
     (if (=number? B 0) A (list '- A B)))
    ((and (number? A) (number? B)) (- A B))                                  ; 2 numbers - all possibilities exhausted
    (else (error "Invalid expression. Please try again."))))

(define (make-product A B)
  (cond
    ((and (list? A) (list? B)) (list '* (compute A) (compute B)))            ; two lists
    ((and (atom? A) (list? B)) (list '* A (compute B)))                      ; 1 atom, 1 list
    ((and (atom? B) (list? A)) (list '* (compute A) B))                      ; 1 atom, 1 list
    ((or (=number? A 0) (=number? B 0)) 0)
    ((=number? A 1) B)
    ((=number? B 1) A)
    ((and (number? A) (number? B)) (* A B))
    (else (list '* A B))))

(define (make-quotient A B)
  (cond ((=number? B 0) (error "Invalid expression. Dividing by infinity in make-quotient"))    ; handle DIV by 0
   ((and (list? A) (list? B)) (list '/ (compute A) (compute B)))            ; two lists
   ((and (atom? A) (list? B)) (list '/ A (compute B)))                      ; 1 atom, 1 list
   ((and (atom? B) (list? A)) (list '/ (compute A) B))                      ; 1 atom, 1 list
   ((=number? A 0) 0)                                                       ; 0 in numerator is just 0
   ((=number? B 1) A)                                                       ; V=1 on bottom is just U
   ((and (number? A) (number? B)) (/ A B))                                  ; Actually divide U/V
   (else (list '/ A B))))

; Gets a pass on the naming convertion since ** is just a wrapper for expt anyways
; ADDTIONTAL CONDITION: EXPONENT must a #
(define (** BASE EXPONENT)
  (cond
        ((and (list? BASE) (list? EXPONENT)) (list '** (compute BASE) (compute EXPONENT)))            ; two lists
        ((and (atom? BASE) (list? EXPONENT)) (list '** BASE (compute EXPONENT)))                      ; 1 atom, 1 list
        ((and (atom? EXPONENT) (list? BASE)) (list '** (compute BASE) EXPONENT))                      ; 1 atom, 1 list
        ((not (number? EXPONENT)) (error "Invalid expression. Use format: (** <list or atom> <number>)."))
        ((=number? EXPONENT 0) 1)
        ((=number? EXPONENT 1) BASE)
        ((and (integer? BASE) (integer? EXPONENT)) (expt BASE EXPONENT))
        (else (list '** BASE EXPONENT))))

;------------------------------------------------------------------------------

; Create a controller for each operation that allows general expressions to be easily handled:
; compute

; (compute E) - calls the next supported operation on a list else throws an error on invalid input
;
; PRECONDITIONS: dependencies above
; POSTCONDITIONS: simplified answer for any supported operation
; EXAMPLE USE: (compute '(- x 0)) returns x
(define (compute E)
  (cond
    ((sum? E) (make-sum (cadr E) (cadr (cdr E))))
    ((difference? E) (make-difference (cadr E) (cadr (cdr E))))
    ((product? E) (make-product (cadr E) (cadr (cdr E))))
    ((quotient? E) (make-quotient (cadr E) (cadr (cdr E))))
    ((**? E) (** (cadr E) (cadr (cdr E))))
    (else (error "Invalid expression. Please try again."))))


;==============================================================================================================
;****** PREDICATES AND DERIV HELPERS ***********
; d-foo? functions are meant to include test for everything and include calls to previously defined MISC Help Functions
; deriv-foo functions will actually take the derivatives

;PRECONDITIONS: Operations take the appropriate form (e.g- (deriv-sum '(+ x 5) 'x) -)
;--------------------------------------------------------------------------------------------------------------
; returns true if EXP is an atom, EXAMPLE INPUT: (d-variable? 'x 'x) (d-variable 5 'y)
; acts a wrapper for variable? for naming consistent naming convention
(define (d-variable? EXP)
  (if (variable? EXP) #t #f))

; returns symbolic derivative of a atomic variable
(define (deriv-variable EXP VAR)    ; assumes EXP is an b/c d-variable? will have been called
   (cond
     ((equal? EXP VAR) 1)           ; only treat vars being differentiated w/ respect to - vars
     (else 0)))

;------------------------------------------------------------------------------
; returns true if EXP is a sum
(define (d-sum? EXP)
  (if (sum? EXP) #t #f))

; returns the symbolic derivative of a sums
(define (deriv-sum A B VAR)
  (cond
    ((and (list? A) (list? B)) (compute (list '+ (deriv A VAR) (deriv B VAR))))   ; calls to deriv will implicitly call
    ((and (atom? A) (list? B)) (compute (list '+ (deriv A VAR) (deriv B VAR))))   ; ... deriv-variable when the time is right
    ((and (atom? B) (list? A)) (compute (list '+ (deriv A VAR) (deriv B VAR))))   ; end list handling
    (else (compute (list '+ (deriv-variable A VAR) (deriv-variable B VAR))))))    ; add the two derivatives

;------------------------------------------------------------------------------
; returns true if EXP is a difference
(define (d-difference? EXP)
  (if (difference? EXP) #t #f))

; returns the symbolic derivative of a difference
(define (deriv-difference A B VAR)
  (cond
    ((and (list? A) (list? B)) (compute (list '- (deriv A VAR) (deriv B VAR))))   ; calls to deriv will implicitly call
    ((and (atom? A) (list? B)) (compute (list '- (deriv A VAR) (deriv B VAR))))   ; ... deriv-variable when the time is right
    ((and (atom? B) (list? A)) (compute (list '- (deriv A VAR) (deriv B VAR))))   ; end list handling
    (else (compute (list '- (deriv-variable A VAR) (deriv-variable B VAR))))))    ; subtract the two derivatives

;------------------------------------------------------------------------------
; returns true if EXP is a product
(define (d-product? EXP)
  (if (product? EXP) #t #f))
  
; returns the symbolic derivative of a product
(define (deriv-product A B VAR)
    (cond
    ((and (list? A) (list? B)) (compute (list '* (deriv A VAR) (deriv B VAR))))                  ; calls to deriv will implicitly call
    ((and (atom? A) (list? B)) (compute (list '* (deriv A VAR) (deriv B VAR))))                  ; ... deriv-variable when the time is right
    ((and (atom? B) (list? A)) (compute (list '* (deriv A VAR) (deriv B VAR))))                  ; end list handling
    ((and (equal? A VAR) (equal? B VAR)) (list '* 2 A))                                          ; sort of an edge case... it works
    (else (compute (list '+ (list '* (deriv-variable A VAR) B) (list '* (deriv-variable B VAR) A)))))) ; multiply the two derivatives

;------------------------------------------------------------------------------
; returns true if EXP is an base to a power and fits input reqs
; SAMPLE INPUT: (d-power? '(** x 2) 'x) or (d-power? '(** 2 2) 'x) --assumes variable^number so 2**x or x**x is a no-no
(define (d-**? EXP)
  (if (**? EXP) #t #f))
  
; (deriv-power EXP VAR) assuming d-power? is true
; A - base
; B - power
(define (deriv-** A B VAR)
    (cond
    ((and (list? A) (list? B)) (compute (list '** (deriv A VAR) (deriv B VAR))))   ; calls to deriv will implicitly call
    ((and (atom? A) (list? B)) (compute (list '** (deriv A VAR) (deriv B VAR))))   ; ... deriv-variable when the time is right
    ((and (atom? B) (list? A)) (compute (list '** (deriv A VAR) (deriv B VAR))))   ; end list handling
    (else (
  (list '* (deriv A VAR) (list '* B (list '** A (list '- B 1))))))))               ; simplified: exp*base^(exp-1)*deriv-variable(base)        
      
;------------------------------------------------------------------------------
; returns true if we have U/V AND throw infinity error if V = 0
; SAMPLE INPUT: (d-quotient? '(/ 3 5) 'x) or (... '(/ x 5) 'y) or (... '(/ y / 5) 'y)
(define (d-quotient? EXP)
  (if (quotient? EXP) #t #f))

; derive the quotient assuming d-quotient? is satisfied
(define (deriv-quotient A B VAR)
    (cond
    ((and (list? A) (list? B)) (compute (list '/ (deriv A VAR) (deriv B VAR))))       ; calls to deriv will implicitly call
    ((and (atom? A) (list? B)) (compute (list '/ (deriv A VAR) (deriv B VAR))))       ; ... deriv-variable when the time is right
    ((and (atom? B) (list? A)) (compute (list '/ (deriv A VAR) (deriv B VAR))))       ; end list handling
    ;((and (equal? A VAR) (equal? B VAR)) 1)
    (else (compute (list '/ (list '- (list '* B (deriv A VAR)) (list '* A (deriv B VAR))) (list '* B B))))))  ; (v*'u - u*'v) / v^2


;------------------------------------------------------------------------------
; d-sin needs a input '(sin EXPR) thus input must be list
(define (d-sin? EXP)
  (if (equal? (car EXP) 'sin) #t #f))

; PRECONDITIONS: call (d-sin? EXP VAR)
; d sin(u) = cos(u) * chain rule d (u) -- DON'T FORGET THE NEGATIVE IF DERIVING a negative u
(define (deriv-sin A VAR)
    (list '* (list 'cos A) (deriv A VAR))) 

;------------------------------------------------------------------------------
(define (d-cos? EXP)
  (if (equal? (car EXP) 'cos) #t #f))

; d cos(u) = (-1 * sin(u)) * (chain rule d (u))  -- DON'T FORGET THE NEGATIVE IF DERIVING a negative u
(define (deriv-cos A VAR)
  (list '* (list 'sin A) (* -1 (deriv A VAR))))

;------------------------------------------------------------------------------
; returns true if u in (log u) is not 0
; SAMPLE INPUT: '(log EXP), 'x
(define (d-log? EXP VAR)
  (cond
    ((not (equal? (car EXP) 'log)) #f)                                          ; check function
    ((=number? EXP 0) (error "Operation: log(0) is an illegal operation."))))   ; check illegal log operation         

; (deriv-log EXP VAR)
(define (deriv-log A VAR)
  (compute (list '* (list '/ 1 A) (deriv A VAR))))

;------------------------------------------------------------------------------
; deriv-exp viable if if's we're deriving exp
; SAMPLE INPUT: (d-exp EXP VAR) , where EXP = '(exp x) or '(exp 5) and VAR = 'x
(define (d-exp? EXP VAR)
  (if (equal? (car EXP) 'exp) #t #f))

(define (deriv-exp A VAR)
  (list '* (list 'exp A) (deriv A VAR)))

;==============================================================================
;==============================================================================
;==============================================================================
; (deriv EXP VAR) - returns expression list that is the derivative
;          of EXP with respect to VAR
;
; PRECONDITIONS:  EXP is a list representing a linear expression,
;                 VAR is a variable
; POSTCONDITIONS: returns the derivative of EXP with respect to VAR
;          returns an error message if improperly formatted expression
; EXAMPLE USE: (deriv '(+ (* 3 x) (* a x)) 'x) returns (+ 3 a)
;(define (deriv EXP VAR)...    - accepts prefix prefix expression and produces prefix derivative
(define (deriv EXP VAR)
  (cond ((not (variable? VAR)) (error "Invalid expression. Pass an appropriate var."))
    ((number? EXP) 0)                                                        ; derive constant
    ; beyond here, functions can receive only variables and lists
    ((d-variable? EXP) (deriv-variable EXP VAR))                             ; derive variables
    ; beyond here, functions can receive only lists
    ((d-sum? EXP) (deriv-sum (cadr EXP) (cadr (cdr EXP)) VAR))               ; derive sums
    ((d-difference? EXP) (deriv-difference (cadr EXP) (cadr (cdr EXP)) VAR)) ; derive differences 
    ((d-product? EXP) (deriv-product (cadr EXP) (cadr (cdr EXP)) VAR))       ; derive products
    ((d-**? EXP) (deriv-** (cadr EXP) (cadr (cdr EXP)) VAR))                 ; derive expr raised to const powers - e.g. power rule
    ((d-quotient? EXP) (deriv-quotient (cadr EXP) (cadr (cdr EXP)) VAR))     ; derive quotients
    ((d-sin? EXP) (deriv-sin (cadr EXP) VAR))                                ; derive sin expressions
    ((d-cos? EXP) (deriv-cos (cadr EXP) VAR))                                ; derive cos expressions
    ((d-log? EXP VAR) (deriv-log (cadr EXP) VAR))                                ; derive logs - assuming natural logs per instructions
    ((d-exp? EXP VAR) (deriv-exp (cadr EXP) VAR))                            ; derive e^EXP - e.g. exponent rule
    (else (error "Invalid expression. Bad input to deriv."))))               ; catch all if any EXP or sub-EXP is syntatically invalid 

;-----------------------------------------------------------------------------
; (eval-deriv EXP VAR VALUE) - evaluates prefix derivative at VAR = VALUE
(define (eval-deriv EXP VAR VALUE)
  (deriv (replace VAR VALUE EXP) VAR))

;-----------------------------------------------------------------------------
; (derivative EXP VAR) - accepts infix expression and produces infix expression
(define (derivative EXP VAR)
  (prefix-2-infix (deriv (infix-2-prefix EXP) VAR)))

; (eval-derivative EXP VAR VALUE) - evaluates infix derivative at VAR = VALUE
; (derivative EXP VAR) --> if (car == VAR) then ret VALUE --> compute
(define (eval-derivative EXP VAR VALUE)
  (deriv (replace VAR VALUE EXP) VAR))

; (infix-2-prefix EXP) - converts expression EXP from infix to prefix
;
; *** ADD CODE TO HANDLE POWERS, QUOTIENTS, COSINES, AND LOG EXPRESSIONS ***

(define (infix-2-prefix EXP)
  (cond
    ((or (null? EXP) (variable? EXP) (number? EXP)) EXP)
    ((and (list? EXP) (= (length EXP) 1)) (infix-2-prefix (car EXP))) ; handles (x) from cos, sin, ...
    ((or (sum? EXP) (product? EXP) )
         (list (cadr EXP) (infix-2-prefix (car EXP)) (infix-2-prefix (caddr EXP))))
    ((or (d-sin? EXP) )
         (list (car EXP) (infix-2-prefix (cadr EXP))))
    (else
         (error "Could not convert prefix expression to infix."))))

; (prefix-2-infix EXP) - converts expression EXP from prefix to infix
;
; *** ADD CODE TO HANDLE POWERS, QUOTIENTS, COSINES, AND LOG EXPRESSIONS ***

(define (prefix-2-infix EXP)
  (cond 
    ((or (null? EXP) (variable? EXP) (number? EXP)) EXP)
    ((or (sum? EXP) (product? EXP) )
         (list (prefix-2-infix (cadr EXP)) (car EXP) (prefix-2-infix (caddr EXP))))
    ((or (d-sin? EXP)  )
         (list (car EXP) (list (cadr EXP))))
    (else
         (error "Could not convert prefix expression to infix."))))

; VAR VALUE EXP
(define (replace VAR VALUE EXP)
  (cond
    ((null? EXP) (list))
    ((equal? (car EXP) VAR) append(VALUE (replace VAR VALUE (cdr EXP))))
    (else (cons((car EXP) (append VAR VALUE (cdr EXP)))))))
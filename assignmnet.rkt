#lang pl


;question 1
#| 
    ---------- fill ----------
|#






;question 2
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares  lst)

    (foldl (lambda ([x : Number] [sum : Number]) (+ (* x x) sum)) 0 lst)

)

#|
alternative way

(: sum-of-squares-map : (Listof Number) -> Number)
(define (sum-of-squares-map  lst)

    (foldl + 0 (map (lambda ([x : Number]) (* x x))  lst))

)
|#

(test (sum-of-squares '(1 2 3)) => 14) 
(test (sum-of-squares '()) => 0) 
(test (sum-of-squares '(2)) => 4) 
(test (sum-of-squares '(1)) => 1) 
(test (sum-of-squares '(2 3 4 5)) => 54) 
(test (sum-of-squares '(2 3 4 5.0)) => 54.0) 
(test (sum-of-squares '(2 3 4 -5.0)) => 54.0) 
(test (sum-of-squares '(2 3 4 -5.0 0.5)) => 54.25) 


;question 3 A

(: createPolynomial : (Listof Number) -> (Number -> Number)) ;return type is a function (Number -> Number)
(define (createPolynomial coeffs) 

    (: poly : (Listof Number) Number Integer Number -> Number) ;helper function for tail recursion
    (define (poly argsL x power accum) 
        (if (null? argsL)
            accum ;list is now empty
            (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power)))) ;add to accum and go on with the list
        ) 
    )

    (: polyX : Number -> Number) ;function we want to finally return
    (define (polyX x) 
        (poly coeffs x 0 0) ;first power is 0 and we start accum at 0
    )        
    polyX ;finally return polyX
)


(define p2345 (createPolynomial '(2 3 4 5))) 
(test (p2345 0) =>  
   (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3)))) 

(test (p2345 4) => 
    (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3)))) 

(test (p2345 11) => 
    (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3)))) 

(test (p2345 -11) => 
    (+ (* 2 (expt -11 0)) (* 3 (expt -11 1)) (* 4 (expt -11 2)) (* 5 (expt -11 3)))) 


(define p536 (createPolynomial '(5 -3 6))) 
(test (p536 11) => 
    (+ (* 5 (expt 11 0)) (* -3 (expt 11 1)) (* 6 (expt 11 2)))) 

(test (p536 -11) => 
    (+ (* 5 (expt -11 0)) (* -3 (expt -11 1)) (* 6 (expt -11 2)))) 

(test (p536 0) => 
    (+ (* 5 (expt 0 0)) (* -3 (expt 0 1)) (* 6 (expt 0 2)))) 


(define p504 (createPolynomial '(5 0 4))) 
(test (p504 11) => 
    (+ (* 5 (expt 11 0)) (* 0 (expt 11 1)) (* 4 (expt 11 2)))) 

(test (p504 -11) => 
    (+ (* 5 (expt -11 0)) (* 0 (expt -11 1)) (* 4 (expt -11 2)))) 
    
(test (p504 0) => 
    (+ (* 5 (expt 0 0)) (* 0 (expt 0 1)) (* 4 (expt 0 2)))) 


(define p500 (createPolynomial '(5 0 0))) 
(test (p500 11) => 
    (+ (* 5 (expt 11 0)) (* 0 (expt 11 1)) (* 0 (expt 11 2)))) 

(test (p500 -11) => 
    (+ (* 5 (expt -11 0)) (* 0 (expt -11 1)) (* 0 (expt -11 2)))) 
    
(test (p500 0) => 
    (+ (* 5 (expt 0 0)) (* 0 (expt 0 1)) (* 0 (expt 0 2)))) 


(define p030 (createPolynomial '(0 3 0))) 
(test (p030 11) => 
    (+ (* 0 (expt 11 0)) (* 3 (expt 11 1)) (* 0 (expt 11 2)))) 

(test (p030 -11) => 
    (+ (* 0 (expt -11 0)) (* 3 (expt -11 1)) (* 0 (expt -11 2)))) 
    
(test (p030 0) => 
    (+ (* 0 (expt 0 0)) (* 3 (expt 0 1)) (* 0 (expt 0 2)))) 


(define p000 (createPolynomial '(0 0 0))) 
(test (p000 11) => 
    (+ (* 0 (expt 11 0)) (* 0 (expt 11 1)) (* 0 (expt 11 2)))) 

(test (p000 -11) => 
    (+ (* 0 (expt -11 0)) (* 0 (expt -11 1)) (* 0 (expt -11 2)))) 
    
(test (p000 0) => 
    (+ (* 0 (expt 0 0)) (* 0 (expt 0 1)) (* 0 (expt 0 2)))) 


(define p_0 (createPolynomial '())) 
(test (p_0 4) => 0) 



;question 3 B i
#| 
    ---------- fill ----------
|#

;question 3 B ii

(define-type PLANG 
    [Poly (Listof AE) (Listof AE)]) ;; Poly is made of list of args and list of points
 
(define-type AE 
[Num  Number] 
[Add  AE AE] 
[Sub  AE AE] 
[Mul  AE AE] 
[Div  AE AE]) 


 (: parse-sexpr : Sexpr -> AE) 
;; to convert s-expressions into AEs 
(define (parse-sexpr sexpr) 
    (match sexpr 
        [(number: n) (Num n)] 
        [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))] 
        [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))] 
        [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))] 
        [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))] 
        [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]
    )
) 
   
(: parse : String -> PLANG) 
;; parses a string containing a PLANG expression to a PLANG AST   
(define (parse str) 
    (let ([code (string->sexpr str)]) 
        (match code ;; break the sexpr to 2 halves, poly args and points
            [(list (list 'poly sexprArgs ...) (list sexprPoints ...)) 
                (cond   [(null? sexprArgs) (error 'parse "at least one coefficient is required in ~s" code)] ;; check for empty args
                        [(null? sexprPoints) (error 'parse "at least one point is required in ~s" code)] ;; check for empty points
                        [else (Poly (map parse-sexpr sexprArgs) (map parse-sexpr sexprPoints))] ;; parse every sexpr in sexprArgs and sexprPoints
                        ;; and map the results into Poly 
                )
            ]
            [else (error 'parse "bad syntax in ~s" code)]
        )
    )
) 


(test (parse "{{poly 1 2 3} {1 2 3}}") => 
    (Poly (list (Num 1) (Num 2) (Num 3)) 
          (list (Num 1) (Num 2) (Num 3)))) 

(test (parse "{{poly -1 4} {0 -3 5 7}}") => 
    (Poly (list (Num -1) (Num 4)) 
          (list (Num 0) (Num -3) (Num 5) (Num 7)))) 



(test (parse "{{poly 4/5 } {1/2 2/3 3}}") => (Poly (list (Num 4/5) ) (list (Num 1/2) (Num 2/3) (Num 3)))) 
(test (parse "{{poly 2 3} {4}}") => (Poly (list (Num 2) (Num 3)) (list (Num 4)))) 
(test (parse "{{poly 1 1 0} {-1 3 3}}") => (Poly (list (Num 1) (Num 1) (Num 0)) (list (Num -1) (Num 3) (Num 3)))) 
(test (parse "{{poly {/ 4 2}  {- 4 1}} {{- 8 4}}}") => (Poly (list (Div (Num 4) (Num 2)) (Sub (Num 4) (Num 1))) (list (Sub (Num 8) (Num 4))))) 
(test (parse "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => (Poly (list (Add (Num 0) (Num 1)) (Num 1) (Mul (Num 0) (Num 9))) (list (Sub (Num 4) (Num 5)) (Num 3) (Div (Num 27) (Num 9))))) 



(test (parse "{{poly } {1 2 3} }") =error> 
    "parse: at least one coefficient is required in ((poly) (1 2 3))") 
(test (parse "{{poly 1 2} {} }") =error> 
    "parse: at least one point is required in ((poly 1 2) ())") 
(test (parse "{{pol 1 2 3} {1 2 3}}") =error> "parse: bad syntax in ((pol 1 2 3) (1 2 3))")
(test (parse "{{poly 1 2 3} }") =error> "parse: bad syntax in ((poly 1 2 3))")
(test (parse "{{poly 4/5} {1/2 2/3 3} {poly 1 2 4} {1 2}}") =error> "parse: bad syntax in ((poly 4/5) (1/2 2/3 3) (poly 1 2 4) (1 2))")


;question 3 B iii
;; evaluates AE expressions to numbers 
(: eval : AE -> Number) 
(define (eval expr) 
(cases expr 
    [(Num n)  n] 
    [(Add l r) (+ (eval l) (eval r))] 
    [(Sub l r) (- (eval l) (eval r))] 
    [(Mul l r) (* (eval l) (eval r))] 
    [(Div l r) (/ (eval l) (eval r))]))  


(: eval-poly : PLANG -> (Listof Number)) 
(define (eval-poly p-expr) 
    (cases p-expr
        [(Poly args points) 
            (map ; map the inputed points using the polynomial function
                (createPolynomial (map eval args)) ; create the list of args from mapping the AE list to numbers
                (map eval points) ; get the desired points from mapping the list of AE to thier values
            )
        ]
    )
) 

(: run : String -> (Listof Number)) 
;; evaluate a FLANG program contained in a string 
(define (run str) 
    (eval-poly (parse str))
) 

#|
    ---------- add more(?) ----------
|#
(test (run "{{poly 1 2 3} {1 2 3}}")  => '(6 17 34)) 
(test (run "{{poly 4 2 7} {1 4 9}}")  => '(13 124 589)) 
(test (run "{{poly 1 2 3} {1 2 3}}")   => '(6 17 34)) 
(test (run "{{poly 4/5 } {1/2 2/3 3}}")  => '(4/5 4/5 4/5)) 
(test (run "{{poly 2 3} {4}}")  => '(14)) 
(test (run "{{poly 1 1 0} {-1 3 3}}")  => '(0 4 4))
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => '(14)) 
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4)) 












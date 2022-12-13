#lang pl










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


(define p536 (createPolynomial '(5 3 6))) 
(test (p536 11) => 
    (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2)))) 


(define p_0 (createPolynomial '())) 
(test (p_0 4) => 0) 
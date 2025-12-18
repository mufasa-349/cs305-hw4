(define example-program
  '(
    (
      (f (x) (= (* x 3)))
      (g (t u) (= (+ t v)))
      (f (y) (= (* y y)))
      (k (a b) (= (+ (* a b) c)))
    )
    (
      (calculate (+ (f x) (h y)))
      (calculate (+ (f (o y)) (((x (b l)) y)) ((c b)))))
      (calculate (+ (g a)))
      (calculate (+ ((g a))))
    )
  )

(find-redefined-functions example-program)        ; => (f)
(find-undefined-parameters example-program)       ; => (v c)
(find-arity-contradictions example-program)       ; => (g g)
(find-missing-function-names example-program)     ; => ( (((x (b l)) y)) ((x (b l)) y) ((c b)) ((g a)) )
(find-undefined-functions example-program)        ; => (h o x b c)
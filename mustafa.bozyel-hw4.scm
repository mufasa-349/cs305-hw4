;; CS305 HW4 - Semantic Analyzer for Calculator Scientifique (CS)
;; Template implementation (MIT Scheme / R5RS-ish)
;;
;; Expected CS program format:
;; '(
;;   ( (f (x) (= (+ x 2))) (g (t u) (= (+ t u))) ... )   ; definitions
;;   ( (calculate (+ (f x) (g a b))) (calculate (f x)) ) ; calculations
;; )
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small utilities

(define (symbol=? a b) (eq? a b))

(define (member-sym? x xs)
  (cond ((null? xs) #f)
        ((eq? x (car xs)) #t)
        (else (member-sym? x (cdr xs)))))

(define (append1 xs x) (append xs (list x)))

(define (filter pred xs)
  (cond ((null? xs) '())
        ((pred (car xs)) (cons (car xs) (filter pred (cdr xs))))
        (else (filter pred (cdr xs)))))

(define (foldl f acc xs)
  (if (null? xs) acc (foldl f (f acc (car xs)) (cdr xs))))

(define operators '(+ - * / ^))
(define (operator? s) (and (symbol? s) (member-sym? s operators)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS program accessors

(define (cs-definitions cs) (car cs))
(define (cs-calculations cs) (cadr cs))

;; A definition is expected as: (fname (p1 p2 ...) (= <expr>))
(define (def-name d) (car d))
(define (def-params d) (cadr d))

;; body is something like: (= (+ x 2)) or (= <expr>)
(define (def-body d) (caddr d))

(define (rhs-expr-from-body body)
  ;; body: (= <expr>)  ==> return <expr>
  (if (and (pair? body) (symbol? (car body)) (eq? (car body) '=))
      (cadr body)
      ;; fallback: if format differs, try to use as-is
      body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression traversals

;; Preorder traversal collecting *missing function name* expressions:
;; any list whose first element is itself a list.
(define (collect-missing-fn expr)
  (cond
    ((null? expr) '())
    ((pair? expr)
     (let ((here (if (pair? (car expr)) (list expr) '()))
           (rest1 (if (pair? (car expr)) (collect-missing-fn (car expr)) '()))
           (rest2 (foldl (lambda (acc arg) (append acc (collect-missing-fn arg))) '() (cdr expr))))
       (append here rest1 rest2)))
    (else '())))

;; Collect symbols that are used as variables in an expression, for Rule 2.
;; - Ignore operator/function position (car of a list).
;; - Traverse the arguments (cdr of a list).
;; - Numbers are ignored automatically (number?).
(define (collect-vars expr)
  (cond
    ((null? expr) '())
    ((number? expr) '())
    ((symbol? expr) (list expr))
    ((pair? expr)
     ;; if it's a list like (op arg1 arg2 ...)
     ;; ignore the first element (op / function name), traverse args recursively
     (foldl (lambda (acc arg) (append acc (collect-vars arg))) '() (cdr expr)))
    (else '())))

;; Collect function names used in calculations (Rule 5 + Rule 3):
;; any list whose car is a symbol and not an operator and not 'calculate.
;; Also collect symbols that are the car of missing function names.
(define (collect-called-functions expr)
  (cond
    ((null? expr) '())
    ((pair? expr)
     (let* ((head (car expr))
            (is-missing-fn (pair? head))
            (here (if (and (not is-missing-fn)
                           (symbol? head)
                           (not (operator? head))
                           (not (eq? head 'calculate)))
                      (list head)
                      '()))
            (missing-fn-symbols (if (and is-missing-fn
                                         (pair? head)
                                         (symbol? (car head))
                                         (not (operator? (car head)))
                                         (not (eq? (car head) 'calculate)))
                                    (list (car head))
                                    '()))
            (rest (if is-missing-fn
                      ;; If head is a pair, it's a missing function name, traverse it and cdr
                      (append (collect-called-functions head)
                              (foldl (lambda (acc arg) (append acc (collect-called-functions arg))) '() (cdr expr)))
                      ;; Otherwise, traverse arguments normally
                      (foldl (lambda (acc arg) (append acc (collect-called-functions arg))) '() (cdr expr)))))
       (append here missing-fn-symbols rest)))
    (else '())))

;; Collect arity mismatches in calculations: return function names (possibly repeated)
(define (collect-arity-mismatches expr arity-alist)
  (define (arity-of fname)
    (let ((p (assoc fname arity-alist)))
      (if p (cdr p) #f)))

  (cond
    ((null? expr) '())
    ((pair? expr)
     (let* ((head (car expr))
            (tail (cdr expr))
            (expected (and (symbol? head) (arity-of head)))
            (this-mismatch
             (if (and expected
                      (not (operator? head))
                      (not (eq? head 'calculate)))
                 (let ((given (length tail)))
                   (if (= given expected) '() (list head)))
                 '()))
            (rest (foldl (lambda (acc arg) (append acc (collect-arity-mismatches arg arity-alist))) '() tail)))
       (append this-mismatch rest)))
    (else '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build arity table from definitions

(define (defs->arity-alist defs)
  (map (lambda (d) (cons (def-name d) (length (def-params d)))) defs))

(define (defs->names defs) (map def-name defs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule 1: Redefined Functions

(define find-redefined-functions
  (lambda (cs)
    (let ((defs (cs-definitions cs)))
      (let loop ((ds defs) (seen '()) (out '()))
        (if (null? ds)
            out
            (let ((name (def-name (car ds))))
              (if (member-sym? name seen)
                  (loop (cdr ds) seen (append1 out name))
                  (loop (cdr ds) (append1 seen name) out))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule 2: Undefined Parameters

(define find-undefined-parameters
  (lambda (cs)
    (let ((defs (cs-definitions cs)))
      (define (undefs-in-def d)
        (let* ((params (def-params d))
               (expr (rhs-expr-from-body (def-body d)))
               (vars (collect-vars expr)))
          (filter (lambda (v) (and (symbol? v) (not (member-sym? v params))))
                  vars)))
      (foldl (lambda (acc d) (append acc (undefs-in-def d))) '() defs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule 3: Arity Contradiction

(define find-arity-contradictions
  (lambda (cs)
    (let* ((defs (cs-definitions cs))
           (calcs (cs-calculations cs))
           (arity (defs->arity-alist defs)))
      (define (calc-expr c)
        ;; (calculate <expr>)
        (if (and (pair? c) (eq? (car c) 'calculate))
            (cadr c)
            c))
      (foldl (lambda (acc c)
               (append acc (collect-arity-mismatches (calc-expr c) arity)))
             '()
             calcs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule 4: Missing Function Name

(define find-missing-function-names
  (lambda (cs)
    (let ((calcs (cs-calculations cs)))
      (define (calc-expr c)
        (if (and (pair? c) (eq? (car c) 'calculate))
            (cadr c)
            c))
      (foldl (lambda (acc c) (append acc (collect-missing-fn (calc-expr c))))
             '()
             calcs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule 5: Undefined Functions

(define find-undefined-functions
  (lambda (cs)
    (let* ((defs (cs-definitions cs))
           (calcs (cs-calculations cs))
           (defined (defs->names defs)))
      (define (calc-expr c)
        (if (and (pair? c) (eq? (car c) 'calculate))
            (cadr c)
            c))
      (define (undefs-in-calc c)
        (let ((called (collect-called-functions (calc-expr c))))
          (filter (lambda (fname) (not (member-sym? fname defined))) called)))
      (foldl (lambda (acc c) (append acc (undefs-in-calc c))) '() calcs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optional: quick self-test (won't be graded; safe to keep or remove)

;; (define example-program
;;   '(
;;     (
;;       (f (x) (= (* x 3)))
;;       (g (t u) (= (+ t v)))
;;       (f (y) (= (* y y)))
;;       (k (a b) (= (+ (* a b) c)))
;;     )
;;     (
;;       (calculate (+ (f x) (h y)))
;;       (calculate (+ (f (o y)) (((x (b l)) y)) ((c b)))))
;;       (calculate (+ (g a)))
;;       (calculate (+ ((g a))))
;;     )
;;   ))
;;
;; (find-redefined-functions example-program)        ; => (f)
;; (find-undefined-parameters example-program)       ; => (v c)
;; (find-arity-contradictions example-program)       ; => (g g)
;; (find-missing-function-names example-program)     ; => ( (((x (b l)) y)) ((x (b l)) y) ((c b)) ((g a)) )
;; (find-undefined-functions example-program)        ; => (h o x b c)

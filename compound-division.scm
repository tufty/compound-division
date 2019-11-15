(import (srfi :1) (srfi :26))

;; Standard Brown and Sharpe indexing plates 
(define *plates* '((15 16 17 18 19 20)
                   (21 23 27 29 31 33)
                   (37 39 41 43 47 49)))

;; Brown and Sharpe dividing head ratio
(define *ratio* 40)

(define *tolerated-error-percentage* 0.002)

(define factors
  (lambda (n)
    (filter (lambda (x) (zero? (mod n x))) (iota n 1))))


;; For ratio R and division D, we need to advance the handle by R/D turns or some non-factor-of-D
;; multiple thereof
;; This gives us a set of fractions R/D ... x.R/D to solve for.
(define possible-targets
  (lambda (division)
    (sort <= (map (lambda (x) (* x (/ *ratio* division))) (cons 1 (lset-difference = (iota division 1) (factors division)))))))

;; For a given division circle c, ratio R and target value T, there are two possible solutions.
;; Either there is an exact number of divisions providing T, or two divisions spanning T.
(define divisions-from-circle-for
  (lambda (circle target)
    (let ((max-d (* target circle)))
      (if (integer? max-d)
          max-d
          (list (floor max-d) (ceiling max-d))))))

;; Now that we know this, we can, for any given pair of circles, calculate the possible pairs of
;; divisions to solve for a value, thusly :
(define divisions-from-circles-for
  (lambda (circle-1 circle-2 target)
    (let ((c1s (divisions-from-circle-for circle-1 target)))
      (filter-map
       (lambda (x) (if (< (car x) *tolerated-error-percentage*) x #f)) ; Filter on percentage error
       (if (number? c1s)                     ; Exact result from circle 1
           (if (zero? (mod c1s circle-1))     ; is an integer number of rotations
               `((0 any ,(div c1s circle-1) any 0))
               `((0 ,circle-1 ,c1s any 0))) 
           (append-map                       ; Otherwise produce a list of potentials with error percentage
            (lambda (x)
              (let ((d (divisions-from-circle-for circle-2 (- target (/ x circle-1))))
                    (k (lambda (x y)
                         (let* ((bigy (div y circle-2))
                                (x (if (zero? bigy) x (+ x (* bigy circle-1))))
                                (y (if (zero? bigy) y (- y (* bigy circle-2)))))
                           (list (abs (/ (* 100 (- target (+ (/ x circle-1) (/ y circle-2)))) target)) circle-1 x circle-2 y)))))
                (cond
                 ((and (zero? x) (number? d))
                  (if (zero? (mod d circle-2))
                      `((0 any ,(div d circle-2) any 0))
                      `((0 ,circle-2 ,d any 0))))
                 ((number? d) (list (k x d)))
                 (else (map (cut k x <>) d)))))
            (iota (cadr c1s))))))))

;; For a plate, we need to do all the possible combinations
(define divisions-from-plate-for
  (lambda (plate target)
    (let loop ((c1 (car plate)) (rest (cdr plate)) (result '()))
      (if (null? rest) result
          (loop (car rest) (cdr rest)
                (append (append-map (cut divisions-from-circles-for c1 <> target) rest) result))))))

;; And we can run the entire set of plates thuswise
(define all-divisions-for
  (lambda (target)
    (append-map (cut divisions-from-plate-for <> target) *plates*)))

;; If we do this for all targets using append-map, we can get massive numbers of results for
;; the bigger divisions, even with a very low error tolerance.
;; So go through them one at a time, if we get an exact result or at least 3, drop out
(define divisions-all-targets-for
  (lambda (division)
    (let ((result-sort (lambda (a b) (< (car a) (car b)))))
      (delete-duplicates
       (sort result-sort
             (let loop ((targets (possible-targets division)) (results '()))
               (let ((f0 (lambda (x) (zero? (car x))))
                     (f1 (lambda (x) (eqv? (cadddr x) 'any))))
                 (cond
                  ((null? targets) results)
                  ((any f1 results) (filter f1 results))  ;; Uses one ring, must be exact
                  ((any f0 results) (filter f0 results))  ;; Zero error, exact
                  ((>= (length results) 3) results)
                  (else (loop (cdr targets) (append (all-divisions-for (car targets)) results)))))))))))
  
;; And thus we can get all possible approximate and exact divisions for a set of
;; divisions
(define all-divisions-for-set
  (lambda (divisions)
    (map (lambda (x)
           (list x (divisions-all-targets-for x)))
         divisions)))

(define (join l s) 
  (cond 
   ((null? l) l)
   ((null? (cdr l)) l)
   (else (cons* (car l) s (join (cdr l) s)))))

(define (string-join l s)
  (reduce-right string-append "" (join l s)))

(define fractionate
  (lambda (x y)
    (let-values (((turns holes) (div-and-mod x y)))
      (if (zero? turns)
          (format "\\nicefrac{~a}{~a}" holes y)
          (format "~a\\nicefrac{~a}{~a}" turns holes y)))))
 
(define caddddr (lambda (x) (cadr (cdddr x))))

(define latex-format-entry
  (lambda (division x)
    (let* ((error (car x))
           (intturns? (eqv? (cadr x) 'any))
           (c1 (if intturns? 1 (cadr x)))
           (h1 (caddr x))
           (c2? (not (eqv? (cadddr x) 'any)))
           (c2 (if c2? (cadddr x) 1))
           (h2 (if c2? (caddddr x) 0))
           (turns (ceiling (* division (+ (/ h1 (* *ratio* c1)) (/ h2 (* *ratio* c2)))))))
      (cond
       (intturns? (format " & ~a & ~a & $ Exact $ \\\\\n" h1 turns))
       (c2? (format " & $ ~a + ~a $ & ~a & $ ~a $ \\\\\n" (fractionate h1 c1) (fractionate h2 c2) turns (if (zero? error) "Exact" (format "~9,,0f" error))))
       (else (format " & $ ~a $ & ~a & $ ~a $ \\\\\n" (fractionate h1 c1) turns (if (zero? error) "Exact" (format "~9,,0f" error))))))))

    
(define latex-header "
\\documentclass[a4paper,landscape,10pt]{article}
\\usepackage[a4paper,margin=1cm]{geometry}
\\usepackage{multirow}
\\usepackage{multicol}
\\usepackage{nicefrac}
\\usepackage{tabularx}
\\usepackage{printlen}
\\begin{document}
\\begin{multicols}{3}
" )

(define latex-footer "
\\end{multicols}
\\end{document}
")

(define latex-page-header "
\\thispagestyle{empty}
")

(define latex-page-footer "
\\pagebreak
")

(define latex-column-header "
\\begin{tabularx}{0.9\\columnwidth}{|c|c|c|X|}
\\hline
Division & Action & Turns & Error \\% \\\\
\\hline
")

(define latex-column-footer "
\\end{tabularx}
\\columnbreak
")

(define latex-accumulate
  (let ((column-height 520)
        (line-height 12)
        (current-column 1)
        (height-left 520))
    (lambda (content lines)
      (let ((height (* lines line-height))) 
        (if (<= (- height-left height) 0)
            (begin
              (display latex-column-footer)
              (if (= current-column 3)
                  (begin
                    (set! current-column 1)
                    (display latex-page-footer)
                    (display latex-page-header)))
              (display latex-column-header)
              (set! height-left column-height)))
        
        (set! height-left (- height-left height))
        (display content)))))
              

(define latex-accumulate-result
  (lambda (division-set)
    (let* ((division (car division-set))
           (results (cadr division-set))
           (lines (length results))) 
      (if (zero? lines)
          (latex-accumulate (format "~a & \\multicolumn{3}{c}{$ No Solution $} \\\\\n\\hline\n" division) 1)
          (latex-accumulate (format "\\multirow{~a}{*}{~a} ~a \n\\hline\n"
                                    lines division
                                    (apply string-append (map (cut latex-format-entry division <>) results)))
                            lines)))))


(define produce-latex-document
  (lambda (result-set)
    (display latex-header)
    (display latex-page-header)
    (display latex-column-header)

    (map latex-accumulate-result result-set)
    
    (display latex-column-footer)
    (display latex-page-footer)
    (display latex-footer)))


(define produce
  (lambda ()
    (produce-latex-document (all-divisions-for-set (iota 400 1))))) 


(with-output-to-file "result.tex" produce 'replace)

(exit)


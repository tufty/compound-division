(import (srfi :1) (srfi :26))

(define factors
  (lambda (n)
    (filter (lambda (x) (zero? (mod n x))) (iota n 1))))


;; For ratio R and division D, we need to advance the handle by R/D turns or some non-factor-of-D
;; multiple thereof
;; This gives us a set of fractional divisions R/D ... x.R/D to solve for.
(define possible-targets
  (lambda (division)
    (sort <= (map (lambda (x) (* x (/ *ratio* division))) (cons 1 (lset-difference = (iota division 1) (factors division)))))))

;; For a given division circle c, ratio R and fractional division T, there are two possible solutions.
;; Either there is an exact number of steps providing T, or two divisions spanning T.
(define steps-from-circle-for
  (lambda (circle target)
    (let ((max-d (* target circle)))
      (if (integer? max-d)
          (list max-d)
          (list (floor max-d) (ceiling max-d))))))

;; For a proposed solution of the form (error circle-1 holes-1 circle2 holes-2), and a number of steps,
;; returns a list of the actual angles of each division made.
(define angles-for
  (lambda (solution steps)
    (unfold (lambda (x) (= x steps))
            (lambda (x) (let ((a1 (/ (caddr solution) (cadr solution)))
                              (a2 (/ (caddddr solution) (cadddr solution))))
                          (* 360 (/ x *ratio*) (+ a1 a2))))
            (lambda (x) (+ x 1))
            0)))

;; Determine if a proposed solution (error circle-1 holes-1 circle-2 holes-2) for a given division is
;; acceptable
;; It must both have an error percentage under the threshold, and not generate repeated divisions.
(define acceptable-solution-for
  (lambda (solution division)
    (if (and (< (car solution) *tolerated-error-percentage*)
             (let ((angles (map (cut mod <> 360) (angles-for solution division))))
               (= (length angles) (length (delete-duplicates angles)))))
        solution #f)))

(define error-percentage-for
  (lambda (c1 h1 c2 h2 target)
    (abs (/ (* 100 (- target (+ (/ h1 c1) (/ h2 c2)))) target))))

;; Wrap up a proposed solution with its error percentage with respect to a particular target
;; fractional division
(define format-solution
  (lambda (c1 h1 c2 h2 target)
    (if (> c1 c2) 
        (format-solution c2 h2 c1 h1 target)
        ;; For uniquification, we move all the full turns to circle 1
        (let* ((turns2 (div h2 c2))  ;; Is h2 more than c2 holes?
               (h1 (if (zero? turns2) h1 (+ h1 (* turns2 c1))))
               (h2 (if (zero? turns2) h2 (- h2 (* turns2 c2)))))
          ;; And now, if we have simply an integer number of turns on c1,
          ;; reduce c1 to "1 hole circle" with that number of holes advance
          (let* ((intc1 (zero? (mod h1 c1)))
                 (h1 (if intc1 (div h1 c1) h1))
                 (c1 (if intc1 1 c1))
                 (intc2 (zero? (mod h2 c2)))
                 (h2 (if intc2 (div h2 c2) h2))
                 (c2 (if intc2 1 c2)))
            (if intc1
                (if (zero? h1)
                    (list (error-percentage-for c2 h2 1 0 target) c2 h2 1 0)
                    (let ((h2 (+ h2 (* c2 h1))))
                      (list (error-percentage-for c2 h2 1 0 target) c2 h2 1 0)))
                (list (error-percentage-for c1 h1 c2 h2 target) c1 h1 c2 h2)))))))

(define format-solutions
  (lambda (c1 h1 c2 h2s target)
    (map (cut format-solution c1 h1 c2 <> target) h2s)))

;; For a pair of circles, a target fractional division, and a number of divisions, we can create
;; a list of potential solutions, viz:
;; circle 1 alone (1 or two solutions)
;; circle 1 + circle 2 (in the case of 2 solutions for circle 1)
;; circle 2 alone
;; circle 2 + circle 1 (in the case of 2 solutions for circle 2)
;; Every possible combination of 1 <= x < circle 1 alone + y circle 2
;; Every possible combination of 1 <= x < circle 2 alone + y circle 1
(define potential-solutions-from-circles-for
  (lambda (circle-1 circle-2 target)
    (let* ((c1s (steps-from-circle-for circle-1 target))
           (c2s (steps-from-circle-for circle-2 target))
           (c12s (steps-from-circle-for circle-2 (- target (/ (car c1s) circle-1))))
           (c21s (steps-from-circle-for circle-1 (- target (/ (car c2s) circle-2))))
           ;; Now the easy bits of solution
           (results (map (cut format-solution circle-1 <> 1 0 target) c1s))
           (results (append (map (cut format-solution circle-2 <> 1 0 target) c2s) results))
           (results (append (map (cut format-solution circle-1 (car c1s) circle-2 <> target) c12s) results))
           (results (append (map (cut format-solution circle-1 <> circle-2 (car c2s) target) c21s) results))
           ;; Work out the resuts we don't have for circle 1
           (c1n (lset-difference (iota (- (car c1s) 1) 1) (map caddr results)))
           (c1t (map (lambda (x) (- target (/ x circle-1))) c1n))
           (c1ns (map (cut steps-from-circle-for circle-2 <>) c1t))
           ;; and get those results
           (results (append (append-map (cut format-solutions circle-1 <> circle-2 <> target) c1n c1ns) results))

           ;; Do the same for cirle 2
           (c2n (lset-difference (iota (- (car c2s) 1) 1) (map caddddr results)))
           (c2t (map (lambda (x) (- target (/ x circle-2))) c2n))
           (c2ns (map (cut steps-from-circle-for circle-1 <>) c2t)))
      (append
       results
       (append-map (cut format-solutions circle-2 <> circle-1 <> target) c2n c2ns)
       ))))

;; And thus we can find all potential solutions from a single plate
(define potential-solutions-from-plate-for
  (lambda (plate target)
    (let loop ((c1 (car plate)) (rest (cdr plate)) (result '()))
      (if (null? rest) result
          (loop (car rest) (cdr rest)
                (append (append-map (cut potential-solutions-from-circles-for c1 <> target) rest) result))))))
    
;; And all potential solutions from all plates is gotten by iterating through all potential plates
(define potential-solutions-from-plate-set-for
  (lambda (target)
    (append-map (cut potential-solutions-from-plate-for <> target) *plates*)))

(define deduplicate
  (lambda (a b)
    (or (equal? a b)
        (and (= (cadr a) (cadr b)) (= (cadddr a) (cadddr b))))))

;; We can now filter those results to get the number of acceptable solutions
(define acceptable-solutions-for
  (lambda (target division)
    (delete-duplicates! (filter-map (cut acceptable-solution-for <> division) (potential-solutions-from-plate-set-for target)) deduplicate)))


(define sort-by-ring
  (lambda (a b)
    (or (< (cadr a) (cadr b)) (and (= (cadr a) (cadr b)) (< (caddr a) (caddr b))))))

(define sort-by-error
  (lambda (a b)
    (< (car a ) (car b))))

(define head
  (lambda (l c)
    (let loop ((l l) (c c) (result '()))
      (cond
       ((or (null? l) (zero? c)) (reverse result))
       (else (loop (cdr l) (- c 1) (cons (car l) result)))))))
    
;; If we do this for all targets using append-map, we can get massive numbers of results for
;; the bigger divisions, even with a very low error tolerance.
;; So go through them one at a time, if we get an exact result or at least 3, drop out
(define acceptable-solutions-all-targets-for
  (lambda (division)
    (head 
     (sort sort-by-error
           (let loop ((targets (possible-targets division)) (results '()))
             (let ((f0 (lambda (x) (zero? (car x))))
                   (f1 (lambda (x) (and (zero? (car x)) (= 1 (cadddr x))))))
               (cond
                ((any f1 results) (filter f1 results))     ;; Zero error, exact
                ((any f0 results) (filter f0 results))     ;; Zero error, exact
                ((>= (length results) 3) (take results 3)) ;; Return top 3 approximations
                ((null? targets) results)                  ;; No targets left, return what results we have
                (else (loop (cdr targets) (delete-duplicates! (append results (acceptable-solutions-for (car targets) division)) deduplicate))))))) 3)))

;; And thus we can get all possible approximate and exact solutions for a set of divisions
(define acceptable-solutions-for-set
  (lambda (divisions)
    (map (lambda (x)
           (list x (acceptable-solutions-all-targets-for x)))
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
          (format "~a + \\nicefrac{~a}{~a}" turns holes y)))))
 
(define caddddr (lambda (x) (cadr (cdddr x))))

(define pi (/ 355 133))  ;; Shitty approximation to pi but it's rational

(define latex-format-entry
  (lambda (division x)
    (let* ((error (car x))
           (intturns? (= (cadr x) 1))
           (c1 (if intturns? 1 (cadr x)))
           (h1 (caddr x))
           (c2? (not (= (cadddr x) 1)))
           (c2 (if c2? (cadddr x) 1))
           (h2 (if c2? (caddddr x) 0))
           (turns (round (* division (+ (/ h1 (* *ratio* c1)) (/ h2 (* *ratio* c2))))))
           (degrees (* (+ (/ (* h1 360) (* c1 *ratio*)) (/ (* h2 360) (* c2 *ratio*))) division))
           (divisions (if (zero? error) division (/ turns (+ (/ h1 (* c1 *ratio*)) (/ h2 (* c2 *ratio*))))))
           (error-diameter (if (zero? error) "\\infty"
                               (exact (floor (abs (/ 0.01 (* pi (- 1 (/ divisions division))))))))))
      (cond
       (intturns? (format " & $ ~a $ & $ ~a $ & $ Exact $ & $ ~a $ \\\\\n" h1 turns error-diameter))
       (c2? (format " & $ ~a + ~a $ & $ ~a $ & $ ~a $ & $ ~a $ \\\\\n" (fractionate h1 c1) (fractionate h2 c2) turns (if (zero? error) "Exact" (format "~8,,0f" divisions)) error-diameter))
       (else (format " & $ ~a $ & $ ~a $ & $ ~a $ & $ ~a $ \\\\\n" (fractionate h1 c1) turns (if (zero? error) "Exact" (format "~8,,0f" divisions)) error-diameter))))))

    
(define latex-header (format "
\\documentclass[a4paper,landscape,9pt]{extarticle}
\\usepackage[a4paper,top=2cm,bottom=2cm,left=1cm,right=1cm]{geometry}
\\usepackage[utf8]{inputenc}
\\usepackage{textcomp}
\\usepackage{wasysym}
\\usepackage{multirow}
\\usepackage{multicol}
\\usepackage{nicefrac}
\\usepackage{tabularx}
\\usepackage{makecell}
\\usepackage{printlen}
\\usepackage{fancyhdr}

\\pagestyle{fancy}
\\fancyhead{}
\\fancyfoot{}
\\fancyhead[C]{Division Tables pour ~a ~a:1}
\\fancyfoot[R]{\\small{Plateaux Diviseurs : ~a}}
\\fancyfoot[L]{\\tiny{La colonne $ \\diameter $ représente le diamètre en mm de la pièce au delà duquel le dernier trou de la division sera déplacé de plus de 0,01 mm}}

\\begin{document}
\\begin{multicols}{3}
\\small

" *divider-name* *ratio* *plates* ))

(define latex-footer "
\\end{multicols}
\\end{document}
")

(define latex-page-header "" )

(define latex-page-footer "" )

(define latex-column-header "
\\begin{tabularx}{0.98\\columnwidth}{|c|c|c|c|>{\\centering\\arraybackslash}X|}
\\hline
\\makecell[cc]{Division\\\\Voulu} & \\makecell[cc]{Manivelle Tours\\\\et Divisions} & \\makecell[cc]{Tours de\\\\la Piece} & \\makecell[cc]{Divisions\\\\Reels} & \\makecell[cc]{\\diameter} \\\\
\\hline
")

(define latex-column-footer "\\end{tabularx}\n")

(define latex-accumulate
  (let ((column-height 490)
        (line-height 9.8)
        (current-column 1)
        (height-left 490))
    (lambda (content lines)
      (let ((height (* lines line-height))) 
        (if (<= (- height-left height) 0)
            (begin
              (display latex-column-footer)
              (if (= current-column 3)
                  (begin
                    (set! current-column 0)
                    (display latex-page-footer)
                    (display "\\pagebreak\n")
                    (display latex-page-header))
                  (display "\\columnbreak\n"))
              (display latex-column-header)
              (set! current-column (+ current-column 1))
              (set! height-left column-height)))
        
        (set! height-left (- height-left height))
        (display content)))))
              

(define latex-accumulate-result
  (lambda (division-set)
    (let* ((division (car division-set))
           (results (cadr division-set))
           (lines (length results))) 
      (cond
       ((zero? lines) (latex-accumulate (format "$ ~a $ & \\multicolumn{4}{c|}{ Pas de solution } \\\\\n\\hline\n" division) 1))
       ((= 1 lines) (latex-accumulate (format " $ ~a $ ~a \n\\hline\n" division (latex-format-entry division (car results))) lines))
        (else (latex-accumulate (format "\\multirow{~a}{*}{$ ~a $} ~a \n\\hline\n"
                                        lines division
                                        (apply string-append (map (cut latex-format-entry division <>) results)))
                                lines))))))

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
    (produce-latex-document (acceptable-solutions-for-set (iota (- *last-division* *first-division*) *first-division*)))))

(let ((tex-file-name (format "output/~a.tex" *divider-short-name*)))
  (with-output-to-file tex-file-name produce 'replace)
  (system (format "pdflatex -output-directory output ~a" tex-file-name)))

(exit)

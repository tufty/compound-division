(import (srfi :1) (srfi :26) (srfi :132))
  
;; Factors of a number
(define factors
  (lambda (n)
    (filter (lambda (x) (zero? (mod n x))) (iota n 1))))

(define min-max
  (lambda (x y)
    (values (min x y) (max x y))))

;; For ratio R and division D, we need to advance the handle by R/D turns or some non-factor-of-D
;; multiple thereof
;; This gives us a set of fractional divisions R/D ... x.R/D to solve for.
(define possible-targets
  (lambda (division)
    (list-sort <= (map (lambda (x) (* x (/ *ratio* division))) (cons 1 (lset-difference = (iota division 1) (factors division)))))))


;; Is this an exact result?
(define exact-result?
  (lambda (x)
    (and (not (null? x)) (zero? (car x)))))

;; Previously I'd been going too far down the rabbit hole in terms of resolving this problem.
;; We can consider the number of divisions available in a single rotation of the piece
;; as being m * n, where m and n are the number of holes on a pair of circles on the dividing plate
;; We can thus develop either the exact fractional division, or a pair of divisions spanning the
;; desired fractional division, in terms of the number of "atomic" divisions available
(define steps-from-circles-for
  (lambda (c1 c2 target)
    (let ((max-d (* target c1 c2)))
      (if (integer? max-d)
          (list 0 (* max-d *ratio*) c1 c2)                ; This is an exact result already
          (let* ((e (- max-d (round max-d)))
                 (e% (abs (* 100 (/ e max-d)))))
            (if (<= e% *tolerated-error-percentage*)
                (if (<= e 0.5)
                    (list target (* (floor max-d) *ratio*) c1 c2)   ; This pair are inexact and will
                    (list target (* (ceiling max-d) *ratio*) c1 c2)); require further attention later
                #f))))))

(define sorter
  (lambda (pred fn)
    (lambda (x y) (pred (fn x) (fn y)))))

;; generate the above for all possible fractional divisions for a given division
;; If we generate any exact results, return the one with the lowest number of steps
;; and discard the rest.
(define all-solutions-from-circles-for
  (lambda (c1 c2 targets)
    (let-values (((er ir) (partition exact-result? (lset-union equal? (filter-map (cut steps-from-circles-for c1 c2 <>) targets)))))
      (if (null? er)
          ir
          (head (list-sort! (sorter < cadr) er) 1)))))

;; For a given plate, we must permute the circles and generate all solutions.
;; Again, throw away inexact solutions if we have exact ones
(define all-solutions-from-plate-for
  (lambda (plate targets)
    (let loop ((results '()) (c1 (car plate)) (rest (cdr plate)))
      (if (null? rest)
          (let-values (((er ir) (partition exact-result? results)))
            (if (null? er)
                ir
                er))
          (loop (append (append-map (cut all-solutions-from-circles-for c1 <> targets) rest) results)
                (car rest)
                (cdr rest))))))

;; And finally, we can generate the potential solutions for a given division
;; with the entire plate set.
(define all-solutions-for
  (lambda (division)
    (let* ((targets (possible-targets division))
           (results (append-map (cut all-solutions-from-plate-for <> targets) *plates*)))
      (let-values (((er ir) (partition exact-result? results)))
        (if (null? er)
            (head (list-sort (sorter < cadr) ir) 20)
            er)))))

(define r=
  (lambda (x y)
    (and (= (cadr x) (cadr y))
         (= (caddr x) (caddr y))
         (= (caddddr x) (caddddr y)))))

(define r<
  (lambda (x y)
    (or (< (cadr x) (cadr y))
        (< (caddr x) (caddr y))
        (< (caddddr x) (caddddr y)))))

(define error-for
  (lambda (target turns c1 h1 c2 h2)
    (abs (/ (* 100 (- target (+ turns (/ h1 c1) (/ h2 c2)))) target))))

    
;; Now that we have a solution, we need to resolve it into a number of turns,
;; and steps on each wheel
(define resolve-solution
  (lambda (x)
    (let ((e (car x)) (steps (div (cadr x) *ratio*)) (c1 (caddr x)) (c2 (cadddr x)))
      (let*-values (((turns left) (div-and-mod steps (* c1 c2)))
                    ((h2 s1) (div-and-mod left c1))
                    ((h1 s2) (div-and-mod left c2)))
        (if (zero? e)  ;; Is this an exact result?
            (cond
             ((zero? left)
              (if (member turns (factors *ratio*))
                  (list (list e turns 1 0 1 0))       ;; Just an integer number of turns
                  '()))
             ((zero? s2) (list (list e turns c1 h1 1 0)))     ;; Integer turns on wheel 1
             ((zero? s1) (list (list e turns c2 h2 1 0)))     ;; integer turns on wheel 2
             ((find (lambda (x) (= s2 (* c1 x))) (iota (- c2 1) 1)) => (lambda(x) (list (list e turns c1 h1 c2 x))))
             ((find (lambda (x) (= s1 (* c2 x))) (iota (- c1 1) 1)) => (lambda(x) (list (list e turns c2 h2 c1 x))))
             (else '()))
             ;; inexact solution - calculate all possible results and return the top one
            (let-values (((c1 c2) (min-max c1 c2)))
              (let* ((left (- steps (* turns c1 c2)))
                     (c1s (iota 2 (div left c2)))   ; Possible solutions using only c1
                     (c2s (iota 2 (div left c1)))   ; possible solutions using only c2
                     (solve (lambda (c1 m c2)       ; minimum set of possible solutions using both
                          (append-map
                           (lambda (x)
                             (let ((left (- left (* x c2))))
                               (filter-map (lambda (y) (if (and (> y 0) (< y c2)) (list x y) #f))
                                           (iota 2 (div left c1)))))
                           (iota m 1))))
                     (c12s (solve c1 (car c1s) c2))
                     (c21s (solve c2 (car c2s) c1)))
                (filter (lambda (x) (< (car x) *tolerated-error-percentage*))
                        (append 
                         (list (list (error-for e turns c1 (car c1s) 1 0) turns c1 (car c1s) 1 0)
                               (list (error-for e turns c1 (cadr c1s) 1 0) turns c1 (cadr c1s) 1 0)
                               (list (error-for e turns c2 (car c2s) 1 0) turns c2 (car c2s) 1 0)
                               (list (error-for e turns c2 (cadr c2s) 1 0) turns c2 (cadr c2s) 1 0))
                         (map (lambda (p) (list (error-for e turns c1 (car p) c2 (cadr p)) turns c1 (car p) c2 (cadr p))) c12s)
                         (map (lambda (p) (list (error-for e turns c1 (cadr p) c2 (car p)) turns c1 (cadr p) c2 (car p))) c21s)))
                )))))))
 
;; Generate a list of fully resolved solutions for a set of divisions 
(define solutions-for-set
  (lambda (divisions)
    (map (lambda (x)
           (list x (list-sort (sorter < car)
                              (apply lset-adjoin equal? '()
                                          (append-map resolve-solution (all-solutions-for x))))))
         divisions)))

(define head
  (lambda (l c)
    (let loop ((l l) (c c))
      (cond
       ((or (null? l) (zero? c)) '())
       (else (cons (car l) (loop (cdr l) (- c 1))))))))

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
(define cadddddr (lambda (x) (caddr (cdddr x))))

(define pi (/ 355 133))  ;; Shitty approximation to pi but it's rational

(define latex-format-entry
  (lambda (division x)
    (let* ((error (car x))
           (cranks (cadr x))
           (c1 (caddr x))
           (h1 (+ (cadddr x) (* cranks c1)))
           (c2? (not (= (caddddr x) 1)))
           (c2 (if c2? (caddddr x) 1))
           (h2 (if c2? (cadddddr x) 0))
           (intturns? (= c1 1))
           (turns (round (* division (+ (/ h1 (* *ratio* c1)) (/ h2 (* *ratio* c2))))))
           (degrees (* (+ (/ (* h1 360) (* c1 *ratio*)) (/ (* h2 360) (* c2 *ratio*))) division))
           (divisions (if (zero? error) division (/ turns (+ (/ h1 (* c1 *ratio*)) (/ h2 (* c2 *ratio*))))))
           (error-diameter (if (zero? error) "\\infty"
                               (exact (floor (abs (/ 0.01 (* pi (- 1 (/ divisions division))))))))))
      (cond
       (intturns? (format " & $ ~a $ & $ ~a $ & $ Exact $ & $ ~a $ \\\\\n" h1 turns error-diameter))
       (c2?
        ;; Sod about to display the ring with the least number of holes to advance last
        (let* ((r (+ (div h1 c1) (div h2 c2)))
               (ho1 (mod h1 c1))
               (ho2 (mod h2 c2))
               (h1 (if (> ho1 ho2) (+ ho1 (* c1 r)) ho1))
               (h2 (if (>= ho2 ho1) (+ ho2 (* c2 r)) ho2)))
          (if (> ho1 ho2)
              (format " & $ ~a + ~a $ & $ ~a $ & $ ~a $ & $ ~a $ \\\\\n" (fractionate h1 c1) (fractionate h2 c2) turns (if (zero? error) "Exact" (format "~8,,0f" divisions)) error-diameter)
              (format " & $ ~a + ~a $ & $ ~a $ & $ ~a $ & $ ~a $ \\\\\n" (fractionate h2 c2) (fractionate h1 c1) turns (if (zero? error) "Exact" (format "~8,,0f" divisions)) error-diameter)
              )))
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
    (produce-latex-document (solutions-for-set (iota (- *last-division* *first-division*) *first-division*)))))

(let ((tex-file-name (format "output/~a.tex" *divider-short-name*)))
  (with-output-to-file tex-file-name produce 'replace)
  (system (format "pdflatex -output-directory output ~a" tex-file-name)))

(exit)

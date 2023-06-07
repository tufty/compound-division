(import (srfi :1) (srfi :26) (srfi :132))

(define caddddr (lambda (x) (cadr (cdddr x))))
(define cadddddr (lambda (x) (caddr (cdddr x))))

(define flatten
  (lambda (l)
    (fold-left append '() l)))

;; "minor factors" of a number
(define minor-factors
  (lambda (n)
    (filter (lambda (x) (zero? (mod n x))) (iota (- n 2) 2))))

(define head
  (lambda (l c)
    (let loop ((l l) (c c))
      (cond
       ((or (null? l) (zero? c)) '())
       (else (cons (car l) (loop (cdr l) (- c 1))))))))

(define sorter
  (lambda (pred fn)
    (lambda (x y) (pred (fn x) (fn y)))))


(define rings-providing
  (lambda (x)
    (filter (lambda (y) (zero? (mod y x))) (flatten *plates*))))

(define list-pairs
  (lambda (l)
    (let loop ((l l) (a '()))
      (if (null? l) a
          (loop (cdr l) (append (map (cut cons (car l) <>) (cdr l)) a))))))

(define pairs-providing
  (lambda (x)
    (filter (lambda (p) (zero? (mod (* (car p) (cdr p)) x))) (append-map list-pairs *plates*))))


;; Is there a solution using *any* disk (i.e. whole turns only)
(define any-disk-solution?
  (lambda (division)
    (let-values (((turns left) (div-and-mod *ratio* division)))
      (if (zero? left) (list 0 turns 1 0 1 0) #f))))

;; Is there a solution using only one ring on a disk?
(define one-ring-solution?
  (lambda (division)
     (let-values (((turns left) (div-and-mod *ratio* division)))
       (let* ((v (/ *ratio* division))
              (n (numerator v))
              (d (denominator v))
              (rings (rings-providing d)))
         (if (null? rings) #f
             ;; At least one ring provides a complete solution
             (map (lambda (r) (list 0 turns r (* r (/ (mod n d) d)) 1 0)) rings))))))
             
;; is there a compound division solution?
(define two-ring-solution?
  (lambda (division)
    (let-values (((turns left) (div-and-mod *ratio* division)))
      (let* ((v (/ *ratio* division))
             (n (numerator v))
             (d (denominator v))
             (pairs (pairs-providing d)))
        (if (null? pairs) #f
            ;; At least one pair of rings might provide a complete solution.
            (let ((r (filter-map (lambda (p)
                                   (let* ((c1 (car p))
                                          (c2 (cdr p))
                                          (x (find (lambda (x) (integer? (* c2 (- v (/ x c1))))) (iota c1 1))))
                                     (if x (list 0 turns c1 x c2 (* c2 (- v (/ x c1)))) #f)))
                                 pairs)))
              (if (null? r) #f r)))))))

;; OK, brute force the rest for any valid approximations
;; Consider solutions for the set of x.d divisions where x is not a factor of d 
(define approximation?
  (lambda (division)
    (append-map
     (lambda (plate)
       (let* ((pairs (list-pairs plate))
              (x* (lset-difference eqv? (iota (- division 1) 1) (minor-factors division)))
              (v* (map (lambda (x) (/ (* x *ratio*) division)) x*))
              (n* (map numerator v*))
              (d* (map denominator v*)))
         (head
          (list-sort
           (sorter < car)
           (append-map
            (lambda (pair)
              (let* ((c1 (car pair))
                     (c2 (cdr pair))
                     (h1* (iota c1 1)))
                (append-map
                 (lambda (h1)
                   (filter-map
                    (lambda (v n d)
                      (let* ((turns (div n d))
                             (rest (/ (mod n d) d)))
                        (if (> rest (+ (/ h1 c1) (/ 1 c2)))
                            (let* ((h2 (round (* (- rest (/ h1 c1)) c2)))
                                   (e% (abs (/ (- rest (/ h1 c1) (/ h2 c2)) rest))))
                              (if (<= e% *tolerated-error-percentage*)
                                  (list e% turns c1 h1 c2 h2)
                                  #f))
                            #f)))
                    v* n* d*))
                 h1*)))
            pairs)) 1)))
     *plates*)))

(define solutions-for
  (lambda (d)
    (cond
     ((any-disk-solution? d) => (cut list d <>))
     ((one-ring-solution? d) => (cut append (list d) <>))
     ((two-ring-solution? d) => (cut append (list d) <>))
     ((approximation? d) => (cut append (list d) <>))
     (else (list d '(#f 0 1 0 1 0))))))

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
           (results (cdr division-set))
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
    (produce-latex-document (map solutions-for (iota (- *last-division* *first-division*) *first-division*)))))

(let ((tex-file-name (format "output/~a.tex" *divider-short-name*)))
  (with-output-to-file tex-file-name produce 'replace)
  (system (format "pdflatex -output-directory output ~a" tex-file-name)))

;(exit)

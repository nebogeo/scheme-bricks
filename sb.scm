;; [ Copyright (C) 2012 Dave Griffiths : GPLv3 see LICENCE ]

;; todo:
;; * show/hide palette
;; * rotate/hide block
;; * click undock bug 


;; * auto record edits
;; * load/save code
;; * copy/paste
;; * drag resized distance
;; * resize block
;; * execute flash block
;; * play flash insertion
;; * middle click code select
;; * palette move with mouse wheel
;; * lock pallete items from drag/drop (lock all recusively?)
;; * drop messed up from bottom
;; * right click text edit


(require mzlib/string)

(require fluxus-018/fluxa)
(define drop-fudge 0)
(define lag-fudge 0.7)
(define drag-transparent 0.75)

(set-global-offset 0.12)

(define sound-check #f)

(define (reset) 
  (clear-pings!)
  (searchpath "/home/dave/noiz/nm/")
  (reload))

(reset)

(define palette '(
                  () (play-now) (seq) (lambda) (reset) time clock sync-tempo
                  0 0.1 440 (play) (+) (*) (/) (-) (sine) (squ) (saw)
                  (white) (pink) (sample) (adsr 0 0.1 0 0) (mooglp)
                  (mooghp) (moogbp) (rndf) (note) (random 100)
                  (when (zmod clock 4)) 
                  (modulo clock 8)
                  (pick (list 0.1 0.2) clock)
                  (cond ((zmod clock 4) )(else ))
                  (define (zop time clock))
                  (cond ((< clock 4)))
                  (in time 0.5 zop (+ clock 1))
                  (if (even? clock) 0.25 0.5)
                  (if (< clock 6) 0.25 0.5)
                  (define (root time clock) (synced-in time root (+ clock 1)))
                  (synced-in (time-now) root 0)
                  (when (and (< (modulo clock 34) 3) (zmod clock 2)))
                  ))
   
(define (_println l)
  (map
   (lambda (a)
     (display a)(display " "))
   l)
  (newline))

(define (println . args) (_println args))
(define (dbg . args) (_println args) (car args))

(define (insert-to i p l)
  (cond
   ((null? l) (list i))
   ((zero? p) (cons i l))
   (else
    (cons (car l) (insert-to i (- p 1) (cdr l))))))

;; (list-replace '(1 2 3 4) 2 100) => '(1 2 100 4)
(define (list-replace l i v)
  (cond
    ((null? l) l)
    ((zero? i) (cons v (list-replace (cdr l) (- i 1) v)))
    (else (cons (car l) (list-replace (cdr l) (- i 1) v)))))

(define (in-list? a l)
  (cond 
   ((null? l) #f)
   ((eq? (car l) a) #t)
   (else (in-list? a (cdr l)))))

(define (text-from-code code)
   (cond 
    ((string? code) 
     (string-append "\"" code "\""))
    ((number? code) 
     (number->string code))
    ((symbol? code) 
     (symbol->string code))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; maths hacks, move to fluxa

(define (all-numbers? l)
  (cond
   ((null? l) #t)
   ((not (number? (car l))) #f)
   (else (all-numbers? (cdr l)))))

(define (+ . args)
  (if (all-numbers? args)
      (apply + args)
      (proc-list add args)))

(define (- . args)
  (if (all-numbers? args)
      (apply - args)
      (proc-list sub args)))

(define (/ . args)
  (if (all-numbers? args)
      (apply / args)
      (proc-list div args)))

(define (* . args)
  (if (all-numbers? args)
      (apply * args)
      (proc-list mul args)))

(define (proc-list p l)
  (cond
   ((eq? (length l) 1) (car l))
   ((eq? (length l) 2) (p (car l) (cadr l)))
   (else (p (car l) (proc-list p (cdr l))))))


; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (get-line-from-mouse)
  (let* ((ndcpos (vector (* (- (/ (mouse-x) (vx (get-screen-size))) 0.5) 2)
                         (* (- (- (/ (mouse-y) (vy (get-screen-size))) 0.5)) 1.5) -1))
         (scrpos2 (vtransform (vmul ndcpos 500) (minverse (get-camera-transform))))
         (scrpos (vtransform ndcpos (minverse (get-camera-transform)))))
    (list scrpos scrpos2)))

(define (get-point-from-mouse)
  (let ((line (get-line-from-mouse)))
    (vlerp (car line) (cadr line) (/ (vz (car line)) 
                                     (- (vz (car line)) (vz (cadr line)))))))

(define (linebreak txt)
   (let ((t (foldl
                    (lambda (ch r)
                        (if (and (char=? ch #\ ) (> (string-length (car r)) 40))
                            (list "" (append (cadr r) (list (car r))))
                            (list (string-append (car r) (string ch))
                                (cadr r))))                         
                    (list "" '())
                    (string->list txt))))                    
        (append (cadr t) (list (car t)))))

(define (broadcast t error)
   (display error)(newline)
   (let ((error (linebreak error)))
     (let ((p (build-locator)))
       
       (with-state
        (parent p)
        (translate (vector -24 18 5))
        (scale 1)
        (hint-unlit)
        (hint-depth-sort)
        (texture-params 0 (list 'min 'linear 'mag 'linear))
        (texture (load-texture "oolite-font.png"
                               (list 'generate-mipmaps 0 'mip-level 0)))
        (for-each
         (lambda (line)
           (let ((pp (build-text line)))
             (translate (vector 0 -1 0))
             (with-primitive pp             
                             (text-params line (/ 16 256) (/ 16 256) 16 0 -0.01 0 15 -20 0.005 0.2))))
         error))
      
       (spawn-timed-task (+ (time-now) t) 
                         (lambda () (destroy p))))))


; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (set-text text)
  (text-params text (/ 16 256) (/ 16 256) 16 0 -0.01 0 15 -20 0.005 0.2))

(define (make-brick text children)
  (let* ((atom (not children))
         (prim (build-polygons (if atom 4 8) 'triangle-strip))
         (depth-shape-prim (build-polygons (if atom 16 32) 'quad-list))
         (text-prim  (with-state
                      (parent prim)
                      (translate (vector -0.9 1.1 0.0001))
                      (hint-unlit)
                      (hint-depth-sort)
                      (colour 0)
                      (texture-params 0 (list 'min 'linear 'mag 'linear))
                      (texture (load-texture "oolite-font.png"
                                             (list 'generate-mipmaps 0 'mip-level 0)))
                      (let ((text-prim (build-text text)))
                        (with-primitive 
                         text-prim
                         (set-text text))
                        text-prim))))
    (with-primitive 
     prim
     ;;(hint-none)(hint-wire)
     (cond 
      (atom
       (pdata-set! "p" 0 (vector 5 0 0))
       (pdata-set! "p" 1 (vector 5 1 0))
       (pdata-set! "p" 2 (vector -1 0 0))
       (pdata-set! "p" 3 (vector -1 1 0)))
      (else
       (pdata-set! "p" 0 (vector 5 0 0))
       (pdata-set! "p" 1 (vector 5 1 0))
       (pdata-set! "p" 2 (vector 0 0 0))
       (pdata-set! "p" 3 (vector -1 1 0))
       (pdata-set! "p" 4 (vector 0 0 0))
       (pdata-set! "p" 5 (vector -1 -1 0))
       (pdata-set! "p" 6 (vector 5 0 0))
       (pdata-set! "p" 7 (vector 5 -1 0))))                      
     (pdata-map! (lambda (n) (vector 0 0 1)) "n")
     (apply-transform)
     (pdata-copy "p" "pref"))
  
    (let ((depth -3))
          

    (with-primitive 
   depth-shape-prim
   (parent prim)
   (cond 
    (atom
     (pdata-set! "p" 0 (vector -1 1 depth)) 
     (pdata-set! "p" 1 (vector -1 1 0)) 
     (pdata-set! "p" 2 (vector 5 1 0))
     (pdata-set! "p" 3 (vector 5 1 depth))
     
     (pdata-set! "n" 0 (vector 0 1 0)) 
     (pdata-set! "n" 1 (vector 0 1 0)) 
     (pdata-set! "n" 2 (vector 0 1 0))
     (pdata-set! "n" 3 (vector 0 1 0))
     
     (pdata-set! "p" 4 (vector 5 1 depth))
     (pdata-set! "p" 5 (vector 5 1 0))
     (pdata-set! "p" 6 (vector 5 0 0))
     (pdata-set! "p" 7 (vector 5 0 depth))
     
     (pdata-set! "n" 4 (vector 1 0 0)) 
     (pdata-set! "n" 5 (vector 1 0 0)) 
     (pdata-set! "n" 6 (vector 1 0 0))
     (pdata-set! "n" 7 (vector 1 0 0))
     
     (pdata-set! "p" 8 (vector 5 0 depth))
     (pdata-set! "p" 9 (vector 5 0 0))
     (pdata-set! "p" 10 (vector -1 0 0)) 
     (pdata-set! "p" 11 (vector -1 0 depth))
     
     (pdata-set! "n" 8 (vector 0 -1 0)) 
     (pdata-set! "n" 9 (vector 0 -1 0)) 
     (pdata-set! "n" 10 (vector 0 -1 0))
     (pdata-set! "n" 11 (vector 0 -1 0))
     
     (pdata-set! "p" 12 (vector -1 0 depth)) 
     (pdata-set! "p" 13 (vector -1 0 0)) 
     (pdata-set! "p" 14 (vector -1 1 0))
     (pdata-set! "p" 15 (vector -1 1 depth)) 
     
     (pdata-set! "n" 12 (vector -1 0 0)) 
     (pdata-set! "n" 13 (vector -1 0 0)) 
     (pdata-set! "n" 14 (vector -1 0 0))
     (pdata-set! "n" 15 (vector -1 0 0)))
    (else
     (pdata-set! "p" 0 (vector 0 0 0))
     (pdata-set! "p" 1 (vector 0 0 depth))
     (pdata-set! "p" 2 (vector 5 0 depth))
     (pdata-set! "p" 3 (vector 5 0 0))
     (pdata-set! "n" 0 (vector 0 -1 0))
     (pdata-set! "n" 1 (vector 0 -1 0))
     (pdata-set! "n" 2 (vector 0 -1 0))
     (pdata-set! "n" 3 (vector 0 -1 0))
     
     (pdata-set! "p" 4 (vector 5 0 0))
     (pdata-set! "p" 5 (vector 5 0 depth))
     (pdata-set! "p" 6 (vector 5 1 depth))
     (pdata-set! "p" 7 (vector 5 1 0))
     (pdata-set! "n" 4 (vector 1 0 0))
     (pdata-set! "n" 5 (vector 1 0 0))
     (pdata-set! "n" 6 (vector 1 0 0))
     (pdata-set! "n" 7 (vector 1 0 0))
     
     (pdata-set! "p" 8 (vector 5 1 0))
     (pdata-set! "p" 9 (vector 5 1 depth))
     (pdata-set! "p" 10 (vector -1 1 depth))
     (pdata-set! "p" 11 (vector -1 1 0))
     (pdata-set! "n" 8 (vector 0 1 0))
     (pdata-set! "n" 9 (vector 0 1 0))
     (pdata-set! "n" 10 (vector 0 1 0))
     (pdata-set! "n" 11 (vector 0 1 0))
     
     (pdata-set! "p" 12 (vector 0 0 depth))
     (pdata-set! "p" 13 (vector 0 0 0))
     (pdata-set! "p" 14 (vector 0 0 0)) ; --
     (pdata-set! "p" 15 (vector 0 0 depth)) ; --
     (pdata-set! "n" 12 (vector 1 0 0))
     (pdata-set! "n" 13 (vector 1 0 0))
     (pdata-set! "n" 14 (vector 1 0 0))
     (pdata-set! "n" 15 (vector 1 0 0))                            
     
     (pdata-set! "p" 16 (vector -1 1 0))
     (pdata-set! "p" 17 (vector -1 1 depth))
     (pdata-set! "p" 18 (vector -1 -1 depth)) ;--
     (pdata-set! "p" 19 (vector -1 -1 0)) ;--
     (pdata-set! "n" 16 (vector -1 0 0))
     (pdata-set! "n" 17 (vector -1 0 0))
     (pdata-set! "n" 18 (vector -1 0 0))
     (pdata-set! "n" 19 (vector -1 0 0))                            
     
     (pdata-set! "p" 20 (vector -1 -1 0)) ;--
     (pdata-set! "p" 21 (vector -1 -1 depth)) ;--
     (pdata-set! "p" 22 (vector 5 -1 depth)) ;--
     (pdata-set! "p" 23 (vector 5 -1 0)) ;--
     (pdata-set! "n" 20 (vector 0 -1 0))
     (pdata-set! "n" 21 (vector 0 -1 0))
     (pdata-set! "n" 22 (vector 0 -1 0))
     (pdata-set! "n" 23 (vector 0 -1 0))                            
     
     (pdata-set! "p" 24 (vector 5 -1 0)) ;--
     (pdata-set! "p" 25 (vector 5 -1 depth)) ;--
     (pdata-set! "p" 26 (vector 5 0 depth)) ;--
     (pdata-set! "p" 27 (vector 5 0 0)) ;--
     (pdata-set! "n" 24 (vector 1 0 0))
     (pdata-set! "n" 25 (vector 1 0 0))
     (pdata-set! "n" 26 (vector 1 0 0))
     (pdata-set! "n" 27 (vector 1 0 0))                            
     
     (pdata-set! "p" 28 (vector 5 0 0)) ;--
     (pdata-set! "p" 29 (vector 5 0 depth)) ;--
     (pdata-set! "p" 30 (vector 0 0 depth)) ; --
     (pdata-set! "p" 31 (vector 0 0 0)) ; --
     (pdata-set! "n" 28 (vector 0 1 0))
     (pdata-set! "n" 29 (vector 0 1 0))
     (pdata-set! "n" 30 (vector 0 1 0))
     (pdata-set! "n" 31 (vector 0 1 0))))
   
   (pdata-copy "p" "pref")))

  (list text children empty-ghost prim text-prim depth-shape-prim #f #f)))

(define (brick-text b) (list-ref b 0))
(define (brick-modify-text f b) (list-replace b 0 (f (brick-text b))))
(define (brick-children b) (list-ref b 1))
(define (brick-is-atom? b) (not (brick-children b)))
(define (brick-modify-children f b) (list-replace b 1 (f (brick-children b))))
(define (brick-ghost b) (list-ref b 2))
(define (brick-modify-ghost f b) (list-replace b 2 (f (brick-ghost b))))
(define ghost-pos car)
;(define ghost-size cadr)
(define (ghost-size i) 1)
(define empty-ghost (list #f 1))
(define (brick-clear-ghost b) (brick-modify-ghost (lambda (g) empty-ghost) b))
(define (brick-id b) (list-ref b 3))
(define (brick-text-prim b) (list-ref b 4))
(define (brick-depth b) (list-ref b 5))
(define (brick-locked b) (list-ref b 6)) ; for the palette
(define (brick-modify-locked f b) (list-replace b 6 (f (brick-locked b))))
;; set manually when creating the palette
(define (brick-parent-locked b) (list-ref b 7))
(define (brick-modify-parent-locked f b) (list-replace b 7 (f (brick-parent-locked b))))

(define (brick-for-each fn b)
  (fn b)
  (when (brick-children b)
        (for-each 
         (lambda (c)
           (brick-for-each fn c))
         (brick-children b))))

(define (brick-destroy! b)
  (brick-for-each
   (lambda (b)
     (destroy (brick-id b)))
   b))

(define (brick-modify-all-children fn b)
  (if (brick-is-atom? b)
      (fn b)
      (brick-modify-children 
       (lambda (children)
         (map
          (lambda (child) 
            (brick-modify-all-children fn child))
          children))
       (fn b))))

(define (brick-modify-brick fn b id)
  ;; check ourself first
  (if (eq? (brick-id b) id) 
      (fn b)
      ;; search children
      (if (brick-is-atom? b)
          b
          (brick-modify-children 
           (lambda (children)
             (map
              (lambda (child) 
                (brick-modify-brick fn child id))
              children))
           b))))
  
(define (brick-search b id)
  ;; check ourself first
  (if (eq? (brick-id b) id) 
      b
      ;; search children
      (if (brick-is-atom? b)
          #f
          (foldl
           (lambda (child r)
             (cond
              (r r) ;; already found
              ((eq? (brick-id child) id) child)
              (else (brick-search child id))))
           #f
           (brick-children b)))))

(define (brick-search-for-parent b id)
  (if (brick-is-atom? b) 
      #f
      ;; search children
      (foldl
       (lambda (child r)
         (if (not r)
             (cond
              ((eq? (brick-id child) id) b)
              (else (brick-search-for-parent child id)))
             r))
       #f
       (brick-children b))))

(define (brick-transparent! b a)
  (with-primitive 
   (brick-id b)
   (opacity a)
   ;(hint-nozwrite)
   )
  (with-primitive 
   (brick-depth b)
   (opacity a)
   ;(hint-nozwrite)
   ))

(define (brick-opaque! b)
  (with-primitive 
   (brick-id b)
   (opacity 1)
   (hint-none)(hint-solid))
  (with-primitive 
   (brick-depth b)
   (opacity 1)
   (hint-none)(hint-solid)))

(define (brick-text-glow! p)
  (with-primitive 
   p
   (colour (+ 0.5 (fmod (* 4 (flxtime)) 0.5)))))

(define (brick-code-glow! p)
  (with-primitive 
   p
   (colour (vmix 
            (vector 1 0 0) 
            (vector 1 1 0) 
            (abs (sin (* (flxtime) 2)))))))

(define (brick-get-scale b)
  (with-primitive 
   (brick-id b)
   (let ((tx (get-transform)))
     (vmag (vector
            (vector-ref tx 0)
            (vector-ref tx 5)
            (vector-ref tx 11))))))

(define (brick-expand! b n)
  (with-primitive 
   (brick-id b)
   (for ((i (in-range 4 8)))
        (pdata-set! "p" i (vadd (pdata-ref "pref" i) (vector 0 (- n) 0)))))
  (with-primitive 
   (brick-depth b)
   (for-each (lambda (i)
               (pdata-set! "p" i (vadd (pdata-ref "pref" i) (vector 0 (- n) 0))))
             (list 14 15))
   (for ((i (in-range 18 32)))
        (pdata-set! "p" i (vadd (pdata-ref "pref" i) (vector 0 (- n) 0))))))

(define (brick-children-size b)
  (if (not (brick-is-atom? b))
      (foldl
       (lambda (child n)
         (+ n (brick-size child)))
       (if (ghost-pos (brick-ghost b)) 
           (ghost-size (brick-ghost b)) 0)
       (brick-children b))
      0))
  
(define (make-brick-from-atom code)
  (make-brick (text-from-code code) #f))

(define (code->brick code)
  (cond 
   ;; atom
   ((not (list? code)) (make-brick-from-atom code))
   ;; empty list
   ((null? code) (make-brick "" '()))
   ;; list starting with atom
   ((not (list? (car code)))
    (make-brick  
     (text-from-code (car code))
     (map
      (lambda (item)
        (code->brick item))
      (cdr code))))
   ;; anoymous list
   (else
    (make-brick  
     ""
     (map
      (lambda (item)
        (code->brick item))
      code)))))

(define (brick-size b)
  (if (brick-is-atom? b) 1
      (+ 2 (brick-children-size b))))

(define (remove-last str)
  (substring str 0 (- (string-length str) 1)))

(define (brick->code b)
  (eval-string (string-append "'" (brick->text b #f))))

;; does the flashing when it plays the sound business
(define (brick-code-inserts b)
  (cond 
   ((and (eq? 2 (length (brick-children b)))
         (string=? "play" (brick-text b)))
    (string-append " 0 (lambda () (with-primitive " (number->string (brick-id b))
                   " (colour (vector 1 1 1)))"
                   "(with-primitive " (number->string (brick-depth b))
                   " (colour (vector 1 1 1))))"))
   ((and (eq? 3 (length (brick-children b)))
               (string=? "play" (brick-text b)))
    (string-append " (lambda () (with-primitive " (number->string (brick-id b))
                   " (colour (vector 1 1 1)))"
                   "(with-primitive " (number->string (brick-depth b))
                   " (colour (vector 1 1 1))))"))
   (else "")))
  
(define (brick->text b do-insert)
  (if (brick-is-atom? b)
      (brick-text b)
      (string-append
       (remove-last
        (string-append "(" (brick-text b) " "
                       (apply string-append
                              (map
                               (lambda (child)
                                 (string-append 
                                  (brick->text child do-insert) " "))
                               (brick-children b)))))
       (if do-insert (brick-code-inserts b) "") ")")))

(define (brick->sexpr b)
  (if (brick-is-atom? b)
      (string->symbol (brick-text b))
      (apply
       list
       (cons
        (string->symbol (brick-text b))
        (map brick->sexpr (brick-children b))))))

(define (brick-pos->slot b pos)
  (let ((relative-y 
         (vy (vsub
              (with-primitive 
               (brick-id b) 
               (vtransform (vector 0 0 0) (get-global-transform)))
              pos))))
    (car (foldl
          (lambda (child r)
            (list
             ;; if we are lower than relative-y
             (if (< (+ (cadr r) drop-fudge) relative-y) 
                 (+ (car r) 1) ; keep adding
                 (car r)) ; return this index
             (+ (cadr r) (brick-size child))))
          (list 0 1) ; index, y
          (brick-children b)))))

(define (brick-update-ghost b pos size)
  (brick-modify-ghost
   (lambda (g) (list (brick-pos->slot b pos) size))
   b))

(define (brick-dock b new)
  (cond ((or (brick-parent-locked b) 
             (brick-locked b)) b)
        (else
         (with-primitive 
          (brick-id new)
          (identity)
          (parent (brick-id b)))
         (brick-modify-children
          (lambda (children)
            (insert-to new (ghost-pos (brick-ghost b)) children))
          (brick-clear-ghost b)))))

(define (brick-undock b id)
  (cond ((or (brick-locked b)
             (brick-parent-locked b)) b)
        (else
         (with-primitive id (detach-parent))
         (brick-modify-children
          (lambda (children)
            (filter
             (lambda (b)
               (not (eqv? (brick-id b) id)))
             children))
          b))))

; update the primitive and children to match the state
(define (brick-update! b d)
  (with-primitive 
   (brick-text-prim b)
   (set-text (brick-text b)))
  (with-primitive 
   (brick-id b)
   (colour (vector 1 (/ (modulo d 6) 6) (/ (modulo d 4) 4))))
  (with-primitive 
   (brick-depth b)
   (colour (vector 1 (/ (modulo d 6) 6) (/ (modulo d 4) 4))))
  (when (not (brick-is-atom? b))
        (let ((size
               (car 
                (foldl
                 (lambda (child p)
                   (with-primitive 
                    (brick-id child)
                    (identity)
                    (parent (brick-id b))
                    (when (brick-locked b) (translate (vector 0 (* (cadr p) -1.3) 0)))
                    (translate (vector 1 (- (car p)) 0)))
                   (brick-update! child (+ d 1)) 
                   (list
                    (if (and (ghost-pos (brick-ghost b))
                             (eq? (ghost-pos (brick-ghost b)) 
                                  (+ (cadr p) 
                                     (ghost-size (brick-ghost b)))))
                        ;; insert ghost
                        (+ (car p) (brick-size child) 
                           (ghost-size (brick-ghost b)))
                        (+ (car p) (brick-size child)))
                    (+ (cadr p) 1)))
                 (list 
                  (if (and (ghost-pos (brick-ghost b))
                           (eq? (ghost-pos (brick-ghost b)) 0))
                      (+ 1 (ghost-size (brick-ghost b))) 1)
                  0) ; y, index
                 (brick-children b)))))
          (brick-expand! b (- size 1)))))

#;(define (brick-intersect b line)
  (with-primitive 
   (brick-id b)
   (let ((p (geo/line-intersect (vtransform (car line) (minverse (get-global-transform)))
                                (vtransform (cadr line) (minverse (get-global-transform)))))
         (m (get-global-transform)))
     (if (not (null? p))
         b
         (if (not (brick-is-atom? b)) 
             (foldl
              (lambda (child r)
                (if (not r) (brick-intersect child line) r))
              #f
              (brick-children b))
             #f)))))

(define (brick-intersect b pos)
  (let ((hit (if (not (brick-is-atom? b)) 
                 (foldl
                  (lambda (child r)
                    (if (not r) (brick-intersect child pos) r))
                  #f
                  (brick-children b))
                 #f)))
    (if (not hit)
        (with-primitive 
         (brick-id b)
         (recalc-bb)
         (if (bb/point-intersect? pos 0.1) b #f))
        hit)))

(define (brick-intersect-no-atoms b pos)
  (if (not (brick-is-atom? b))
      (let ((hit (foldl
                  (lambda (child r)
                    (if (not r) (brick-intersect-no-atoms child pos) r))
                  #f
                  (brick-children b))))
        (if (not hit)
            (with-primitive 
             (brick-id b)
             (recalc-bb)
             (if (bb/point-intersect? pos 0.1) b #f))
            hit))
      #f))

;---------------------------------------------------------  

(define (make-bricks)
  (list '() (vector 0 0 0) #f #f #f #f #f '() #f
        (let* ((p (open-input-file "history/persist.scm"))
               (r (read p)))
          (println "history level is: " r)
          (close-input-port p)
          r)))

(define (bricks-roots b) (list-ref b 0))
(define (bricks-modify-roots f b) (list-replace b 0 (f (bricks-roots b))))
(define (bricks-mouse b) (list-ref b 1))
(define (bricks-modify-mouse f b) (list-replace b 1 (f (bricks-mouse b))))
(define (bricks-button b) (list-ref b 2))
(define (bricks-modify-button f b) (list-replace b 2 (f (bricks-button b))))
(define (bricks-current b) (list-ref b 3))
(define (bricks-modify-current f b) (list-replace b 3 (f (bricks-current b))))
(define (bricks-drop-over b) (list-ref b 4))
(define (bricks-modify-drop-over f b) (list-replace b 4 (f (bricks-drop-over b))))
(define (bricks-typing b) (list-ref b 5))
(define (bricks-modify-typing f b) (list-replace b 5 (f (bricks-typing b))))
(define (bricks-typing-current b) (list-ref b 6))
(define (bricks-modify-typing-current f b) (list-replace b 6 (f (bricks-typing-current b))))
(define (bricks-keys b) (list-ref b 7))
(define (bricks-modify-keys f b) (list-replace b 7 (f (bricks-keys b))))
(define (bricks-code-current b) (list-ref b 8))
(define (bricks-modify-code-current f b) (list-replace b 8 (f (bricks-code-current b))))
(define (bricks-history b) (list-ref b 9))
(define (bricks-modify-history f b) (list-replace b 9 (f (bricks-history b))))

(define (bricks-save b fn)
  (let ((f (open-output-file fn #:exists 'replace)))
    (write 
     (map 
      (lambda (root)
        (list
         (with-primitive (brick-id root) (get-global-transform))
         (brick->text root #f)))
      (filter (lambda (root) (not (brick-locked root))) (bricks-roots b)))
     f)
    (close-output-port f)))

(define (bricks-load b fn)
  (let* ((f (open-input-file fn))
         (ret (foldl 
               (lambda (e b)
                 (println (cadr e))
                 (let ((brick (code->brick (eval-string (string-append "'" (cadr e))))))
                   (with-primitive 
                    (brick-id brick) 
                    (concat (car e)))
                   (bricks-add-root b brick)))
               b
               (read f))))
    (close-input-port f)
    ret))

(define (get-max-history)
  (let* ((f (open-input-file "history/persist.scm"))
         (max (read f)))
    (close-input-port f)
    max))

(define (increase-history!)
  (let* ((h (get-max-history))
         (f (open-output-file "history/persist.scm" #:exists 'replace)))
    (write (+ h 1) f)
    (close-output-port f)
    (+ h 1)))
  
(define (bricks-load-history b)
  (bricks-load 
   (bricks-clear b) 
   (string-append 
    "history/" 
    (number->string (bricks-history b)) 
    ".scm")))

(define (bricks-save-history! b)
  (bricks-save 
   b 
   (string-append 
    "history/" 
    (number->string (increase-history!)) 
    ".scm")))

(define (bricks-modify-brick fn b id)
  (bricks-modify-roots
   (lambda (roots)
     (map
      (lambda (root)
        (brick-modify-brick fn root id))
      roots))
   b))

(define (bricks-delete b brick)
  (let* ((id (brick-id brick))
         (parent (bricks-search-for-parent b id))
         (new-b (if parent 
                    (bricks-modify-brick 
                     (lambda (parent)
                       (brick-undock parent id))
                     b (brick-id parent))
                    (bricks-modify-roots
                     (lambda (roots)
                       (filter 
                        (lambda (r)
                          (not (eq? (brick-id r) id)))
                        roots))
                     b))))
    (brick-destroy! brick) ;; destroy after potentially undocking
    new-b))

(define (bricks-palette b) (car (bricks-roots b)))

(define (bricks-add-root b root)
  (bricks-modify-roots
   (lambda (roots)
     ;; add to end so palette is always first element
     (append roots (list root)))
   b))

(define (bricks-remove-root b id)
  (bricks-modify-roots
   (lambda (roots)
     (filter (lambda (root) (not (eq? id (brick-id root)))) roots))
   b))

(define (bricks-clear b)
  (for-each 
   (lambda (root)
     (when (not (brick-locked root))
           (brick-destroy! root)))
   (bricks-roots b))
  (bricks-modify-roots 
   (lambda (r) (list (car r))) 
   (bricks-modify-code-current
    (lambda (c) #f)
    (bricks-modify-typing-current
     (lambda (t) #f)
     b)))) 

(define (bricks->text b)
  (apply
   string-append
   (map brick->text 
        (filter (lambda (brx)
                  (not (brick-locked brx)))
                (bricks-roots b))
        #f)))

(define (bricks->sexpr b)
  (apply list (map brick->sexpr (bricks-roots b))))

(define (bricks-add-clone b brick)
  (bricks-add-root b (code->brick (brick->code brick))))

(define (bricks-add-spawn b brick)
  (let ((copy (code->brick (brick->code brick)))
        ;; copy the transform
        (tx (with-primitive (brick-id brick) (get-global-transform))))
    (brick-for-each 
     (lambda (brick)
       (brick-transparent! brick drag-transparent)) ;; make transparent as we are dragging
     copy)
    (with-primitive (brick-id copy) (identity) (concat tx) (scale 2)) 
    (bricks-modify-current 
     (lambda (c) copy) 
     (bricks-add-root b copy))))
       
(define (bricks-add-code b code)
  (bricks-add-root b (code->brick code)))

(define (bricks-get-over b pos)
  (foldl
   (lambda (brick r)
     (if (and 
          (not r) ;; not found anything yet
          (or
           (not (bricks-current b)) ;; not dragging anything     
           ;; what we are dragging is not this
           (not (eq? (brick-id brick) (brick-id (bricks-current b))))))
         (brick-intersect brick pos)
         r))
   #f
   (bricks-roots b)))

(define (bricks-get-over-no-atoms b pos)
  (foldl
   (lambda (brick r)
     (if (and 
          (not r) ;; not found anything yet
          (or
           (not (bricks-current b)) ;; not dragging anything     
           ;; what we are dragging is not this
           (not (eq? (brick-id brick) (brick-id (bricks-current b))))))
         (brick-intersect-no-atoms brick pos)
         r))
   #f
   (bricks-roots b)))

;; look for the brick with id
(define (bricks-search b id)
  (foldl
   (lambda (b r)
     (if (not r) (brick-search b id) r))
   #f
   (bricks-roots b)))

;; look for the brick which is the parent of the supplied one
(define (bricks-search-for-parent b id)
  (foldl
   (lambda (b r)
     (if (not r) (brick-search-for-parent b id) r))
   #f
   (bricks-roots b)))

;; find out which root a brick belongs to
(define (bricks-root-search b id)
  (foldl
   (lambda (root r)
     (if (and (not r) (brick-search root id))
         root r))
   #f
   (bricks-roots b)))

(define (bricks-mouse-down b)
  (and (mouse-button 1) (not (bricks-button b))))

(define (bricks-mouse-up b)
  (and (not (mouse-button 1)) (bricks-button b)))

(define (bricks-key-pressed? b key)
  (in-list? key (bricks-keys b)))

(define (keys-contains-enter? keys)
  (foldl
   (lambda (key r)
     (if (and (not r) (eq? (char->integer key) 13)) ; return
         #t r))
   #f
   keys))

(define (bricks-type-into b bx keys-pressed)
  ;; modify the brick text based on the keys pressed
  (foldl
   (lambda (key r)
     (if (char? key)             
         (cond             
          ((eq? (char->integer key) 8) ; delete (128 on mac)
           (if (> (string-length (brick-text r)) 0)
               (brick-modify-text
                (lambda (t)
                  (substring 
                   t 0 (- (string-length t) 1)))
                r) r))
          (else
           (brick-modify-text
            (lambda (t)
              (string-append t (string key)))
            r)))
         r))
   bx
   keys-pressed))

(define (bricks-do-keys b pos)
  ;; get keys pressed since last update
  (let ((keys-pressed
         (filter
          (lambda (key)
            (not (in-list? key (bricks-keys b))))
          (keys-down))))
    (bricks-modify-keys 
     (lambda (k) (keys-down))
     ;; dispatch
     (cond
      ;; do typing if we have a current brick
      ((bricks-typing-current b)
       (bricks-modify-brick
        (lambda (bx)
          ;; don't put returns into the code text...
          (if (not (keys-contains-enter? keys-pressed))
              (bricks-type-into b bx keys-pressed)
              bx))
        (if (keys-contains-enter? keys-pressed)
            (bricks-modify-typing-current 
             (lambda (t) #f)
             b)
            b)
        (brick-id (bricks-typing-current b))))

      ;; do execute key if we have a code selection
      ((and (in-list? #\x keys-pressed)   
            (bricks-code-current b))
       (let ((current (bricks-search b (bricks-code-current b))))
         (bricks-save-history! b)
         (broadcast 1 (brick->text current #t))
         (eval-string (brick->text current #t)
                      (lambda (error)
                        (broadcast 5 (exn-message error))))
         b))

      ;; do copy when code selection is active
      ((and (in-list? #\C keys-pressed)   
            (bricks-code-current b))
       (bricks-add-root 
        b 
        (code->brick 
         (brick->code (bricks-search b (bricks-code-current b))))))

      ;; keyboard alt for middle mouse (h)
      ((in-list? #\h keys-pressed)
       (bricks-modify-code-current 
        (lambda (o)
          (let ((brick (bricks-get-over b pos)))
            (if brick (brick-id brick) #f))) 
        b))

      ;; o saves
      ((in-list? #\o keys-pressed)
       (bricks-save b "test.scm")
       b)
      ((in-list? #\p keys-pressed)
       (bricks-load b "test.scm"))

      ;; history navigation, backwards
      ((in-list? #\, keys-pressed)
       (bricks-load-history
        (bricks-modify-history 
         (lambda (h) (max 1 (- h 1))) b)))
      
      ;; history navigation, forwards
      ((in-list? #\. keys-pressed)
       (let* ((f (open-input-file "history/persist.scm"))
              (max (read f)))
         (close-input-port f)
         (bricks-load-history
          (bricks-modify-history 
           (lambda (h) (min max (+ h 1))) b))))

      ((in-list? #\P keys-pressed)
       (let ((brick (bricks-get-over b pos)))
         (if brick (bricks-delete b brick)
             b)))

      (else b)))))

(define (bricks-do-dragging b pos)
  ;; find what is underneath
  (let* ((temp (bricks-get-over-no-atoms b pos))
         (over (if (and temp (brick-is-atom? temp)) #f temp))
         (old-over (bricks-drop-over b)))
    (bricks-modify-drop-over
     (lambda (dropover) over)
     (if (and over (not (brick-parent-locked over)))
         ;; update the ghost position
         (bricks-modify-brick
          (lambda (over)
            (brick-update-ghost over pos (brick-size (bricks-current b))))
          (if old-over
              ;; update the ghost for the old over brick
              (bricks-modify-brick
               (lambda (over)
                 (brick-clear-ghost over))
               b
               (brick-id old-over))
              b)
          (brick-id over))
         b))))

(define (bricks-update-current b pos)
  (bricks-modify-current
   (lambda (current)
     (cond 
      ((bricks-mouse-down b) 
       (let ((c (bricks-get-over b pos)))
         (when (and c (not (brick-parent-locked c)))
               (brick-for-each
                (lambda (c)
                  (brick-transparent! c drag-transparent))
                c))
         c))
      ((bricks-mouse-up b) 
       (when current
             (brick-for-each
              (lambda (c)
                (brick-opaque! c))
              current))
       #f)
      (else current)))
   ;; if we are dragging something 
   (if (bricks-current b)
       (bricks-do-dragging b pos)
       b)))

(define (bricks-do-mouse-wheel b pos)
  (let ((over (bricks-get-over b pos)))
    (when over
          (let ((root (bricks-root-search b (brick-id over))))
            (if (brick-locked root) ;; it's the palette, translate
                (with-primitive 
                 (brick-id root)
                 (when (or (< (mouse-wheel) 0) (key-special-pressed 103))
                       (translate (vector 0 6 0)))
                 (when (or (> (mouse-wheel) 0) (key-special-pressed 101))
                       (translate (vector 0 -6 0))))
                (with-primitive  ;; it's a normal brick, scale
                 (brick-id root)
                 (let* ((t (get-transform))
                        (d (vsub pos (vtransform (vector 0 0 0) (get-transform))))
                        (d2 (vmul d (/ 1 (vector-ref (get-transform) 0)))))
                   (when (or (< (mouse-wheel) 0) (key-special-pressed 103))
                         (translate d2)
                         (scale (vector 0.9 0.9 0.9))
                         (translate (vmul d2 -1)))
                   (when (or (> (mouse-wheel) 0) (key-special-pressed 101))
                         (translate d2)
                         (scale (vector 1.1 1.1 1.1))
                         (translate (vmul d2 -1))
                         ))))))
    b))

(define (bricks-do-input b pos)
  (bricks-do-mouse-wheel
   (bricks-do-keys
    (bricks-modify-mouse 
     (lambda (m) pos)
     (bricks-modify-code-current 
      (lambda (o) 
        (if (mouse-button 2)
            (let ((brick (bricks-get-over b pos)))
              (if brick (brick-id brick) #f))
            o))
      (bricks-modify-typing
       (lambda (t) (mouse-button 3))
       (bricks-modify-button 
        (lambda (button) (mouse-button 1))
        ;; keep track of the selection
        (bricks-update-current b pos))))) pos) pos))

(define (bricks-drag-start? b new-b)
  (and (not (list? (bricks-current b)))
       (list? (bricks-current new-b))))

(define (bricks-drag-end? b new-b)
  (and (list? (bricks-current b))
       (not (list? (bricks-current new-b)))))

(define (bricks-over-out? b new-b)
  (and (list? (bricks-drop-over b))
       (not (list? (bricks-drop-over new-b)))))

(define (bricks-do-docking b new-b)
  ;; check for brick to dock
  (if (and (bricks-drag-end? b new-b)
           (bricks-drop-over new-b)
           ;; don't want to dock with the palette (or do we?...)
           (not (brick-parent-locked (bricks-drop-over new-b))))
      ;; note - assume if drop over exists, then current must also exist
      (let* ((id (brick-id (bricks-current b)))
             (new-parent-id (brick-id (bricks-drop-over new-b))))
        (bricks-remove-root
         (bricks-modify-brick 
          (lambda (new-parent)
            ;; docks to the ghost location
            (brick-dock new-parent (bricks-current b)))
          new-b
          new-parent-id)
         id))
      new-b))

(define (bricks-do-undocking b new-b)
  ;; check for brick to undock
  (if (bricks-drag-start? b new-b)
      (let* ((id (brick-id (bricks-current new-b)))
             (prnt (bricks-search-for-parent b id)))
        (if prnt ;; can't undock from root
            (if (or (brick-locked prnt)
                    (brick-parent-locked prnt))
                ;; for the pallette... make a copy
                (bricks-add-spawn new-b (bricks-current new-b))
                ;; add as new root
                (bricks-add-root
                 ;; undock from parent
                 (bricks-modify-brick 
                  (lambda (parent)
                    (brick-undock parent id))
                  new-b
                  (brick-id prnt))
                 (bricks-current new-b)))
            new-b))
      new-b))

(define (bricks-remove-ghost b new-b)
  ;; check for brick to undock
  (if (bricks-over-out? b new-b)
      (let* ((id (brick-id (bricks-drop-over b))))
        (bricks-modify-brick 
         (lambda (over)
           (brick-clear-ghost over))
         new-b
         id))
      new-b))

(define (bricks-do-typing b new-b pos)
  (if (and (not (bricks-typing b))
           (bricks-typing new-b))
      (bricks-modify-typing-current
       (lambda (t)
         (bricks-get-over new-b pos))
       new-b)
      new-b))

(define (bricks-do-glow! b)
  (let ((tc (bricks-typing-current b)))
    (when tc
          (brick-text-glow! (brick-id tc))
          (brick-text-glow! (brick-depth tc)))
  
  (let ((tc (bricks-code-current b)))
    (when tc
          (brick-code-glow! tc)
          #;(brick-code-glow! (brick-depth tc))))))
                
(define (bricks-update! b)
  (let* ((pos (vadd (vector 0 drop-fudge 0) (get-point-from-mouse))))
    #;(with-primitive pointer
                    (identity)
                    (translate pos))
    (when
    ; update bricks when mouse button is held down 
     ; TODO - check for optimisation
     (or #t (bricks-button b))
      (for-each
       (lambda (b)
         (brick-update! b 0))
       (bricks-roots b)))

    (bricks-do-glow! b)

    ; move the current brick
    (when (list? (bricks-current b))
          (with-primitive 
           (brick-id (bricks-current b))
           (when (and (mouse-button 1) (bricks-button b))
                 (translate (vdiv
                             (vsub pos (bricks-mouse b))
                             (* (brick-get-scale (bricks-current b))
                                lag-fudge))))))

    ; update the input stuff
    (bricks-do-typing
     b (bricks-remove-ghost
        b (bricks-do-docking  
           b (bricks-do-undocking 
              b (bricks-do-input b pos)))) pos)))

;---------------------------------------------------------  

(clear)
;(hint-depth-sort)

(set-camera-transform (mtranslate (vector 0 0 -30)))

(define b
  (bricks-add-code
    (make-bricks) 
   palette))

;; activate palette
(set! b (bricks-modify-brick
         (lambda (palette)
           (brick-for-each
            (lambda (c)
              (with-primitive 
               (brick-id c)
               (opacity 0.8))
              (with-primitive 
               (brick-depth c)
               (opacity 0.8)))
            palette)
           ;; collapse the verts as we want to hide, but want to 
           ;; keep the children visible
           (with-primitive (brick-id palette)
                           (translate (vector 25 10 0))
                           (scale 0.7)
                           (opacity 0)
                           (hint-nozwrite))
           (with-primitive (brick-depth palette)
                           (opacity 0)
                           (hint-nozwrite))
           (brick-modify-locked 
            (lambda (l) #t) 
            (brick-modify-all-children
             (lambda (c)
               (brick-modify-parent-locked 
                (lambda (p) #t) c))
             palette)))
         b
         (brick-id (bricks-palette b))))

(define pointer (with-state (scale 0.1) (build-cube)))

;(clear-colour (vector 0.5 0.2 0.1))
(define t (with-state 
           (translate (vector -28 -20 0))
           (scale (vector 1 1 1))
           (colour (vector 0 0.5 1)) (build-cube)))

(when sound-check (set! b (bricks-load b "sc.scm"))) 

(every-frame 
 (begin
   (with-primitive pointer
                   (identity)
                   (translate (get-point-from-mouse))
                   (scale 0.1))
   (set! b (bricks-update! b))
   (with-primitive t (rotate (vector 1 2 3)))))

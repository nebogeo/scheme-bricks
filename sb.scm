;; [ Copyright (C) 2012 Dave Griffiths : GPLv3 see LICENCE ]

(require mzlib/string)

(require fluxus-018/fluxa)
(searchpath "/home/dave/noiz/nm/")
(reload)


(clear)

(define (_println l)
  (map
   (lambda (a)
     (display a)(display " "))
   l)
  (newline))

(define (println . args) (_println args))
(define (dbg . args) (_println (cdr args)) (car args))

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

(define (text-from-code code)
   (cond 
    ((string? code) 
     (string-append "\"" code "\""))
    ((number? code) 
     (number->string code))
    ((symbol? code) 
     (symbol->string code))))

(define drop-fudge 1)

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
        (scale 2)
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
                         (text-params text (/ 16 256) (/ 16 256) 16 0 -0.01 0 15 -20 0.005 0.2))
                        text-prim))))
    (with-primitive 
     prim
     (hint-unlit)
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
     (apply-transform)
     (pdata-copy "p" "pref"))
  
  (with-primitive 
   depth-shape-prim
   (parent prim)
   (cond 
    (atom
     (pdata-set! "p" 0 (vector -1 1 -3)) 
     (pdata-set! "p" 1 (vector -1 1 0)) 
     (pdata-set! "p" 2 (vector 5 1 0))
     (pdata-set! "p" 3 (vector 5 1 -3))
     
     (pdata-set! "n" 0 (vector 0 1 0)) 
     (pdata-set! "n" 1 (vector 0 1 0)) 
     (pdata-set! "n" 2 (vector 0 1 0))
     (pdata-set! "n" 3 (vector 0 1 0))
     
     (pdata-set! "p" 4 (vector 5 1 -3))
     (pdata-set! "p" 5 (vector 5 1 0))
     (pdata-set! "p" 6 (vector 5 0 0))
     (pdata-set! "p" 7 (vector 5 0 -3))
     
     (pdata-set! "n" 4 (vector 1 0 0)) 
     (pdata-set! "n" 5 (vector 1 0 0)) 
     (pdata-set! "n" 6 (vector 1 0 0))
     (pdata-set! "n" 7 (vector 1 0 0))
     
     (pdata-set! "p" 8 (vector 5 0 -3))
     (pdata-set! "p" 9 (vector 5 0 0))
     (pdata-set! "p" 10 (vector -1 0 0)) 
     (pdata-set! "p" 11 (vector -1 0 -3))
     
     (pdata-set! "n" 8 (vector 0 -1 0)) 
     (pdata-set! "n" 9 (vector 0 -1 0)) 
     (pdata-set! "n" 10 (vector 0 -1 0))
     (pdata-set! "n" 11 (vector 0 -1 0))
     
     (pdata-set! "p" 12 (vector -1 0 -3)) 
     (pdata-set! "p" 13 (vector -1 0 0)) 
     (pdata-set! "p" 14 (vector -1 1 0))
     (pdata-set! "p" 15 (vector -1 1 -3)) 
     
     (pdata-set! "n" 12 (vector -1 0 0)) 
     (pdata-set! "n" 13 (vector -1 0 0)) 
     (pdata-set! "n" 14 (vector -1 0 0))
     (pdata-set! "n" 15 (vector -1 0 0)))
    (else
     (pdata-set! "p" 0 (vector 0 0 0))
     (pdata-set! "p" 1 (vector 0 0 -3))
     (pdata-set! "p" 2 (vector 5 0 -3))
     (pdata-set! "p" 3 (vector 5 0 0))
     (pdata-set! "n" 0 (vector 0 -1 0))
     (pdata-set! "n" 1 (vector 0 -1 0))
     (pdata-set! "n" 2 (vector 0 -1 0))
     (pdata-set! "n" 3 (vector 0 -1 0))
     
     (pdata-set! "p" 4 (vector 5 0 0))
     (pdata-set! "p" 5 (vector 5 0 -3))
     (pdata-set! "p" 6 (vector 5 1 -3))
     (pdata-set! "p" 7 (vector 5 1 0))
     (pdata-set! "n" 4 (vector 1 0 0))
     (pdata-set! "n" 5 (vector 1 0 0))
     (pdata-set! "n" 6 (vector 1 0 0))
     (pdata-set! "n" 7 (vector 1 0 0))
     
     (pdata-set! "p" 8 (vector 5 1 0))
     (pdata-set! "p" 9 (vector 5 1 -3))
     (pdata-set! "p" 10 (vector -1 1 -3))
     (pdata-set! "p" 11 (vector -1 1 0))
     (pdata-set! "n" 8 (vector 0 1 0))
     (pdata-set! "n" 9 (vector 0 1 0))
     (pdata-set! "n" 10 (vector 0 1 0))
     (pdata-set! "n" 11 (vector 0 1 0))
     
     (pdata-set! "p" 12 (vector 0 0 -3))
     (pdata-set! "p" 13 (vector 0 0 0))
     (pdata-set! "p" 14 (vector 0 0 0)) ; --
     (pdata-set! "p" 15 (vector 0 0 -3)) ; --
     (pdata-set! "n" 12 (vector 1 0 0))
     (pdata-set! "n" 13 (vector 1 0 0))
     (pdata-set! "n" 14 (vector 1 0 0))
     (pdata-set! "n" 15 (vector 1 0 0))                            
     
     (pdata-set! "p" 16 (vector -1 1 0))
     (pdata-set! "p" 17 (vector -1 1 -3))
     (pdata-set! "p" 18 (vector -1 -1 -3)) ;--
     (pdata-set! "p" 19 (vector -1 -1 0)) ;--
     (pdata-set! "n" 16 (vector -1 0 0))
     (pdata-set! "n" 17 (vector -1 0 0))
     (pdata-set! "n" 18 (vector -1 0 0))
     (pdata-set! "n" 19 (vector -1 0 0))                            
     
     (pdata-set! "p" 20 (vector -1 -1 0)) ;--
     (pdata-set! "p" 21 (vector -1 -1 -3)) ;--
     (pdata-set! "p" 22 (vector 5 -1 -3)) ;--
     (pdata-set! "p" 23 (vector 5 -1 0)) ;--
     (pdata-set! "n" 20 (vector 0 -1 0))
     (pdata-set! "n" 21 (vector 0 -1 0))
     (pdata-set! "n" 22 (vector 0 -1 0))
     (pdata-set! "n" 23 (vector 0 -1 0))                            
     
     (pdata-set! "p" 24 (vector 5 -1 0)) ;--
     (pdata-set! "p" 25 (vector 5 -1 -3)) ;--
     (pdata-set! "p" 26 (vector 5 0 -3)) ;--
     (pdata-set! "p" 27 (vector 5 0 0)) ;--
     (pdata-set! "n" 24 (vector 1 0 0))
     (pdata-set! "n" 25 (vector 1 0 0))
     (pdata-set! "n" 26 (vector 1 0 0))
     (pdata-set! "n" 27 (vector 1 0 0))                            
     
     (pdata-set! "p" 28 (vector 5 0 0)) ;--
     (pdata-set! "p" 29 (vector 5 0 -3)) ;--
     (pdata-set! "p" 30 (vector 0 0 -3)) ; --
     (pdata-set! "p" 31 (vector 0 0 0)) ; --
     (pdata-set! "n" 28 (vector 0 1 0))
     (pdata-set! "n" 29 (vector 0 1 0))
     (pdata-set! "n" 30 (vector 0 1 0))
     (pdata-set! "n" 31 (vector 0 1 0))))
   
   (pdata-copy "p" "pref"))

  (list text children empty-ghost prim text-prim depth-shape-prim #f)))

(define (brick-text b) (list-ref b 0))
(define (brick-modify-text f b) (list-replace b 0 (f (brick-text b))))
(define (brick-children b) (list-ref b 1))
(define (brick-is-atom? b) (not (brick-children b)))
(define (brick-modify-children f b) (list-replace b 1 (f (brick-children b))))
(define (brick-ghost b) (list-ref b 2))
(define (brick-modify-ghost f b) (list-replace b 2 (f (brick-ghost b))))
(define ghost-pos car)
(define ghost-size cadr)
(define empty-ghost (list #f 1))
(define (brick-clear-ghost b) (brick-modify-ghost (lambda (g) empty-ghost) b))
(define (brick-id b) (list-ref b 3))
(define (brick-text-prim b) (list-ref b 4))
(define (brick-depth b) (list-ref b 5))
(define (brick-locked b) (list-ref b 6)) ; for the palette
(define (brick-modify-locked f b) (list-replace b 6 (f (brick-locked b))))

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
              (brick-search child id)))
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
  (eval-string (string-append "'" (brick->text b))))

(define (brick->text b)
  (if (brick-is-atom? b)
      (brick-text b)
      (string-append
       (remove-last
        (string-append "(" (brick-text b) " "
                       (apply string-append
                              (map
                               (lambda (child)
                                 (string-append 
                                  (brick->text child) " "))
                               (brick-children b)))
                       ""))")")))

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

(define (brick-dock b new pos)
  (cond ((brick-locked b) b)
        (else
         (with-primitive 
          (brick-id new)
          (identity)
          (parent (brick-id b)))
         (brick-modify-children
          (lambda (children)
            (insert-to new (brick-pos->slot b pos) children))
          (brick-clear-ghost b)))))

(define (brick-undock b id)
  (cond ((brick-locked b) b)
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

;---------------------------------------------------------  

(define (make-bricks)
  (list '() (vector 0 0 0) #f #f #f))

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

(define (bricks-modify-brick fn b id)
  (bricks-modify-roots
   (lambda (roots)
     (map
      (lambda (root)
        (brick-modify-brick fn root id))
      roots))
   b))

(define (bricks-add-root b root)
  (bricks-modify-roots
   (lambda (roots)
     (cons root roots))
   b))

(define (bricks-remove-root b id)
  (bricks-modify-roots
   (lambda (roots)
     (filter (lambda (root) (not (eq? id (brick-id root)))) roots))
   b))

(define (bricks->text b)
  (apply
   string-append
   (map brick->text 
        (filter (lambda (brx)
                  (not (brick-locked brx)))
                (bricks-roots b)))))

(define (bricks->sexpr b)
  (apply list (map brick->sexpr (bricks-roots b))))

(define (bricks-add-clone b brick)
  (bricks-add-root b (code->brick (brick->code brick))))

(define (bricks-add-spawn b brick)
  (let ((copy (code->brick (brick->code brick)))
        ;; copy the transform
        (tx (with-primitive (brick-id brick) (get-global-transform))))
    (with-primitive (brick-id copy) (identity) (concat tx)) 
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

(define (bricks-search-for-parent b id)
  (foldl
   (lambda (b r)
     (if (not r) (brick-search-for-parent b id) r))
   #f
   (bricks-roots b)))

(define (bricks-mouse-down b)
  (and (mouse-button 1) (not (bricks-button b))))

(define (bricks-mouse-up b)
  (and (not (mouse-button 1)) (bricks-button b)))

(define (bricks-do-keys b)
  (cond
   ((key-pressed "x") 
    (broadcast 1 (bricks->text b))
    (eval-string (bricks->text b)
                 (lambda (error)
                   (broadcast 5 (exn-message error))))   
    ))
  b)

(define (bricks-do-input b pos)
  (bricks-do-keys
   (bricks-modify-mouse 
    (lambda (m) pos)
    (bricks-modify-button 
     (lambda (button) (mouse-button 1))
     ;; keep track of the selection
     (bricks-modify-current
      (lambda (current)
        (cond 
         ((bricks-mouse-down b) (println "mouse down") (bricks-get-over b pos))
         ((bricks-mouse-up b) (println "mouse up") #f)
         (else current)))
      ;; if we are dragging something 
      (if (bricks-current b)
          ;; find what is underneath
          (let* ((temp (bricks-get-over b (vadd pos (vector 0 drop-fudge 0))))
                 (over (if (and temp (brick-is-atom? temp)) #f temp))
                 (old-over (bricks-drop-over b)))
            (bricks-modify-drop-over
             (lambda (dropover) over)
             (if over
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
                 b)))
          b))))))
  
(define (bricks-drag-start? b new-b)
  (and (not (list? (bricks-current b)))
       (list? (bricks-current new-b))))

(define (bricks-drag-end? b new-b)
  (and (list? (bricks-current b))
       (not (list? (bricks-current new-b)))))

(define (bricks-over-out? b new-b)
  (and (list? (bricks-drop-over b))
       (not (list? (bricks-drop-over new-b)))))

(define (bricks-do-docking b new-b pos)
  ;; check for brick to dock
  (if (and (bricks-drag-end? b new-b)
           (bricks-drop-over new-b))
      ;; note - assume if drop over exists, then current must also exist
      (let* ((id (brick-id (bricks-current b)))
             (new-parent-id (brick-id (bricks-drop-over new-b))))
        (bricks-remove-root
         (bricks-modify-brick 
          (lambda (new-parent)
            (brick-dock new-parent (bricks-current b) pos))
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
            (if (brick-locked prnt)
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
 
(define (bricks-update! b)
  (let* ((pos (get-point-from-mouse)))
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

    ; move the current brick
    (when (list? (bricks-current b))
          (with-primitive 
           (brick-id (bricks-current b))
           (when (and (mouse-button 1) (bricks-button b))
                 (translate (vsub pos (bricks-mouse b))))))

    ; update the input stuff
    (bricks-remove-ghost
     b (bricks-do-docking  
        b (bricks-do-undocking 
           b (bricks-do-input b pos)) 
        pos))))

;---------------------------------------------------------  

(set-camera-transform (mtranslate (vector 0 0 -30)))

;(hint-wire)

(define b
  (bricks-add-code
   (bricks-add-code 
    (make-bricks) 
    '(begin (display "hello") (display (+ 1 2 3 4))))
   '(
     (define (z time a)
       (play (+ time 3) (mul (adsr 0 0.1 0 0) (sine 440)))
       (in time 0.1 z (+ a 1))
       )
     (define) 
     z time a () (play-now)
     (if (< a 10))
     (play (+ time 3) (mul (adsr 0 0.1 0 0) (sine 440)))
     (in (time-now) 0.1 z 0)
     0.1 1 2 3 (+ (+ 1 2) 3))))

;; activate pallette
(set! b (bricks-modify-brick
         (lambda (pallette)
           (brick-modify-locked (lambda (l) #t) pallette))
         b
         (brick-id (car (bricks-roots b)))))

(define (setup b loc)
  (with-primitive 
   (brick-id b)
   (translate loc))
  (brick-update! b 0))

(setup (car (bricks-roots b)) (vector 20 10 0))
(setup (cadr (bricks-roots b)) (vector 5 10 0))

           


(clear-colour (vector 0.5 0.2 0.1))

(define t (with-state 
           (translate (vector -28 -20 0))
           (scale (vector 1 1 1))
           (colour (vector 0 0.5 1)) (build-cube)))

(every-frame 
 (begin
   (set! b (bricks-update! b))
   (with-primitive t (rotate (vector 1 2 3)))))

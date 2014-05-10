(define (smartbomb c)
    (foreach
        (lambda (c)
            (smartbomb c)
            (with-primitive c
                (pdata-map! 
                    (lambda (p)
                        (vadd p (srndvec)))
                "p")))
        (get-children)))

(smartbomb 0)
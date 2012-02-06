(load "~~/lib/syntax-case")

(define (curry f . c) (lambda x (apply f (append c x))))
(define (apply-f f . fs)
  (lambda x
    (let loop
      ((rfs (cdr (reverse fs)))
       (current (apply (car (reverse fs)) x)))
      (if (null? rfs)
        (f current)
        (loop (cdr rfs) ((car rfs) current))))))
(define (reduce f lst)
  (let recur ((lst (cdr lst)) (acc (car lst)))
    (if (null? lst) acc
      (recur (cdr lst) (f acc (car lst))))))
(define (filter f lst)
  (let recur ((lst lst) (acc '()))
    (cond
      ((null? lst) (reverse acc))
      ((f (car lst)) (recur (cdr lst) (cons (car lst) acc)))
      (else (recur (cdr lst) acc)))))

(define-syntax ->>
  (syntax-rules
    ()
    ((_ s expr ...)
     (let-syntax
       ((expr-or-func
          (syntax-rules 
            ()
            ((_ (f default-arg (... ...)))
             (curry f default-arg (... ...)))
            ((_ f) f))))
       (let loop
         ((current s)
          (rest (list (expr-or-func expr) ...)))
         (if (null? rest)
           current
           (loop ((car rest) current) (cdr rest))))))))

(define opening-tag #\{)
(define closing-tag #\})

(define (strip-whitespace s)
  (->> s
       string->list
       (filter (apply-f not char-whitespace?))
       list->string))


(define (stream-ready? stream)
  (char-ready? stream))

(define (read-successful? stream f)
  (and 
    (stream-ready? stream) 
    (f (peek-char stream))))

(define (at-tag? tag stream)
  (read-successful? stream (curry eq? tag)))

(define at-opening-tag? (curry at-tag? opening-tag))
(define at-closing-tag? (curry at-tag? closing-tag))

(define (read-until stream f)
  (let ((output (open-string)))
    (let recur ()
      (cond 
        ((not (stream-ready? stream)) output)
        ((read-successful? stream f) output)
        (else
          (begin
            (write-char (read-char stream) output)
            (recur)))))))

(define (read-until-char stream char)
  (read-until stream (curry eq? char)))

(define (sread-until-char stream char)
  (get-output-string (read-until-char stream char)))

(define (read-next stream f)
  (if (read-successful? stream f)
    (read-char stream) #f))

(define (read-past stream f)
  (let* ((before (read-until stream f))
         (next (read-next stream f)))
    (if next 
      (begin
        (write-char next before) before)
      before)))

(define (read-past-char stream char)
  (read-past stream (curry eq? char)))

(define (classify-section-token stream token context)
  (let ((tsymbol (string->symbol token))
        (first-char 
          (->> token
               string->list
               car))
        (esymbol
          (->>
            token
            string->list
            cdr
            list->string
            string->symbol)))
    (case first-char
      ((#\#) (parse-section stream esymbol))
      ((#\/) (if (eq? esymbol context)
               #f
               (raise "Interpolated closing tags")))
      (else tsymbol))))

(define (parse-section stream key)
  (let recur ((tree '()))
    (cond
      ((not (stream-ready? stream)) (list key (reverse tree)))
      ((at-opening-tag? stream)
       (let ((tag (classify-section-token
                    stream
                    (parse-token stream) key)))
         (if (not tag)
           (list key (reverse tree))
           (recur (cons tag tree)))))
      (else 
        (recur
          (cons (sread-until-char stream opening-tag) tree))))))

(define (classify-token stream token)
  (let ((tsymbol (string->symbol token))
        (first-char 
          (->> token
               string->list
               car))
        (esymbol
          (->>
            token
            string->list
            cdr
            list->string
            string->symbol)))
    (case first-char
      ((#\#) (parse-section stream esymbol))
      (else tsymbol))))

(define (parse-token stream)
  (begin
    (read-past-char stream opening-tag)
    (read-past-char stream opening-tag)
    (let ((token
            (->>
              (read-until-char stream closing-tag)
              get-output-string
              strip-whitespace)))
      (read-past-char stream closing-tag)
      (read-past-char stream closing-tag)
      token)))

(define (parse stream)
  (let recur ((tree '()))
    (cond
      ((not (stream-ready? stream)) (reverse tree))
      ((at-opening-tag? stream)
       (recur
         (cons (classify-token stream (parse-token stream)) tree)))
      (else 
        (recur
          (cons (sread-until-char stream opening-tag) tree))))))

(define test-string (open-string "This is Great {{ #it-really-is }} {{ #i }} love to {{ eat }}  {{ #dogshit }} is a great food {{ /dogshit }} {{ /i }} shit {{ /it-really-is }} heh"))
(parse test-string)


(load "~~/lib/syntax-case")

(define (curry f . c) (lambda x (apply f (append c x))))
(define (reverse-arguments f) (lambda a (apply f (reverse a))))
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
      (recur (cdr lst) (f (car lst) acc)))))

(define (filter f lst)
  (let recur ((lst lst) (acc '()))
    (cond
      ((null? lst) (reverse acc))
      ((f (car lst)) (recur (cdr lst) (cons (car lst) acc)))
      (else (recur (cdr lst) acc)))))
(define (any? f lst)
  (cond
    ((null? lst) #f)
    ((f (car lst)) #t)
    (else (any? f (cdr lst)))))
(define (all? f lst)
  (cond
    ((null? lst) #t)
    ((f (car lst)) (all? f (cdr lst)))
    (else #f)))
(define (alist? lst)
  (all? pair? lst))

(define-syntax case-cond
  (syntax-rules
    (else)
    ((_ e (c r) ... (else d))
     (cond ((c e) r) ... (else d)))))

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

(define (truthy? term)
  (case-cond term
    (boolean? term)
    (null? #f)
    (else #t)))
(define (falsy? term)
  (not (truthy? term)))

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

(define self-node '|.|)
(define (get-assoc elem alist)
  (cadr (assoc elem alist)))
(define text-node? string?)
(define section-node? pair?)
(define self-node? (curry eq? self-node))
(define (make-list lst)
  (if (list? lst)
    lst
    `((,self-node ,lst))))

(define (get-alist-item fn alist)
  (and
    (list? alist)
    (pair? (car alist))
    (fn (car alist))))
(define get-alist-key (curry get-alist-item car))
(define get-alist-value (curry get-alist-item cadr))

(define (get-alist-assoc fn key alist)
  (and
    (alist? alist)
    (pair? (assoc key alist))
    (fn (assoc key alist))))
(define get-alist-avalue (curry get-alist-assoc cadr))

(define get-alist-arest (curry get-alist-assoc cdr))


(define (create-subcontext context key)
  (map 
    (curry (reverse-arguments append) context)
    (map
      make-list 
      (filter 
        truthy? 
        (or (get-alist-arest key context) '())))))

(define (render tree context)
  (let loop ((elems tree) (rest '()) (current-context context))
    (if (falsy? elems)
      (apply string-append (reverse rest))
      (let ((node (car elems))
            (remaining (cdr elems)))
        (cond
          ((text-node? node)
           (loop remaining (cons node rest) current-context))
          ((self-node? node)
           (let ((self-value (get-alist-value current-context)))
             (if self-value
               (loop remaining (cons self-value rest) current-context)
               (loop remaining rest current-context))))
          ((section-node? node)
           (let ((node-label (get-alist-key elems))
                 (sub-elems (get-alist-value elems)))
             (if (and node-label sub-elems)
               (loop remaining
                     (append
                       (reverse
                         (map (curry loop sub-elems '())
                              (create-subcontext current-context node-label)))
                       rest) current-context)
               (loop remaining rest current-context))))
           (else
             (let ((node-value (get-alist-avalue node current-context)))
               (if node-value
                 (loop remaining (cons node-value rest) current-context)
                 (loop remaining rest current-context)))))))))

(define test-string
  (open-string 
"<html>
    <head>
        <title>{{ title }} {{ john }}</title>
    </head>
    <body>
        <h1>{{ title }}</h1>
        <ul>
            {{ #list }}
            <li>
                {{ sectionheaderer }}
                {{ sectionheader }}
                {{ term }}: {{ description }}
            </li>
            {{ /list }}
        </ul>
        {{ #names }}{{ . }}, {{ /names }}
    </body>
</html>"))
(define tree (parse test-string))
(define context
  '((title "Fun languages")
    (sectionheader "Language")
    (list 
      ((term "Scheme")
       (description "A great functional language"))
      ((term "Python")
       (description "A nice high level scripting language"))
      ((term "Lua")
       (description "A well thought out, fast, simple, embedded language")))
    (names "Frank" "John" "Peter")
    ))

(print (render tree context))


(define t2 
  (parse (open-string "hello {{ #more }} {{ . }} {{ #sub }} {{ name }} {{ /sub }} {{ /more }} {{ label }}")))
(define c2
  '((label "maybe")
    (more
      "1" "2" "3")
    (sub
      ((name "a"))
      ((name "b"))
      ((name "c")))))

(print (render t2 c2))

(define t3 
  (parse (open-string 
"hello
{{ #person }}
{{ name }} 
   {{ #friends }}
   {{ count }}
   {{ #person }}
       {{ . }}
   {{/person }}
   {{/friends }}
{{/person }}")))
(define c3
  '((person
      ((name "rich")
       (friends
         ((count "2")
          (person 
            "frank"
            "john"))))
      ((name "ramin")
       (friends
         ((count "3")
          (person
            "goondor"
            "menthol"
            "birds"))))
      ((name "shoe")
       (friends
         ((count "5")
          (person
            "laces"
            "toes"
            "frogpe"
            "dogpe"
            "jogpe")))))))

(print (render t3 c3))



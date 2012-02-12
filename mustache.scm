(load "~~/lib/syntax-case")

; Higher order functions
;; Bind arguments to a function for delayed evaluation
(define (curry f . c) (lambda x (apply f (append c x))))
;; Reverse the order a function takes arguments
(define (reverse-arguments f) (lambda a (apply f (reverse a))))
;; Returns a function in which each function in fs is applied in reverse, with
;; the output chained to the next
(define (apply-f f . fs)
  (lambda x
    (let loop
      ((rfs (cdr (reverse fs)))
       (current (apply (car (reverse fs)) x)))
      (if (null? rfs)
        (f current)
        (loop (cdr rfs) ((car rfs) current))))))
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
(define (drop-while f lst)
  (if (or (null? lst)
          (not (f (car lst))))
    lst
    (drop-while f (cdr lst))))
(define (take-while f lst)
  (let recur ((lst lst) (acc '()))
    (if (or
          (null? lst)
          (not (f (car lst))))
      (reverse acc)
      (recur (cdr lst) (cons (car lst) acc)))))
;; Similar to any but operates on a list of truthy functions and one argument,
;; returning true if any of the functions evaluate true when given the argument
(define (any-f? arg fs)
  (cond
    ((null? fs) #f)
    (((car fs) arg) #t)
    (else (any-f? arg (cdr fs)))))
(define (all? f lst)
  (cond
    ((null? lst) #t)
    ((f (car lst)) (all? f (cdr lst)))
    (else #f)))
(define (alist? lst)
  (all? pair? lst))

; Macros
;; Case that uses procedures for truth instead of matching
(define-syntax case-cond
  (syntax-rules
    (else)
    ((_ e (c r) ... (else d))
     (cond ((c e) r) ... (else d)))))
;; Chain functions on an initial argument
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


; Tag procedures
(define opening-tag "{{")
(define closing-tag "}}")
(define section-delimiter #\#)
(define partial-delimiter #\>)
(define inverted-delimiter #\^)
(define closing-delimiter #\/)
(define comment-delimiter #\!)
(define set-delimiter #\=)

(define self-node '|.|)
; Creates function to check if two delimiters are the same
; ((matches-delimiter? #\#) #\#) -> #t
(define (matches-delimiter? d) (lambda (t) (eq? d t)))
; Cases based on known delimiters
(define section-delimiter? (matches-delimiter? section-delimiter))
(define partial-delimiter? (matches-delimiter? partial-delimiter))
(define inverted-delimiter? (matches-delimiter? inverted-delimiter))
(define closing-delimiter? (matches-delimiter? closing-delimiter))
(define comment-delimiter? (matches-delimiter? comment-delimiter))
(define set-delimiter? (matches-delimiter? set-delimiter))

; Tag is a cons pair ( delimiter . label )
(define get-tag-delimiter car)
(define get-tag-label cdr)
; Creates a function to check whether tag delimiters match, given a
; function derived from (matches-delimiter? ...)
; ((matches-tag? (matches-delimiter? #\#)) (cons #\# 'label)) -> #t
(define (matches-tag? fn) 
  (lambda (t)
    (and (pair? t) (fn (get-tag-delimiter t)))))
(define section-tag? (matches-tag? section-delimiter?))
(define partial-tag? (matches-tag? partial-delimiter?))
(define inverted-tag? (matches-tag? inverted-delimiter?))
(define closing-tag? (matches-tag? closing-delimiter?))
(define comment-tag? (matches-tag? comment-delimiter?))

(define set-tag? (matches-tag? set-delimiter?))

; Node is a cons pair ( tag . subtree )
; Creates a function to check whether node tags match, given a
; function derived from (matches-tag? ...)
; ((matches-node?
;   (matches-tag?
;     (matches-delimiter? #\#)))
; (cons (cons #\# 'label) '("tree" "elements"))) -> #t
(define get-node-tag car)
(define get-node-tree cdr)
(define (get-node-delimiter n)
  (get-tag-delimiter (get-node-tag n)))
(define (get-node-label n)
  (get-tag-label (get-node-tag n)))
(define (matches-node? fn)
  (lambda (n)
    (and (pair? n) (fn (get-node-tag n)))))
(define section-node? (matches-node? section-tag?))
(define partial-node? (matches-node? partial-tag?))
(define inverted-node? (matches-node? inverted-tag?))
(define closing-node? (matches-node? closing-tag?))
(define comment-node? (matches-node? comment-tag?))
(define text-node? string?)
(define self-node? (curry eq? self-node))


; String procedures
(define (strip-whitespace s)
  (->> s
       string->list
       (filter (apply-f not char-whitespace?))
       list->string))
(define (strip-edge-f fn str)
  (->> str
       string->list
       (drop-while fn)
       reverse
       (drop-while fn)
       reverse
       list->string))
(define strip-edge-whitespace 
  (curry strip-edge-f char-whitespace?))

; Stream procedures

(define (stream-ready? stream)
  (and 
    (char-ready? stream)
    (not (eof-object? (peek-char stream)))))
(define (read-until-string stream str)
  (let ((output (open-string))
        (target (string->list str)))
    (let loop ((buf '()))
      (cond
        ((equal? buf target)
         (cons #t (get-output-string output)))
        ((not (stream-ready? stream))
         (do ((tbuf buf (cdr tbuf)))
           ((null? tbuf) (cons #f (get-output-string output)))
           (write-char (car tbuf) output)))
        (else
          (if (< (length buf) (length target))
            (loop (append buf (list (read-char stream))))
            (begin
              (write-char (car buf) output)
              (loop (append (cdr buf) (list (read-char stream)))))))))))

; Parsing
;; Is a string surrounded in set delimiters?
(define (set-delimiters? str)
  (let* ((stripped (strip-edge-whitespace str))
         (stripped-list (string->list stripped)))
    (and (set-delimiter? (car stripped-list))
         (set-delimiter? (car (reverse stripped-list)))
         (any? char-whitespace? stripped-list))))
;; Parse new opening and closing tags from a set delimiter string
(define (make-set-delimiters str)
  (let* ((stripped
           (strip-edge-f set-delimiter? (strip-edge-whitespace str)))
         (stripped-list (string->list stripped)))
    (cons (list->string
            (take-while
              (apply-f not char-whitespace?)
              stripped-list))
          (list->string
            (reverse
              (take-while
                (apply-f not char-whitespace?)
                (reverse stripped-list)))))))
;; Create a tag pair from a tag string
(define (classify-token token)
  (let ((tsymbol 
          (string->symbol (strip-whitespace token)))
        (ctoken (strip-edge-whitespace token)))
    (if (> (string-length token) 0)
      (let ((first-char 
              (->> ctoken
                   string->list
                   car))
            (esymbol
              (->>
                ctoken
                string->list
                cdr
                list->string
                string->symbol)))
        (cond
          ((set-delimiters? ctoken)
           (cons first-char (make-set-delimiters ctoken)))
          ((any-f? 
             first-char
             (list section-delimiter?
                   partial-delimiter?
                   inverted-delimiter?
                   closing-delimiter?
                   comment-delimiter?))
           (cons first-char esymbol))
          (else tsymbol)))
        tsymbol)))

;; Create a parse tree
(define (parse-section openingd closingd stream key)
  (let recur ((openingd openingd)
              (closingd closingd)
              (tree '())
              (at-tag #f))
    (let ((parse-subsection (curry parse-section openingd closingd))
          (next (curry recur openingd closingd)))
      (if (not (stream-ready? stream))
        (if key
          (cons key (reverse tree))
          (reverse tree))
        (if at-tag
          (let* ((tag (read-until-string stream closingd)) 
                 (tag-status (car tag))
                 (tag-text (cdr tag)))
            (if tag-status
              (let ((token (classify-token tag-text)))
                (case-cond token
                  (set-tag?
                    (recur (cadr token) (cddr token) tree #f))
                  (comment-tag?
                    (next tree #f))
                  (section-tag?
                    (next
                      (cons (parse-subsection stream token) tree) #f))
                  (inverted-tag?
                    (next
                      (cons (parse-subsection stream token) tree) #f))
                  (closing-tag?
                    (if (eq? (get-tag-label token) (get-tag-label key))
                      (cons key (reverse tree))
                      (raise "Interpolated closing tags")))
                  (else (next (cons token tree) #f))))
              (next (cons tag-text tree) #f)))
          (let* ((tag (read-until-string stream openingd))
                 (tag-status (car tag))
                 (tag-text (cdr tag)))
            (next (cons tag-text tree) tag-status)))))))
(define (parse stream)
  (parse-section opening-tag closing-tag stream #f))

; Procedures for dealing with contexts
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
(define (in-proc-context? context)
  (procedure? (get-alist-key context)))

(define (create-proccontext context key)
  (let ((proc (get-alist-avalue key context)))
    (and (procedure? proc)
         (cons (list proc proc) context))))

(define (create-subcontext context key)
  (map (curry (reverse-arguments append) context)
       (map make-list
            (filter truthy? (or (get-alist-arest key context) '())))))

; Render a tree with a given context
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
           (let* ((node-label (get-node-label node))
                 (sub-elems (get-node-tree node))
                 (proc-context (create-proccontext current-context node-label)))
             (if (and node-label sub-elems)
               (if proc-context
                 (loop remaining
                       (cons (render sub-elems proc-context) rest) current-context)
                 (loop remaining
                       (append
                         (reverse
                           (map (curry loop sub-elems '())
                                (create-subcontext current-context node-label)))
                         rest) current-context))
               (loop remaining rest current-context))))
          ((inverted-node? node)
           (let* ((node-label (get-node-label node)) 
                  (sub-elems (get-node-tree node))
                  (node-value (get-alist-avalue node-label current-context)))
             (if (not node-value)
               (loop remaining
                     (cons (render sub-elems current-context) rest)
                     current-context)
               (loop remaining rest current-context))))
          ((partial-tag? node)
           (loop remaining 
                 (cons (render (eval (get-node-tree node)) current-context) rest)
                 current-context))
          (else
            (let ((node-value (get-alist-avalue node current-context)))
              (if node-value
                (loop remaining
                      (cons 
                        (if (in-proc-context? current-context)
                          ((get-alist-value current-context) node-value)
                          node-value)
                        rest) current-context)
                (loop remaining rest current-context)))))))))
(define (render-file filename context)
  (render (parse (open-input-file filename)) context))

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
                {{ !this-is-the-header }}
                {{ sectionheader }}
                {{ term }}: {{ description }}
            </li>
            {{ /list }}
            {{ ^lister }}
                Negated
            {{ /lister }}
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

(println (render tree context))


(define t3 
  (parse (open-string 
" hello
{{ !ignore-this-please }}
{{ #person }}
{{ #reversed }}
{{ name }} 
{{ /reversed }}
   {{ #friends }}
   {{ count }}
   {{ #person }}
       {{ . }}
   {{/person }}
   {{/friends }}
{{/person }}")))

(define c3
  `((person
      ((name "name a")
       (friends
         ((count "2")
          (person 
            "friend a"
            "friend b"))))
      ((name "name b")
       (friends
         ((count "3")
          (person
            "friend c"
            "friend d"
            "friend e"))))
      ((name "name c")
       (friends
         ((count "1")
          (person
            "friend f")))))
    (reversed ,(apply-f list->string reverse string->list))
    ))

(println (render t3 c3))

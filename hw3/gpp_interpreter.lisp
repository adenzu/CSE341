;;===MACROS===;;

(defconstant true '(1 1))
(defconstant false '(0 1))

;;===MACROS===;;


;;===SYNTAX ELEMENTS===;;

(defconstant keyword-head "KW_")
(defconstant keywords
    '(
        ;; keyword
        "and"
        "or"
        "not"
        "equal"
        "less"
        "greater"
        "list"
        "append"
        "concat"
        "set"
        "deffun"
        "for"
        "if"
        "exit"
        "while"
        "true"
        "false"
        "progn"
    )
)

;;---;;

(defconstant operator-head "OP_")
(defconstant operators 
    '(
        ;; (operator token)
        ("+" "PLUS")
        ("-" "MINUS")
        ("/" "DIV")
        ("*" "MULT")
        ("(" "OP")
        (")" "CP")
        ("**" "DBLMULT")
        ("," "COMMA")
    )
)

;;---;;

(defconstant white-spaces
    '(
        ;; only one character
        #\space
        #\tab
        #\newline
    )
)

;;---;;

(defconstant zero #\0)

(defconstant digits
    '(
        ;; only one character
        #\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
    )
)

;;---;;

(defconstant letters
    '(
        ;; (lowercase uppercase)
        (#\a #\A)
        (#\b #\B)
        (#\c #\C)
        (#\d #\D)
        (#\e #\E)
        (#\f #\F)
        (#\g #\G)
        (#\h #\H)
        (#\i #\I)
        (#\j #\J)
        (#\k #\K)
        (#\l #\L)
        (#\m #\M)
        (#\n #\N)
        (#\o #\O)
        (#\p #\P)
        (#\q #\Q)
        (#\r #\R)
        (#\s #\S)
        (#\t #\T)
        (#\u #\U)
        (#\v #\V)
        (#\w #\W)
        (#\x #\X)
        (#\y #\Y)
        (#\z #\Z)
    )
)

;;===SYNTAX ELEMENTS===;;


;;===UTIL===;;

(defun elast (l)
    (car (last l))
)

(defun char-index (str c &optional (i 0))
    (if
        (char= (char str i) c)
        i
        (char-index str c (+ i 1))
    )
)

(defun as-second (l x)
    (if
        (equal (second l) x)
        l
        (as-second (cdr l) x)
    )
)

;;===UTIL===;;


;;===BUILDING LEXER===;;

;; DFA graphs (hash-tables)
(setq base-dfa (make-hash-table ))
(setq comment-dfa (make-hash-table ))
(setq integer-dfa (make-hash-table ))
(setq fraction-dfa (make-hash-table ))
(setq identifier-dfa (make-hash-table ))
(setq string-dfa (make-hash-table ))

;; The structure of the DFA is one that only takes one character as input (key) and returns a list as the output (value).
;; The returned list always has the next state (sub-graph or sub-hash-table) as the first element and the token
;; that would be returned on an undefined input in that state.
;; If output of a character is not a list it is returned as a list with default values. If its the state alone
;; then the token is set to nil and the output list becomes "(state nil)". If its only the token then the state
;; is set as the base state which is base-dfa thus output list is "(base-dfa token)".
;; With this design after character input is given to current state the next state and corresponding token is
;; returned and the current state gets to update. 

;;---;;

(defun create&get-hash-table (k ht)
    "
    Returns the final value of the given key k in given hashtable ht.
    If the value is not a hashtable it is changed to a hashtable with previous value being value of the key 'default.
    If the value does not exist it is set to an empty hashtable.

    ex:
        k <- 'key'
        ht: (
            'key': 5
        )
        ...
        (create&get-hash-table k ht)
        ...
        ht: (
            'key': (
                'default: 5
            )
        )
    "
    (when (not (gethash k ht))
        (setf (gethash k ht) (make-hash-table ))
    )
    (when (not (equal (type-of (gethash k ht)) 'hash-table))
        (progn
            (setq dv (gethash k ht))
            (setq dht (make-hash-table ))
            (setf (gethash 'default dht) dv)
            (setf (gethash k ht) dht)
        )
    )
    (gethash k ht)
)

(defun string-to-list (kw)
    "Returns the list of characters in the given string"
    (setq l '())
    (loop for i from 0 to (- (length kw) 1)
        do (setq l (cons (char kw i) l))
    )
    (reverse l)
)

(defun string-to-base-dfa (s &optional value)
    "
    Adds given string's characters to base-dfa with prior characters being parents of following ones.
    
    ex:
        s <- 'test'
        value <- 'TOKEN'
        ...
        (string-to-base-dfa s value)
        ...
        base-dfa: (
            ...
            #\t: (
                #\e: (
                    #\s: (
                        #\t: 'TOKEN'
                    )
                )
            )
            ...
        )
    "
    (setq char-list (string-to-list s))
    (setq first-char (car char-list))
    (if (> (length char-list) 1)
        (progn
            (setq curr-map (create&get-hash-table first-char base-dfa))
            (loop for curr-char in (cdr (reverse (cdr (reverse char-list))))
                do (setq curr-map (create&get-hash-table curr-char curr-map))
            )
            (when (> (length char-list) 1) (setf (gethash (car (reverse char-list)) curr-map) value))
        )
        (setf (gethash (car char-list) base-dfa) value)
    )
)

(defun keyword-to-base-dfa (kw)
    "Adds given keyword <kw> to base-dfa with it's last character having value of the corresponding keyword token"
    (string-to-base-dfa kw (concatenate 'string keyword-head (string-upcase kw)))
)

(defun operator-to-base-dfa (op)
    "Adds given operator <op> to base-dfa with it's last character having value of the corresponding operator token"
    (string-to-base-dfa (car op) (concatenate 'string operator-head (string-upcase (car (last op)))))
)

(defun recursive-state-connector (ht keys defval)
    "Recursively sets all values of given keys to defval in given hashtable and all sub hashtables of it"
    (loop for k in keys
        do 
        (case (type-of (gethash k ht))
            ('null (setf (gethash k ht) defval))
            ('hash-table 
                (progn
                    (when (null (gethash 'default (gethash k ht))) (setf (gethash 'default (gethash k ht)) defval))
                    (recursive-state-connector (gethash k ht) keys defval)
                )
            )
            (t
                (progn
                    (setq kht (create&get-hash-table k ht))
                    (recursive-state-connector kht keys defval)
                )
            )
        )
    )
)

;;---;;

(defconstant syntax-error-token "SYNTAX_ERROR")
(defconstant white-space-token "WHITE_SPACE")
(defconstant comment-token "COMMENT")
(defconstant integer-token "VALUEI")
(defconstant fraction-token "VALUEF")
(defconstant string-token "VALUESTR")
(defconstant identifier-token "ID")

(defun build-keywords ()
    "Build the keyword related dfa part of the lexer"
    (loop for kw in keywords
        do (keyword-to-base-dfa kw)
    )
)

(defun build-operators ()
    "Build the operator related dfa part of the lexer"
    (loop for op in operators
        do (operator-to-base-dfa op)
    )
)

(defun build-comment ()
    "Build the comment related dfa part of the lexer
    ;;[^\n]*\n"
    (setf (gethash #\; base-dfa) comment-dfa)
    (setq dm (create&get-hash-table #\; comment-dfa))
    (setf (gethash 'default dm) (lambda () dm))
    (setf (gethash #\newline dm) comment-token)
)

(defun build-integer ()
    "Build the integer related dfa part of the lexer
    0|[1-9][0-9]*"
    (setq zht (create&get-hash-table #\0 base-dfa))
    (setf (gethash 'default zht) integer-token)

    (setq dwz '())
    (loop for dgt in digits
        do (progn
            (setf (gethash dgt zht) syntax-error-token)
            (when (not (equal dgt zero)) (setq dwz (cons dgt dwz)))
        )
    )

    (loop for dgt in dwz
        do (setf (gethash dgt base-dfa) integer-dfa)
    )

    (loop for dgt in digits
        do (setf (gethash dgt integer-dfa) (lambda () integer-dfa))
    )

    (setf (gethash 'default integer-dfa) integer-token)
)

(defun build-fraction ()
    "Build the fraction related dfa part of the lexer
    {int}f[1-9][0-9]*"
    (setq fht0 (create&get-hash-table 0 fraction-dfa))
    (setq fht1 (create&get-hash-table 1 fraction-dfa))
    (setq fht2 (create&get-hash-table 2 fraction-dfa))

    (setq zht (create&get-hash-table #\0 base-dfa))
    (setf (gethash #\f zht) fht0)
    (setf (gethash #\f integer-dfa) fht0)

    (setq dwz '())
    (loop for dgt in digits
        do (when (not (equal dgt zero))
            (setq dwz (cons dgt dwz))
        )
    )

    (loop for dgt in dwz
        do (setf (gethash dgt fht0) fht1)
    )

    (loop for dgt in digits
        do (setf (gethash dgt fht1) (lambda () fht1))
    )

    (setf (gethash 'default fht1) fraction-token)
)

(defun build-identifier ()
    "Build the identifier related dfa part of the lexer
    (_|[a-zA-Z])([a-zA-Z]|[0-9]|_)*"
    (setf (gethash #\_ base-dfa) identifier-dfa)
    (setf (gethash #\_ identifier-dfa) (lambda () identifier-dfa))

    (setq lts '())
    (loop for l in letters 
        do (progn
            (setq lts (cons (car l) lts))
            (setq lts (cons (car (last l)) lts))
        )
    )
    (loop for l in lts 
        do (setf (gethash l identifier-dfa) (lambda () identifier-dfa))
    )

    (recursive-state-connector base-dfa lts identifier-dfa)

    (loop for d in digits
        do 
        (progn
            (setq dht (create&get-hash-table d base-dfa))
            (setf (gethash d identifier-dfa) (lambda () identifier-dfa))
            (loop for l in lts 
                do (setf (gethash l dht) syntax-error-token)
            )
        )
    )

    (setf (gethash 'default identifier-dfa) identifier-token)
)

(defun build-string ()
    "
    Build the string related dfa part of the lexer
    \".*\""
    
    (setq sht0 (make-hash-table ))

    (setf (gethash #\" base-dfa) (list string-dfa "OP_OC"))
    (setf (gethash 'default string-dfa) (lambda () string-dfa))
    (setf (gethash #\" string-dfa) (list base-dfa "OP_CC" string-token))
)

(defun build-lexer ()
    "Builds lexer DFA, the order of build functions' execution are important and affecting"
    (format t "Building lexer...") (terpri )
    (build-keywords)    (format t "Built keyword tokenization...") (terpri )
    (build-operators)   (format t "Built operator tokenization...") (terpri )
    (build-comment)     (format t "Built comment tokenization...") (terpri )
    (build-integer)     (format t "Built integer tokenization...") (terpri )
    (build-identifier)  (format t "Built identifier tokenization...") (terpri )
    (build-fraction)    (format t "Built fraction tokenization...") (terpri )
    (build-string)      (format t "Built string tokenization...") (terpri )
    (format t "Built lexer.") (terpri ) (terpri )
)

;;===BUILDING LEXER===;;



;;===LEXER FUNCTIONS===;;

(defun state-token-lister (st &optional deftoken)
    "
    Returns list of two elements with first element being the corresponding state and the second being the corresponding token.
    If no token is found given deftoken is returned as token.
    If no state is found base-dfa is returned as state.
    "
    (case (type-of st)
        ('cons          (values-list st))
        ('hash-table    (values st deftoken))
        (t              (values base-dfa st))
    )
)

(defun state-token-func-lister (f &optional deftoken)
    "
    Evaluates f if it's a function before feeding it to state-token-lister.
    This is needed to be able to make recursive hash-tables, since they can't contain themselves and need to have
    lambda functions to refer to themselves.
    "
    (if (equal (type-of f) 'function)
        (state-token-lister (funcall f) deftoken)
        (state-token-lister f deftoken)
    )
)

(defun get-state-value (k ht &optional deftoken)
    "
    Return next state and the corresponding token of given character k. With current state being ht.
    If no token was found deftoken is returned as the token.
    "
    (state-token-func-lister (gethash k ht) deftoken)
)

(defun get-next-state (k ht &optional pt)
    "
    Return next state and the corresponding token of given character k. With current state being ht.
    pt argument is used internally and should be avoided. It is used for keeping and returning the token
    returned just one state before the returned token.  
    "
    (if (null (gethash k ht)) 
        (if (null (gethash 'default ht))
            (values base-dfa (if (is-white-space k) white-space-token syntax-error-token) pt)
            (progn
                (multiple-value-setq (st dt dpt) (state-token-func-lister (gethash 'default ht)))
                (if (equal ht st)
                    (values st (or pt dpt))
                    (get-next-state k st dt)
                )
            )
        )
        (progn
            (multiple-value-setq (state token dpt) (get-state-value k ht))
            (values state token (or pt dpt))
        )
    )
)

(defun is-white-space (c)
    "Checks if given character c is an element of list white-spaces."
    (loop for ws in white-spaces
        do (when (char= ws c) (return-from is-white-space t))
    )
    (return-from is-white-space nil)
)

(defun print-token (token cl &optional tf)
    "
    Prints the read word and the corresponding token where cl is the list of characters of the read word and token is the token.
    tf is the function to stop printing read word if such thing is needed, it takes the current character of the cl to be printed
    as argument and returns t to stop printing it, and returns nil to continue.
    Returns printed result as a list. 
    "
    (setq result (make-array 0
                                        :element-type 'character
                                        :fill-pointer 0
                                        :adjustable t))
    (when (null tf) (setq tf (lambda (c) nil)))
    (loop for src in cl
        do 
        (progn
            (when (funcall tf src) (return ))
            (vector-push-extend src result)
        )
    )
    ;; (format t token) (terpri )
    (string result)
)

;;===LEXER FUNCTIONS===;;


;;===MAIN LEXER FUNCTION===;;

(defun gpplexer ()
    "G++ Lexer"

    ;; ready stream to be read
    (setq in-str nil)
    (setq filename (car *args*))
    (when (not (null filename)) (setq in-str (open filename)))

    ;; no syntax error has occured
    (setq seo nil)

    ;; initialize the list to returned upon finishing this function call
    (setq string-token-list '())

    ;; initialize list of read chars between tokens
    (setq read-chars '())

    ;; intialize initial state
    (setq curr-state base-dfa)

    ;; repl
    (loop
        (when (not (or (null in-str) (listen in-str))) (progn (format t "File is fully tokenized, exiting.") (return )))
        (setq rc (read-char in-str))
        (setq read-chars (cons rc read-chars))
        (multiple-value-setq (curr-state token prev-token) (get-next-state rc curr-state))
        (cond
            ((and (not (equal token syntax-error-token)) seo)
                (progn
                    (format t "SYNTAX_ERROR ")
                    (loop for src in (reverse (cdr read-chars))
                        do (write-char src)
                    )
                    (format t " cannot be tokenized") (terpri )
                    (return )
                )
            )
            ((is-white-space rc)
                (progn
                    (when (not (or (null prev-token) (equal prev-token comment-token))) 
                        (setq string-token-list 
                            (cons 
                                (list (print-token prev-token (reverse (cdr read-chars))) prev-token) 
                                string-token-list
                            )
                        )
                    )
                    (when (not (or (null token) (equal token white-space-token) (equal token comment-token))) 
                        (setq string-token-list 
                            (cons 
                                (list (print-token token (reverse (cdr read-chars)) (lambda (c) (is-white-space c))) token)
                                string-token-list
                            )
                        )
                    )
                    (when (not (null token)) (setq read-chars '()))
                )
            )
            ((null token))
            ((equal token syntax-error-token) (setq seo t))
            (t  
                (progn
                    (when (not (or (null prev-token) (equal token comment-token))) 
                        (progn 
                            (setq string-token-list 
                                (cons 
                                    (list (print-token prev-token (reverse (cdr read-chars))) prev-token)
                                    string-token-list
                                )
                            )
                            (setq read-chars (last (reverse read-chars)))
                        )
                    )
                    (when (not (equal token comment-token))
                        (setq string-token-list 
                            (cons 
                                (list (print-token token (reverse read-chars)) token)
                                string-token-list
                            )
                        )
                    )
                    (setq read-chars '())
                )
            )
        )
        (when (char= rc #\newline) (return ))
    )

    ;; return 
    (reverse string-token-list)
)

;;===MAIN LEXER FUNCTION===;;


;;===VARIABLE AND FUNCTION STORING===;;

(setq cfg-vars (list (make-hash-table :test 'equal)))
(setq cfg-funcs (make-hash-table :test 'equal))

;;===VARIABLE AND FUNCTION STORING===;;


;;===CFG UTILS===;;

(defun open-scope ()
    (setq cfg-vars (cons (make-hash-table :test 'equal) cfg-vars))
)

(defun close-scope ()
    (setq cfg-vars (cdr cfg-vars))
)

(defun get-var (var-name &optional (l cfg-vars))
    (when 
        (not (null l))
        (progn
            (setq var-val (gethash var-name (car l)))
            (if
                var-val
                var-val
                (get-var var-name (cdr l))
            )
        )
    )
)

(defun set-var (var-name var-value)
    (setf (gethash var-name (car cfg-vars)) var-value)
)

(defun get-func (func-name)
    (gethash func-name cfg-funcs)
)

(defun set-func (func-name func-params func-exps func-strings)
    (setf (gethash func-name cfg-funcs) (list func-params func-exps func-strings))
)

(defun b2f (b)
    (if
        b
        true
        false
    )
)

(defun f2b (f)
    (not (equal f false))
)

(defun p2f (p)
    (concatenate 'string (write-to-string (first p)) "f" (write-to-string (second p)))
)

(defun f2p (f)
    (setq i (char-index f #\f))
    (list
        (parse-integer (subseq f 0 i))
        (parse-integer (subseq f (+ i 1)))
    )
)

(defun f-reduce (f)
    (setq f-gcd (gcd (first f) (second f)))
    (mapcar #'(lambda (x) (/ x f-gcd)) f)
)

(defun f-not (f)
    (if
        (f2b f)
        false
        true
    )
)

(defun f-sum (f1 f2)
    (setq rf1 (second f1))
    (setq rf2 (second f2))
    (setq ef1 (mapcar #'(lambda (d) (* rf2 d)) f1))
    (setq ef2 (mapcar #'(lambda (d) (* rf1 d)) f2))
    (f-reduce (list (+ (first ef1) (first ef2)) (second ef1)))
)

(defun f-sub (f1 f2)
    (setq rf1 (second f1))
    (setq rf2 (second f2))
    (setq ef1 (mapcar #'(lambda (d) (* rf2 d)) f1))
    (setq ef2 (mapcar #'(lambda (d) (* rf1 d)) f2))
    (f-reduce (list (- (first ef1) (first ef2)) (second ef1)))
)

(defun f-mul (f1 f2)
    (f-reduce (list (* (first f1) (first f2)) (* (second f1) (second f2))))
)

(defun f-div (f1 f2)
    (f-mul f1 (reverse f2))
)

(defun f-less (f1 f2)
    (b2f 
        (<
            (/ (first f1) (second f1))
            (/ (first f2) (second f2))
        )
    )
)

(defun f-greater (f1 f2)
    (b2f 
        (>
            (/ (first f1) (second f1))
            (/ (first f2) (second f2))
        )
    )
)

(defun f-equal (f1 f2)
    (b2f
        (=
            (/ (first f1) (second f1))
            (/ (first f2) (second f2))
        )
    )
)

(defun get-param-ids (getter)
    (let
        (
            (curr-param (funcall getter))
        )
        (when
            curr-param
            (cons curr-param (get-param-ids getter))
        )
    )
)

(defun init-args (names vals)
    (when
        names
        (progn
            (set-var (car names) (car vals))
            (init-args (cdr names) (cdr vals))
        )
    )
)

;;===CFG UTILS===;;


;;===CFG ACTIONS===;;

(setq exp-stack '())
(setq saved-exps '())
(setq string-stack '())
(setq saved-strings '())

(defun pop-exp-stack ()
    (setq e (car exp-stack))
    (setq exp-stack (cdr exp-stack))
    e
)

(defun eval-next-exp (&optional (e T))
    (setq cur-exp (car exp-stack))
    (setq saved-exps (append saved-exps (list cur-exp)))
    (funcall cur-exp e)
)

(defun pass-next-exp ()
    (eval-next-exp nil)
)

(defun pop-string-stack ()
    (setq s (car string-stack))
    (setq string-stack (cdr string-stack))
    (setq saved-strings (append saved-strings (list s)))
    s
)

(defun cfg-id (&optional (e T))
    (pop-exp-stack)
    (setq s (pop-string-stack))
    (when e (get-var s))
)

(defun cfg-valuef (&optional (e T))
    (pop-exp-stack)
    (setq s (pop-string-stack))
    (when e (f2p s))
)

(defun cfg-true (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    true
)

(defun cfg-false (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    false
)

(defun cfg-not (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (result
                (if 
                    e
                    (f-not (eval-next-exp e))
                    (eval-next-exp e)
                )
            )
        )
        (progn
            (pop-string-stack)
            result
        )
    )
)

(defun cfg-sum (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (lo (eval-next-exp e))
            (ro (eval-next-exp e))
            (_ (pop-string-stack))
        )
        (when e (f-sum lo ro))
    )
)

(defun cfg-sub (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (lo (eval-next-exp e))
            (ro (eval-next-exp e))
            (_ (pop-string-stack))
        )
        (when e (f-sub lo ro))
    )
)

(defun cfg-mul (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (lo (eval-next-exp e))
            (ro (eval-next-exp e))
            (_ (pop-string-stack))
        )
        (when e (f-mul lo ro))
    )
)

(defun cfg-div (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (lo (eval-next-exp e))
            (ro (eval-next-exp e))
            (_ (pop-string-stack))
        )
        (when e (f-div lo ro))
    )
)

(defun cfg-exit (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (when e (exit))
    (pop-string-stack)
)

(defun cfg-set (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (var-name (pop-string-stack))
            (var-value (eval-next-exp e))
            (_ (pop-string-stack))
        )
        (when e (set-var var-name var-value))
    )
)

(defun cfg-less (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (lo (eval-next-exp e))
            (ro (eval-next-exp e))
            (_ (pop-string-stack))
        )
        (when e (f-less lo ro))
    )
)

(defun cfg-greater (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (lo (eval-next-exp e))
            (ro (eval-next-exp e))
            (_ (pop-string-stack))
        )
        (when e (f-greater lo ro))
    )
)

(defun cfg-equal (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (lo (eval-next-exp e))
            (ro (eval-next-exp e))
            (_ (pop-string-stack))
        )
        (when e (f-equal lo ro))
    )
)

(defun cfg-if (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (if-cond (f2b (eval-next-exp e)))
        )
        (let
            (
                (b1 (eval-next-exp (and e if-cond)))
                (b2 (eval-next-exp (and e (not if-cond))))
            )
            (progn
                (pop-string-stack)
                (if 
                    if-cond
                    b1
                    b2
                )
            )
        )
    )
)

(defun cfg-progn (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (result (eval-next-exp e))
        )
        (progn
            (pop-string-stack)
            result
        )
    )
)

(defun cfg-while (&optional (e T))
    (setq exp-stack-copy (copy-list exp-stack))
    (setq string-stack-copy (copy-list string-stack))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (while-cond (f2b (eval-next-exp e)))
        )
        (let
            (
                (result (eval-next-exp (and while-cond e)))
            )
            (if 
                (and e while-cond) 
                (progn
                    (setq exp-stack exp-stack-copy)
                    (setq string-stack string-stack-copy)
                    (setq sub-result (eval-next-exp e))
                    (if 
                        sub-result
                        sub-result
                        result
                    )
                )
                (progn
                    (pop-string-stack)
                    result
                )
            )
        )
    )
)

(defun cfg-fcall (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (let
        (
            (func-name (pop-string-stack))
        )
        (let
            (
                (func-info (get-func func-name))
            )
            (progn
                (setq result
                    (if 
                        e
                        (if
                            func-info
                            (progn
                                (open-scope)
                                (init-args (first func-info) (eval-next-exp e))
                                (setq exp-stack (append (second func-info) exp-stack))
                                (setq string-stack (append (third func-info) string-stack))
                                (setq inner-result (eval-next-exp e))
                                (close-scope)
                                inner-result
                            )
                            (progn
                                (format t "Syntax Error: Unknown identifier.") (terpri )
                                (exit)
                            )
                        )
                        (eval-next-exp e)
                    )
                )
                (pop-string-stack)
                result
            )
        )
    )
)

(defun cfg-def (&optional (e T))
    (pop-exp-stack)
    (pop-string-stack)
    (pop-string-stack)
    (let
        (
            (func-name (pop-string-stack))
            (_ (pop-string-stack))
            (func-params 
                (get-param-ids 
                    (lambda () 
                        (progn
                            (setq s (pop-string-stack))
                            (when 
                                (not (equal s ")"))
                                s
                            )
                        )
                    )
                )
            )
        )
        (progn
            (setq saved-exps '())
            (setq saved-strings '())
            (eval-next-exp nil)
            (let
                (
                    (func-exps saved-exps)
                    (func-strings saved-strings)
                )
                (progn
                    (set-func func-name func-params func-exps func-strings)
                    (pop-string-stack)
                )
            )
        )
    )
    false
)

(defun cfg-explist (&optional (e T))
    (pop-exp-stack)
    (eval-next-exp e)
    (eval-next-exp e)
)

(defun cfg-narg (&optional (e T))
    (pop-exp-stack)
    nil
)

(defun cfg-arg (&optional (e T))
    (pop-exp-stack)
    (list (eval-next-exp e))
)

(defun cfg-args (&optional (e T))
    (pop-exp-stack)
    (let
        (
            (ne (eval-next-exp e))
        )
        (cons ne (eval-next-exp e))
    )
)

(defun eval-exp-list (exp-list string-list)
    (setq exp-stack exp-list)
    (setq string-stack string-list)
    (funcall (car exp-stack))
)

;;===CFG ACTIONS===;;


;;===CFG===;;

(defconstant cfg 
    (list
        ;; (symbol rule1 rule2 ...)
        (list                           "start" 
            (list                           "input"))
        (list                           "input" 
            (list                           "explist"))
        (list                           "explist" 
            (list                           "exp")
            (list (function cfg-explist)    "exp" "explist"))
        (list                           "params" 
            (list                           "empty")
            (list                           "ID" "params"))
        (list                           "args"
            (list (function cfg-narg)       "empty")
            (list (function cfg-arg)        "exp")
            (list (function cfg-args)       "exp" "args"))
        (list                           "empty"
                                            nil)
        (list                           "exp" 																
			(list (function cfg-id)         "ID"                                                            )												
			(list (function cfg-valuef)     "VALUEF"                                                        )			
			(list (function cfg-true)       "KW_TRUE"                                                       )			
			(list (function cfg-false)      "KW_FALSE"                                                      )			
			(list (function cfg-exit)       "OP_OP" "KW_EXIT" "OP_CP"                                       )						
			(list (function cfg-set)        "OP_OP" "KW_SET" "ID" "exp" "OP_CP"                             )					
			(list (function cfg-not)        "OP_OP" "KW_NOT" "exp" "OP_CP"                                  )							
			(list (function cfg-progn)      "OP_OP" "KW_PROGN" "explist" "OP_CP"                            )			
            (list (function cfg-while)      "OP_OP" "KW_WHILE" "exp" "exp" "OP_CP"                          )				
			(list (function cfg-sum)        "OP_OP" "OP_PLUS" "exp" "exp" "OP_CP"                           )						
			(list (function cfg-sub)        "OP_OP" "OP_MINUS" "exp" "exp" "OP_CP"                          )					
			(list (function cfg-mul)        "OP_OP" "OP_MULT" "exp" "exp" "OP_CP"                           )						
			(list (function cfg-div)        "OP_OP" "OP_DIV" "exp" "exp" "OP_CP"                            )						
			(list (function cfg-less)       "OP_OP" "KW_LESS" "exp" "exp" "OP_CP"                           )						
			(list (function cfg-greater)    "OP_OP" "KW_GREATER" "exp" "exp" "OP_CP"                        )					
			(list (function cfg-equal)      "OP_OP" "KW_EQUAL" "exp" "exp" "OP_CP"                          )	
			(list (function cfg-if)         "OP_OP" "KW_IF" "exp" "exp" "exp" "OP_CP"                       )							
		 	(list (function cfg-fcall)      "OP_OP" "ID" "args" "OP_CP"                                     )											
			(list (function cfg-def)        "OP_OP" "KW_DEFFUN" "ID" "OP_OP" "params" "OP_CP" "explist" "OP_CP" ))
    )
)

;;===CFG===;;


;;===PARSER FUNCTIONS===;;

(defun get-sentence (rule)
    (if 
        (equal (type-of (elast rule)) 'function)
        (reverse (cdr (reverse rule)))
        rule
    )
)

(defun get-action (rule)
    (car (last rule))
)

(defun get-cfg-rules (non-term &optional (cfg-list cfg))
    (if 
        (null cfg-list)
        nil
        (if
            (equal non-term (car (car cfg-list)))
            (cdr (car cfg-list))
            (get-cfg-rules non-term (cdr cfg-list)) 
        )
    )
)

(defun is-exact (rule)
    (if
        (null rule)
        t
        (and (equal (type-of (car rule)) 'function) (is-exact (cdr rule)))
    )
)

(defun get-exact (rules)
    (when
        (not (null rules))
        (if
            (is-exact (car rules))
            (car rules)
            (get-exact (cdr rules))
        )
    )
)

(defun is-terminal (word)
    (setq fc (char word 0))
    (upper-case-p fc)
)

(defun expand-rule (rule)
    (setq expanded (cdr rule))
    (mapcar #'(lambda (expanding) (append expanding expanded)) (get-cfg-rules (car rule)))
)

(defun check-rule (tokens rule)
    (if
        (null tokens)
        (list rule)
        (when 
            (not (null rule))
            (if
                (equal (type-of (car rule)) 'function)
                (mapcar #'(lambda (rrule) (cons (car rule) rrule)) (check-rule tokens (cdr rule)))
                (if 
                    (is-terminal (car rule))
                    (when 
                        (equal (car tokens) (car rule))
                        (check-rule (cdr tokens) (cdr rule))
                    )
                    (process-tokens tokens (expand-rule rule))
                )
            )
        )
    )
)

(defun process-tokens (tokens rules)
    (when
        (not (null rules))
        (append (check-rule tokens (car rules)) (process-tokens tokens (cdr rules)))
    )
)

(defun get-strings (string-token-list)
    (if (null string-token-list)
        (list )
        (cons (car (car string-token-list)) (get-strings (cdr string-token-list)))
    )
)

(defun get-tokens (string-token-list)
    (if (null string-token-list)
        (list )
        (cons (car (last (car string-token-list))) (get-tokens (cdr string-token-list)))
    )
)

(defun parse (st-list)

)

;;===PARSER FUNCTIONS===;;


;;===MAIN PARSER FUNCTION===;;

(defun gppinterpreter ()
    "G++ Parser"
    (build-lexer)   

    (format t "Disclaimer: There will be no 'Syntax OK' prints, the interpreter behaves like a real one.") (terpri )
    (format t "'defvar' keyword is not implemented due to its similarity to 'set'.") (terpri )
    (format t "Function bodies and expression lists are handled the same way lisp does, you need 'progn' keyword for expression lists, but not for function body.") (terpri )
    (format t "Upon a syntax error the interpreter terminates.") (terpri )
    (terpri )

    (setq rules '(("start")))
    (setq strings '())
    (loop 
        (when 
            (setq string-token-list (gpplexer)) 
            (progn
                (setq strings (append strings (get-strings string-token-list)))
                (setq valid-rules (process-tokens (get-tokens string-token-list) rules))
                (when (null valid-rules) (return ))
                (setq exp-list (get-exact valid-rules))
                (if 
                    (null exp-list)
                    (setq rules valid-rules)
                    (progn
                        (format t (p2f (eval-exp-list exp-list strings))) (terpri )
                        (setq strings '())
                        (setq rules '(("start")))
                    )
                )
            ) 
        )
    )
    (format t "SYNTAX_ERROR") (terpri )
)

;;===MAIN PARSER FUNCTION===;;


;;===EXECUTION===;;

(write (gppinterpreter))

;;===EXECUTION===;;
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
        "nil"
        "list"
        "append"
        "concat"
        "set"
        "deffun"
        "for"
        "if"
        "exit"
        "load"
        "disp"
        "true"
        "false"
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
(defconstant identifier-token "IDENTIFIER")

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



;;===MAIN LEXER FUNCTION===;;

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
            (write-char src)
        )
    )
    (write-char #\:) (write-char #\space)
    (format t token) (terpri )
    (string result)
)

(defun gppinterpreter ()
    "G++ Interpreter"

    (build-lexer)   

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
                    (when (not (null prev-token)) 
                        (setq string-token-list 
                            (cons 
                                (list (print-token prev-token (reverse (cdr read-chars))) prev-token) 
                                string-token-list
                            )
                        )
                    )
                    (when (not (or (null token) (equal token white-space-token))) 
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
                    (when (not (null prev-token)) 
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
                    (setq string-token-list 
                        (cons 
                            (list (print-token token (reverse read-chars)) token)
                            string-token-list
                        )
                    )
                    (setq read-chars '())
                )
            )
        )
    )

    ;; return 
    (return-from gppinterpreter (reverse string-token-list))
)

;;===MAIN LEXER FUNCTION===;;



;;===EXECUTION===;;

(write (gppinterpreter))

;;===EXECUTION===;;
#lang rosette
(require rosette/lib/destruct)
(require rosette/lib/angelic)
(require rosette/solver/smt/cvc4)
 (require racket/cmdline)

; (current-bitwidth #f)
(current-solver (cvc4))

;; NOTES:
;; - It's probably too hard to discover the overflow-preventing trick.
;; - We're looking at algorithmic changes, but not scheduling
;;   operations.
;; TODO:
;; - Safe exponentiation.
;; - Manual implementation of online softmax.
;; - Add the operators used for online to the grammar.

(define REGISTERS 3)
(define MAX-LOOPS 3)
(define SETREG-ADJ-ONLY #t)
(define INITIAL-LOADED-DATA #t)
(define LOOPS-LOAD-DATA #t)
(define LOOP-REG 1)
(define LOOP-REG-NEEDS-READ #t)
(define REQUIRE-ALL-READ-AT-TERM #t)

; TODO: What to do about Euler in integers?
(define-symbolic epow (~> integer? integer?))
(define-symbolic sdiv (~> integer? integer? integer?))
; (define (epow n) (expt e n))

(struct SetReg (i)   #:transparent)  ; Set an active register A
(struct Swap   (a b) #:transparent)
(struct Data   ()    #:transparent)  ; Set register A to data with index in reg. A
(struct Add    (i)   #:transparent)  ; Accumulate-add register `i` into reg. A
(struct Neg    ()    #:transparent)  ; Negate the value in reg. A
(struct Copy   (s)   #:transparent)  ; Copy the value from reg. `s` to reg. A
(struct Div    (d)   #:transparent)  ; Divide value in reg. A by value in reg. `b`
(struct Epow   ()    #:transparent)
(struct Do     ()    #:transparent)
(struct Loop   ()    #:transparent)

(define (interpret prog inp-idx data)
  (define registers (make-vector REGISTERS 0))
  (vector-set! registers 0 (if INITIAL-LOADED-DATA (vector-ref data inp-idx) inp-idx))
  (inner-interpret prog registers data)
  (vector-ref registers 0))

(define (inner-interpret prog registers data)
  (define block-accum '())
  (define regs-need-read (make-vector REGISTERS #f))
  (define act-reg (box 0))
  (define loops-ended 0)
  (for-each (lambda (inst)
              (destruct inst
                        [(Do)   (begin
                                  (assume (< loops-ended MAX-LOOPS) "Too many loops")
                                  (interpret-block block-accum registers data act-reg regs-need-read)
                                  (set! block-accum '()))]
                        [(Loop) (begin
                                  (assume (< loops-ended MAX-LOOPS) "Too many loops")
                                  (interpret-loop block-accum registers data act-reg regs-need-read)
                                  (set! block-accum '())
                                  (set! loops-ended (add1 loops-ended)))]
                        [_      (set! block-accum (append block-accum (list inst)))]
                        ))
            prog)
  (interpret-block block-accum registers data act-reg regs-need-read)
  (and REQUIRE-ALL-READ-AT-TERM
       (assert (andmap (lambda (r) (not r)) (cdr (vector->list regs-need-read)))
               "Not all post-0 registers read")))

(define (interpret-loop body registers data act-reg regs-need-read)
  (for-each (lambda (loop-iter)
              (begin
                (vector-set! registers LOOP-REG
                             (if LOOPS-LOAD-DATA (vector-ref data loop-iter) loop-iter))
                (vector-set! regs-need-read LOOP-REG LOOP-REG-NEEDS-READ)
                (interpret-block body registers data act-reg regs-need-read)))
            (range (vector-length data))))

(define (interpret-block block registers data act-reg regs-need-read)
  (define (r i)
    (assume (>= i 0) "Negative register index")
    (assume (< i REGISTERS) "Register index too large")
    (vector-ref registers i))
  (for-each (lambda (instruction)
              (destruct instruction
                        [(SetReg r) (begin
                                      (assume (>= r 0) "Negative register index")
                                      (assume (< r REGISTERS) "Register index too large")
                                      (define a (unbox act-reg))
                                      (assert (not (= r a)))
                                      (assert
                                       (or
                                        (<= REGISTERS 3)
                                        (not SETREG-ADJ-ONLY)
                                        (and (= a (sub1 (vector-length registers))) (= r 0))
                                        (and (= r (sub1 (vector-length registers))) (= a 0))
                                        (= r (sub1 a))
                                        (= r (add1 a))))
                                      (set-box! act-reg r))]
                        [(Swap a b) (begin
                                      (assume (>= a 0) "Negative register index a")
                                      (assume (< a REGISTERS) "Register index a too large")
                                      (assume (>= b 0) "Negative register index b")
                                      (assume (< b REGISTERS) "Register index b too large")
                                      (assume (not (= a b)) "Swap to self")
                                      (define prev-a (vector-ref registers a))
                                      (define prev-a-need (vector-ref regs-need-read a))
                                      (vector-set! registers a (r b))
                                      (vector-set! registers b prev-a)
                                      (vector-set! regs-need-read a (vector-ref regs-need-read b))
                                      (vector-set! regs-need-read b prev-a-need))]
                        [(Data)
                         (begin
                           (define a (unbox act-reg))
                           (assert (not (vector-ref regs-need-read a)) "Abandoned unread register A")
                           (vector-set! regs-need-read a #t)
                           (vector-set! registers a (vector-ref data (r a))))]
                        [(Copy s)
                         (begin
                           (define d (unbox act-reg))
                           (assert (not (= s d)) "Copy to self")
                           (assert (not (vector-ref regs-need-read d)) "Clobbering register")
                           (vector-set! regs-need-read s #f)
                           (vector-set! regs-need-read d #t)
                           (vector-set! registers d (r s)))]
                        [(Div d)
                         ; Define division to be zero if the divisor is zero.
                         (begin
                           (define a (unbox act-reg))
                           (vector-set! regs-need-read d #f)
                           (vector-set! regs-need-read a #t)
                           (vector-set! registers a (sdiv (r a) (r d))))]
                        [(Add i)
                         (begin
                           (define a (unbox act-reg))
                           (vector-set! regs-need-read i #f)
                           (vector-set! regs-need-read a #t)
                           (vector-set! registers a (+ (r a) (r i))))]
                        [(Neg)
                         (begin
                           (define a (unbox act-reg))
                           (vector-set! regs-need-read a #t)
                           (vector-set! registers a (- (r a))))]
                        [(Epow)
                         (begin
                           (define a (unbox act-reg))
                           (vector-set! regs-need-read a #t)
                           (vector-set! registers a (epow (r a))))]
                        [(Loop) (assume #f "Unexpected Loop")]
                        [(Do)   (assume #f "Unexpected Do")]))
            block))

; TODO: Need to initially fill the registers with input, which is an idx.
; (define (inst*)
;   (choose* (inst-no-do) (Do)))
; (define (inst-no-loop*)
;   (choose* (inst-no-do) (Do)))
(define (inst-straightline* inst-idx)
  (define arg0 (ridx*))
  (choose*
   (SetReg arg0)
   ; (Swap arg0 (ridx*))
   (Data)
   (Add arg0)
   ; (Neg)
   (Copy arg0)
   (Div arg0)
   (Epow)))

(define (ridx*) (apply choose* (range REGISTERS)))
(define (fast-softmax* prog-len [inside-loop #f] [loops-done 0] [inst-idx 0])
  (assert (>= prog-len 0) "Negative program length")
  (assume (>= loops-done 0) "Negative loop count")
  (assume (<= loops-done MAX-LOOPS) "Too many loops")

  (cond [(= prog-len 0)                   '()]
        [(and (= prog-len 1) inside-loop) (list (Loop))]
        [else (begin
                (define do-loop-control?
                  (if (= loops-done MAX-LOOPS)
                      (if inside-loop (assume #f) #f)
                      (cond [(and ( = prog-len 1) inside-loop)       #t]
                            [(and ( > prog-len 1) inside-loop)       (choose* #t #f)]
                            [(and ( < prog-len 3) (not inside-loop)) #f]
                            [(and (>= prog-len 3) (not inside-loop)) (choose* #t #f)])))
                (define next-inst
                  (cond [(and do-loop-control? inside-loop)       (Loop)]
                        [(and do-loop-control? (not inside-loop)) (Do)]
                        [else                                     (inst-straightline* inst-idx)]))
                (define now-inside-loop
                  (if do-loop-control? (not inside-loop) inside-loop))
                (define loops-now-done
                  (if (and do-loop-control? inside-loop)
                      (+ loops-done 1) loops-done))
                (cons next-inst (fast-softmax* (- prog-len 1) now-inside-loop loops-now-done (add1 inst-idx))))]))

;; Oracle.
(define (softmax i data)
  (define numer (epow (vector-ref data i)))
  ; (define denom (foldl + 0 (map (lambda (j) (epow (vector-ref data j))) (range (vector-length data)))))
  (define denom (foldl + 0 (map (lambda (j) (vector-ref data j)) (range (vector-length data)))))
  (assume (not (= denom 0)) "Denominator is zero")
  (sdiv numer denom))

;; Specification. (Check equiv. with the oracle.)
(define (check imp i data)
  (assume (>= i 0) "Negative index")
  (assume (< i (vector-length data)) "Index too large")
  (assume (>= (vector-length data) 1) "Data too short")
  (define expected (softmax i data))
  ; (displayln (format "expected: ~a" expected))
  (define synth-answer (interpret imp i data))
  ; (displayln (format "synth-answer: ~a" synth-answer))
  (= expected synth-answer))


; (displayln
;   (interpret
;     (list
;       (Data 0)
;       (Epow))
;     si
;     sym-data))

(define (naive-syn sketch)
  (define-symbolic si integer?)
  (define sym-data
    (map (lambda (i) (begin (define-symbolic* d integer?) d)) (range 2)))

  ; (displayln (format "e.g., ~a"
  ;   (interpret
  ;     (list
  ;       (SetReg 2)
  ;       (Do)
  ;       (Add 1)
  ;       (Loop)
  ;       (SetReg 0)
  ;       ; (Data)
  ;       (Div 2)
  ;       )
  ;     0
  ;     (list->vector sym-data))))

  (define start-time (current-inexact-milliseconds))
  (define result
    (synthesize #:forall    (append (list si epow sdiv) sym-data)
                #:guarantee (assert (check sketch si (list->vector sym-data)))))
  (displayln (format "Took ~a ms" (- (current-inexact-milliseconds) start-time)))
  result)

(define (sampling-syn sketch)
  (define test-idx0 0)
  (define test-idx1 1)
  (define test-data #(1 2))
  (synthesize #:forall    (list epow sdiv)
              #:guarantee (assert (and
                                   (check sketch test-idx0 test-data)
                                   (check sketch test-idx1 test-data)))))

(define (display-sol sol sketch)
  (if (sat? sol)
      (begin
        (define prog-concrete (evaluate sketch sol))
        (displayln prog-concrete)
        ; (displayln (interpret prog-concrete 0 sym-data))
        )
      (displayln "UNSAT")))

(define (compare-synthesis-algos-across-sizes sketch)
  (displayln "Synthesizing naively.")
  (define naive-start (current-inexact-milliseconds))
  (display-sol (naive-syn sketch) sketch)
  (displayln (format "Took ~a ms" (- (current-inexact-milliseconds) naive-start)))
  (displayln "Synthesizing with sampling.")
  (define sampling-start (current-inexact-milliseconds))
  (display-sol (sampling-syn sketch) sketch)
  (displayln (format "Took ~a ms" (- (current-inexact-milliseconds) sampling-start))))

(define (growing-syn syn-fn [start 1])
  (define sketch (fast-softmax* start))
  (displayln (format "Trying size ~a" start))
  (define sol (syn-fn sketch))
  (if (sat? sol)
      (evaluate sketch sol)
      (growing-syn syn-fn (add1 start))))
  
(define progsize #f)
(match (current-command-line-arguments)
  [(vector) (growing-syn naive-syn 1)]
  [(vector prog-size-arg) (naive-syn (fast-softmax* (string->number prog-size-arg)))]
  [(vector prog-size-arg max-loops)
   (begin
    (set! MAX-LOOPS (string->number max-loops))
    (naive-syn (fast-softmax* (string->number prog-size-arg))))]
  [_  (error "Expected zero arguments or one integer argument")])

; (displayln "Size 3:")
; (compare-synthesis-algos-across-sizes (fast-softmax* 3))
; (displayln "Size 4:")
; (compare-synthesis-algos-across-sizes (fast-softmax* 4))
; (displayln "Size 5:")
; (compare-synthesis-algos-across-sizes (fast-softmax* 5))
; (displayln "Size 6:")
; (compare-synthesis-algos-across-sizes (fast-softmax* 6))
; (displayln "Size 7:")
; (compare-synthesis-algos-across-sizes (fast-softmax* 7))

; (naive-syn (fast-softmax* 4))

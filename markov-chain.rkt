;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname markov-chain) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;MARKOV CHAIN SIMULATION;;;;;;

;;;;;;DATA DEFINITIONS;;;;;;
(define-struct node [id next])
;; A Node is a (make-node String [List-of Number])
;; and represents a state in the chain and the probabilities of each next Node in a Markov chain.

;; A Chain is a [List-of Node]
;; and represents a complete Markov diagram

;;;;;;EXAMPLES;;;;;;
;; Node Examples:
(define ANODE1 (make-node "0" '(0.333 0.334 0 0 0 0.333)))
(define ANODE2 (make-node "1" '(0.333 0.333 0.334 0 0 0)))
(define ANODE3 (make-node "2" '(0 0.333 0.333 0.334 0 0)))
(define ANODE4 (make-node "3" '(0 0 0.333 0.333 0.334 0)))
(define ANODE5 (make-node "4" '(0 0 0 0.333 0.333 0.334)))
(define ANODE6 (make-node "6" '(0.334 0 0 0 0.333 0.333)))
(define BNODE1 (make-node "1" '(0.7 0.2 0.1)))
(define BNODE2 (make-node "2" '(0.3 0.6 0.1)))
(define BNODE3 (make-node "3" '(0.3 0.2 0.5)))

;; Chain Examples:
(define ACHAIN (list ANODE1 ANODE2 ANODE3 ANODE4 ANODE5 ANODE6))
(define BCHAIN (list BNODE1 BNODE2 BNODE3))

;;;;;;TEMPLATES;;;;;;
;; node-temp : Node -> ???
#;(define (node-temp node)
  (... (node-id node)
   ... (cond [(empty? (node-next node)) ...]
             [(cons? (node-next node))  ... (first (node-next node))
                                        ... (rest (node-next node))]) ...))
;; chain-temp : Chain -> ???
#;(define (chain-temp chain)
  (cond [(empty? chain) ...]
        [(cons? chain) ... (node-temp (first chain))
                       ... (node-temp (rest chain))]))

;;;;;;FUNCTIONS;;;;;;
(define rand-decimal (λ (_) (/ (random 10000) 10000)))

;; simulate : Number Chain Node -> [List-of Node]
;; simulates a run-though of a Markov Chain for n iterations.
(define (simulate n chain initial)
  (local [;; next-node : Number Node -> Node
          ;; uses the random Number to get to the next node given the current one
          (define (next-node rand current)
            (list-ref chain (sub1 (get-ref rand (node-next current)))))
          ;; get-ref : Number [List-of Number] -> Number
          ;; retrieves the index of the probability that the rand num r satisfies
          (define (get-ref r lop)
            (cond [(empty? lop) 0]
                  [(cons? lop) (if (< r (add-lon lop))
                                   (add1 (get-ref r (rest lop)))
                                   (get-ref r (rest lop)))]))
          ;; add-lon : [List-of Number] -> Number
          ;; adds all the numbers in the list
          (define (add-lon lon)
            (foldr + 0 lon))
          ;; get-node : Number [List-of Node] -> Node
          ;; uses the random Number r and the probabilities from the first node in the list l
          ;; to get the next node in the chain
          (define (get-node r l)
            (cond [(empty? l) (cons initial l)]
                  [(cons? l) (cons (next-node r (first l)) l)]))]
    (foldr get-node '() (build-list n rand-decimal))))

;; distribution : Chain [List-of Node] -> [List-of Number]
;; calculates the distribution for each type of Node in the list
(define (distribution chain lon)
  (local [;; frequency : String -> Number
          ;; calculates the frequency of the Node (given its id) appearing in the simulation
          (define (frequency id)
            (local [;; node-id? : Node -> Boolean
                    ;; is the id of the Node equal to the given id?
                    (define (node-id? n)
                      (string=? (node-id n) id))]
              (/ (length (filter node-id? lon)) (length lon))))]
    (map (compose frequency node-id) chain)))

(check-within (distribution ACHAIN (simulate 500000 ACHAIN ANODE1))
              (list 0.1667 0.1667 0.1667 0.1667 0.1667 0.1667) 0.01)
(check-within (distribution BCHAIN (simulate 500000 BCHAIN BNODE1))
              (list 0.5 0.3334 0.1667) 0.01)
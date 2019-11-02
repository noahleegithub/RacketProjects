;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mnist-digit-classifier) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;MNIST NUMBER CLASSIFIER;;;;;;

;;;;;;IMPORTS;;;;;;
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
;;;;;;END IMPORTS;;;;;;

;;;;;;DATA DEFINITIONS;;;;;;
;; A Feature is an Integer in the range of [0-255]
;; Represents a single pixel, with a grayscale color from 0 to 255.

;; A Bitmap is a [List-of [List-of Feature]]
;; and is a numerical representation of some image.

;; An Instance is a [List-of Feature]
;; Represents a single flattened vector of features to perform calculations on.

(define-struct train [bitmap visual instance digit])
;; A Training is a (make-train Bitmap Image Instance Nat)
;; b is the bitmap representation of an image
;; v is the visual representation of the bitmap
;; i is the flattened vector of all features in the image
;; d is the digit the image corresponds to.

(define-struct test [bitmap visual instance])
;; A Testing is a (make-test Bitmap Image Instance)
;; b is the bitmap representation of an image
;; v is the visual representation of the bitmap
;; i is the flattened vector of all features in the image
;; the difference from a Training is that in a Testing, the digit is unknown.

(define-struct neighbor [train dist])
;; A Neighbor is a (make-neighbor Training NonNegNum)
;; t represents the Training image
;; d represents the distance of the training image from a testing image
;;;;;;END DATA DEFINITIONS;;;;;;

;;;;;;EXAMPLES;;;;;;
;; Feature Examples:
(define FEAT0 0)
(define FEAT25 25)
(define FEAT100 100)
(define FEAT175 175)
(define FEAT255 255)
;; Bitmap Examples:
(define BITM-EMPTY '())
(define BITM5 '((255 255 255)
                (255 0 0)
                (255 255 255)
                (0 0 255)
                (255 255 255)))
(define BITM8 '((255 255 255)
                (255 0 255)
                (255 255 255)
                (255 0 255)
                (255 255 255)))
;; Instance Examples:
(define INST0 '())
(define INST5 '(255 255 255 255 0 0 255 255 255 0 0 255 255 255 255))
(define INST8 '(255 255 255 255 0 255 255 255 255 255 0 255 255 255 255))
;;;;;;END EXAMPLES;;;;;;

;;;;;;TEMPLATES;;;;;;
;; feature-temp : Feature -> ???
#;(define (feature-temp f)
    (... f ...))
;; bitmap-temp : Bitmap -> ???
#;(define (bitmap-temp b)
    (cond [(empty? b) ...]
          [(cons? b) ... (list-temp (first b))
                     ... (bitmap-temp (rest b)) ...]))
;; instance-temp : Instance -> ???
#;(define (instance-temp i)
    (cond [(empty? i) ...]
          [(cons? i) ... (feature-temp (first i))
                     ... (instance-temp (rest i)) ...]))
;;;;;;END TEMPLATES;;;;;;

;;;;;;GRAPHICAL FUNCTIONS;;;;;;
;; GRAPHICAL CONSTANTS:
(define CELL-SIZE 10)

;; cell : Feature -> Image
;; draws a single square of size 10 and grayscale color according to Feature f
(define (cell f)
  (square CELL-SIZE "solid" (make-color 0 0 0 f)))

(check-expect (cell FEAT0) (square 10 "solid" (make-color 0 0 0 0)))
(check-expect (cell FEAT25) (square 10 "solid" (make-color 0 0 0 25)))
(check-expect (cell FEAT255) (square 10 "solid" (make-color 0 0 0 255)))

;; bitmap->image : Bitmap -> Image
;; produces a visualization of a bitmap
(define (bitmap->image b)
  (foldr (λ (lof img)
           (above (foldr (λ (f img)
                           (beside (cell f) img))
                         empty-image lof) img))
         empty-image b))

(check-expect (bitmap->image BITM5) (above (beside (cell 255) (cell 255) (cell 255))
                                           (beside (cell 255) (cell 0) (cell 0))
                                           (beside (cell 255) (cell 255) (cell 255))
                                           (beside (cell 0) (cell 0) (cell 255))
                                           (beside (cell 255) (cell 255) (cell 255))))
(check-expect (bitmap->image BITM8) (above (beside (cell 255) (cell 255) (cell 255))
                                           (beside (cell 255) (cell 0) (cell 255))
                                           (beside (cell 255) (cell 255) (cell 255))
                                           (beside (cell 255) (cell 0) (cell 255))
                                           (beside (cell 255) (cell 255) (cell 255))))
(check-expect (bitmap->image (list
                              (list 200 255 205)
                              (list 255 0   213)
                              (list 252 255 105))) (above (beside (cell 200) (cell 255) (cell 205))
                                                          (beside (cell 255) (cell 0) (cell 213))
                                                          (beside (cell 252) (cell 255) (cell 105))))
;;;;;;END GRAPHICAL FUNCTIONS;;;;;;

;;;;;;FUNCTIONS;;;;;;
;; return-former : (X Y) X Y -> X
;; always returns the value of the first argument.
(define (return-former x y)
  x)

(check-expect (return-former 1 2) 1)
(check-expect (return-former "second" "first") "second")

;; next-index : (X) [List-of X] Nat -> Nat
;; gets the next valid index in the list, 0 if the supplied index is the last in the list.
(define (next-index lox i)
  (if (= i (sub1 (length lox))) 0 (add1 i)))

(check-expect (next-index INST5 5) 6)
(check-expect (next-index INST5 14) 0)

;; prev-index : (X) [List-of X] Nat -> Nat
;; gets the previous valid index in the list, highest valid index if the supplied index is 0
(define (prev-index lox i)
  (if (= 0 i) (sub1 (length lox)) (sub1 i)))

(check-expect (prev-index INST5 5) 4)
(check-expect (prev-index INST5 0) 14)

;; flatten : (X) [List-of [List-of X]] -> [List-of X]
;; converts a list of lists into a single list
(define (flatten lolox)
  (foldr (λ (lox accum)
           (append lox accum))
         '() lolox))

(check-expect (flatten '((255 255 255)
                         (255 0 0)
                         (255 255 255)
                         (0 0 255)
                         (255 255 255))) '(255 255 255 255 0 0 255 255 255 0 0 255 255 255 255))
(check-expect (flatten BITM5) INST5)
(check-expect (flatten BITM8) INST8)

;; training-fnames : Natural -> [List-of String]
;; produces a list of n training file names given a number n
(define (training-fnames n)
  (flatten (build-list 10
                       (λ (i) (build-list n
                                          (λ (n) (string-append "train/d_"
                                                                (number->string (add1 n))
                                                                "_"
                                                                (number->string i)
                                                                ".txt")))))))

(check-expect (training-fnames 0) '())
(check-expect (training-fnames 3)
              (list "train/d_1_0.txt" "train/d_2_0.txt" "train/d_3_0.txt"
                    "train/d_1_1.txt" "train/d_2_1.txt" "train/d_3_1.txt"
                    "train/d_1_2.txt" "train/d_2_2.txt" "train/d_3_2.txt"
                    "train/d_1_3.txt" "train/d_2_3.txt" "train/d_3_3.txt"
                    "train/d_1_4.txt" "train/d_2_4.txt" "train/d_3_4.txt"
                    "train/d_1_5.txt" "train/d_2_5.txt" "train/d_3_5.txt"
                    "train/d_1_6.txt" "train/d_2_6.txt" "train/d_3_6.txt"
                    "train/d_1_7.txt" "train/d_2_7.txt" "train/d_3_7.txt"
                    "train/d_1_8.txt" "train/d_2_8.txt" "train/d_3_8.txt"
                    "train/d_1_9.txt" "train/d_2_9.txt" "train/d_3_9.txt"))

;; fname->label : String -> Integer
;; extracts the label from the file name
(define (fname->label fname)
  (local [(define number-list-fname (filter string-numeric? (explode fname)))]
    (string->number (list-ref number-list-fname (prev-index number-list-fname 0)))))

(check-expect (fname->label "hello2.jpg") 2)
(check-expect (fname->label "train/d_3_7.txt") 7)

;; map-lol : (X Y) [X -> Y] [List-of [List-of X]] -> [List-of [List-of Y]]
;; transforms a list of lists of one type to a list of lists of another type according to a function
(define (map-lol func lolox)
  (map (λ (lox) (map func lox)) lolox))

(check-expect (map-lol string->number (list
                                       (list "0" "0" "0")
                                       (list "1" "2" "3")
                                       (list "3" "2" "1")))
              (list
               (list 0 0 0)
               (list 1 2 3)
               (list 3 2 1)))

;; f-left-to-right : (X) [X X -> X] [NEList-of X] -> X
;; calls a function on a non-empty list from left to right
(define (f-left-to-right func nel)
  (foldl (λ (x accum) (func accum x)) (first nel) (rest nel)))

(check-expect (f-left-to-right - '(1 2 3)) -4)

;; smallest-of-list-by-f : (X Y) [X -> Y] [NEList-of X] -> X
;; finds the first element in the non empty list that minimizes a function
(define (smallest-of-list-by-f f lox)
  (first (filter (λ (x) (equal? (f x) (apply min (map (λ (x) (f x)) lox)))) lox)))

(check-expect (smallest-of-list-by-f length (list (list 1 2 3)
                                                  (list 100)
                                                  (list -1000 -99 -1 0)
                                                  (list 2)))
              (list 100))

;; read-lolon : String -> [List-of [List-of Number]]
;; supplied a path to a file, retrieve the contents of the file of numbers.
(define (read-lolon path)
  (map-lol string->number (read-words/line path)))

(check-expect (read-lolon "numbers.txt") (list (list 0 0 0 0)
                                               (list 1 2 3 4)
                                               (list 2 1 0 -0.5)))
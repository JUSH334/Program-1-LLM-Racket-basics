#lang racket

;; Function to generate a list of random integers
(define (generate-random-integers count min-value max-value)
  (define (generate n)
    (if (zero? n)
        '()
        (cons (+ min-value (random (+ 1 (- max-value min-value))))
              (generate (- n 1)))))
  (generate count))

;; Function to partition a list based on a pivot
(define (partition lst pivot)
  (define less-than '())
  (define equal-to '())
  (define greater-than '())
  (for-each (lambda (x)
              (cond
                [(< x pivot) (set! less-than (cons x less-than))]
                [(= x pivot) (set! equal-to (cons x equal-to))]
                [else (set! greater-than (cons x greater-than))]))
            lst)
  (list (reverse less-than) (reverse equal-to) (reverse greater-than)))

;; Function to find the median of a sorted list
(define (find-median sorted-group)
  (list-ref sorted-group (quotient (length sorted-group) 2)))

;; Function to check if the list has at least 5 items without counting all items
(define (has-at-least-5-items? lst)
  (define (check-items remaining count)
    (cond
      [(null? remaining) #f]
      [(>= count 5) #t]
      [else (check-items (cdr remaining) (+ count 1))]))
  (check-items lst 0))

;; Modified function to group list into sublists of up to 5 elements using the new check
(define (group-into-fives lst)
  (if (null? lst)
      '()
      (if (has-at-least-5-items? lst)
          (let* ((group (take lst 5)))
            (cons group (group-into-fives (drop lst 5))))
          (list lst))))

;; Insertion sort implementation
(define (insertion-sort lst)
  (define (insert x sorted)
    (cond
      [(empty? sorted) (list x)]
      [(< x (first sorted)) (cons x sorted)]
      [else (cons (first sorted) (insert x (rest sorted)))]))
  (foldl insert '() lst))

;; Function to compute the median of medians recursively
(define (median-of-medians lst)
  (let* ((groups (group-into-fives lst))
         (sorted-groups (map insertion-sort groups))
         (medians (map find-median sorted-groups)))
    (if (<= (length medians) 5)
        (find-median (insertion-sort medians))
        (median-of-medians medians))))

;; Optimized Quicksort that passes the list length as a parameter
(define (quicksort lst)
  (letrec ((quicksort-helper (lambda (lst len)
                               (if (<= len 1)
                                   lst
                                   (let* ((pivot (median-of-medians lst))
                                          (partitions (partition lst pivot))
                                          (less-than (first partitions))
                                          (equal-to (second partitions))
                                          (greater-than (third partitions)))
                                     (append (quicksort-helper less-than (length less-than))
                                             equal-to
                                             (quicksort-helper greater-than (length greater-than))))))))
    (quicksort-helper lst (length lst))))

;; Function to compute min and max values of a list
(define (find-min-max lst)
  (foldl (lambda (x acc)
           (list (min (first acc) x) (max (second acc) x)))
         (list (first lst) (first lst))
         (rest lst)))

;; Adaptive bucketize function based on data range
(define (adaptive-bucketize lst num-buckets)
  (if (null? lst)
      '()
      (let* ((min-max (find-min-max lst))
             (min-val (first min-max))
             (max-val (second min-max))
             (range (+ 1 (- max-val min-val)))  ; Calculate the total range
             (bucket-size (max 1 (floor (/ range num-buckets))))  ; Ensure bucket-size is an integer
             (buckets (make-vector num-buckets '())))  ; Create an empty vector for buckets

        ;; Distribute each element into the appropriate bucket
        (for-each (lambda (x)
                    (let* ((bucket-index (min (quotient (- x min-val) bucket-size)  ; Ensure integer division
                                              (- num-buckets 1))))  ; Ensure index stays within bounds
                      (vector-set! buckets bucket-index
                                   (cons x (vector-ref buckets bucket-index)))))
                  lst)

        ;; Convert each bucket in the vector to a list and return a list of lists
        (map reverse (vector->list buckets)))))

;; Function to recursively split large buckets
(define (split-large-buckets buckets threshold num-buckets)
  (map (lambda (bucket)
         (if (> (length bucket) threshold)
             (apply append (adaptive-bucketize bucket num-buckets))  ; Further split large bucket
             bucket))  ; Keep small buckets as they are
       buckets))

;; Function to sort the buckets and combine the results using adaptive bucketing
(define (bucket-sort lst)
  (let* ((num-buckets 10)  ; Define the number of initial buckets
         (threshold 1000)   ; Define a threshold for splitting large buckets
         (initial-buckets (adaptive-bucketize lst num-buckets))  ; First pass of bucketization
         (final-buckets (split-large-buckets initial-buckets threshold num-buckets))  ; Split large buckets
         (sorted-buckets (map quicksort final-buckets)))  ; Sort each bucket using quicksort
    (apply append sorted-buckets)))  ; Concatenate all sorted buckets

;; Test bucket sort with random integers and add timer
(define test-list (generate-random-integers 10000000 1 1000))

(define start-time (current-inexact-milliseconds))  ; Record the start time

(displayln "Sorting...")
(define result (bucket-sort test-list))  ; Run the sorting algorithm

(define end-time (current-inexact-milliseconds))  ; Record the end time
(define elapsed-time (/ (- end-time start-time) 1000))  ; Calculate elapsed time in seconds

(displayln (string-append "Elapsed time: " (number->string elapsed-time) " seconds"))  ; Display elapsed time
(displayln result) 
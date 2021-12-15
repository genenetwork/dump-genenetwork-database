(define-module (dump string-similarity)
  #:use-module (srfi srfi-1)
  #:export (jaccard-string-similar?))

(define (trigrams str)
  "Return all trigrams in STR."
  (if (< (string-length str) 3)
      '()
      (map (lambda (start)
             (substring str start (+ start 3)))
           (iota (- (string-length str) 2)))))

(define (jaccard-index set1 set2)
  "Return the Jaccard similarity coefficient between lists SET1 and
SET2. Similarity between null sets is defined to be 0."
  (if (and (null? set1)
           (null? set2))
      0
      (let ((length-of-intersection (length (lset-intersection equal? set1 set2))))
        (exact->inexact
         (/ length-of-intersection
            (- (+ (length set1) (length set2))
               length-of-intersection))))))

(define (jaccard-string-similarity str1 str2)
  "Return the trigram similarity between strings STR1 and STR2 as
defined by the Jaccard index."
  (jaccard-index (trigrams (string-downcase str1))
                 (trigrams (string-downcase str2))))

(define (jaccard-string-similar? str1 str2)
  "Return #t if STR1 and STR2 have a trigram similarity greater than
0.8. Else, return #f. The Jaccard index is used as the similarity
metric."
  (let ((similarity-threshold 0.8))
    (> (jaccard-string-similarity str1 str2)
       similarity-threshold)))

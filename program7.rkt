#lang racket
;Anthony Mace CSC240 2/17/14
;This program is to show our understanding of Scheme
;syntax, functions, and recursion. The first function
;computes the 'dot product' of two given lists. The
;second one returns whether or not a list contains
;a duplicate. The third returns the largest difference
;between corresponding pairs of numbers in two lists.

;Dot Product
(define (computeDotProduct myList1 myList2)
  (if (or (null? myList1) (null? myList2))
      0
      (+ 
        (* (car myList1) (car myList2))
        (computeDotProduct (cdr myList1) (cdr myList2))
      )
   )
)


;Determines whether there are duplicates in a list
(define (containsDuplicates myList) 
  (if (null? myList)
      #f
      (if (>= (findMatches myList (car myList)) 2)
          #t
          (containsDuplicates (cdr myList))
      )
   )
)

;Helper function for containsDuplicates
(define (findMatches myList numToFind)
  (if (null? myList)
      0
      (+
       (if (= (car myList) numToFind)
           1
           0
       )
       (findMatches (cdr myList) numToFind)
      )
   )
)

;Finds the largest difference
(define (findLargestDifference myListOne myListTwo)
  (if (or (null? myListOne) (null? myListTwo))
      #f
      (if (or (null? (cdr myListOne)) (null? (cdr myListTwo)))
          (abs (- (car myListOne) (car myListTwo)))
          (if (>
               (abs (- (car myListOne) (car myListTwo)))
               (findLargestDifference (cdr myListOne) (cdr myListTwo))
              )
              (abs (- (car myListOne) (car myListTwo)))
              (findLargestDifference (cdr myListOne) (cdr myListTwo))
          )
      )
   )
)
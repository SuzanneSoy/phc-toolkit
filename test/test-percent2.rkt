#lang typed/racket
(require phc-toolkit/percent2
         typed/rackunit)
(check-equal? (% #,x = #'y
                 in
                 x)
              'y)
#lang racket
(require rackunit)

;--- Day 1: The Tyranny of the Rocket Equation ---

;Santa has become stranded at the edge of the Solar System while delivering
;presents to other planets! To accurately calculate his position in space,
;safely align his warp drive, and return to Earth in time to save Christmas,
;he needs you to bring him measurements from fifty stars.
;
;Collect stars by solving puzzles. Two puzzles will be made available on
;each day in the Advent calendar; the second puzzle is unlocked when you
;complete the first.
;Each puzzle grants one star. Good luck!
;
;The Elves quickly load you into a spacecraft and prepare to launch.

;At the first Go / No Go poll, every Elf is Go until the Fuel Counter-Upper.
;They haven't determined the amount of fuel required yet.

;Fuel required to launch a given module is based on its mass. Specifically, to
;find the fuel required for a module, take its mass, divide by three, round down,
;and subtract 2.
;
;For example:

;  For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
;  For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel
;  required is also 2.
;  For a mass of 1969, the fuel required is 654
;  For a mass of 100756, the fuel required is 33583.

;The Fuel Counter-Upper needs to know the total fuel requirement. To find it,
;individually calculate the fuel needed for the mass of each module (your puzzle input),
;then add together all the fuel values.

;What is the sum of the fuel requirements for all of the modules on your spacecraft?

;; Number -> Number
;; The required fuel to launch a given module, based on its mass

(define (fuel-required mass)
  (- (floor (/ mass 3)) 2))  ; Floor matches the expected values, round didn't 

(check-equal? (fuel-required 12) 2)
(check-equal? (fuel-required 14) 2)
(check-equal? (fuel-required 1969) 654)
(check-equal? (fuel-required 100756) 33583)

;; ListOfNumber -> Number
;; The sum of the fuel requirements of all modules

;(define (sum-of-fuel lom) 0) ;stub

(define (total-fuel-required lom)
  (cond [(empty? lom) 0]
        [else
         (+ (fuel-required (first lom))
            (total-fuel-required (rest lom)))]))

(check-equal? (total-fuel-required (list 12 14)) 4)
(check-equal? (total-fuel-required (list 12 14 1969 100756)) (+ 2 2 654 33583))

;; List of all modules

(define ALL-MODULES
  (list 102473 84495 98490 68860 62204 72810 65185 145951 77892 108861
        70764 67286 74002 80773 52442 131505 107162 126993 59784 64231
        91564 68585 98735 69020 77332 60445 65826 111506 95431 146687
        135119 86804 95915 85434 111303 148127 132921 136213 89004 143137
        144853 143017 104386 100612 54760 63813 144191 84481 69718 84936
        98621 124993 92736 60369 137284 101902 112726 51784 126496 85005
        101661 137278 136637 90340 100209 53683 50222 132060 98797 139054
        135638 100632 137849 125333 103981 76954 134352 74229 93402 62552
        50286 57066 98439 120708 117827 107884 72837 148663 125645 61460
        120555 142473 106668 58612 58576 143366 90058 121087 89546 126161))


(total-fuel-required ALL-MODULES)

; Final answer: 3286680




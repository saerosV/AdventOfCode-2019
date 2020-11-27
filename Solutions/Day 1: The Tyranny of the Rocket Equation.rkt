#lang racket
(require rackunit)

;--- Day 1: The Tyranny of the Rocket Equation ---
;
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
;
;At the first Go / No Go poll, every Elf is Go until the Fuel Counter-Upper.
;They haven't determined the amount of fuel required yet.
;
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
;
;The Fuel Counter-Upper needs to know the total fuel requirement. To find it,
;individually calculate the fuel needed for the mass of each module (your puzzle input),
;then add together all the fuel values.
;
;What is the sum of the fuel requirements for all of the modules on your spacecraft?

;; Constants:

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

;; Functions:

;; Number -> Number
;; The required fuel to launch a given module, based on its mass

(module+ test
  (test-case "mass->fuel test"
             (check-equal? (mass->fuel 12) 2)
             (check-equal? (mass->fuel 14) 2)
             (check-equal? (mass->fuel 1969) 654)
             (check-equal? (mass->fuel 100756) 33583)))

(define (mass->fuel mass)
  (- (floor (/ mass 3)) 2))  ; Floor matches the expected values, instead of round 

;; ListOfNumber -> Number
;; The sum of the fuel requirements of all modules

(module+ test
  (test-case "sum-of-requirements test"
             (check-equal? (sum-of-requirements (list 12 14)) 4)
             (check-equal? (sum-of-requirements (list 12 14 1969 100756))
                           (+ 2 2 654 33583))))

(define (sum-of-requirements lom)
  (cond [(empty? lom) 0]
        [else
         (+ (mass->fuel (first lom))
            (sum-of-requirements (rest lom)))]))

; --- Part Two ---
; 
; During the second Go / No Go poll, the Elf in charge of the Rocket Equation
; Double-Checker stops the launch sequence. Apparently, you forgot to include
; additional fuel for the fuel you just added.
; 
; Fuel itself requires fuel just like a module - take its mass, divide by three, round
; down, and subtract 2. However, that fuel also requires fuel, and that fuel requires
; fuel, and so on. Any mass that would require negative fuel should instead be treated
; as if it requires zero fuel; the remaining mass, if any, is instead handled by wishing
; really hard, which has no mass and is outside the scope of this calculation.
; 
; So, for each module mass, calculate its fuel and add it to the total. Then, treat the
; fuel amount you just calculated as the input mass and repeat the process, continuing
; until a fuel requirement is zero or negative. For example:
; 
;   A module of mass 14 requires 2 fuel. This fuel requires no further fuel (2 divided
;   by 3 and rounded down is 0, which would call for a negative fuel), so the total fuel
;   required is still just 2.
;   At first, a module of mass 1969 requires 654 fuel. Then, this fuel requires 216 more
;   fuel (654 / 3 - 2). 216 then requires 70 more fuel, which requires 21 fuel, which
;   requires 5 fuel, which requires no further fuel. So, the total fuel required for a
;   module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966.
;   The fuel required by a module of mass 100756 and its fuel is: 33583 + 11192 + 3728 +
;   1240 + 411 + 135 + 43 + 12 + 2 = 50346.
; 
; What is the sum of the fuel requirements for all of the modules on your spacecraft when
; also taking into account the mass of the added fuel? (Calculate the fuel requirements
; for each module separately, then add them all up at the end.)

;; Number -> Number
;; The updated function to calculate the fuel required by a module, takes into account the
;; fuel required for fuel

(module+ test
  (test-case "total-fuel-required test"
             (check-equal? (module-total 14) 2)
             (check-equal? (module-total 1969) 966)
             (check-equal? (module-total 100756) 50346)))
  
(define (module-total mass)
  (define module-fuel (mass->fuel mass))
  (cond [(<= module-fuel 0) 0]
        [else
         (+ module-fuel (module-total module-fuel))]))



;; ListOfNumber -> Number
;; Produces the sum of the fuel requirements of all modules, now taking into account the
;; fuel required for fuel

(module+ test
  (test-case "sum-of-all-modules test"
             (check-equal? (sum-of-all-modules (list 14)) 2)
             (check-equal? (sum-of-all-modules (list 14 1969)) (+ 2 966))
             (check-equal? (sum-of-all-modules (list 14 1969 100756)) (+ 2 966 50346))))

(define (sum-of-all-modules lom)
  (cond [(empty? lom) 0]
        [else
         (+ (module-total (first lom))
            (sum-of-all-modules (rest lom)))]))

(sum-of-requirements ALL-MODULES)
(sum-of-all-modules ALL-MODULES)

; Answer part 1: 3286680
; Answer part 2: 4927158

#lang plait
;; HW00 -- Name: Ben Shaprio, uID: u1195235


;; Part 1 — Implement 3rd-power

(define (3rd-power [num : Number])
  (* num (* num num)))

;; ---- Tests for Part 1 ----

(test (3rd-power 3)
      27)
(test (3rd-power 17)
      4913)
(test (3rd-power -2)
      -8)
(test (3rd-power 0)
      0)

;; Part 2 — Implement 42nd-power
(define (9th-power [num : Number])
  (* (3rd-power num) (* (3rd-power num) (3rd-power num))))

(define (27th-power [num : Number])
  (* (9th-power num) (* (9th-power num) (9th-power num))))

(define (15th-power [num : Number])
  (* (9th-power num) (* (3rd-power num) (3rd-power num))))

(define (42nd-power [num : Number])
  (* (27th-power num) (15th-power num)))

;; ---- Tests for Part 2 ----

(test (42nd-power 17) '4773695331839566234818968439734627784374274207965089)
(test (42nd-power -4) '19342813113834066795298816)
(test (42nd-power 0) 0)
(test (42nd-power 100) '1000000000000000000000000000000000000000000000000000000000000000000000000000000000000)


;; Part 3 — Implement plural

(define (plural [str : String])
  (let [(last-letter (first (reverse (string->list str))))]
    (cond
      [(equal? last-letter #\y) (string-append (substring str 0 (- (length (string->list str)) 1)) "ies")]
      [else (string-append str "s")])))

;; ---- Tests for Part 3 ----

(test (plural "dad") "dads")
(test (plural "flurry") "flurries")
(test (plural "baby") "babies")
(test (plural "fish") "fishs")


;; Part 4 — Implement energy-usage

(define-type Light
  (bulb [watts : Number]
        [technology : Symbol])
  (candle [inches : Number]))

;; kilowatthours = Power (kilowatt) * time (Hr)
(define (energy-usage [light : Light])
  (type-case Light light
    ;; 0.024 combines dividing W by 1000 to get KW and multiplying by 24
    [(bulb watt tech) (* 0.024 watt)]
    [(candle len) 0]))


;; ---- Tests for Part 4 ----

(test (energy-usage (bulb 100.0 'halogen)) 2.4)

(test (energy-usage (candle 10.0)) 0.0)

(test (energy-usage (bulb -100 'somehow-power-generating-bulb)) -2.40)

(test (energy-usage (bulb 0 'somehow-powerless-bulb)) 0)

(test (energy-usage (bulb 24.0 'led)) 0.576)

;; Part 5 — Implement use-for-one-hour

(define (use-for-one-hour [light : Light])
  (type-case Light light
    [(bulb watt tech) (bulb watt tech)]
    [(candle len) (cond
                    [(<= len 1.0) (candle 0.0)]
                    [else (candle (- len 1))])]))

;; ---- Tests for Part 5 ----

(test (use-for-one-hour (bulb 100.0 'halogen)) (bulb 100.0 'halogen))

(test (use-for-one-hour (candle 10.0)) (candle 9.0))

(test (use-for-one-hour (candle 0)) (candle 0.0))

(test (use-for-one-hour (candle 0.5)) (candle 0.0))

;; Break 3 into multiple functions
;; doesn't explicitly say what if candle is less than 1 in 
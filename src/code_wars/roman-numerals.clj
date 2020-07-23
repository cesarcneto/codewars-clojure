(ns code-wards.roman-numerals)

; Kata URL - https://www.codewars.com/kata/51b6249c4612257ac0000005
; Create a function that takes a Roman numeral as its argument and returns its value as a numeric decimal integer. You don't need to validate the form of the Roman numeral.
; Modern Roman numerals are written by expressing each decimal digit of the number to be encoded separately, starting with the leftmost digit and skipping any 0s. So 1990 is rendered "MCMXC" (1000 = M, 900 = CM, 90 = XC) and 2008 is rendered "MMVIII" (2000 = MM, 8 = VIII). The Roman numeral for 1666, "MDCLXVI", uses each letter in descending order.
; 
; Example:
; (translate-roman-numerals "XXI") ;; should return 21
; 
; Help:
; Symbol    Value
; I          1
; V          5
; X          10
; L          50
; C          100
; D          500
; M          1,000 

(def char-to-value {\I 1
                    \V 5
                    \X 10
                    \L 50
                    \C 100
                    \D 500
                    \M 1000})

(defn translate-roman-numerals-v1 [roman]
  (loop [values (reverse (map #(get char-to-value %) (seq roman)))
         acc 0]
    (let [x (first values)
          y (second values)]
      (if (nil? y)
        (+ acc x)
        (recur
         (rest values)
         (if (and (or (= y 1) (= (mod y 10) 0)) (> x y))
           (+ acc (- x (* 2 y)))
           (+ acc x)))))))

(defn roman-op [x y]
  (if (< x y) + -))

(defn translate-roman-numerals-v2 [roman]
  (->> (map #(get char-to-value %) roman)
       (reverse)
       (partition-by identity)
       (map (partial reduce +))
       (reduce #((roman-op %1 %2) %1 %2))))
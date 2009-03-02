(ns number-theory)

(defn expt [base power]
  (if (= power 0)
    1
    (* base (expt base (- power 1)))))

(defn congruent? [m n modulus]
  (let [result (/ (- m n) modulus)]
    (= (int result) result)))

(defn ln [n]
  (. Math (log n)))

(defn log [base n]
  (/ (ln n) (ln base)))

(defn mod [n modulus]
  (. Math (abs (- n (*  (int (/ n modulus)) modulus)))))

(defn gcd
  "Returns the greatest common divisor of a and b"
  [a b]
  (let [bigger (max a b)
	smaller (min a b)]
    (print (str bigger " = " smaller "(" (int (/ bigger smaller)) ") + " (mod bigger smaller) "\n"))
    (if (congruent? bigger 0 smaller)
      smaller
      (gcd smaller (mod bigger smaller))))) 


(defn legendre [a p]
  "Returns the legendre symbol (a|p) assuming p is prime"
  (let [euler-criterion (mod (expt a (/ (- p 1) 2)) p)]
    (if (= p 2)
      (mod a 2)
      (if (congruent? euler-criterion 1 p)
	1
	(if (congruent? euler-criterion 0 p)
	  0
	  (if (congruent? euler-criterion -1 p)
	    -1))))))

(defn rel-prime? 
  "Returns t if a and b are relatively prime"
  [a b]
  (= (gcd a b) 1))
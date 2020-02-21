(ns tailor.solver)

(defmacro get-code [func]
  (list 'quote (nth func 2)))

(defmacro get-variable [func]
  (list 'quote (nth (nth func 1) 0)))

(defmacro make-function [variable core]
  (list 'fn [variable] core))

(defn derivative
  [function variable]
  (if (not (seq function)) (if (= function variable) 1 0)
      (let [main (first function)
            args (rest function)
            der (fn [x] (derivative x variable))]
        (cond
          (= '+ main) (cons '+ (map der args))
          (= '- main) (cons '- (map der args))
          (= '* main) (let [u (nth args 0)
                            du (der u)
                            v (nth args 1)
                            dv (der v)]
                        (list '+ (list '* u dv) (list '* v du)))
          (= '/ main) (let [u (nth args 0)
                            du (der u)
                            v (nth args 1)
                            dv (der v)]
                        (list '/ (list '- (list '* du v) (list
                                                          '* u dv)) (list '** v 2)))
          (= '** main) (let [u (nth args 0)
                             du (der u)
                             v (nth args 1)
                             dv (der v)]
                         (list '_ (list '+ dv (list '/ du u)
                                        (list '** u v))))
          (= 'exp main) (let [u (nth args 0)
                              du (der u)]
                          (list '* du '(exp u)))
          (= 'ln main) (let [u (nth args 0)
                             du (der u)]
                         (list '/ du u))
          (= 'cos main) (let [u (nth args 0)
                              du (der u)]
                          (list '* ('* -1 du) (list 'sin u)))
          (= 'sin main) (let [u (nth args 0)
                              du (der u)]
                          (list '* du (list 'sin u)))
          :default nil))))

(defn approx-nr
  [func target start tolerance n-iterations]
  (let [variable (get-variable func)
        code (get-code func)
        der (make-function (derivative code variable))]
    (loop [k 0
           res start]
      (if (or
           (>= k n-iterations)
           (< (Math/abs (- (func res) target)) tolerance))
        res
        (recur (inc k) (+ res (/ (- target (func res)) (der res))))))))

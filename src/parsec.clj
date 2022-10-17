(ns parsec
  (:require [clojure.core :as c]
            [parsec :as p])
  (:use [clojure.test])
  (:import (clojure.lang Delay)
           (java.util.regex Pattern)))



; Parser State Definition
(defrecord State [s line col])
(defn drop [n state]
  (let [{:keys [s line col]} state
        dropped (.substring s 0 n)
        remind (.substring s n)
        ss (.split dropped "\n")
        newline-num (dec (alength ss))]
    (if (> newline-num 0)
      (let [last-line (last ss)
            new-col (.length last-line)]
        (->State remind (+ line newline-num) new-col))
      (->State remind line (+ col (.length dropped))))))

(defn eof? [state]
  (empty? (:s state)))

(defn err-expected [exp state]
  (let [{:keys [s line col]} state]
    (str "Parsing error. (line " line ", column " col "):" "\n"
         "unexpected: " (if (eof? state) "<EOF>" s) "\n"
         "expecting: " exp)))

(defn err-unexpected [ue state]
  (let [{:keys [line col]} state]
    (str "Parsing error. (line " line ", column " col "):" "\n"
         "unexpected: " ue)))




; Parser Definition
(defrecord Result [succ? val err state])
(defn succ-result [val state]
  (map->Result {:succ? true :val val :state state}))
(defn err-result [err state]
  (map->Result {:succ? false :err err :state state}))

(defprotocol Parser
  (run [this state]))

(extend-protocol Parser
  Delay
  (run [this state] (run @this state)))

(defn parser [f]
  (reify Parser
    (run [_ state] (f state))))


(defn str-p [x]
  (parser
    (fn [state]
      (if (.startsWith (:s state) x)
        (let [matched-len (.length x)
              new-state (p/drop matched-len state)]
          (succ-result x new-state))
        (err-result (err-expected x state) state)))))

(defn re [pat]
  (parser
    (fn [state]
      (let [s (:s state)
            matcher (.matcher pat s)]
        (if (.lookingAt matcher)
          (let [matched (.group matcher)
                matched-len (.length matched)
                new-state (p/drop matched-len state)]
            (succ-result (if (empty? matched) nil matched) new-state))
          (err-result (err-expected pat state) state))))))


(defn- run-str-parser [this state]
  (run (str this) state))

(extend-protocol Parser
  CharSequence
  (run [this state] (run (str-p this) state))
  Pattern
  (run [this state] (run (re this) state)))
(extend Character Parser {:run run-str-parser})
(extend Number Parser {:run run-str-parser})
(extend Boolean Parser {:run run-str-parser})

; Combinators
(defn unit-p [x]
  (parser
    (fn [state]
      (map->Result {:succ? true :val x :state state}))))

(defn flat-map [f p]
  (parser
    (fn [state]
      (let [{:keys [val succ?] next-state :state :as res} (run p state)]
        (if succ?
          (run (f val) next-state)
          res)))))

(defn map [f p] (flat-map #(unit-p (f %)) p))

(defmacro for [bindings & body]
  (let [[v p & bs] bindings]
    (cond
      (not-empty bs) `(flat-map (fn [~v] (p/for ~bs ~@body)) ~p)
      (= :return (first body)) `(map (fn [~v] ~@(rest body)) ~p)
      :else `(flat-map (fn [~v] ~@body) ~p))))

(defn- compile-seq-map
  ([f args p]
   (let [v (gensym)]
     `(map
        (fn [~v]
          (~f ~@args ~v))
        ~p)))
  ([f args p & ps]
   (let [v (gensym)]
     `(flat-map
        (fn [~v]
          ~(apply compile-seq-map f (conj args v) ps))
        ~p))))


(defmacro seq-map [f p & ps]
  (apply compile-seq-map f [] p ps))

(defmacro seq [& ps]
  `(seq-map vector ~@ps))

(defn try> [p]
  (parser
    (fn [state]
      (let [res (run p state)]
        (if (:succ? res)
          res
          (update res :state (constantly state)))))))

(defmacro choice
  ([p] `(parser #(run ~p %)))
  ([p & ps]
   (let [state (gensym)
         succ? (gensym)
         next-state (gensym)
         res (gensym)]
     `(parser
        (fn [~state]
          (let [{~succ? :succ? ~next-state :state :as ~res} (run ~p ~state)]
            (cond
              ~succ? ~res
              (not= ~state ~next-state) ~res
              :else (run (choice ~@ps) ~state))))))))

(defmacro option [val p]
  `(choice ~p (unit-p ~val)))

(defn optional [p]
  (option nil p))

(defn many1 [p]
  (for [a p
        b (optional (many1 p))]
    :return (cons a b)))

(defn many [p]
  (option nil (many1 p)))

(defn skip-many [p]
  (for [_ (many p)] :return nil))

(defn skip-many1 [p]
  (for [_ (many1 p)] :return nil))

(defn count [n p]
  (if (= n 0)
    (unit-p nil)
    (for [x p
          xs (count (- n 1) p)]
      :return (cons x xs))))

(defn between [lp p rp]
  (for [_ lp
        v p
        _ rp]
    :return v))

(defn sep-by1 [sep p]
  (let [rest (for [_ sep
                   ys (sep-by1 sep p)]
               :return ys)]
    (for [x p
          xs (optional rest)]
      :return (cons x xs))))

(defn sep-by [sep p]
  (option nil (sep-by1 sep p)))

(defn end-by1 [end p]
  (for [x p
        _ end
        xs (optional (end-by1 end p))]
    :return (cons x xs)))

(defn end-by [end p]
  (option nil (end-by1 end p)))

(defn sep-end-by1 [sep p]
  (choice (try> (sep-by1 sep p)) (end-by1 sep p)))


(defn sep-end-by [sep p]
  (choice (try> (sep-by sep p)) (end-by sep p)))

(defn chainl1 [op p]
  (letfn [(f [x]
            (option
              x
              (for [op op
                    y p]
                (f (op x y)))))]
    (for [x p] (f x))))

(defn chainl [op p default]
  (option default (chainl1 op p)))

(defn chainr1 [op p]
  (let [op-rest (for [op-f op rest (chainr1 op p)]
                  :return [op-f rest])]
    (for [x p
          [op z] (optional op-rest)]
      :return (if op (op x z) x))))

(defn chainr [op p default]
  (option default (chainr1 op p)))

(def eof
  (parser
    (fn [state]
      (if (eof? state)
        (succ-result nil state)
        (err-result (err-expected "<EOF>" state) state)))))

(defn not-followed-by [p]
  (parser
    (fn [state]
      (let [res (run p state)]
        (if (:succ? res)
          (err-result (err-unexpected (:val res) state) state)
          (succ-result nil state))))))

(defn many-till [p end]
  (choice
    (map (constantly nil) end)
    (seq-map (fn [x xs] (cons x xs)) p (many-till p end))))

(defn parse [p text]
  (run p (->State (str text) 1 1)))

(defn parse-all [p text]
  (let [pe (seq-map (fn [x _] x) p eof)]
    (run pe (->State text 1 1))))



; Parser State Tests
(defmacro is=
  ([a b] `(is (= ~a ~b)))
  ([a b msg]
   `(is (= ~a ~b) ~msg)))

(defmacro is-succ [p s r]
  `(is= (:val (parse ~p ~s)) ~r (str "result: " (prn-str (parse ~p ~s)))))

(defmacro is-err [p s]
  `(is (not (:succ? (parse ~p ~s)))))

(deftest text-parser
         (is-succ "1" "1" "1")
         (is-succ #"1+" "1111" "1111"))

(deftest parser-combinator
         (is-succ (unit-p "x") "aaa" "x")
         (is-succ (flat-map #(unit-p %) 1) "1" "1")
         (is-err (flat-map #(unit-p %) 1) "2")
         (is-succ (p/map #(str % "2") 1) "1" "12")
         (is-succ (p/seq 1 2 3) "123" ["1" "2" "3"])
         (is-succ (seq-map str 1 2 3) "123" "123")
         (let [p (p/seq 1 2)
               res (parse p "13")
               res-with-try (parse (try> p) "13")]
           (is (not (:succ? res-with-try)))
           (is= (:s (:state res-with-try)) "13")
           (is (not (:succ? res)))
           (is= (:s (:state res)) "3"))
         (is-succ (choice 1 2) "1" "1")
         (is-succ (choice 1 2) "2" "2")
         (is-err (choice (p/seq 1 2) 1) "13")
         (is-succ (option 2 1) "1" "1")
         (is-succ (option 2 1) "3" 2)
         (is-err (many1 1) "")
         (is-succ (many1 1) "111" ["1" "1" "1"])
         (is-succ (many 1) "" nil)
         (is-succ (many 1) "111" ["1" "1" "1"])
         (is-succ (skip-many 1) "111" nil)
         (is-succ (skip-many 1) "222" nil)
         (is-succ (skip-many1 1) "111" nil)
         (is-err (skip-many1 1) "222")
         (is-succ (p/count 3 1) "111" ["1" "1" "1"])
         (is-succ (between \( #"\d+" \)) "(123456)" "123456")
         (is-err (between \( #"\d+" \)) "(abc)")
         (is-succ (between \( #"\d*" \)) "()" nil)
         (is-succ (sep-by "," #"\d+") "1,23" ["1" "23"])
         (is-succ (sep-by1 "," #"\d+") "1,23" ["1" "23"])
         (is-err (sep-by1 "," #"\d+") "a")
         (is-succ (end-by "," #"\d+") "1,23," ["1" "23"])
         (is-succ (end-by1 "," #"\d+") "1,23," ["1" "23"])
         (is-succ (sep-end-by "," #"\d+") "1,23," ["1" "23"])
         (is-succ (sep-end-by "," #"\d+") "1,23" ["1" "23"])
         (is-succ (sep-end-by1 "," #"\d+") "1,23," ["1" "23"])
         (is-succ (sep-end-by1 "," #"\d+") "1,23" ["1" "23"])
         (is-succ (chainl1
                    (map (constantly -) "-")
                    (map #(Long/parseLong %) #"\d+")) "5-3-1" 1)
         (is-succ (chainl
                    (map (constantly -) "-")
                    (map #(Long/parseLong %) #"\d+")
                    0) "" 0)
         (is-succ (chainr1
                    (map (constantly -) "-")
                    (map #(Long/parseLong %) #"\d+")) "5-3-1" 3)
         (is-succ (chainr
                    (map (constantly -) "-")
                    (map #(Long/parseLong %) #"\d+")
                    0) "" 0)
         (is-err (seq-map (fn [x _] x) "let" (not-followed-by #"[a-z]+"))
                 "letabc")
         (is-succ (seq-map (fn [x _] x) "let" (not-followed-by #"[a-z]+"))
                  "let"
                  "let")
         (is-err (p/seq "1" eof) "123")
         (is-err (map (fn [[_ x _]] x) (p/seq "/*" (many #"[\s\S]") "*/")) "/*ab*//*cd*/")
         (is-succ (map (fn [[_ x]] x) (p/seq "/*" (many-till #"[\s\S]" "*/"))) "/*ab*//*cd*/" ["a" "b"]))

(run-tests)
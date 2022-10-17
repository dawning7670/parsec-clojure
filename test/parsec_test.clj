(ns parsec_test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [parsec :as p])
  (:use [clojure.test])
  (:use [parsec])
  (:import (java.io File)))


; JSON-TEST
(def j-ws (option "" #"[\u0020\u000a\u000d\u0009]+"))
(def j-sign (option "" #"[+-]"))
(def j-onenine #"[1-9]")
(def j-digit #"\d")
(def j-digits #"\d+")
(def j-integer (let [pos (p/for [x j-onenine
                                 y j-digits]
                           :return (str x y))]
                 (choice (try> pos)
                         j-digit
                         (try> (p/for [_ "-" x pos] :return (str "-" x)))
                         (p/for [_ "-" x j-digit] :return (str "-" x)))))
(def j-fraction (option "" (p/for [_ "." x #"\d+"] :return (str "." x))))
(def j-exponent (option "" (p/for [x #"[eE]" y j-sign z j-digits]
                             :return (str x y z))))
(def j-number (choice
                (try> (p/for [x j-integer
                              _ (not-followed-by #"[.eE]")]
                        :return (Long/parseLong x)))
                (p/for [a j-integer
                        b j-fraction
                        c j-exponent]
                  :return (Double/parseDouble (str a b c)))))
(def j-unicode (p/for [x #"\\u[\da-fA-F]{4}"]
                 :return (char (Integer/parseInt (.substring x 2) 16))))
(def j-escape (p/for [x #"\\[\"\\/bfnrt]"]
                :return (let [mapping-f (reduce (fn [m [k v]]
                                                  (assoc m v k))
                                                {"\\/" \/}
                                                char-escape-string)]
                          (mapping-f x))))
(def j-char (p/for [x #"(?![\"\\])[\x{0020}-\x{10ffff}]"]
              :return (.charAt x 0)))
(def j-chars (many (choice (try> j-escape) (try j-unicode) j-char)))
(def j-string (p/for [x (between "\"" j-chars "\"")]
                :return (reduce str "" x)))
(declare j-value)
(def j-bool (p/for [x #"true|false"]
              :return (Boolean/parseBoolean x)))
(def j-null (p/for [_ "null"]
              :return nil))
(def j-element (delay (between j-ws j-value j-ws)))
(def j-elements (sep-by1 "," j-element))
(def j-array (choice
               (try> (between "[" j-elements "]"))
               (p/for [_ (between "[" j-ws "]")]
                 :return [])))
(def j-member (p/for [_ j-ws
                      k j-string
                      _ j-ws
                      _ ":"
                      v j-element]
                :return {k v}))
(def j-members (p/for [x (sep-by1 "," j-member)]
                 :return (reduce merge x)))
(def j-object (choice
                (try> (between "{" j-members "}"))
                (p/for [_ (between "{" j-ws "}")]
                  :return {})))
(def j-value (choice
               j-object
               j-array
               j-string
               j-number
               j-bool
               j-null))

(def parse-c
  (fn [s]
    (let [res (p/parse j-element s)]
      (if (:succ? res) (:val res)))))

(def skipped-files #{"n_structure_100000_opening_arrays"
                     "n_structure_open_array_object"
                     "n_string_unescaped_ctrl_char"
                     "n_string_unescaped_tab"
                     "y_string_utf8"
                     "n_multidigit_number_then_00"
                     "y_string_nonCharacterInUTF-8_U+10FFFF"
                     "n_string_unescaped_newline"
                     "n_structure_number_with_trailing_garbage"
                     "y_string_reservedCharacterInUTF-8_U+1BFFF"})

(def test-files (let [dir (io/file (io/resource "json_suite"))
                      fs (file-seq dir)]
                  (filter #(not (.isDirectory %)) fs)))
(defn parse-std [s] (try (json/read-str s) (catch Exception _ nil)))

(deftest json-parser-test
  (doseq [file test-files
          :let [file-name (.getName file)
                json-string (slurp file)]
          :when (not (skipped-files (.replaceAll file-name "\\.json" "")))]
    (is (= (parse-std json-string) (parse-c json-string)) (str "file: " file-name "\ncontent: " json-string))))


(run-tests 'parsec_test)
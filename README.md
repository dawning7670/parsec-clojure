# Parsec-clojure

A parser combinator library

# Usage

## Basic

```clojure
(require '[parsec :as p])

; string parser
(def p1 "a")

; regular expression parser
(def p2 "b*")

; p3 succeeds if p1 succeeds and p2 succeeds
(def p3 (p/for [s1 p1
				s2 p2]
			:return (str s1 s2)))

; p4 succeeds if p1 succeeds or p2 succeeds
(def p4 (choice p1 p2))
```

## JSON-Parser

```clojure
(ns core
  (:require [parsec :as p])
  (:use [parsec]))

; JSON-Parser Definition
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

; Parse JSON
(parse j-element "{\"a\": 1}")
```

# Combinator

- choice
- try>
- option
- optional
- many1
- many
- skip-many
- skip-many1
- count
- between
- sep-by1
- sep-by
- end-by1
- end-by
- sep-end-by1
- sep-end-by
- chainl1
- chainl
- chainr1
- chainr
- eof
- not-followed-by
- many-till

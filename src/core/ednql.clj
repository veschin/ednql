(ns core.ednql
  (:require [clojure.string :as string]))

(def *ctx* (atom {}))

(defn throw! [message]
  (throw (Exception. message)))

(defn wrap-by [by keyword]
  (let [[s e] (if (-> by rest not-empty)
                by
                [by by])]
    (str s (name keyword) e)))

(defn operator? [coll]
  (and (coll? coll)
       (comp first namespace)))

(defmulti sql-el
  (fn [coll]
    (-> coll
        first
        namespace
        keyword)))

(defmethod sql-el :cond [[op & coll]]
  (if (not-empty coll)
    (let [must-be-question-marked?
          (some-fn symbol?
                   (and string?
                        #(->> (str %)
                              (re-find #"(\(|\)|\?)")
                              not))
                   number?)]
      (->> coll
           (map (fn [val]
                  (if (must-be-question-marked? val)
                    (do
                      (swap! *ctx* update :qmarks (fn [qmarks] (conj qmarks val)))
                      "?")
                    val)))
           (string/join (wrap-by " " op))
           (wrap-by "()")))
  (throw! "Condition without operands")))

(defmethod sql-el :default [_]
  nil)

(defn type-conversion [el]
  (cond
    (map-entry? el)
    (map type-conversion el)

    (operator? el)
    (->> (rest el)
         (map type-conversion)
         (cons (first el))
         sql-el)

    (keyword? el) (name el)
    (number? el)  el
    (nil? el)     "null"

    (or (string? el)
        (symbol? el))
    (wrap-by "'" el)))

(defn ->sql [m]
  (reset! *ctx* {})
  (let [sql    (->> (mapv type-conversion m)
                    flatten
                    (string/join " "))
        qmarks (-> @*ctx*
                   :qmarks
                   reverse
                   vec)]
    [sql qmarks]))

(ns octohabit.data
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [octohabit.common :as common]))

(defn pad [n val coll]
  (take n (concat coll (repeat val))))

(defn habit-history-array [start-date dates]
  (when (and (seq dates) start-date)
    (let [end (->> dates last (js/Date.) (.getTime))
          start (->> start-date (js/Date.) (.getTime))
          one-day (* 24 60 60 1000)
          dates-range (range start (+ end one-day) one-day)
          dates-matches (set (map #(->> %
                                        (js/Date.)
                                        (.getTime))
                                  dates))]
      (->> dates-range
           (map #(if (dates-matches %) 1 0))
           (apply str)))))

(defn edn->state-str [e]
  (let [start-date (->> e
                        :tasks
                        (map :dates)
                        (map first)
                        (filter identity)
                        sort
                        first)
        email (-> e :email)
        task-names (->> e
                        :tasks
                        (map :title)
                        (pad common/MAX-HABITS nil)
                        (interpose ",")
                        (apply str))
        history-arrays (->> e
                            :tasks
                            (map :dates)
                            (map (partial habit-history-array
                                          start-date))
                            (interpose ",")
                            (apply str))
        state-string (apply str (interpose ","
                                           [start-date
                                            email
                                            task-names
                                            history-arrays]))]
    (js/LZString.compressToEncodedURIComponent state-string)))

(defn binary-string->dates [start-date bin-str]
  (->> bin-str
       (map edn/read-string)
       (map-indexed
        (fn [idx b]
          (let [d (js/Date. start-date)]
            (when (= b 1)
              (.setDate d (+ (.getDate d) idx))
              (-> d
                  (js/Date.)
                  (.toISOString)
                  (subs 0 10))))))
       (filter identity)))

(defn url-data->edn [s]
  (let [decoded-str (js/LZString.decompressFromEncodedURIComponent s)
        [start-date email & rst] (string/split decoded-str #",")
        [habit-names history-arrays] (split-at 8 rst)
        habit-names (remove string/blank? habit-names)
        history-arrays (map (partial binary-string->dates start-date)
                            history-arrays)
        habits (map
                (fn [n arr id] {:title n :dates arr :id id})
                habit-names
                (pad (count habit-names) [] history-arrays)
                (range (count habit-names)))]
    {:email email
     :tasks habits}))

(defn get-data-from-url
  []
  (->> js/window
       (.-location)
       (.-search)
       (new js/URLSearchParams)
       (seq)
       (js->clj)
       (into {})
       (walk/keywordize-keys)
       :data
       url-data->edn))

(defn set-data-param [value]
  (let [encoded-value (edn->state-str value)
        new-url (str js/location.protocol "//"
                     js/location.host
                     js/location.pathname
                     "?data=" encoded-value)]
    (js/history.pushState nil "" new-url)))

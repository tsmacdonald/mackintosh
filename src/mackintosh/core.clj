(ns mackintosh.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as str]))

(def separator "::")

(defn convert-key
  [key]
  (get {"Jigg or Quick Step" "Quick Step"
        "[Reel]" "Reel"
        "Yes" true
        "No" false
        "" nil
        }
       key
       key))

(defn with-counts
  ([map]
   (with-counts (keys map) map))
  ([keys map]
   (clojure.core/map #(format "%s: %s" % (count (get map %))) keys)))

(defn read-csv
  [filename]
  ((juxt first (comp (partial map (partial map convert-key)) rest))
   (with-open [in-file (io/reader filename)]
     (doall
      (csv/read-csv in-file)))))

(defn index-for
  [header column-name]
  (.indexOf header column-name))

(defn get-indices
  [indices row]
  (map (partial nth row) indices))

(defn group-by-columns
  [header data column-names]
  (let [indices (map (partial index-for header) column-names)]
    (group-by #(str/join separator (get-indices indices %)) data)))



(defn frequencies-for-column
  [all-data column-index prefix suffices]
  (letfn [(per-suffix [suffix]
            (let [data-key (str/join separator [prefix suffix])
                  rows (get all-data data-key)
                  specific-data (map #(nth % column-index) rows)
                  freqs (frequencies specific-data)]
              {data-key (frequencies specific-data)}))]
    (map per-suffix suffices)))

(defn pretty-print
  [list-of-freqs]
  (->> list-of-freqs
       (map (fn [freqs]
              (let [data-key (first (keys freqs))
                    content (get freqs data-key)]
                (str/join "\t"
                          (concat
                           [data-key]
                           (map #(format "%s: %2d" (first %) (second %)) content)
                           [(format "Total: %d" (reduce + (map second content)))])))))
       (str/join "\n")
       (println)))

(defn -main
  ([] (-main "mackintosh.csv"))
  ([filename]
   (let [[header data] (read-csv filename)
         data-by-type (group-by-columns header data ["Type" "Book"])
         syncopation-index (index-for header "Syncopation?")
         frequency-list (frequencies-for-column data-by-type syncopation-index "Reel" ["Mackintosh-1" "Mackintosh-2"])]
     (pretty-print frequency-list))))


#_ (
    {"Reel::Mackintosh-1" {false 8, true 9}}
    {"Reel::Mackintosh-2" {true 19, false 3}}
    )

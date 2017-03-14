(ns mackintosh.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as str]))

(def separator "::")
(def alpha 0.05)

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

(defn combinations ;; borrowed from clojure.math.combinatorics
  [n k]
  (cond
    (< k 0) 0
    (> k n) 0
    (zero? k) 1
    (= k 1) n
    (> k (quot n 2)) (recur n (- n k))
    :else (/ (apply *' (range (inc (- n k)) (inc n)))
             (apply *' (range 1 (inc k))))))

(defn fisher-test
  [a b c d]
  (/
   (*
    (combinations (+ a b) a)
    (combinations (+ c d) c))
   (combinations (+ a b c d) (+ a c))))

(defn test-statistic
  [[first-freqs second-freqs] [first-key second-key]]
  (letfn [(get-key [map key]
            (get
             (get map (first (keys map)))
             key
             0))]
    (fisher-test
     (get-key first-freqs first-key)
     (get-key first-freqs second-key)
     (get-key second-freqs first-key)
     (get-key second-freqs second-key))))

(defn significant?
  [T]
  (<= T alpha))

(defn test-significance
  ([header data-by-type column type sources] (test-significance header data-by-type column type sources [true false]))
  ([header data-by-type column type sources options]
   (let [index (index-for header column)
         frequency-list (frequencies-for-column data-by-type index type sources)]
     (significant? (test-statistic frequency-list options)))))

(defn -main
  ([] (-main "mackintosh.csv"))
  ([filename]
   (let [[header data] (read-csv filename)
         data-by-type (group-by-columns header data ["Type" "Book"])
         fields-of-interest ["Syncopation?" "Trill?" "Missed chance for SbS apart from known exceptions?" "Staccato?" "Slurred Staccato?" "Apoggiatura?" "Triplet" "Dotted triplet" "SSL" "LSS" "Apog. 16Th"]]
     (map vector
          fields-of-interest
          (map #(test-significance header data-by-type % "Reel" ["Mackintosh-1" "Mackintosh-2"])
               fields-of-interest)))))

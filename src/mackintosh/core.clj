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

(defn get-indices
  [indices row]
  (map #(convert-key (nth row %)) indices))

(defn group-by-factors
  [data factor-indices]
  (group-by #(str/join separator (get-indices factor-indices %)) data))

(defn -main
  ([] (-main "mackintosh.csv"))
  ([filename]
   (let [[headers data] (read-csv filename)
         tune-type-index (.indexOf headers "Type")]
     (group-by-factors data (map #(.indexOf headers %) ["Type" "Book"])))))


;;C-c M-e (println (str/join "\n" (with-counts (sort (keys +result+)) +result+)))

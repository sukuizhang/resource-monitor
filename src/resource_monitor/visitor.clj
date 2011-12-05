(ns resource-monitor.visitor
  (:import [java.io FileInputStream InputStreamReader BufferedReader]))

(defn visit-all [resource f]
  (let [path (:path resource)
        text (slurp path)]
    (f resource text)))

(defn visit-line [resource f]
  (let [path (:path resource)]
    (with-open [in (-> path
                       (FileInputStream.)
                       (InputStreamReader.)
                       (BufferedReader.))]
      (loop [l (.readLine in)]
        (if l
          (do
            (f resource l)
            (recur (.readLine in))))))))

(defn visitor []
  {:visit-all visit-all :visit-line visit-line})

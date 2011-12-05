(ns resource-monitor.core
  (:require [resource-monitor.visitor :as visitor]
            [clojure.tools.logging :as logging])
  (:import [java.io FileInputStream InputStreamReader BufferedReader File]))

(defn resource-changed
  [resource visitors]
  (doseq [[key listeners] (:listeners resource)]
    (doseq [listener listeners]
      (try
        ((visitors key) resource listener)
        (catch Exception e (logging/error e))))))

(defn ^:dynamic get-last-visit-time
  [resource]
  (let [f (File. (:path resource))]
    (.lastModified f)))

(defn monitor-thread
  [resources & [ops]]
  (proxy [Thread] [] 
    (run []
         (loop []
           (Thread/sleep (or (:cycle ops) 2000))
           (try
             (doseq [[resource-name resource] (:resources @resources)]
               (let [last-visit (get-last-visit-time resource)]
                 (if (> last-visit (:last-visit resource))
                   (try 
                     (resource-changed resource (:visitors @resources))
                     (finally (swap! resources assoc-in
                                     [:resources resource-name :last-visit]
                                     last-visit))))))
             (catch Throwable t (logging/error t)))
           (recur)))))

(defn monitor-factory [& [ops]]
  (let [resources (atom {:visitors (or (:visitors ops) (visitor/visitor))})
        start? (atom false)
        start-fn (fn []
                   (let [t (monitor-thread resources ops)]
                     (doto t 
                       (.setDaemon true)
                       (.setName "file resource monitor")
                       (.start))))]
    
    (fn [resource-name resource-path listeners]
      (swap! start? (fn [_] (start-fn) true))
      (swap! resources assoc-in [:resources (keyword resource-name)]
             {:name resource-name
              :path resource-path
              :last-visit 0
              :listeners listeners}))))

(def monitor (monitor-factory))

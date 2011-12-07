(ns resource-monitor.core
  (:require [resource-monitor.visitor :as visitor]
            [clojure.tools.logging :as logging])
  (:import [java.io FileInputStream InputStreamReader BufferedReader File]))


(defn ^:dynamic get-last-modify-time
  "get last modify time of special file resource."
  [resource]
  (let [f (File. (:path resource))]
    (.lastModified f)))

(defn monitor-change
  "monitor file resource change, if last modify time is change, it means file resource has changed,it invoke listeners and update last modify time of this resource.
resource: resource to monitor change, a resource hold a resource name, file name and it's last modify time, and it's listeners, all listener relation to a key that help to find it's listener visitor,listeners struction is a map contains below:
:visitor-key1 [listener11 listener12 ...]
:visitor-key2 [listener21 listener22 ...]
......
visitors: listener visitors used to invoke listeners, it is a map contains pairs of keyword to a visitor function like: (visitor [resource f] ...), in it, f is listener functions special in resource."
  [resource visitors]
  (let [last-modify (get-last-modify-time resource)]
    (if (> last-modify (:last-modify resource))
      (let [listeners (->> (:listeners resource)
                           (mapcat (fn [[k ls]] (map (fn [l] [(visitors k) l]) ls)))
                           (into {}))]
        (doseq [[visitor listener] listeners]
          (try
            (visitor resource listener)
            (catch Exception e (logging/error e))))))
    (assoc resource :last-modify last-modify)))

(defn monitor-thread
  [resources & [ops]]
  (proxy [Thread] [] 
    (run []
         (loop []
           (Thread/sleep (or (:cycle ops) 2000))
           (try
             (doseq [[resource-name resource] (:resources @resources)]
               (swap! resources assoc-in [:resources resource-name]
                      (monitor-change resource (:visitors @resources))))
             (catch Throwable t (logging/error t)))
           (recur)))))

(defn monitor-factory [& [ops]]
  (let [resources (atom {:visitors (or (:visitors ops) (visitor/visitor))})
        start? (atom false)
        start-fn (fn []
                   (logging/info "start monitor thread ...")
                   (let [t (monitor-thread resources ops)]
                     (doto t 
                       (.setDaemon true)
                       (.setName "file resource monitor")
                       (.start))))]    
    (fn [resource-name resource-path listeners]
      (swap! start? (fn [_] (if (not @start?) (start-fn)) true))
      (let [resource (-> {:name resource-name
                          :path resource-path
                          :last-modify 0
                          :listeners listeners}
                         (monitor-change (:visitors @resources)))]
        (logging/info (str "add monitor resource:" resource))
        (swap! resources assoc-in [:resources (keyword resource-name)] resource)))))

(def monitor (monitor-factory))

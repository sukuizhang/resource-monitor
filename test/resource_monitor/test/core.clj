(ns resource-monitor.test.core
  (:use [resource-monitor.core])
  (:use [clojure.test]))

(deftest test-monitor-change
  (binding [get-last-modify-time (fn [resource] 100)]
    (let [m (atom 0)
          resource {:last-modify 99
                    :listeners {:x [(fn [resource] (swap! m inc))]}}
          resource (monitor-change resource {:x (fn [resource f] (f resource))})
          resource (monitor-change resource {:x (fn [resource f] (f resource))})]
      (is (= 1 @m))
      (is (= 100 (:last-modify resource))))))

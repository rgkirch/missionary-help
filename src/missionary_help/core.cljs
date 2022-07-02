(ns missionary-help.core
  (:require
   [clojure.string :as string]
   [cljs.pprint :refer [pprint]]
   [goog.dom :as gdom]
   [missionary.core :as m]
   [goog.events :as events]
   [debux.cs.core :as debux :refer-macros [clog clogn dbg dbgn break
                                           clog_ clogn_ dbg_ dbgn_ break_]]))

(enable-console-print!)
(debux.cs.core/set-cljs-devtools! true)

(defn observe-event
  [dom-node event-name]
  (m/observe (fn mount [send!]
               (.addEventListener dom-node event-name send!)
               (fn unmount [] (.removeEventListener dom-node event-name send!)))))

(defn observe-ref
  ([r]
   (observe-ref r nil))
  ([r initial]
   (m/observe (fn mount [send!]
                (let [id (random-uuid)]
                  (add-watch r id (fn [k r o n] (send! n)))
                  (send! @r)
                  (fn unmount [] (remove-watch r id)))))))

(defonce dispose! (atom nil))

(defn ^:dev/after-load start!
  []
  (when @dispose!
    (@dispose!))
  (reset! dispose!
          ((m/reactor
            (let [switchboard (atom {})]
              (-> (gdom/createDom "div" (clj->js {:id "root"})
                                  (doto (gdom/createTextNode)
                                    (as-> node
                                      ((m/sp
                                        (let [clicks (m/? (->> (observe-ref switchboard)
                                                               (m/eduction (filter #(contains? % :clicks)))
                                                               (m/eduction (map :clicks))
                                                               (m/reduce (comp reduced {}) nil)))]
                                          (m/? (m/reduce (fn [_ count] (set! (.-textContent node) (str count)))
                                                         nil
                                                         (m/reductions + 0 (m/eduction (map (constantly 1)) clicks))))))
                                       (.-log js/console) (.-error js/console))))
                                  (doto (gdom/createDom "input" (clj->js {:type "button"
                                                                          :value "Click me."}))
                                    (as-> node
                                      (swap! switchboard assoc :clicks (observe-event node "click")))))
                  (gdom/replaceNode
                   (gdom/getElement "root")))))
           (.-log js/console) (.-error js/console))))

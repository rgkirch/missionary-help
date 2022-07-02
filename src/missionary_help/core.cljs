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

(defn mksub
  [switchboard]
  (fn sub [k]
    (m/ap
     (let [flow (m/?> (->> (observe-ref switchboard)
                           (m/eduction (filter #(contains? % k)))
                           (m/eduction (map k))))]
       (m/?> flow)))))

(defn mkpub
  [switchboard]
  (fn pub! [k flow]
    (swap! switchboard assoc k flow)))

(defn Text
  [flow]
  (doto (gdom/createTextNode)
    (as-> node
      ((m/sp
        (m/? (m/reduce (fn [_ text] (set! (.-textContent node) (str text)))
                       nil
                       flow)))
       (.-log js/console) (.-error js/console)))))

(defn ^:dev/after-load start!
  []
  (when @dispose!
    (@dispose!))
  (reset! dispose!
          ((m/reactor
            (let [switchboard (atom {})
                  sub (mksub switchboard)
                  pub! (mkpub switchboard)
                  click-count (->> (sub :clicks)
                                   (m/eduction (map (constantly 1)))
                                   (m/reductions + 0))]
              (-> (gdom/createDom "div" (clj->js {:id "root"})
                                  (Text click-count)
                                  (doto (gdom/createDom "input" (clj->js {:type "button"
                                                                          :value "Click me."}))
                                    (as-> node
                                      (pub! :clicks (observe-event node "click")))))
                  (gdom/replaceNode
                   (gdom/getElement "root")))))
           (.-log js/console) (.-error js/console))))

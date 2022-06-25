(ns mh
  (:require
   [clojure.string :as string]
   [cljs.pprint :refer [pprint]]
   [goog.dom :as gdom]
   [missionary.core :as mi]))

(enable-console-print!)

(defn observe-ref
  ([r]
   (observe-ref r nil))
  ([r initial]
   (->> (mi/observe (fn mount [send!]
                      (let [id (random-uuid)]
                        (add-watch r id (fn [k r o n] (send! n)))
                        (send! @r)
                        (fn unmount [] (remove-watch r id)))))
        (mi/reductions {} initial)
        (mi/relieve {}))))

(defn observe-event
  [dom-node event-name]
  (->> (mi/observe (fn mount [send!]
                     (.addEventListener dom-node event-name send!)
                     (fn unmount [] (.removeEventListener dom-node event-name send!))))))


;; this code works
(comment
  ((mi/reactor
    (let [db (atom nil)
          field (observe-ref db)
          valid? (mi/signal! (mi/ap (seq (mi/?< field))))
          button-node (doto (gdom/createDom "input" (clj->js {:type "button"
                                                              :value "Submit"}))
                        (as-> node (mi/stream!
                                    (mi/ap
                                     (let [disabled (not (mi/?< valid?))]
                                       (gdom/setProperties node (clj->js {:disabled disabled})))))))
          input-node (gdom/createDom "input" (clj->js {:type "text"}))]
      (mi/stream!
       (mi/ap (let [x (mi/?< (observe-event button-node "click"))]
                (println (.-pointerType x) "clicked: text is" (mi/?< (mi/eduction (take 1) field))))))
      ;; begin different code
      (mi/stream!
       (mi/ap
        (let [x (mi/?< (observe-event input-node "input"))]
          (reset! db (.. x -target -value)))))
      ;; end different code
      (gdom/replaceNode
       (gdom/createDom "div" (clj->js {:id "app-container"})
                       (gdom/createDom "div" nil input-node)
                       (gdom/createDom "div" nil button-node))
       (gdom/getElement "app-container"))))
   prn prn))


;; this code is broken
(defn ^:dev/after-load start! []
  ((mi/reactor
  (let [db (atom nil)
        field (observe-ref db)
        valid? (mi/signal! (mi/ap (seq (mi/?< field))))
        button-node (doto (gdom/createDom "input" (clj->js {:type "button"
                                                            :value "Submit"}))
                      (as-> node (mi/stream!
                                  (mi/ap
                                   (let [disabled (not (mi/?< valid?))]
                                     (gdom/setProperties node (clj->js {:disabled disabled})))))))
        input-node (gdom/createDom "input" (clj->js {:type "text"}))]
    (mi/stream!
     (mi/ap
      (let [func (fn [x]
                   (println (.-pointerType x) "clicked: text is" (mi/?< (mi/eduction (take 1) field))))]
        (func (mi/?< (observe-event button-node "click"))))))
    ;; begin different code
    (mi/stream!
     (mi/ap
      (let [x (mi/?< (observe-event input-node "input"))]
        (reset! db (.. x -target -value)))))
    ;; end different code
    (gdom/replaceNode
     (gdom/createDom "div" (clj->js {:id "app-container"})
                     (gdom/createDom "div" nil input-node)
                     (gdom/createDom "div" nil button-node))
     (gdom/getElement "app-container"))))
   prn prn))

;; mouse clicked: text is #object[missionary.impl.Ambiguous.Branch]

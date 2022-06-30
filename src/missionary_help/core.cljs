(ns missionary-help.core
  (:require
   [clojure.string :as string]
   [cljs.pprint :refer [pprint]]
   [goog.dom :as gdom]
   [missionary.core :as m]))

(enable-console-print!)

(defn ^:dev/after-load start! []
  (m/? (m/reduce
        (constantly nil)
        (m/ap (println "bye" (m/?> (m/seed (range 20))))))))

;; Error: Unsupported operation.
;;     at Object.eval [as missionary$impl$Fiber$Fiber$park$arity$2] (Fiber.cljs:12:22)
;;     at Object.missionary$core$park [as park] (core.cljc:162:16)
;;     at Object.missionary_help$core$start_BANG_ [as start_BANG_] (core.cljs:11:3)
;;     at eval (shadow.module.main.append.js:4:28)
;;     at eval (<anonymous>)
;;     at goog.globalEval (main.js:472:11)
;;     at env.evalLoad (main.js:1534:12)
;;     at main.js:1700:12

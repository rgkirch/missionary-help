(ns missionary-help.core
  (:require
   [clojure.string :as string]
   [meander.epsilon :as mr]
   [cljs.pprint :refer [pprint]]
   [goog.dom :as gdom]
   [goog.events :as gevents]
   [goog.Timer :as gtimer]
   [missionary.core :as m]
   [cljs.spec.alpha :as s]
   [tick.core :as tick]
   tick.locale-en-us
   [debux.cs.core :as debux :refer-macros [clog clogn dbg dbgn break
                                           clog_ clogn_ dbg_ dbgn_ break_]])
  (:import missionary.Cancelled))

(enable-console-print!)

(debux.cs.core/set-cljs-devtools! true)

(enable-console-print!)

(def ^:dynamic *in-reactor* false)

(def re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(defn dom-node?
  [node]
  (instance? js/Node node))

(s/def ::hiccup-leader
  (s/and keyword?
         (s/conformer #(if (namespace %)
                         ::s/invalid
                         (let [[match tag id class] (re-matches re-tag (name %))]
                           (if-not match
                             ::s/invalid
                             (into {}
                                   [(when (seq tag) [:tag tag])
                                    (when (seq id) [:id id])
                                    (when (seq class)
                                      [:class (string/replace class #"\." " ")])]))))
                      #(let [{:keys [tag id class]} %]
                         (keyword
                          (str tag
                               (when (seq id) (str "#" id))
                               (when (seq class) (str "." (string/replace class #" " ".")))))))))

(s/def ::hiccup-attributes
  (s/map-of (s/or :keyword keyword? :string string?)
            (s/or :fn fn? :string string? :number number? :boolean boolean? :any any?)
            :conform-keys true))

(s/def ::hiccup
  (s/or :text string?
        :number number?
        :flow fn?
        :dom-node dom-node?
        :hiccup (s/cat :leader ::hiccup-leader
                       :attributes (s/? ::hiccup-attributes)
                       :children (s/* ::hiccup))))
(defn set-property!
  [node k v]
  (if (and *in-reactor* (fn? v))
    (m/stream!
     (m/ap
      (do (println "setting property:" {k (m/?> v)})
          (gdom/setProperties node (clj->js {k (m/?> v)})))))
    (gdom/setProperties node (clj->js {k v}))))

(defn observe-ref
  [r]
  (->> (m/observe (fn mount [send!]
                    (let [id (random-uuid)]
                      (add-watch r id (fn [_ _ _ n] (send! n)))
                      (send! @r)
                      (fn unmount [] (remove-watch r id)))))
       (m/relieve {})))


(defn observe-event
  [dom-node event-name]
  (m/observe (fn mount [send!]
               (.addEventListener dom-node event-name send!)
               (fn unmount [] (.removeEventListener dom-node event-name send!)))))

(def did-you-mean {"onclick" "on-click"
                   "click" "on-click"
                   "readonly" "readOnly"})

(s/def ::property-kv
  (s/conformer (fn [[k v]]
                 (cond
                   (not (keyword? k)) ::s/invalid
                   (namespace k) ::s/invalid
                   :else (let [ks (name k)
                               on? (string/starts-with? ks "on-")
                               flow? (string/ends-with? ks "-flow")]
                           (mr/match [on? flow?]
                                     [false _] [:attribute ks k v]
                                     [true false] [:event-handler (subs ks 3) k v]
                                     [true true] [:event-flow-handler (subs ks 3 (- (count ks) 5)) k v]))))
               (fn [[_ _ k v]] [k v])))

(defn help-attributes
  [dom-node attributes]
  (doseq [[k v] (s/unform ::hiccup-attributes attributes)]
    (when (contains? did-you-mean (name k)) (println "Setting '" k "'. Did you mean '" (did-you-mean k) "'?"))
    (mr/match (s/conform ::property-kv [k v])
              [:attribute _ _ _]
              (set-property! dom-node k v)

              [:event-handler ?event-name _ ?func]
              (m/stream! (->> (m/eduction (map #(?func (.. % -target -value)))
                                          (observe-event dom-node ?event-name))
                              (m/reduce (constantly nil))))
              #_((m/reduce (fn [_ e] (?func (.. e -target -value)))
                         nil
                         (observe-event dom-node ?event-name))
               (.-log js/console) (.-error js/console))

              [:event-flow-handler ?event-name _ ?func]
              (?func (m/eduction (map #(.. % -target -value))
                                 (observe-event dom-node ?event-name))))))
(defn as-element
  [form]
  (when-not (s/valid? ::hiccup form)
    (throw (ex-info (s/explain ::hiccup form) (s/explain-data ::hiccup form))))
  (let [conformed (s/conform ::hiccup form)]
    (mr/match conformed
              [:text ?text]
              (gdom/createTextNode ?text)

              [:number ?number]
              (gdom/createTextNode (str ?number))

              [:dom-node ?dom-node]
              ?dom-node

              (mr/and (mr/guard *in-reactor*) [:flow ?flow])
              #_(let [old (volatile! nil)]
                  (m/stream! (m/ap (let [elem (as-element (m/?> ?flow))]
                                     (do (when @old
                                           (gdom/replaceNode elem @old))
                                         (vreset! old elem)))))
                  @old)
              (->> ?flow
                   (m/reductions (fn [old elem] (gdom/replaceNode elem old))
                                 (gdom/createDom nil))
                   (m/reduce (constantly nil))
                   (m/stream!))

              [:hiccup {:leader {:tag ?tag
                                 :id ?id
                                 :class ?class}
                        :attributes ?attributes
                        :children (mr/seqable (mr/cata !children) ...)}]
              (doto (gdom/createDom ?tag)
                (as-> node (do
                             (when ?id (set-property! node "id" ?id))
                             (when ?class (set-property! node "class" ?class))
                             (help-attributes node ?attributes)
                             (doseq [child !children]
                               (.appendChild node child)))))

              ?x (throw (ex-info "non exhaustive pattern match" {:value ?x})))))

(defn parse-ymd
  [s]
  (try (tick/parse-date s (tick/formatter "yyyy-MM-dd"))
       (catch js/Error _)))

(defn subscribe-flow
  [& {:keys [k xform init rf]
      switchboard :db
      :as arg-map}]
  ;; (println "subscribe-flow:" (pr-str (pr-str (dissoc arg-map :xform :db :flow))))
  (m/ap
   (try (let [flow (m/?< (->> (observe-ref switchboard)
                              (m/eduction (filter #(contains? % k)))
                              (m/eduction (map k))))]
          (m/?< (cond->> flow
                  (some? xform) (m/eduction xform)
                  (some? init) (m/reductions (or rf {}) init))))
        (catch Cancelled _))))

(defn publish-flow!
  [& {:keys [flow xform init rf]
      k :key
      switchboard :db
      :as arg-map}]
  ;; (println "publish-flow!:" (pr-str (dissoc arg-map :xform :db :flow)))
  (cond->> flow
    (some? xform) (m/eduction xform)
    (some? init) (m/reductions (or rf {}) init)
    true (swap! switchboard assoc k)))



(defn pr-flow-db
        [flow-db]
        (m/stream!
         (m/ap (try
                 (let [db (m/?< (m/watch flow-db))
                       current (m/?< (apply m/latest (fn [& args] (zipmap (keys db) args))
                                            (vals db)))]
                   (println "current db:" (pr-str current)))
                 (catch Cancelled _)))))

(defn ^:dev/after-load start!
  []
  (let [switchboard (atom {})
        sub (partial subscribe-flow :db switchboard)
        pub! (partial publish-flow! :db switchboard)]
    (binding [*in-reactor* true]
      ((m/reactor
        ;; (pr-flow-db switchboard)        ; Error: Undefined continuous flow.
        (let [flight-type (->> (sub :key :flight-type
                                    :initial :one-way)
                               (m/reductions {} :one-way)
                               (m/relieve {})
                               (m/signal!))
              first-flight (->> (sub :key :departing)
                                (m/reductions {} nil)
                                (m/relieve {})
                                (m/signal!))
              second-flight (->> (sub :key :returning)
                                 (m/reductions {} nil)
                                 (m/relieve {})
                                 (m/signal!))
              bookable? (->> (m/latest
                              (fn [fst snd typ]
                                (do
                                  (println "fst " (pr-str fst) " snd " (pr-str snd) " typ " (pr-str typ))
                                  (-> (case typ
                                        :one-way (parse-ymd fst)

                                        :two-way (tick/<= (parse-ymd fst) (parse-ymd snd)))
                                      (try (catch js/Error e (js/console.error e))))))
                              first-flight second-flight flight-type)
                             (m/signal!))]
          (->> (sub :key :book-flight)
               (m/eduction (map (constantly nil)))
               (m/sample (fn [flight-type departing returning _]
                           (-> (str "You have booked a " (name flight-type) " flight departing on " departing
                                    (when (and returning (= flight-type :two-way)) (str " and returning on " returning))
                                    ".")
                               println))
                         flight-type
                         first-flight
                         second-flight)
               (m/stream!))
          (gdom/replaceNode
           (as-element [:div#root
                        [:div [:select.select {:name "one way or return flight"
                                               :on-input-flow #(pub! :key :flight-type
                                                                     :xform (map keyword)
                                                                     :init "one-way"
                                                                     :flow %)}
                               [:option {:value "one-way"} "one-way flight"]
                               [:option {:value "two-way"} "return flight"]]]
                        [:div [:input {:type "date"
                                       :on-input-flow #(pub! :key :departing :flow %)}]]
                        [:div [:input {:type "date"
                                       :on-input-flow #(pub! :key :returning :flow %)
                                       :disabled (m/eduction (map #(not (= :two-way %))) flight-type)}]]
                        [:div [:input.button {:type "button"
                                              :on-click-flow #(pub! :key :book-flight :flow %)
                                              :value "Book"
                                              :disabled (m/eduction (map #(do (println "bookable?" %)
                                                                              (not %)))
                                                                    bookable?)}]]])
           (gdom/getElement "root"))))
       (.-log js/console) (.-error js/console)))))

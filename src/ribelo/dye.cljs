(ns ribelo.dye
  (:require
   [clojure.string :as str]
   [taoensso.encore :as enc]
   [taoensso.timbre :as timbre]
   [reagent.core :as r]
   [meander.epsilon :as m]
   [cljs-bean.core :refer [->clj]]
   [ribelo.dye.util :as u]
   ["process" :as process]
   ["readline" :as readline]
   ["chalk" :as chalk]
   ["ink" :as ink]
   ["ink/build/components/StdinContext" :default stdin-context]
   ["cli-spinners" :as spinners]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ink
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def render          ink/render)

(defn measure-element [el]
  (let [m (ink/measureElement el)]
    {:width  (.-width  m)
     :height (.-height m)}))

(def static (r/adapt-react-class ink/Static))
(def text   (r/adapt-react-class ink/Text))

(defn text-raw
  ([s]
   (text-raw {} s))
  ([{:keys [width align margin-left margin-right
            padding-left padding-right] :as props} s]
   (let [w   (- width padding-left padding-right)
         fmt (cond-> (enc/str-builder "%")
               margin-left
               (enc/sb-append (.repeat " " margin-left))
               padding-left
               (enc/sb-append (.repeat " " padding-left))
               (or (= :left align) (nil? align))
               (enc/sb-append "-")
               (enc/pos-num? w)
               (enc/sb-append w)
               true (enc/sb-append "s")
               margin-right
               (enc/sb-append (.repeat " " margin-right))
               padding-right
               (enc/sb-append (.repeat " " padding-right)))]
     [text props (enc/format (str fmt) (cond-> (str s) width (enc/get-substr 0 (- width padding-left padding-right))))])))

(comment
  (let [width 8
        align :left
        fmt (cond-> (enc/str-builder "%")
              (or (= :left align) (nil? align))
              (enc/sb-append "-")
              (enc/pos-num? width)
              (enc/sb-append width)
              true (-> (enc/sb-append "s") str))]
    (enc/format fmt (name :goog))))

(defn box []
  (let [this (r/current-component)]
    (into [:> ink/Box (->> (r/props this) (enc/filter-vals identity))]
          (r/children this))))

(defn hbox []
  (let [this (r/current-component)]
    (into [box (into {:flex-direction :row :border-color :white} (->> (r/props this) (enc/filter-vals identity)))]
          (r/children this))))

(defn vbox []
  (let [this (r/current-component)]
    (into [box (into {:flex-direction :column :border-color :white} (->> (r/props this) (enc/filter-vals identity)))]
          (r/children this))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; spinner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn spinner
  ([] (spinner {}))
  ([{:keys [type] :or {type :dots}}]
   (r/with-let [sp        (aget spinners "dots")
                frame_    (r/atom 0)
                interval  (js/setInterval
                           (fn []
                             (swap! frame_
                                    (fn [frame]
                                      (if (< frame (dec (->> sp .-frames count)))
                                        (inc frame) 0))))
                           (->> sp .-interval))]
     [text (aget (->> sp .-frames) @frame_)]
     (finally
       (js/clearInterval interval)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; handle keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def keymap_ (atom (sorted-map-by enc/rcompare)))

(comment (tap> [:keymap @keymap_]))

(defn mount-keymap! [id keymap]
  (swap! keymap_ (fn [m] (assoc m id keymap))))

(defn unmount-keymap! [id]
  (swap! keymap_ (fn [m] (dissoc m id))))

(defn- lower-case? [s]
  (= s (str/lower-case s)))

(defn upper-case? [s]
  ((complement lower-case?) s))

(defn -decode-key [s]
  (m/rewrite (str/split s "-")
    [(m/cata !xs) ...]
    {& [!xs ...]}
    ""
    {:any true}
    "C"
    {:ctrl true}
    "M"
    {:meta true}
    "S"
    {:shift true}
    (m/pred lower-case? ?x)
    {:name ?x}
    (m/pred upper-case? (m/app str/lower-case ?x))
    {:shift true
     :name ?x}))

(defn -decode-keys [s]
  (->> (str/split s " ") (mapv -decode-key)))

;; (defn -encode-key [s]
;;   (m/rewrite "S-M-a"
;;     (m/re #"^(C)?-?(S)?-?(M)?-?([a-z])?$" [_ ?c ?s ?m ?x])
;;     (m/cata {& [[:ctrl  ~(some? ?c)]
;;                 [:shift ~(some? ?s)]
;;                 [:meta  ~(some? ?m)]
;;                 [:name ?x]]})
;;     {}
;;     (enc/filter-vals)))

(defn -encode-key [m]
  (m/rewrite m
    (m/map-of !ks !vs)
    (m/cata [(m/cata [!ks !vs]) ...])
    [:ctrl true]
    "C"
    [:shift true]
    "S"
    [:meta true]
    "M"
    [:name ?x]
    ?x
    [!xs ...]
    ~(str/join "-" (sort-by #({"C" 0 "S" 1 "M" 2} % 3) !xs))))

(comment (-encode-key {:ctrl true :name "1" :meta true}))

(defn -encode-keys [key-seq]
  (str/join " " (mapv -encode-key key-seq)))

(defn- handle-keys-seq [key-seq]
  (reduce
   (fn [acc [_id keymap]]
     (reduce
      (fn [acc [key dispatch]]
        (when (= (last key-seq) {:name "c" :ctrl true})
          (.exit process))
        (when (and (not key) dispatch)
          (dispatch key-seq))
        (if dispatch
          (let [dispatched?
                (m/rewrite {:keys-seq key-seq
                            :map      (-decode-keys key)}
                  {:keys-seq [!ks ..?n]
                   :map      [!ms ..?n]}
                  (m/cata [(m/cata {:k !ks :m !ms}) ...])
                  {:k (m/some ?m) :m (m/some ?m)}
                  true
                  [true ...]
                  ~(enc/do-true (dispatch))
                  _ false)]
            (or dispatched? acc))
          acc))
      acc
      keymap))
   false
   @keymap_))

(def handle-key
  (let [timeout (atom nil)
        key-seq (atom [])]
    (fn [m ms]
      (some-> @timeout js/clearTimeout)
      (swap! key-seq conj (enc/filter-vals identity m))
      (if (handle-keys-seq @key-seq)
        (reset! key-seq [])
        (reset! timeout (js/setTimeout #(reset! key-seq []) ms))))))

(defn stdin-consumer [{:keys [handler raw-mode?]
                       :or   {handler  (fn [_ m] (handle-key m 500))
                              raw-mode? true}}]
  (let [this   (r/current-component)
        gstdin (atom nil)
        f      (fn [k e]
                 (handler k (dissoc (enc/filter-vals identity (->clj e)) :sequence :code)))]
    (r/create-class
     {:component-will-unmount
      (fn [_]
        (.removeListener ^js @gstdin "keypress" f)
        (reset! gstdin nil))
      :reagent-render
      (fn [_]
        [:> (.-Consumer ^js stdin-context)
         (fn [^js obj]
           (let [stdin        (.-stdin obj)
                 set-raw-mode (.-setRawMode obj)]
             (when-not @gstdin
               (reset! gstdin stdin)
               (when raw-mode? (set-raw-mode true))
               (.on stdin "keypress" f))
             (r/as-element (into [:<>] (r/children this)))))])})))

(defn with-keys [{:keys [id keymap]} & _children]
  (let [id [(.getTime (js/Date.)) (or (str id) (enc/uuid-str))]]
    (r/create-class
     {:component-will-unmount
      (fn [_props & _children]
        (unmount-keymap! id))
      :component-did-update
      (fn [this [_ _old-props]]
        (let [props   (-> (reagent.core/argv this) second)
              active? (props :active?)
              keymap  (props :keymap)]
          (if active?
            (mount-keymap!   id keymap)
            (unmount-keymap! id))))
      :reagent-render
      (fn [{:keys [active?]} & children]
        (if active?
          (mount-keymap!   id keymap)
          (unmount-keymap! id))
        (into [:<>] children))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn vscroller [{:keys [id height active?]
                  :as   _props} & _children]
  (let [idx_   (r/atom 0)
        start_ (r/atom 0)]
    (fn [{:keys [height up-key down-key keymap on-scroll on-key]
         :or   {up-key "k" down-key "j"}
         :as   props} & children]
      (let [height (or (min height (count children)) (count children))]
        [with-keys {:id      id
                    :active? active?
                    :keymap  (into [[down-key    #(u/scroll-down! idx_ start_ height children on-scroll)]
                                    [up-key      #(u/scroll-up!   idx_ start_ height          on-scroll)]
                                    (when on-key [nil on-key])]
                                   (filter identity)
                                   (vec keymap))}
         (into [vbox props]
               (comp
                (drop @start_)
                (take (or height (count children)))
                (map (fn [child] [box child])))
               (when height children))]))))

(defn vlist [{:keys [idx_] :as _props} & children]
  (let [idx_   (or idx_ (r/atom 0))
        start_ (r/atom 0)]
    (fn [{:keys [id title active? height
                pointer border-style
                up-key down-key
                page-up-key page-down-key
                go-top-key go-bottom-key
                select-key submit-key on-select on-submit
                on-key on-change keymap selected]
         :or   {pointer       "⊳"
                up-key        "up"
                down-key      "down"
                page-up-key   "pageup"
                page-down-key "pagedown"
                go-top-key    "home"
                go-bottom-key "end"
                select-key    "insert"
                submit-key    "return"
                selected      #{}}
         :as   props} & children]
      (let [height     (or (min height (count children)) (count children))]
        [with-keys {:id      (or id title)
                    :active? active?
                    :keymap  (into [[down-key      #(u/go-down!      idx_ start_ height children on-change)]
                                    [up-key        #(u/go-up!        idx_ start_        children on-change)]
                                    [page-down-key #(u/go-page-down! idx_ start_ height children on-change)]
                                    [page-up-key   #(u/go-page-up!   idx_ start_ height children on-change)]
                                    [go-bottom-key #(u/go-bottom!    idx_ start_ height children on-change)]
                                    [go-top-key    #(u/go-top!       idx_ start_        children on-change)]
                                    (when on-select [select-key #(on-select @idx_)])
                                    (when on-submit [submit-key #(on-submit @idx_)])
                                    (when on-key    [nil on-key])]
                                   (filter identity)
                                   (m/rewrite (vec keymap)
                                     [[!k !d] ...]
                                     [(m/cata [!k !d]) ...]
                                     [?k ::down]
                                     ~[?k #(u/go-down! idx_ start_ height children on-change)]
                                     [?k ::up]
                                     ~[?k #(u/go-up! idx_ start_ children on-change)]
                                     [?k ::page-down]
                                     ~[?k #(u/go-page-down! idx_ start_ height children on-change)]
                                     [?k ::page-up]
                                     ~[?k #(u/go-page-up! idx_ start_ height children on-change)]
                                     [?k ::go-bottom]
                                     ~[?k #(u/go-bottom! idx_ start_ height children on-change)]
                                     [?k ::go-top]
                                     ~[?k #(u/go-top! idx_ start_ children on-change)]
                                     ?x ?x))}
         (into [vbox (into {:padding-right 1} props)]
               (comp
                (drop @start_)
                (take (or height (count children)))
                (map-indexed
                 (fn [i child]
                   (cond
                     (and (= i 0) (pos? @start_))
                     [box
                      [box [text " "] child]
                      [box {:align-self :flex-end} [text {:color :gray} " ⯅"]]]

                     (and (= i (dec height)) (not= (+ height @start_) (count children)))
                     [box
                      [box [text " "] child]
                      [box {:align-self :flex-end} [text {:color :gray} " ⯆"]]]

                     (and (= i (- @idx_ @start_)) (selected child))
                     [box [text {:color :green} "+"] child]

                     (and (= i (- @idx_ @start_)) active?)
                     [box [text {:color :blue}  pointer] child]

                     (selected child)
                     [box [text {:color :blue}  "+"] child]

                     :else
                     [box [text " "] child]))))
               children)]))))

(defn hlist [{:keys [idx_] :as _props} & children]
  (let [idx_   (or idx_ (r/atom 0))
        start_ (r/atom 0)]
    (fn [{:keys [id title active? width border-style
                left-key right-key
                select-key submit-key on-select on-submit
                on-key on-change keymap selected]
         :or   {left-key      "left"
                right-key     "right"
                select-key    "insert"
                submit-key    "return"}
         :as   props} & children]
      (let [width (count children)]
        [with-keys {:id      (or id title)
                    :active? active?
                    :keymap  (into [[right-key      #(u/go-down!      idx_ start_ width children on-change)]
                                    [left-key       #(u/go-up!        idx_ start_       children on-change)]
                                    (when on-select [select-key #(on-select @idx_)])
                                    (when on-submit [submit-key #(on-submit @idx_)])
                                    (when on-key    [nil on-key])]
                                   (filter identity)
                                   (m/rewrite (vec keymap)
                                     [[!k !d] ...]
                                     [(m/cata [!k !d]) ...]
                                     [?k ::left]
                                     ~[?k #(u/go-down! idx_ start_ width children on-change)]
                                     [?k ::right]
                                     ~[?k #(u/go-up! idx_ start_ children on-change)]
                                     ?x ?x))}
         (into [hbox (into {:padding-right 1} props)]
               (comp
                (drop @start_)
                (map-indexed
                 (fn [i child]
                   (cond
                     (and (= i (- @idx_ @start_)) active?)
                     [text {:inverse true} child]

                     :else
                     child))))
               children)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn text-input [{:keys [value] :or {value ""}}]
  (let [idx_      (r/atom 1)
        external_ (r/atom value)
        internal_ (r/atom @external_)]
    (fn [{:keys [active? on-change on-submit on-escape placeholder value] :as props}]
      (when (not= value @external_)
        (reset! internal_ value)
        (reset! external_ @internal_))
      [box (into {:height 1} props)
       (conj
        (if active?
          [stdin-consumer
           {:raw-mode true
            :handler
            (fn [ch m]
              (m/match [(u/str-append @internal_ (dec @idx_) ch) m]
                [_ {:name "backspace"}]
                (let [value (u/str-delete @internal_ (- @idx_ 2))]
                  (when on-change (on-change value))
                  (reset! idx_ (u/move-cursor-left @idx_))
                  (reset! internal_ value)
                  (reset! external_ @internal_))

                [_ {:name "delete"}]
                (let [value (u/str-delete @internal_ (dec @idx_))]
                  (reset! internal_ value)
                  (reset! external_ @internal_)
                  (when on-change (on-change value)))

                [_ {:name "left"}]
                (reset! idx_ (u/move-cursor-left @idx_))

                [_ {:name "right"}]
                (reset! idx_ (u/move-cursor-right @idx_ @internal_))

                [?x {:name "return"}]
                (when on-submit (on-submit ?x))

                [?x {:name "escape"}]
                (when on-escape (on-escape ?x))

                [?x {:name "tab"}]
                nil

                [(m/some ?x) ?y]
                (when (not= ?x @internal_)
                  (reset! internal_ ?x)
                  (reset! external_ @internal_)
                  (reset! idx_ (u/move-cursor-right @idx_ ?x))
                  (when on-change (on-change ?x)))
                _ nil))}]
          [:<>])
        [text (select-keys props [:color :dim-color])
         (enc/cond

           (and active? (pos? (count @internal_)))
           [text
            (-> @internal_ (u/str-append (count @internal_) " ") (u/with-cursor (dec @idx_)))]

           (and active? (zero? (count @internal_)) placeholder)
           [text {:color :gray}
            (-> placeholder (u/str-append (count placeholder) " ") (u/with-cursor (dec @idx_)))]

           (and (not active?) (pos? (count @internal_)))
           [text @internal_]
           (and (not active?) (zero? (count @internal_)) placeholder)
           [text {:color :grey}
            placeholder])])])))

(ns ribelo.dye.util
  (:require
   [taoensso.encore :as enc]
   ["chalk" :as chalk]))

(defn scroll-down!
  ([idx_ start_ height data]
   (scroll-down! idx_ start_ height data nil))
  ([idx_ start_ height data on-scroll]
   (let [idx' (min (inc @idx_) (- (count data) height))]
     (tap> [:idx @idx_ :start @start_ (+ @idx_ @start_) (- (count data) height)])
     (when (not= @idx_ idx')
       (reset! idx_ idx')
       (swap! start_ inc)
       (when on-scroll (on-scroll))))))

(defn scroll-up!
  ([idx_ start_ height]
   (scroll-up! idx_ start_ height nil))
  ([idx_ start_ height on-scroll]
   (let [idx' (max 0 (dec @idx_) height)]
     (tap> [:idx @idx_ :idx1 idx'])
     (when (not= @idx_ idx')
       (reset! idx_ idx')
       (reset! start_ (- idx' height))
       (when on-scroll (on-scroll))))))

(defn go-down!
  ([idx_ start_ height data]
   (go-down! idx_ start_ height data nil))
  ([idx_ start_ height data on-change]
   (let [idx' (inc @idx_)]
     (when (<= idx' (dec (count data)))
       (reset! idx_ idx')
       (when on-change (on-change idx' (nth data idx')))
       (when (and (>= idx' (+ @start_ height)) (<= (+ @start_ height) (dec (count data))))
         (swap! start_ inc))))))

(defn go-up!
  ([idx_ start_ data]
   (go-up! idx_ start_ data nil))
  ([idx_ start_ data on-change]
   (let [idx' (dec @idx_)]
     (when (>= idx' 0)
       (swap! idx_ dec)
       (when on-change (on-change idx' (nth data idx'))))
     (when (and (< idx' @start_) (>= idx' 0))
       (swap! start_ dec)))))

(defn go-page-down!
  ([idx_ start_ height data]
   (go-page-down! idx_ start_ height data nil))
  ([idx_ start_ height data on-change]
   (let [idx'   (min (+ @idx_ (dec height)) (dec (count data)))
         shift  (- idx' @idx_)]
     (when (and (pos? shift) (<= idx' (dec (count data))))
       (reset! idx_ idx')
       (when on-change (on-change idx' (nth data idx'))))
     (when (and (>= idx' height) (<= idx' (dec (count data))))
       (reset! start_ (min (+ @start_ shift) (- (count data) height)))))))

(defn go-page-up!
  ([idx_ start_ height data]
   (go-page-up! idx_ start_ height data nil))
  ([idx_ start_ height data on-change]
   (let [idx'  (max (- @idx_ height) 0)
         shift (- @idx_ idx')]
     (when (and (pos? shift) (>= idx' 0))
       (reset! idx_ idx')
       (when on-change (on-change idx' (nth data idx'))))
     (when (and (< idx' @start_) (>= idx' 0))
       (reset! start_ (max 0 (- @start_ shift)))))))

(defn go-bottom!
  ([idx_ start_ height data]
   (go-bottom! idx_ start_ height data nil))
  ([idx_ start_ height data on-change]
   (let [last-idx (dec (count data))]
     (when-not (= last-idx @idx_)
       (reset! idx_ last-idx)
       (reset! start_ (inc (- last-idx height)))
       (when on-change (on-change @idx_ (nth data @idx_)))))))

(defn go-top!
  ([idx_ current_ start_ data]
   (go-top! idx_ current_ start_ data nil))
  ([idx_ current_ start_ data on-change]
   (when-not (zero? @idx_)
     (reset! idx_ 0)
     (reset! start_ 0)
     (reset! current_ (nth data 0))
     (when on-change (on-change current_)))))

(defn with-cursor [s idx]
  (when s
    (let [n (count s)]
      (loop [i 0 stb (enc/str-builder)]
        (enc/cond
          (>= i n)
          (str stb)
          (= i idx)
          (recur (inc i) (enc/sb-append stb (.inverse chalk (.charAt s i))))
          :else
          (recur (inc i) (enc/sb-append stb (.charAt s i))))))))

(defn str-append [s idx ch]
  (when s
    (if ch
      (let [n (count s)]
        (loop [i 0 stb (enc/str-builder)]
          (enc/cond
            (and (> i idx) (>= i n))
            (str stb)
            (= i idx)
            (recur (inc i) (enc/sb-append stb ch (.charAt s i)))
            (not= i idx)
            (recur (inc i) (enc/sb-append stb (.charAt s i))))))
      s)))

(defn str-delete [s idx]
  (when s
    (let [n (count s)]
      (loop [i 0 stb (enc/str-builder)]
        (enc/cond
          (>= i n)
          (str stb)
          (= i idx)
          (recur (inc i) stb)
          :else
          (recur (inc i) (enc/sb-append stb (.charAt s i))))))))

(defn move-cursor-left [idx]
  (max (dec idx) 1))

(defn move-cursor-right [idx s]
  (min (inc idx) (inc (count s))))

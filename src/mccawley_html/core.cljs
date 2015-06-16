(ns mccawley-html.core
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET POST]]
            [cljs.reader :as reader]))

(enable-console-print!)

;; Define reagent atoms we need for state
(def parsed-text (reagent/atom ""))
(def start-text (reagent/atom ""))
(def stats-html (reagent/atom ""))

; The displaying happens here
(defn display-tree [tree-to-display]
  (let [clj-tree (reader/read-string tree-to-display)
        pos-color {"DT" "#FF1493" "NN" "#DC143C" "NNS" "#DC143C" "NNP" "#DC143C"
                   "NP" "#DC143C" "PRP" "#FF0000" "PRP$" "#FF0000" "SQ" "#9370DB"
                   "IN" "#00FF00" "TO" "#00FF00" "CD" "#FA8072" "CC" "#B8860B"
                   "WDT" "#9932CC" "SBAR" "#9932CC" "VB" "#1E90FF" "VBD" "#1E90FF"
                   "VBG" "#1E90FF" "VP" "#1E90FF" "VBN" "#1E90FF" "VBP" "#1E90FF"
                   "VBZ" "#1E90FF" "MD" "#00CED1" "QP" "#FA8072" "WHNP" "#9932CC"
                   "WP" "#9932CC" "EX" "#191970" "POS" "#800000" "UH" "#EE82EE"
                   "RB" "#FFA500" "RBR" "#FFA500" "RBS" "#FFA500" "ADVP" "#FFA500"
                   "PP" "#00FF00" "S" "#800080" "ROOT" "#800080" "FRAG" "#800080"
                   "WRB" "#FF00FF" "WHADVP" "#FF00FF" "PDT" "#FF1493" "SBARQ" "#800080"
                   "JJ" "#FF8C00" "JJR" "#FF8C00" "JJS" "#FF8C00" "ADJP" "#FF8C00"
                   "PRT" "#000080" "RP" "#000080" "SINV" "#800080" "FW" "#000000"
                   "LS" "#8B4513" "SYM" "#2E8B57" "WP$" "#9932CC"}
        svg (-> js/d3
                (.select "svg")
                (.attr "width" "1000")
                (.attr "height" "500")
                (.attr "id" "visualization")
                (.attr "xmlns" "http://www.w3.org/2000/svg"))
        tree (-> js/d3
                 (.-layout)
                 (.tree)
                 (.size (clj->js [1000 470])))
        links (-> tree
                  (.links (-> tree
                              (.nodes (clj->js clj-tree)))))
        node-content (-> svg
                         (.selectAll "g")
                         (.data links)
                         (.enter)
                         (.append "g")
                         (.attr "transform" "translate(0,0)"))
        delay-in-ms 100]
    (letfn [(path-drawer [i]
                         (str "M" (.-x (.-source i)) "," (+ (.-y (.-source i)) 4)
                              "L" (.-x (.-target i)) "," (.-y (.-target i))))
            (get-pos-color [p] (if (nil? (pos-color p))
                                 "#808080" (pos-color p)))
            (get-node-length [n]
                             (* 10 (apply max [(count (.-word n))
                                               (count (.-pos n))])))]
      (-> svg ; lines
          (.selectAll "g")
          (.data links)
          (.append "path")
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.attr "d" (fn [i] (path-drawer i)))
          (.attr "stroke" "black")
          (.attr "stroke-width" "2")
          (.attr "fill" "none"))
      (-> node-content ; rectangle for part of speech
          (.data links)
          (.append "rect")
          (.attr "x" (fn [o] (int (- (.-x (.-target o))
                                     (/ (get-node-length (.-target o)) 2)))))
          (.attr "y" (fn [o] (int (- (.-y (.-target o)) 10))))
          (.attr "height" 14)
          (.attr "fill" (fn [o] (get-pos-color (.-pos (.-target o)))))
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.attr "width" (fn [o] (get-node-length (.-target o)))))
      (-> node-content ; rectangle for word
          (.append "rect")
          (.attr "x" (fn [o] (int (- (.-x (.-target o))
                                     (/ (get-node-length (.-target o)) 2)))))
          (.attr "y" (fn [o] (int (.-y (.-target o)))))
          (.attr "height" (fn [o] (if (not= (.-word (.-target o)) "") 14 0)))
          (.attr "fill" "#FFFFFF")
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.attr "width" (fn [o] (if (not= (.-word (.-target o)) "")
                                   (get-node-length (.-target o)) 0))))
      (-> node-content ; word
          (.append "text")
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.text (fn [o] (.-word (.-target o))))
          (.attr "x" (fn [o] (int (- (.-x (.-target o))
                                     (/ (get-node-length (.-target o)) 2)))))
          (.attr "y" (fn [o] (int (+ (.-y (.-target o)) 10))))
          (.attr "fill" "black"))
      (-> node-content ; part of speech
          (.append "text")
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.text (fn [o] (.-pos (.-target o))))
          (.attr "x" (fn [o] (+ 3 (int (- (.-x (.-target o))
                                     (/ (get-node-length (.-target o)) 2))))))
          (.attr "y" (fn [o] (int (.-y (.-target o)))))
          (.attr "fill" "white")))))


(defn compute-tree-stats [tree]
  (letfn [(get-tree-seq [node-type]
                        (->> (tree-seq #(or (map? %) (vector? %))
                                       identity
                                       (reader/read-string tree))
                             (filter #(and (map? %) (node-type %)))
                             (map node-type)))]
    (let [tree-nodes (rest (get-tree-seq :pos))
          num-of-words (->> (get-tree-seq :word)
                            (remove empty?)
                            count)
          max-depth (->> (loop [input-seq (map #(if (= % "[") 1 -1)
                                               (re-seq #"[\[\]]" tree))
                                output-seq [0]]
                           (if (empty? input-seq)
                             output-seq
                             (recur (rest input-seq)
                                    (conj output-seq
                                          (+ (last output-seq)
                                             (first input-seq))))))
                         (apply max)
                         dec
                         dec)
          top-five (->> tree-nodes
                        frequencies
                        (sort-by val)
                        reverse
                        (take 5))]
    [:b "STATS"
     [:p (str "Number of nodes: " (count tree-nodes))]
     [:p (str "Number of words: " num-of-words)]
     [:p (str "Maximum depth: " max-depth)]
     [:p (str "Most frequent nodes:")]
      (map #(vector :p (str (first %) " " (last %))) top-five)])))


;; Handle GET request to our external service
(defn handler [response]
  (do
    (reset! parsed-text (:parsed-text response))
    (display-tree @parsed-text)
    (reset! stats-html (compute-tree-stats @parsed-text))
    (.log js/console (:parsed-text response))))

(defn error-handler [{:keys [status status-text]}]
  (.log js/console (str "something bad happened: " status " " status-text)))


;; function to call when we click on parse button
(defn retrieve-parsed [param]
  (let [url-encoded-param (clojure.string/replace param #"," "%2C")
        uri (str "http://localhost:3000/parse/" url-encoded-param)]
    (GET uri {:handler handler
              :error-handler error-handler
              :response-format :json
              :keywords? true})))


(defn reset-all []
  (reset! parsed-text "")
  (reset! start-text "")
  (reset! stats-html "")
  (-> js/d3
      (.select "svg")
      (.selectAll "*")
      (.remove)))


;; function to render the page, react/reagent style!
(defn display-page []
  [:div
   [:h2 "McCawley"]
   [:input {:type "text"
            :size 21
            :placeholder "Type an English sentence."
            :value @start-text
            :on-change #(reset! start-text (-> % .-target .-value))}]
   [:p]
   [:button {:on-click #(retrieve-parsed @start-text)} "Parse"]
   " "
   [:button {:on-click #(reset-all)} "Reset"]
   [:p]
   [:p @stats-html]
  ])

(defn display-top []
  [:p @start-text])

(defn main []
  (reagent/render-component [display-page] (.getElementById js/document "app"))
  (reagent/render-component [display-top] (.getElementById js/document "canvastop")))

(ns mccawley-html.core
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET POST]]
            [cljs.reader :as reader]))

(enable-console-print!)

;; Define reagent atoms we need for state
(def parsed-text (reagent/atom ""))
(def start-text (reagent/atom ""))
(def stats-html (reagent/atom ""))

;; The displaying happens here
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
                             (* 10 (apply max [(count (.-word (.-target n)))
                                               (count (.-pos (.-target n)))])))]
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
                                     (/ (get-node-length o) 2)))))
          (.attr "y" (fn [o] (int (- (.-y (.-target o)) 10))))
          (.attr "height" 14)
          (.attr "fill" (fn [o] (get-pos-color (.-pos (.-target o)))))
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.attr "width" (fn [o] (get-node-length o))))
      (-> node-content ; rectangle for word
          (.append "rect")
          (.attr "x" (fn [o] (int (- (.-x (.-target o))
                                     (/ (get-node-length o) 2)))))
          (.attr "y" (fn [o] (int (.-y (.-target o)))))
          (.attr "height" (fn [o] (if (not= (.-word (.-target o)) "") 14 0)))
          (.attr "fill" "#FFFFFF")
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.attr "width" (fn [o] (if (not= (.-word (.-target o)) "")
                                   (get-node-length o) 0))))
      (-> node-content ; word
          (.append "text")
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.text (fn [o] (.-word (.-target o))))
          (.attr "x" (fn [o] (int (- (.-x (.-target o))
                                     (/ (get-node-length o) 2)))))
          (.attr "y" (fn [o] (int (+ (.-y (.-target o)) 10))))
          (.attr "fill" "black"))
      (-> node-content ; part of speech
          (.append "text")
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.text (fn [o] (.-pos (.-target o))))
          (.attr "x" (fn [o] (+ 3 (int (- (.-x (.-target o))
                                     (/ (get-node-length o) 2))))))
          (.attr "y" (fn [o] (int (.-y (.-target o)))))
          (.attr "fill" "white")))))


;; compute and display stats
(defn compute-tree-stats [tree]
  (letfn [(get-tree-seq [node-type]
           ;; function to get a sequence of all the nodes of a given type
                        (->> (tree-seq #(or (map? %) (vector? %))
                                       identity
                                       (reader/read-string tree))
                             (filter #(and (map? %) (node-type %)))
                             (map node-type)))]

    (let [tree-nodes (rest (get-tree-seq :pos))
          ;; using rest is necessary since we don't consider ROOT

          num-of-parsed-words (->> (get-tree-seq :word)
                                   (remove empty?)
                                   count)

          ;; Idea to calculate depth: Take the sequence of the angled brackets
          ;; in a parsed tree; the sequence reflects the tree's gross
          ;; structure.  Replace each left-bracket with 1, replace each
          ;; right-bracket with -1, make an additive sequence of the resulting
          ;; values, find the maximum result of that sequence (the tree's
          ;; depth) and subtract two to account for the ROOT and S nodes which
          ;; we don't count in the displayed tree.
          ;;
          ;; For example, a tree with the gross structure: [[[][[[]]]]]
          ;; gets transformed into the sequence  (1 1 1 -1 1 1 1 -1 -1 -1 -1 -1)
          ;; which becomes the additive sequence (1 2 3  2 3 4 5  4  3  2  1  0)
          ;; whose maximum value is 5, and whose depth for our purposes is
          ;; 5 - 2 = 3.

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
    [:table {:class "table table-striped"}
       [:tr [:td "# Nodes: "] [:td (count tree-nodes)]]
       [:tr [:td "# Words: "] [:td (count (clojure.string/split
                                                   @start-text
                                                   #"\s+"))]]
       [:tr [:td "# Parsed words: "] [:td num-of-parsed-words]]
       [:tr [:td "Max depth: "] [:td (str max-depth)]]
       [:tr [:td "Most frequent nodes:"] [:td " "]]
       (map (fn [x] [:tr [:td (first x)] [:td (last x)]]) top-five)])))


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

(defn get-random-sentence []
  (rand-nth ["A rare black squirrel has become a regular visitor to a suburban garden."
           "Jesus wept."
           "Hulk smash!"
           "Jesus ate nine pizzas."
           "The bush burned with fire, and the bush was not consumed."
           "Man is born free, and everywhere he is in chains."
           "Energy equals mass times the speed of light squared."
           "The freethinking of one age is the common sense of the next."
           "Life is a moderately good play with a badly written third act."
           "How much wood would a woodchuck chuck if a woodchuck would chuck wood?"
           "The sky above the port was the color of television, tuned to a dead channel."
           "All the world's a stage, And all the men and women merely players."
           "Colorless green ideas sleep furiously."
           "The thought of roasted buttery toffees reminds me of Christmas."
           "Nowadays people know the price of everything and the value of nothing."
           "Life appears to me too short to be spent in nursing animosity or registering wrongs."
           ]))

;; function to render the page, react/reagent style!
(defn display-page []
  [:div
   [:h3 "McCawley"]
   [:input {:type "text"
            :size 23
            :placeholder "Type an English sentence."
            :value @start-text
            :on-change #(reset! start-text (-> % .-target .-value))}]
   [:p]
   [:button {:on-click #(retrieve-parsed @start-text)
             :class "btn btn-xs btn-primary"} "Parse"]
   " "
   [:button {:on-click #(reset-all)
             :class "btn btn-xs btn-danger"} "Reset"]
   " "
   [:button {:on-click #(reset! start-text (get-random-sentence))
             :class "btn btn-xs btn-success"} "Random"]
   [:p]
   [:p @stats-html]
  ])

(defn display-top []
  [:p @start-text])

(defn main []
  (reagent/render-component [display-page] (.getElementById js/document "app"))
  (reagent/render-component [display-top] (.getElementById js/document "canvastop")))

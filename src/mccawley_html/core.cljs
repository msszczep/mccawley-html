(ns mccawley-html.core
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET]]
            [cljs.reader :as reader]))

(enable-console-print!)

;; Define reagent atoms we need for state
(def parsed-text (reagent/atom ""))
(def start-text (reagent/atom ""))
(def stats-html (reagent/atom ""))
(def color-scheme (reagent/atom ""))

;; The displaying happens here
(defn display-tree [tree-to-display]
  (let [clj-tree (reader/read-string tree-to-display)
        pos-color {"DT" "#FF1493" "NN" "#DC143C" "NNS" "#DC143C" "NNP" "#DC143C"
                   "NP" "#DC143C" "PRP" "#FF0000" "PRP$" "#FF0000" "SQ" "#9370DB"
                   "IN" "#5CB85C" "TO" "#5CB85C" "CD" "#FA8072" "CC" "#B8860B"
                   "WDT" "#9932CC" "SBAR" "#9932CC" "VB" "#1E90FF" "VBD" "#1E90FF"
                   "VBG" "#1E90FF" "VP" "#1E90FF" "VBN" "#1E90FF" "VBP" "#1E90FF"
                   "VBZ" "#1E90FF" "MD" "#00CED1" "QP" "#FA8072" "WHNP" "#9932CC"
                   "WP" "#9932CC" "EX" "#191970" "POS" "#800000" "UH" "#EE82EE"
                   "RB" "#FFA500" "RBR" "#FFA500" "RBS" "#FFA500" "ADVP" "#FFA500"
                   "PP" "#5CB85C" "S" "#800080" "ROOT" "#800080" "FRAG" "#800080"
                   "WRB" "#FF00FF" "WHADVP" "#FF00FF" "PDT" "#FF1493" "SBARQ" "#800080"
                   "JJ" "#FF8C00" "JJR" "#FF8C00" "JJS" "#FF8C00" "ADJP" "#FF8C00"
                   "PRT" "#000080" "RP" "#000080" "SINV" "#800080" "FW" "#000000"
                   "LS" "#8B4513" "SYM" "#2E8B57" "WP$" "#9932CC" "NNPS" "#DC143C"}
        pos-color-props {"JJ" "#FF8C00" "JJR" "#FF8C00" "JJS" "#FF8C00"
                   "VB" "#FF8C00" "VBD" "#FF8C00" "VBG" "#FF8C00" "VBN" "#FF8C00"
                   "VBP" "#FF8C00" "VBZ" "#FF8C00" "TO" "#FF8C00" "RB" "#FF8C00"
                   "RBR" "#FF8C00" "RBS" "#FF8C00" "PP" "#FF8C00"}
        char-width {\. 4, \a 8, \b 8, \c 8, \d 8, \e 8, \f 4,
                    \g 8, \h 8, \i 4, \j 4, \k 8, \l 4,
                    \m 8, \n 8, \o 8, \p 8, \q 8, \r 8, \s 8, \t 4,
                    \u 8, \v 8, \w 8, \x 8, \y 8, \z 8, \0 10, \1 10,
                    \2 10, \3 10, \4 10, \5 10, \6 10, \7 10, \8 10,
                    \9 10, \A 10, \B 10, \C 10, \D 10, \E 10, \F 10,
                    \G 10, \H 10, \I 5, \J 10, \K 10, \L 10, \M 10,
                    \N 10, \O 10, \P 10, \Q 10, \R 10, \S 10, \T 10,
                    \U 10, \V 10 \W 10, \X 10, \Y 10, \Z 10, \( 10,
                    \) 10, \- 10, \– 10, \— 10, \, 4}
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
        delay-in-ms 100
        color-to-use (if (= @color-scheme "props") pos-color-props pos-color)]
    (letfn [(path-drawer [i]
                         (str "M" (.-x (.-source i)) "," (+ (.-y (.-source i)) 4)
                              "L" (.-x (.-target i)) "," (.-y (.-target i))))
            (get-pos-color [p] (if (nil? (color-to-use p))
                                 "#808080" (color-to-use p)))
            (get-word-length [w] (->> (map char w)
                                      (map #(char-width %))
                                      (remove nil?)
                                      (reduce +)))
            (get-node-length [n]
                             (max (get-word-length (.-word (.-target n)))
                                  (get-word-length (.-pos (.-target n)))))]
      (-> svg ; lines
          (.selectAll "g")
          (.data links)
          (.append "path")
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.attr "d" (fn [i] (path-drawer i)))
          (.attr "stroke" "gray")
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
          (.attr "y" (fn [o] (int (+ (.-y (.-target o)) 2))))
          (.attr "height" (fn [o] (if-not (clojure.string/blank?
                                       (.-word (.-target o))) 14 0)))
          (.attr "fill" "#FFFFFF")
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.attr "width" (fn [o] (if-not (clojure.string/blank?
                                          (.-word (.-target o)))
                                   (get-node-length o) 0))))
      (-> node-content ; word
          (.append "text")
          (.transition)
          (.delay (fn [d i] (* i delay-in-ms)))
          (.text (fn [o] (.-word (.-target o))))
          (.attr "x" (fn [o] (+ 3 (int (- (.-x (.-target o))
                                     (/ (get-node-length o) 2))))))
          (.attr "y" (fn [o] (int (+ (.-y (.-target o)) 12))))
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


;; display stats
(defn output-tree-stats [response]
  (let [idea-density (->> (/ (float (:num-props response))
                             (:num-words response))
                          (* 10)
                          int)]
      [:table {:class "table table-striped"}
       [:tr [:td "# Nodes: "] [:td (:num-nodes response)]]
       [:tr [:td "# Words: "] [:td (:num-words response)]]
       [:tr [:td "# Parsed words: "] [:td (:num-tokens response)]]
       [:tr [:td "# Propositions: "] [:td (:num-props response)]]
       [:tr [:td "# Props per 10 words: "] [:td idea-density]]
       [:tr [:td "Max depth: "] [:td (:max-depth response)]]
       [:tr [:td "Most frequent nodes:"] [:td " "]]
       (map (fn [x] [:tr [:td (first x)] [:td (last x)]])
            (:top-five response))]))


;; Handle GET request to mccawley-api
(defn handler [response]
  (do
    (reset! parsed-text (:parsed-text response))
    (display-tree @parsed-text)
    (reset! stats-html (output-tree-stats response))
    (.log js/console (:parsed-text response))))

(defn error-handler [{:keys [status status-text]}]
  (.log js/console (str "something bad happened: " status " " status-text)))


;; function to call when we click on parse button
(defn retrieve-parsed [param]
  (let [url-encoded-param (clojure.string/replace param #"," "%2C")
        uri (str "http://192.241.182.149:3000/parse/" url-encoded-param)]
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
           "Somewhere in la Mancha in a place whose name I do not care to remember a gentleman lived not long ago, one of those who has a lance and ancient shield on a shelf and keeps a skinny nag and a greyhound for racing."
           "The cosmos is all there is, or ever was, or ever will be."
           "It was a bright cold day in April, and the clocks were striking thirteen."
           "He was an old man who fished alone in a skiff in the Gulf Stream and he had gone eighty-four days now without taking a fish."
           "To the red country and part of the gray country of Oklahoma, the last rains came gently, and they did not cut the scarred earth."
           "It is a truth universally acknowledged, that a single man in possession of a good fortune must be in want of a wife."
           "Many years later, as he faced the firing squad, Colonel Aureliano Buendia was to remember that distant afternoon when his father took him to discover ice."
           "It is wrong to say that text is unstructured when there is a great deal of structure in text and we do ourselves no favor to ignore the lessons it can teach us."
           "The outbreak of the Liberal Revolution of 1820 in Lisbon compelled Pedro I's father to return to Portugal in April 1821, leaving him to rule Brazil as regent."
           "A screaming comes across the sky."
           ]))

;; function to render the page, react/reagent style!
(defn display-page []
  [:div
   [:h3 "The Parse is Right"]
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
   [:input {:type :radio :value :rainbow :name :color-scheme
            :on-click #(reset! color-scheme "rainbow")} "Rainbow Colors"]
   [:p]
   [:input {:type :radio :value :props :name :color-scheme
            :on-click #(reset! color-scheme "props")} "Highlight Props"]
   [:p]
   [:p @stats-html]
  ])

(defn display-top []
  [:p @start-text])

(defn main []
  (reagent/render-component [display-page] (.getElementById js/document "app"))
  (reagent/render-component [display-top] (.getElementById js/document "canvastop")))

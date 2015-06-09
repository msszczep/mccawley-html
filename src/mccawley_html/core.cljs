(ns mccawley-html.core
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET POST]]
            [cljs.reader :as reader]))

(enable-console-print!)

;; Define 2 reagent atoms we need for state
(def parsed-text (reagent/atom ""))
(def start-text (reagent/atom ""))

; The displaying happens here
(defn display-tree [tree-to-display]
  (let [clj-tree (reader/read-string tree-to-display)
        pos-color {"DT" "#FF1493" "NN" "#DC143C" "NNS" "#DC143C" "NNP" "#DC143C"
                   "NP" "#DC143C" "PRP" "#FF0000" "PRP$" "#FF0000" "SQ" "#9370DB"
                   "IN" "#00FF00" "TO" "#00FF00" "CD" "#FA8072" "CC" "#B8860B"
                   "WDT" "#9932CC" "SBAR" "#9932CC" "VB" "#1E90FF" "VBD" "#1E90FF"
                   "VBG" "#1E90FF" "VP" "#1E90FF" "VBN" "#1E90FF" "VBP" "#1E90FF"
                   "VBZ" "#1E90FF" "MD" "#00CED1" "QP" "#FA8072" "WHNP" "#9932CC"
                   "WP" "#9932CC"
                   "RB" "#FFA500" "RBR" "#FFA500" "RBS" "#FFA500" "ADVP" "#FFA500"
                   "PP" "#00FF00" "S" "#800080" "ROOT" "#800080" "FRAG" "#800080"
                   "JJ" "#FF8C00" "JJR" "#FF8C00" "JJS" "#FF8C00" "ADJP" "#FF8C00"
                   }
        svg (-> js/d3
                (.select "svg")
                (.attr "width" "1000")
                (.attr "height" "500")
                (.attr "id" "visualization")
                (.attr "xmlns" "http://www.w3.org/2000/svg"))
        tree (-> js/d3
                 (.-layout)
                 (.tree)
                 (.size (clj->js [1000 450])))
        nodes (-> tree
                  (.nodes (clj->js clj-tree)))
        links (-> tree
                  (.links nodes)
                  )
        node-content (-> svg
                         (.selectAll "g")
                         (.data nodes)
                         (.enter)
                         (.append "g")
                         (.attr "transform" "translate(0,0)"))]
    (letfn [(path-drawer [i]
                         (str "M" (.-x (.-source i)) "," (.-y (.-source i))
                              "L" (.-x (.-target i)) "," (.-y (.-target i))))
            (get-pos-color [p] (if (nil? (pos-color p))
                                 "#808080" (pos-color p)))]
      (-> svg ; lines
          (.selectAll "g")
          (.data links)
          (.append "path")
          (.transition)
          (.delay (fn [d i] (* i 100)))
          (.attr "d" (fn [i] (path-drawer i)))
          (.attr "stroke" "black")
          (.attr "stroke-width" "2")
          (.attr "fill" "none"))
      (-> node-content ; rectangle for part of speech
          (.data nodes)
          (.append "rect")
          (.attr "x" (fn [o] (int (.-x o))))
          (.attr "y" (fn [o] (int (- (.-y o) 20))))
          (.attr "height" 20)
          (.attr "fill" (fn [o] (get-pos-color (.-pos o))))
          (.transition)
          (.delay (fn [d i] (* i 100)))
          (.attr "width" 45))
      (-> node-content ; rectangle for word
          (.append "rect")
          (.attr "x" (fn [o] (int (.-x o))))
          (.attr "y" (fn [o] (int (.-y o))))
          (.attr "height" (fn [o] (if (not= (.-word o) "") 20 0)))
          (.attr "fill" "#FFFFFF")
          (.transition)
          (.delay (fn [d i] (* i 100)))
          (.attr "width" (fn [o] (if (not= (.-word o) "") 45 0))))
      (-> node-content ; word
          (.append "text")
          (.transition)
          (.delay (fn [d i] (* i 100)))
          (.text (fn [o] (.-word o)))
          (.attr "x" (fn [o] (int (.-x o))))
          (.attr "y" (fn [o] (int (+ (.-y o) 20))))
          (.attr "fill" "black"))
      (-> node-content ; part of speech
          (.append "text")
          (.transition)
          (.delay (fn [d i] (* i 100)))
          (.text (fn [o] (.-pos o)))
          (.attr "x" (fn [o] (int (.-x o))))
          (.attr "y" (fn [o] (int (.-y o))))
          (.attr "fill" "white"))

      )))


;; Handle GET request to our external service
(defn handler [response]
  (do
    (reset! parsed-text (:parsed-text response))
    (display-tree @parsed-text)
    (.log js/console (:parsed-text response))))

(defn error-handler [{:keys [status status-text]}]
  (.log js/console (str "something bad happened: " status " " status-text)))


;; function to call when we click on parse button
(defn retrieve-parsed [param]
  (let [uri (str "http://localhost:3000/parse/" param)]
    (GET uri {:handler handler
              :error-handler error-handler
              :response-format :json
              :keywords? true})))


(defn reset-all []
  (reset! parsed-text "")
  (reset! start-text "")
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
  ])

(defn main []
  (reagent/render-component [display-page] (.getElementById js/document "app")))

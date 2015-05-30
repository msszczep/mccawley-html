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
        svg (-> js/d3
                (.select "svg")
                (.attr "width" "1000")
                (.attr "height" "500")
                (.attr "id" "visualization")
                (.attr "xmlns" "http://www.w3.org/2000/svg"))
        tree (-> js/d3
                (.-layout)
                (.tree)
                (.size (clj->js [1000 400])))
        nodes (-> tree
                (.nodes (clj->js clj-tree)))
        ]
    (-> svg
        ;(.append "text")
        ;(.text clj-tree)
        ;(.attr "x" 100)
        ;(.attr "y" 100))))
        (.selectAll "circle")
        (.data nodes)
        (.enter)
        (.append "circle")
        (.attr "cx" (fn [o] (int (.-x o))))
        (.attr "cy" (fn [o] (int (.-y o))))
        (.attr "r" "10")
        (.attr "fill" "black"))))


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

;(def links (-> tree
;               (.links nodes)))

;(-> svg
;    (.selectAll "circle")
;    (.data nodes)
;    (.enter)
;    (.append "circle")
;    (.attr "cx" (fn [o] (int (.-x o))))
;    (.attr "cy" (fn [o] (int (.-y o))))
;    (.attr "r" "10")
;    (.attr "fill" "black")
    ;(.transition)
    ;(.delay "5000")
    ;(.attr "r" "10")
;    )

;(def line (-> js/d3.svg
;              (.line)
;              (.x (fn [o] (.-x o)))
;              (.y (fn [o] (.-y o)))
;              (.interpolate "linear")))

;(defn pow [n p]
;  (.pow js/Math n p))

;(defn get-euclidean-distance [d]
;  (pow (+ (pow (- (.x (first d)) (.x (last d))) 2)
;          (pow (- (.y (first d)) (.y (last d))) 2)) 0.5))

;(defn xy-helper [i]
;  [{x i.target.x, y i.target.y} {x i.source.x, y i.source.y}])

;; function to render the page, react/reagent style!
(defn display-page []
   [:div
     [:h2 "Parsed-text: " @parsed-text]
     [:p "start-text has value: " @start-text]
     [:input {:type "text"
              :value @start-text
              :on-change #(reset! start-text (-> % .-target .-value))}]
     [:p]
     [:button {:on-click #(retrieve-parsed @start-text)} "Parse"]
     [:p]
    ])

(defn main []
  (reagent/render-component [display-page] (.getElementById js/document "app")))

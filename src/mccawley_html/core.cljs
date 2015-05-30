(ns mccawley-html.core
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET POST]]))

(enable-console-print!)

;; Define 2 reagent atoms we need for state
(def parsed-text (reagent/atom ""))
(def t (reagent/atom ""))


;; Handle GET request to our external service
(defn handler [response]
  (reset! parsed-text (:parsed-text response))
  (.log js/console (:parsed-text response)))

(defn error-handler [{:keys [status status-text]}]
  (.log js/console (str "something bad happened: " status " " status-text)))


;; function to call when we click on parse button
(defn retrieve-parsed [param]
  (let [uri (str "http://localhost:3000/parse/" param)]
    (GET uri {:handler handler
              :error-handler error-handler
              :response-format :json
              :keywords? true})))

;; The rendering happens here
(def svg (-> js/d3
             (.select "svg")
             (.attr "width" "500")
             (.attr "height" "500")
             (.attr "id" "visualization")
             (.attr "xmlns" "http://www.w3.org/2000/svg")))

(def tree (-> js/d3
              (.-layout)
              (.tree)
              (.size (clj->js [300 300]))))

(def d2 {:pos "ROOT", :word "", :children [{:pos "S", :word "", :children [{:pos "NP", :word "", :children [{:pos "NNP", :word "Jesus", :children []},]},{:pos "VP", :word "", :children [{:pos "VBD", :word "wept", :children []},]},{:pos ".", :word ".", :children []},]},]})

(def nodes (-> tree
               (.nodes (clj->js d2))))

(def links (-> tree
               (.links nodes)))

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

(-> svg
    (.selectAll "circle")
    (.data nodes)
    (.enter)
    (.append "circle")
    (.attr "cx" (fn [o] (int (.-x o))))
    (.attr "cy" (fn [o] (int (.-y o))))
    (.attr "r" "10")
    (.attr "fill" "steelblue")
    )
;    .-transition)
;    (.delay (fn [d] (if (= (.depth d) 1000) 3000 0)))
;    (.duration (fn [d] 1000))
;    (.attr "r" 10))

;; function to render the page, react/reagent style!
(defn display-page []
   [:div
     [:h2 "Parsed-text: " @parsed-text]
     [:p "t has value: " @t]
     [:input {:type "text"
              :value @t
              :on-change #(reset! t (-> % .-target .-value))}]
     [:p]
     [:button {:on-click #(retrieve-parsed @t)} "Parse"]
     [:p]
    ])

(defn main []
  (reagent/render-component [display-page] (.getElementById js/document "app")))

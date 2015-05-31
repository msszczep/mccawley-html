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
        links (-> tree
                  (.links nodes))
        node-content (-> svg
                         (.selectAll "g")
                         (.data nodes)
                         (.enter)
                         (.append "g")
                         (.attr "transform" "translate(0,0)"))]
    (letfn [(path-drawer [i]
                         (str "M" (.-x (.-source i)) "," (.-y (.-source i))
                              "L" (.-x (.-target i)) "," (.-y (.-target i))))]
      (-> node-content
          (.append "circle")
          (.attr "cx" (fn [o] (int (.-x o))))
          (.attr "cy" (fn [o] (int (.-y o))))
          (.attr "fill" "orange")
          (.transition)
          (.delay "2000")
          (.attr "r" "10"))
      (-> node-content
          (.append "text")
          (.transition)
          (.delay "3000")
          (.text (fn [o] (.-word o)))
          (.attr "x" (fn [o] (int (.-x o))))
          (.attr "y" (fn [o] (int (.-y o))))
          (.attr "fill" "black"))
      (-> svg
          (.selectAll "path")
          (.data links)
          (.enter)
          (.append "path")
          (.transition)
          (.delay "1000")
          (.attr "d" (fn [i] (path-drawer i)))
          (.attr "stroke" "steelblue")
          (.attr "stroke-width" "2")
          (.attr "fill" "none")))))


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
  (-> js/d3 (.select "svg") (.remove)))


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

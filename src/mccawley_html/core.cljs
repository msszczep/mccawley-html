(ns mccawley-reagent.core
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
(defn retrieve-pos [param]
  (let [uri (str "http://localhost:3000/parse/" param)]
  (GET uri {:handler handler
            :error-handler error-handler
            :response-format :json
            :keywords? true})))


;; function to render the page, react/reagent style!
(defn hw []
   [:div
     [:h2 "Parsed-text: " @parsed-text]
     [:p "t has value: " @t]
     [:input {:type "text"
              :value @t
              :on-change #(reset! t (-> % .-target .-value))}]
     [:p]
     [:button {:on-click #(retrieve-pos @t)}
      "Parse"]])


(defn main []
  (reagent/render-component [hw] (.getElementById js/document "app")))

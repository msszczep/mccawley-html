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
                    \N 10, \O 10, \P 10, \Q 10, \R 10, \S 13, \T 10,
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
      [:table {:class "table table-striped table-bordered"}
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

(defn display-interface []
  [:div
   [:center
   [:table {:width "1000px"}
    [:tr [:td
   [:h3 "The Parse is Right"]
    [:p]
    [:button {:class "btn btn-xs btn-warning"
              :type "button"
              :data-toggle "collapse"
              :data-target "#tpirAbout"
              :aria-expanded "false"
              :aria-controls "tpirAbout"} "About"]
    " "
    [:button {:on-click #(reset! start-text (get-random-sentence))
              :class "btn btn-xs btn-success"} "Random"]
    " "
    [:button {:on-click #(retrieve-parsed @start-text)
              :class "btn btn-xs btn-primary"} "Parse"]
    " "
    [:button {:on-click #(reset-all)
              :class "btn btn-xs btn-danger"} "Reset"]
    " "
    [:button {:class "btn btn-xs btn-info"
              :type "button"
              :data-toggle "collapse"
              :data-target "#contactInfo"
              :aria-expanded "false"
              :aria-controls "contactInfo"} "Contact"]
    [:p]
    [:input {:type :radio :value :rainbow :name :color-scheme
             :on-click #(reset! color-scheme "rainbow")} " Rainbow Colors"]
    " "
    [:input {:type :radio :value :props :name :color-scheme
             :on-click #(reset! color-scheme "props")} " Highlight Props"]]
    [:td     [:textarea {:rows 4
                :cols 100
                :placeholder "Type an English sentence in this field, then click Parse.  A syntactic tree will be drawn in the gray box below.
If you can't think of a sentence, click Random to get a randomly-selected English sentence.
Click Reset to clear this field and the entire screen.
Scroll to the bottom after clicking Parse to see some statistics."
                :value @start-text
                :on-change #(reset! start-text (-> % .-target .-value))}]]]]]
    [:p]
    [:div {:class "collapse" :id "contactInfo"}
     "If you have any questions or concerns about this application, you may email the developer,
     mitchell at szcz dot org."
     [:p]
     "Github forks and pull requests to the "
     [:a {:href "http://github.com/msszczep/mccawley-api"} "back end API"] " and "
     [:a {:href "http://github.com/msszczep/mccawley-html"} "front end HTML"]
     " are welcome."
     [:p]
     "To close this window, simply click the Contact button above."
     ]
    [:p]
    [:div {:class "collapse" :id "tpirAbout"}
     "The Parse is Right is a web app built by " [:a {:href "http://www.szcz.org"} "Mitchell Szczepanczyk"]
     " that dynamically draws a syntactic tree of an English sentence typed into the text box at the
     top of this app.  The tree appears in the large gray box below.  An assortment of useful statistics
     appears below the gray box after the Parse button is clicked.  "
     [:p]
     "To clear the statistics, the gray box, and the text, simply click the Reset button.  You can
     select one of a pre-programmed set of random English sentences by clicking the Random button.
     You have two color schemes to choose from: Rainbow Colors (which uses an assortment of colors)
     or Highlight Propositions (which colors what are formally called Propositions in a shade of orange
     and colors all other nodes as gray)"
      [:p]
     "The Parse is Right uses the Stanford CoreNLP library for parsing English, the part-of-speech
     tags in the Penn Treebank Project (listed below for convenience), and is written in the Clojure
     and ClojureScript programming languages.  The source code for this website is
     online, for the " [:a {:href "http://github.com/msszczep/mccawley-api"} "back end API"] " and "
     [:a {:href "http://github.com/msszczep/mccawley-html"} "front end HTML"] "."
      [:p]
     "For reference, here are the part-of-speech tags in the Penn Treebank Project, used in this app."
     [:p]
     [:center
     [:table {:class "table table-striped table-bordered"}
      [:tr [:th "Abbrev."] [:th "Brief Explanation"] [:th "Examples"] [:th "Color"]]
      [:tr [:td "CC"] [:td "Coordinating Conjunction"] [:td "and, or, but"] [:td "Gold"]]
      [:tr [:td "CD"] [:td "Cardinal Number"] [:td "first, second, third"] [:td "Salmon"]]
      [:tr [:td "DT"] [:td "Determiner"] [:td "a, an, the"] [:td "Pink"]]
      [:tr [:td "EX"] [:td "Existential there"] [:td "There lived a mouse..."] [:td "Midnight blue"]]
      [:tr [:td "FW"] [:td "Foreign word"] [:td "savoire-faire"] [:td "Black"]]
      [:tr [:td "IN"] [:td "Preposition or subordinating conjunction"] [:td "of, by, with"] [:td "Green"]]
      [:tr [:td "JJ"] [:td "Adjective"] [:td "blue, dry, angry"] [:td "Dark orange"]]
      [:tr [:td "JJR"] [:td "Adjective, comparative"] [:td "faster, higher, stronger"] [:td "Dark orange"]]
      [:tr [:td "JJS"] [:td "Adjective, superlative"] [:td "fastest, highest, strongest"] [:td "Dark orange"]]
      [:tr [:td "LS"] [:td "List item marker"] [:td "Firstly, secondly..."] [:td "Brown"]]
      [:tr [:td "MD"] [:td "Modal"] [:td "can, would, should"] [:td "Turquoise"]]
      [:tr [:td "NN"] [:td "Noun, singular or mass"] [:td "world, house, salt"] [:td "Red"]]
      [:tr [:td "NNS"] [:td "Noun, plural"] [:td "dogs, alumni, glasses"] [:td "Red"]]
      [:tr [:td "NNP"] [:td "Proper noun, singular"] [:td "France, Olympic"] [:td "Red"]]
      [:tr [:td "NNPS"] [:td "Proper noun, plural"] [:td "Jupiters, Sarahs"] [:td "Red"]]
      [:tr [:td "PDT"] [:td "Predeterminer"] [:td "all, both"] [:td "Deep pink"]]
      [:tr [:td "POS"] [:td "Possessive ending"] [:td "'s"] [:td "Maroon"]]
      [:tr [:td "PRP"] [:td "Personal pronoun"] [:td "She, it, me"] [:td "Bright red"]]
      [:tr [:td "PRP$"] [:td "Possessive pronoun"] [:td "My, our, their"] [:td "Bright red"]]
      [:tr [:td "RB"] [:td "Adverb"] [:td "truly, madly, deeply"] [:td "Yellow"]]
      [:tr [:td "RBR"] [:td "Adverb, comparative"] [:td "earlier, faster, quicker"] [:td "Yellow"]]
      [:tr [:td "RBS"] [:td "Adverb, superlative"] [:td "earliest, best"] [:td "Yellow"]]
      [:tr [:td "RP"] [:td "Particle"] [:td "Up"] [:td "Navy blue"]]
      [:tr [:td "SYM"] [:td "Symbol"] [:td "&, $, %"] [:td "Seagreen"]]
      [:tr [:td "TO"] [:td "to"] [:td "-"] [:td "Green"]]
      [:tr [:td "UH"] [:td "Interjection"] [:td "Wow!"] [:td "Violet"]]
      [:tr [:td "VB"] [:td "Verb, base form"] [:td "eat, drink, love"] [:td "Blue"]]
      [:tr [:td "VBD"] [:td "Verb, past tense"] [:td "ate, drank, loved"] [:td "Blue"]]
      [:tr [:td "VBG"] [:td "Verb, gerund or present participle"] [:td "eating, drinking, loving"] [:td "Blue"]]
      [:tr [:td "VBN"] [:td "Verb, past participle"] [:td "eaten, gone"] [:td "Blue"]]
      [:tr [:td "VBP"] [:td "Verb, non-3rd person singular present"] [:td "fight, want"] [:td "Blue"]]
      [:tr [:td "VBZ"] [:td "Verb, 3rd person singular present"] [:td "fights, wants"] [:td "Blue"]]
      [:tr [:td "WDT"] [:td "Wh-determiner"] [:td "whereupon"] [:td "Dark orchid"]]
      [:tr [:td "WP"] [:td "Wh-pronoun"] [:td "who, what, when"] [:td "Light purple"]]
      [:tr [:td "WP$"] [:td "Possessive wh-pronoun"] [:td "Whose"] [:td "Dark orchid"]]
      [:tr [:td "WRB"] [:td "Wh-adverb"] [:td "When, how, why"] [:td "Magenta"]]]]
     [:p]
     "Here is a table explaining some of the constituents used in the app:"
     [:p]
     [:center
     [:table {:class "table table-striped table-bordered"}
      [:tr [:th "Abbrev."] [:th "Brief Explanation"] [:th "Color"]]
      [:tr [:td "NP"] [:td "Noun phrase"] [:td "Red"]]
      [:tr [:td "PP"] [:td "Prepositional phrase"] [:td "Green"]]
      [:tr [:td "S"] [:td "Sentence"] [:td "Purple"]]
      [:tr [:td "SBAR"] [:td "Clause introduced by a (possibly empty) subordinating conjunction."] [:td "Purple"]]
      [:tr [:td "SBARQ"] [:td "Direct question introduced by a wh-word or a wh-phrase."] [:td "Purple"]]
      [:tr [:td "VP"] [:td "Verb phrase"] [:td "Blue"]]
      [:tr [:td "WHNP"] [:td "WH-noun phrase"] [:td "Light purple"]]]]
     [:p]
     "A fuller list may be found " [:a {:href "https://gist.github.com/nlothian/9240750"} "here"] "."
     [:p]
     "Here is a table explaining the statistics that appear at the bottom of the page:"
     [:p]
     [:center
      [:table {:class "table table-striped table-bordered"}
       [:tr [:th "Abbrev."] [:th "Brief Explanation"]]
       [:tr [:td "Nodes"] [:td "The number of squares in the tree, coinciding roughly with the constituents and words."]]
       [:tr [:td "Words"] [:td "The number of words in the sentences, broken up by spaces."]]
       [:tr [:td "Parsed Words"] [:td "The number of parsed words in the sentences, broken up by nodes."]]
       [:tr [:td "Propositions"] [:td "The number of propositions (idea-bearing units), coinciding with verbs, adjectives, adverbs, and prepositional phrases."]]
       [:tr [:td "Props per 10 words"] [:td "The number of propositions, multiplied by 10, divided by the number of words, and rounded down."]]
       [:tr [:td "Max depth"] [:td "The number of nodes drawn deepest point of the tree, starting with a count of zero at the top."]]
       [:tr [:td "Most frequent nodes"] [:td "The five most common node types in the tree, with a count of each."]]]]
     [:p]
     "To close this window, simply click the About button above."]])

(defn display-stats []
  [:p @stats-html])


(defn main []
  (reagent/render-component [display-interface] (.getElementById js/document "app"))
  (reagent/render-component [display-stats] (.getElementById js/document "stats-battery")))

(ns octohabit.main
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [clojure.string :as string]
   [octohabit.about :as about]
   [octohabit.actions :as actions]
   [octohabit.data :as data]
   [octohabit.common :as common]))

(def Bar js/Recharts.Bar)
(def BarChart js/Recharts.BarChart)
(def XAxis js/Recharts.XAxis)
(def ResponsiveContainer js/Recharts.ResponsiveContainer)

(def today
  (->>
   (.toISOString (js/Date.))
   (#(subs % 0 10))
   (js/Date.)))

(def selected-day-state (r/atom today))
(def app-state (r/atom nil))
(def enter-task-state (r/atom ""))
(def email-address-state (r/atom ""))
(def about-open? (r/atom false))
(def hovered-habit (r/atom nil))

(defn ->value [element]
  (-> element .-target .-value))

(defn input [{:keys [id label name input-type
                     placeholder atom subtext]}]
  [:<>
   [:label {:for id} label]
   [:input {:type input-type
            :id id
            :name name
            :placeholder placeholder
            :value @atom
            :on-change #(reset! atom (->value %))}]
   (when subtext
     [:small subtext])])

(def checkbox-table-cell-style
  {:vertical-align "middle"
   :text-align "center"
   :white-space "nowrap"})

(def days-of-week ["su" "mo" "tu" "we" "th" "fr" "sa"])
(def months-of-year ["jan" "feb" "mar" "apr" "may" "jun"
                     "jul" "aug" "sep" "oct" "nov" "dec"])

(defn checkbox-table-date-row [{:keys [date-now n-days]}]
  (into
   [:tr
    [:th ""]
    [:th ""]]
   (map (fn [d]
          (let [jsDate (->> (- date-now (* d common/DAY-MILLIS))
                            (js/Date.))]
            [:td {:style checkbox-table-cell-style}
             [:strong
              (->> jsDate (.getDay) (get days-of-week))]
             " "
             (.getDate jsDate)]))
        (range 0 n-days 1))))

(defn checkbox [date task checkbox-id]
  [:input {:type "checkbox"
           :checked (->> task :dates (some #{date}))
           :on-click #(actions/toggle-checkbox
                       app-state
                       {:checkbox %
                        :date date
                        :id (:id task)})
           :style {:padding "2px"
                   :display "block"
                   :margin "auto"
                   :width "2em"
                   :height "2em"
                   :background-color (if (->> task
                                              :dates
                                              (some #{date}))
                                       "#1095c1"
                                       "white")}
           :id checkbox-id}])

(defn checkbox-row [{:keys [task n-days]}]
  (into [:tr
         [:td {:style checkbox-table-cell-style}
          [:button
           {:style {:padding "2px"
                    :display "block"
                    :margin "auto"
                    :width "2em"
                    :height "2em"}
            :on-click #(actions/delete-task app-state (:id task))}
           "☒"]]
         [:th [:div
               {:onMouseOver #(reset! hovered-habit (:id task))
                :onMouseOut #(reset! hovered-habit nil)}
               (:title task)]]
         (conj
          (for [d (range 0 n-days 1)]
            (let [date (->>
                        (- @selected-day-state
                           (* d common/DAY-MILLIS))
                        (js/Date.))
                  date-str (->>
                            date
                            (.toISOString)
                            (#(subs % 0 10)))
                  checkbox-id (str (:title task)
                                   "-"
                                   date-str)]
              [:td {:style (merge checkbox-table-cell-style
                                  (when (= date today)
                                    {:background-color
                                     "#1095c122"
                                     :border-left "1px solid #00000055"
                                     :border-right "1px solid #00000055"}))}
               [checkbox date-str task checkbox-id]])))]))

(defn checkbox-table [{:keys [n-days tasks date-now]}]
  [:div.container-fluid
   [:figure
    [:table {:style {:text-align "center"}}
     [checkbox-table-date-row
      {:date-now date-now
       :n-days n-days}]
     (for [task tasks]
       [checkbox-row {:task task :n-days n-days}])]]])

(defn nav-row []
  [:nav.container-fluid
   [:ul
    [:li
     [:a {:href "http://www.lzeitlin.xyz/"}
      "lz blog"]]]
   [:ul
    [:li [:a {:data-target "about-modal"
              :on-click #(reset! about-open? true)}
          "about"]]
    [:li
     (when (-> @app-state :email string/blank? not)
       [:details {:role "list" :dir "rtl"}
        [:summary {:aria-haspopup "listbox"
                   :role "link"}
         (-> @app-state :email)]
        [:ul {:role "listbox"}
         [:li [:button {:on-click #(actions/send-data-via-email
                                    app-state
                                    email-address-state)}
               "save to email"]]]])]]])

(defn title []
  [:hgroup {:style {:text-align "center"}}
   [:h1 "🐙 octohabit"]
   [:h3 "the 8-track, email-based habit tracker"]])

(defn new-user-flow []
  [:article
   [:p "👋 new to " [:strong "octohabit"]
    "? i recommend reading the "
    [:a {:on-click #(reset! about-open? true)}
     "about"]
    "."]
   [input {:id "enter_email_address"
           :label "email address"
           :input-type "email"
           :name "email"
           :placeholder "your email address"
           :atom email-address-state
           :subtext "no validation, i just trust
                               you to type your own email address
                               correctly 😱"}]
   [:button {:on-click #(swap! app-state
                               assoc
                               :email
                               @email-address-state)}
    "add email address"]])

(defn add-habits-form []
  [:details
   [:summary {:role "button"
              :style {:text-align "left"}}
    "add habits"]
   [:div
    [input {:id "enter_task"
            :label "new habit"
            :input-type "text"
            :name "task"
            :placeholder "go on a 🚶‍♀️"
            :atom enter-task-state
            :subtext "house-style is lowercase and emojis but you do you 🪴"}]
    [:button
     {:on-click #(do (actions/append-task app-state enter-task-state)
                     (actions/reset-enter-task-field enter-task-state)
                     (data/set-data-param @app-state))
      :disabled (string/blank? @enter-task-state)}
     "+"]]])

(def date-control-style {:margin "0.5em"})

(defn date-control []
  [:div {:style {:display "flex"
                 :justify-content "center"}}
   [:button
    {:style date-control-style
     :on-click #(actions/change-selected-day selected-day-state 1)}
    "forward"]
   [:input {:type "date"
            :id "date"
            :name "date"
            :style date-control-style
            :value (-> @selected-day-state
                       (.toISOString)
                       (subs 0 10))
            :on-change #(->> %
                             ->value
                             (js/Date.)
                             (reset! selected-day-state))}]
   [:button
    {:style date-control-style
     :on-click #(reset! selected-day-state today)}
    "today"]
   [:button
    {:style date-control-style
     :on-click #(actions/change-selected-day selected-day-state -1)}
    "backward"]])

(defn app-mount-fn []
  (let [url-param-data
        (or (data/get-data-from-url)
            {:tasks [{:title "default"
                      :dates []
                      :id 0}]})]
    (reset! app-state url-param-data)))

(defn- rolling-chart []
  (let [habit-id @hovered-habit
        habit-data (some->> @app-state
                            :tasks
                            (filter #(= (:id %) habit-id))
                            first)
        per-month (some->> habit-data
                           :dates
                           (group-by #(subs % 0 7))
                           (reduce-kv (fn [acc k v]
                                        (conj acc {:month k
                                                   :count (count v)}))
                                      [])
                           (sort-by :month)
                           (reverse))]
    (when habit-id
      [:div
       [:hgroup {:style {:text-align "center"}}
        [:h3 (:title habit-data)]]
       [:> ResponsiveContainer {:width "100%"
                                :height "20%"}
        [:> BarChart {:width 500
                      :height 500
                      :data (clj->js per-month)}
         [:> XAxis {:dataKey "month"}]
         [:> Bar {:dataKey "count"
                  :fill "#1095c1"}]]]
       [:hgroup {:style {:text-align "center"}}
        [:h3 "you are doing gr8"]]
       ])))

(defn app-render-fn []
  (let [_ @app-state]
    [:<>
     [about/modal about-open?]
     [:main.container-fluid
      [nav-row]
      [title]
      [:container
       (if (string/blank? (-> @app-state :email))
         [new-user-flow]
         [:div
          (if (->> @app-state :tasks count (< 7))
            [:article "All eight habit slots filled. Keep it up!"]
            [add-habits-form])
          (when (seq (-> @app-state :tasks))
            [:div
             [date-control]
             [checkbox-table
              {:n-days common/N-DAYS
               :tasks (->> @app-state :tasks (sort-by :id))
               :date-now @selected-day-state}]
             [rolling-chart]])
          ])]]]))

(defn app []
  (r/create-class
   {:component-did-mount app-mount-fn
    :reagent-render app-render-fn}))

(rdom/render [app] (.getElementById js/document "app"))


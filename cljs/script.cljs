(require
 '[reagent.core :as r]
 '[reagent.dom :as rdom]
 '[clojure.string :as string]
 '[clojure.edn :as edn]
 '[clojure.walk :as walk])

(def DAY-MILLIS (* 24 60 60 1000))
(def DATE-NOW (js/Date.))
(def N-DAYS 7)
(def chart-width 50)

(def state (r/atom nil))
(def enter-task-state (r/atom ""))
(def email-address-state (r/atom ""))
(def task-id-atom (r/atom 0))


(defn edn-to-url-param [edn-data]
  (let [edn-str (pr-str edn-data)]
    (js/encodeURIComponent edn-str)))

(defn set-data-param [value]
  (let [encoded-value (js/encodeURIComponent (pr-str value))
        new-url (str js/location.protocol "//" 
                     js/location.host 
                     js/location.pathname 
                     "?data=" encoded-value)]
    (js/history.pushState nil "" new-url)))

(defn send-data-via-email []
  (let [current-url js/location.href
        address @email-address-state
        subject "Your Habit Tracker Data Link"
        body (str "Access your data by clicking on the following link:\n\n" current-url)
        encoded-body (js/encodeURIComponent body)
        mailto-link (str "mailto:" address "?subject=" subject "&body=" encoded-body)]
    (swap! state assoc :email address)
    (js/window.open mailto-link)))

(defn append-task []
  (set-data-param
   (swap! state update :tasks conj {:title @enter-task-state
                                    :dates []
                                    :id (swap! task-id-atom inc)})))

(defn delete-task [id]
  (set-data-param
   (swap! state update :tasks (fn [tasks]
                                (remove
                                 #(= (:id %) id)
                                 tasks)))))

(defn reset-enter-task-field []
  (reset! enter-task-state ""))

(defn input [{:keys [id label name input-type placeholder atom]}]
  [:label {:for id} label
   [:input {:type input-type
            :id id
            :name name
            :placeholder placeholder
            :value @atom
            :on-change #(reset! atom (-> %
                                         .-target
                                         .-value))}]])


(def cell-style {:vertical-align "middle"
                 :text-align "center"})

(defn table-date-row [{:keys [date-now n-days]}]
  (into 
   [:tr
    [:th ""]
    [:th ""]]
   (map #(vector :td {:style cell-style}
                 (->> (- date-now (* % DAY-MILLIS))
                      (js/Date.)
                      (.getDate)
                      ))
        (range 0 n-days 1))))

(defn toggle-checkbox [{:keys [checkbox date id]}]
  (let [checked? (->> checkbox .-target .-checked)
        update-fn (if checked?
                    #(do (conj % date))
                    #(remove #{date} %))]
    (set-data-param
    (swap! state update :tasks
           (fn [tasks]
             (for [t tasks]
               (if (= id (:id t))
                 (update t :dates update-fn)
                 t)))))))


(defn checkbox-row [{:keys [task n-days]}]
  (into [:tr 
         [:td {:style cell-style}
          [:button
           {:style {:padding "2px"
                    :display "block"
                    :margin "auto"
                    :width "2em"
                    :height "2em"}
                    :on-click #(delete-task (:id task))}
               "☒"]]
         [:th [:div (:title task)]]
         (conj
          (for [d (range 0 n-days 1)]
            (let [date (->> 
                        (- DATE-NOW (* d DAY-MILLIS))
                        (js/Date.)
                        (.toISOString)
                        (#(subs % 0 10))) 
                  checkbox-id (str (:title task)
                                   "-"
                                   date)]
              [:td {:style cell-style}
               [:input {:type "checkbox"
                        :checked (->> task :dates (some #{date}))
                        :on-click #(toggle-checkbox {:checkbox %
                                                     :date date
                                                     :id (:id task)})
                        :style {:padding "2px"
                                :display "block"
                                :margin "auto"
                                :width "2em"
                                :height "2em"}
                        :id checkbox-id}]])))]))

(defn checkbox-table [{:keys [n-days tasks date-now]}]
  [:div.container-fluid
   [:figure
    [:table {:style {:text-align "center"}}
     [table-date-row {:date-now date-now
                      :n-days n-days}]
     (for [task tasks]
       [checkbox-row {:task task :n-days n-days}])]]])



(defn get-data-from-url
  []
  (->> js/window
       (.-location)
       (.-search)
       (new js/URLSearchParams)
       (seq)
       (js->clj)
       (into {})
       (walk/keywordize-keys)
       :data
       edn/read-string))

(defn app []
  (r/create-class
   {:display-name "lzb-app"
    :component-did-mount
    (fn []
      (let [url-param-data
            (or (get-data-from-url)
                {:tasks [{:title "default"
                          :dates []
                          :id 0}]})] 
        (reset! state url-param-data)))
    :reagent-render
    (fn []
      (let [_ @state]
        [:<>
         #_[:p (str @state)]
         [:nav.container-fluid
          [:ul
           [:li
            [:a {:href "http://www.lzeitlin.xyz/"}
             "lz blog"]]]
          [:ul
           [:li [:strong (-> @state :email)]]]
          ]
         [:main.container 
          (if (string/blank? (-> @state :email))
            [:article
             [input {:id "enter_email_address"
                     :label "Email Address"
                     :input-type "email"
                     :name "email"
                     :placeholder "Your email address"
                     :atom email-address-state}]
             [:button {:on-click #(swap! state
                                         assoc
                                         :email
                                         @email-address-state )}
              "add email address"]]
            [:div
             [:button
              {:on-click send-data-via-email
               :style {:text-align "left"}}
              "save to email"]
             [:details
              [:summary {:role "button"
                         :style {:text-align "left"}} "add habits"]
              
              [:article 
               [input {:id "enter_task"
                       :label "new habit"
                       :input-type "text"
                       :name "task"
                       :placeholder "go for a walk"
                       :atom enter-task-state}]
               [:button
                {:on-click #(do (append-task)
                                (reset-enter-task-field)
                                (set-data-param @state))
                 :disabled (string/blank? @enter-task-state)}
                "+"]]]
             (when (seq (-> @state :tasks))
               [checkbox-table {:n-days N-DAYS
                                :tasks (->> @state :tasks (sort-by :id))
                                :date-now DATE-NOW}])])]]))}))

(rdom/render [app] (.getElementById js/document "app"))


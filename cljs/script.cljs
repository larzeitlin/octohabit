(require
 '[reagent.core :as r]
 '[reagent.dom :as rdom]
 '[clojure.string :as string]
 '[clojure.edn :as edn]
 '[clojure.walk :as walk])

(def DAY-MILLIS (* 24 60 60 1000))
(def DATE-NOW (js/Date.))
(def N-DAYS 7)
(def MAX-HABITS 8)

(def chart-width 50)

(def state (r/atom nil))
(def enter-task-state (r/atom ""))
(def email-address-state (r/atom ""))
(def about-open? (r/atom false))

(defn pad [n val coll]
  (take n (concat coll (repeat val))))

(defn habit-history-array [start-date dates]
  (when (and (seq dates) start-date)
    (let [end (or (->> dates last (js/Date.) (.getTime)))
          start (->> start-date (js/Date.) (.getTime))
          one-day (* 24 60 60 1000)
          dates-range (range start (+ end one-day) one-day) 
          dates-matches (set (map #(->> %
                                        (js/Date.)
                                        (.getTime))
                                  dates))]
      (->> dates-range
           (map #(if (dates-matches %) 1 0))
           (apply str)))))

(defn edn->state-str [e]
  (let [start-date (->> e
                        :tasks
                        (map :dates)
                        (map first)
                        (filter identity)
                        sort
                        first)
        email (-> e :email)
        task-names (->> e
                        :tasks
                        (map :title)
                        (pad MAX-HABITS nil)
                        (interpose ",")
                        (apply str))
        history-arrays (->> e
                            :tasks
                            (map :dates)
                            (map (partial habit-history-array
                                            start-date))
                            (interpose ",")
                            (apply str))
        state-string (apply str (interpose ","
                                           [start-date
                                            email
                                            task-names
                                            history-arrays]
                                           ))]
    (js/LZString.compressToEncodedURIComponent state-string)))

(defn binary-string->dates [start-date bin-str]
  (->> bin-str
       (map edn/read-string)
       (map-indexed
        (fn [idx b]
          (let [d (js/Date. start-date)]
            (when (= b 1)
              (.setDate d (+ (.getDate d) idx))
              (-> d
                  (js/Date.)
                  (.toISOString)
                  (subs 0 10))))))
       (filter identity)))

(defn url-data->edn [s]
  (let [decoded-str (js/LZString.decompressFromEncodedURIComponent s)
        [start-date email & rst] (string/split decoded-str #",")
        [habit-names history-arrays] (split-at 8 rst)
        habit-names (remove string/blank? habit-names) 
        history-arrays (map (partial binary-string->dates start-date)
                            history-arrays)
        habits (map
                  (fn [n arr id] {:title n :dates arr :id id})
                  habit-names
                  (pad (count habit-names) [] history-arrays)
                  (range (count habit-names)))]
    {:email email
     :tasks habits}))

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
       url-data->edn))

(defn set-data-param [value]
  (let [encoded-value (edn->state-str value)
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
                                    :id (count (:tasks @state))})))

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
                    #(sort (conj % date))
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


(defn about-modal []
  [:dialog {:open @about-open?}
   [:article
    [:a {:on-click #(reset! about-open? false)
         :class "close"}]
    [:h1 "welcome to octohabit"]
    [:p "octohabit is a super simple habit tracker with a slightly unorthodox
         implementation. your data is your own. i don't want it but i make it
         easy for you to look after it. there is no backend service to process 
         your data. this is all \"client side\"."]
    [:p "limitations can help us on our journey. octohabit has some.
         you can only have up to eight habits.
         you can only store up to around two years of data.
         continue reading to understand why."]
    [:h3 "how it works"]
    
    [:p "there is no login or signup process. you simply add your email address
         to the app. when you have logged your habits click your email address 
         and then click \"save to email\". this will open up your email client and
         you will send yourself an email with a link that contains all your current data.
         do this whenever you use the app. rather than logging in just go to your
         email inbox and click the latest link. you can think of it like a magic-link
         login but backwards. compression is used to squeeze a whole bunch of data into
         the url (which has a character limit). this is the reason for the limitations
         above."]
    [:h3 "won't this flood my inbox with emails?"]
    [:p "i use my email client's filtering and labeling tools to move
         all my habit tracking emails to a folder. this prevents it cluttering my
         inbox and makes finding the latest link super easy."]
    [:h3 "technology"]
    [:p "built with clojurescript / scittle. you can find the code on "
     [:a {:href "https://github.com/larzeitlin/lzboard"} "github"]
     ". there is no licence attached to it but feel free to use it however you
      want and i promise to not be upset."]]])

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
         ;; State debug
         #_[:p (str @state)]
         #_[:p (edn->state-str @state)]
         #_[:p (str (url-data->edn (edn->state-str @state)))]
         ;; -----------
         [about-modal]
         
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
            (when (-> @state :email string/blank? not)
              [:details {:role "list" :dir "rtl"}
               [:summary {:aria-haspopup "listbox"
                          :role "link"}
                (-> @state :email)]
               [:ul {:role "listbox"}
                [:li [:button {:on-click send-data-via-email}
                      "save to email"]]]])]]]
         [:h1 {:style {:text-align "center"}} "octohabit"]
         [:main.container 
          (if (string/blank? (-> @state :email))
            [:article
             [:p "new to octohabit? i recommend reading the "
              [:a {:on-click #(reset! about-open? true)}
               "about"]]
             [input {:id "enter_email_address"
                     :label "email address"
                     :input-type "email"
                     :name "email"
                     :placeholder "your email address"
                     :atom email-address-state}]
             [:button {:on-click #(swap! state
                                         assoc
                                         :email
                                         @email-address-state)}
              "add email address"]]
            [:div 
             (if (->> @state :tasks count (< 7))
               [:article "All eight habit slots filled. Keep it up!"]
               [:details {:open true}
                [:summary {:role "button"
                           :style {:text-align "left"}}
                 "add habits"]
                [:div 
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
                  "+"]]])
             (when (seq (-> @state :tasks))
               [checkbox-table {:n-days N-DAYS
                                :tasks (->> @state :tasks (sort-by :id))
                                :date-now DATE-NOW}])])]]))}))

(rdom/render [app] (.getElementById js/document "app"))


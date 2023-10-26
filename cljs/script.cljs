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

(def selected-day-atom (r/atom (js/Date.)))
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
    (set! (.-href js/window.location) mailto-link)))

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

(defn change-selected-day [n-days]
  (swap! selected-day-atom
         #(js/Date.
           (+ (.getTime %) (* n-days DAY-MILLIS)))))

(defn reset-enter-task-field []
  (reset! enter-task-state ""))

(defn input [{:keys [id label name input-type placeholder atom
                     subtext]}]
  [:<>
   [:label {:for id} label]
   [:input {:type input-type
            :id id
            :name name
            :placeholder placeholder
            :value @atom
            :on-change #(reset! atom (-> %
                                         .-target
                                         .-value))}]
   (when subtext
     [:small subtext])])


(def cell-style {:vertical-align "middle"
                 :text-align "center"})

(def days-of-week ["su" "mo" "tu" "we" "th" "fr" "sa"])
(def months-of-year ["jan" "feb" "mar" "apr" "may" "jun"
                     "jul" "aug" "sep" "oct" "nov" "dec"])

(defn table-date-row [{:keys [date-now n-days]}]
  (into 
   [:tr
    [:th ""]
    [:th ""]]
   (map (fn [d]
          (let [jsDate (->> (- date-now (* d DAY-MILLIS))
                            (js/Date.)
                            )]
            [:td {:style cell-style}
             [:strong 
              (->> jsDate (.getDay) (get days-of-week))]
             "\n"
             (.getDate jsDate)]))
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
                        (- @selected-day-atom (* d DAY-MILLIS))
                        (js/Date.)
                        (.toISOString)
                        (#(subs % 0 10))) 
                  checkbox-id (str (:title task)
                                   "-"
                                   date)]
              [:td {:style (merge cell-style
                                  (when (= d 0)
                                    {:background-color
                                     "#1095c122"
                                     :border-left "1px solid #00000055"
                                     :border-right "1px solid #00000055"}))}
               [:input {:type "checkbox"
                        :checked (->> task :dates (some #{date}))
                        :on-click #(toggle-checkbox {:checkbox %
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
   [:article {:style {:text-align "center"}}
    [:a {:on-click #(reset! about-open? false)
         :class "close"}]
    [:hgroup
     [:h1 "🐙"]
     [:h1 "welcome to octohabit"]
     [:h3 "the 8-track, email-based habit tracker"]]
    [:p "octohabit is a super simple habit tracker with a slightly unorthodox
         implementation. your data is your own
         i don't want it but i make it easy for you to look after it."]
    [:p "there is no backend service to process your data. 
         no password to forget or reset,
         no app to download or update.\n
         octohabit is available and synced across all
         your devices so long as you can access your email." ]
    [:h3 "limitations"]
    [:p "limitations can help us on our journey. octohabit has some.
         you can only have up to eight habits.
         you can only store up to around two years of data. 
         octohabit is simple and no-frills because that's what
         i want from a habit tracker. maybe you like this?"]
    [:h3 "how it works"]
    [:p "all your data gets compressed and encoded into the url which is sent
         to your email inbox. sceptical? seems inconvenient? see the FAQ below."]
    [:h5 "logging out"]
    [:p "there is no signup process. simply add your email address to the app.
         when you have logged your habits click your email address 
         and then click \"save to email\". this will open up your email client and
         you will send yourself an email with a link that contains all your current data.
         do this once you are done updating your habits."]
    [:h5 "logging in"]
    [:p "just go to your email inbox and click the link in the latest email."]
    [:h3 "FAQ"]
    [:h6 "won't this flood my inbox with emails?"]
    [:p "i use my email client's filtering and labeling tools to move
          all my habit tracking emails to a folder. this prevents it cluttering my
          inbox and makes finding the latest link super easy.
          you can also set up some automatic deleting of old tagged emails,
          but since they are so small i wouldn't bother."]
    [:h6 "isn't it wasteful to send an email for every update?"]
    [:p "i don't think so. each email is about 3kb on the wire. 
           the average modern app would make several http requests back and forth
           of this size or larger simply to handle auth and then a bunch more while
           using the app itself. then there is the impact of running a server,
           a database, and so on. 
           i think this approach has a much lighter impact on the
           planet and my brain!"]
    [:h6 "is this secure?"]
    [:p "is your email secure? in a sense this is the most secure
          habit tracker app you will ever use because i don't ever
          handle your data. i'm curious about your super secret habits
          now though!"]
    [:h6 "i prefer a normal login process / mobile app / etc"]
    [:p "hey that's fine. this one isn't for you. best of luck on your habit
           tracking journey. ✌️"]
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
         [:main.container-fluid
          
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
          [:hgroup {:style {:text-align "center"}}
           [:h1 "🐙 octohabit"]
           [:h3 "the 8-track, email-based habit tracker"]]
          
          [:container 
           (if (string/blank? (-> @state :email))
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
                               correctly 😱" }]
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
                          :placeholder "go on a 🚶‍♀️"
                          :atom enter-task-state
                          :subtext "house-style is lowercase and emojis but you do you 🪴"}]
                  [:button
                   {:on-click #(do (append-task)
                                   (reset-enter-task-field)
                                   (set-data-param @state))
                    :disabled (string/blank? @enter-task-state)}
                   "+"]]])
              (when (seq (-> @state :tasks))
                [:div
                 [:div {:style {:display "flex"
                                :justify-content "center"}}
                  [:td [:button
                        {:on-click #(change-selected-day 1)} "⇦"]]
                  [:th {:colspan "3"} [:input {:type "date"
                                               :id "date"
                                               :name "date"
                                               :value (-> @selected-day-atom
                                                          (.toISOString)
                                                          (subs 0 10))
                                               :on-change #(->> %
                                                                .-target
                                                                .-value
                                                                (js/Date.)
                                                                (reset! selected-day-atom))}]]
                  [:td [:button
                        {:on-click #(change-selected-day -1)} "⇨"]]]
                 
                 [checkbox-table {:n-days N-DAYS
                                  :tasks (->> @state :tasks (sort-by :id))
                                  :date-now @selected-day-atom}]])])]]]))}))

(rdom/render [app] (.getElementById js/document "app"))


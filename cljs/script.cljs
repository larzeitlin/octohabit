(require
 '[reagent.core :as r]
 '[reagent.dom :as rdom])

(def DAY-MILLIS (* 24 60 60 1000))
(def DATE-NOW (js/Date.))

(def state (r/atom {:tasks [{:title "default" :dates []}]}))
(def enter-task-state (r/atom ""))
(def task-id-atom (r/atom 0))

(defn append-task []
  (swap! state update :tasks conj {:title @enter-task-state
                                   :dates []
                                   :id (swap! task-id-atom inc)}))

(defn delete-task [id]
  (swap! state update :tasks (fn [tasks]
                               (remove
                                #(= (:id %) id)
                                tasks))))

(defn reset-enter-task-field []
  (reset! enter-task-state ""))

(defn task-input []
  [:input {:type "text"
           :id "enter_task"
           :name "task"
           :value @enter-task-state
           :on-change #(reset! enter-task-state (-> %
                                                    .-target
                                                    .-value))}])

(defn table-date-row [{:keys [date-now n-days]}]
  (into 
   [:tr
    [:th ""]
    [:th ""]]
   (map #(vector :td
                 (->> (- date-now (* % DAY-MILLIS))
                      (js/Date.)
                      (.getDate)
                      ))
        (range (dec n-days) -1 -1))))

(def close-button-svg
  [:svg {:xml:space "preserve"
         :width "512px"
         :viewbox "0 0 512 512"
         :style "enable-background:new 0 0 512 512;"
         :height "512px"}
   [:path {:d "M443.6,387.1L312.4,255.4l131.5-130c5.4-5.4,5.4-14.2,0-19.6l-37.4-37.6c-2.6-2.6-6.1-4-9.8-4c-3.7,0-7.2,1.5-9.8,4  L256,197.8L124.9,68.3c-2.6-2.6-6.1-4-9.8-4c-3.7,0-7.2,1.5-9.8,4L68,105.9c-5.4,5.4-5.4,14.2,0,19.6l131.5,130L68.4,387.1  c-2.6,2.6-4.1,6.1-4.1,9.8c0,3.7,1.4,7.2,4.1,9.8l37.4,37.6c2.7,2.7,6.2,4.1,9.8,4.1c3.5,0,7.1-1.3,9.8-4.1L256,313.1l130.7,131.1  c2.7,2.7,6.2,4.1,9.8,4.1c3.5,0,7.1-1.3,9.8-4.1l37.4-37.6c2.6-2.6,4.1-6.1,4.1-9.8C447.7,393.2,446.2,389.7,443.6,387.1z"}]])


(defn toggle-checkbox [{:keys [checkbox date id]}]
  (let [checked? (->> checkbox .-target .-checked)
        update-fn (if checked?
                    #(do (conj % date))
                    #(remove #{date} %))]
    (swap! state update :tasks
           (fn [tasks]
             (for [t tasks]
               (if (= id (:id t))
                 (update t :dates update-fn)
                 t))))))

(defn checkbox-row [{:keys [task n-days]}]
  (into [:tr 
         [:td [:button {:style {:padding "5px"
                                :display "block"
                                :margin "auto"}
                        :on-click #(delete-task (:id task))}
               "\u2716"]]
         [:th (:title task)]
         (for [d (range (dec n-days) -1 -1)]
           (let [date (->> 
                       (- DATE-NOW (* d DAY-MILLIS))
                       (js/Date.)
                       (.toISOString)
                       (#(subs % 0 10))) 
                 checkbox-id (str (:title task)
                         "-"
                         date)]
             [:td [:input {:type "checkbox"
                           :on-click #(toggle-checkbox {:checkbox %
                                                        :date date
                                                        :id (:id task)})
                           :id checkbox-id}]]))]))

(defn checkbox-table [{:keys [n-days tasks date-now]}]
  [:table {:style {:text-align "center"}}
      [table-date-row {:date-now date-now
                       :n-days n-days}]
      (for [task tasks]
        [checkbox-row {:task task :n-days n-days}])])

(defn app [] 
  (let [state @state
        tasks (:tasks state)]
    [:div
     [:p "State: " (str state)]
     [:label {:for "task"} "Enter a task"]
     [task-input]
     [:p [:button {:on-click #(do (append-task)
                                  (reset-enter-task-field))}
          "add task"]] 
     (when (seq tasks)
       [checkbox-table {:n-days 10
                        :tasks tasks
                        :date-now DATE-NOW}])]))

(rdom/render [app] (.getElementById js/document "app"))

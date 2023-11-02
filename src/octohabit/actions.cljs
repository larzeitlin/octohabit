(ns octohabit.actions
  (:require [octohabit.data :as data]
            [octohabit.common :as common]))

(defn send-data-via-email [app-state email-address-state]
  (let [current-url js/location.href
        address @email-address-state
        subject "Your Habit Tracker Data Link"
        body (str "Access your data by clicking on the following link:\n\n" current-url)
        encoded-body (js/encodeURIComponent body)
        mailto-link (str "mailto:" address "?subject=" subject "&body=" encoded-body)]
    (swap! app-state assoc :email address)
    (set! (.-href js/window.location) mailto-link)))

(defn append-task [app-state enter-task-state]
  (data/set-data-param
   (swap! app-state update :tasks conj {:title @enter-task-state
                                    :dates []
                                    :id (count (:tasks @app-state))})))

(defn delete-task [app-state id]
  (data/set-data-param
   (swap! app-state update :tasks (fn [tasks]
                                (remove
                                 #(= (:id %) id)
                                 tasks)))))

(defn change-selected-day [selected-day-state n-days]
  (swap! selected-day-state
         #(js/Date.
           (+ (.getTime %) (* n-days common/DAY-MILLIS)))))

(defn toggle-checkbox [app-state
                       {:keys [checkbox date id]}]
  (let [checked? (->> checkbox .-target .-checked)
        update-fn (if checked?
                    #(sort (conj % date))
                    #(remove #{date} %))]
    (data/set-data-param
    (swap! app-state update :tasks
           (fn [tasks]
             (for [t tasks]
               (if (= id (:id t))
                 (update t :dates update-fn)
                 t)))))))

(defn reset-enter-task-field [enter-task-state]
  (reset! enter-task-state ""))

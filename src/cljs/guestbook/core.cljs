(ns guestbook.core
    (:require [reagent.core :as r]
              [re-frame.core :as rf]
              [ajax.core :refer [GET POST]]
              [clojure.string :as string]
              [guestbook.validation :refer [validate-message]]))

(rf/reg-event-fx 
 :app/initialize
 (fn [_ _]
   {:db {:messages/loading? true}}))

(rf/reg-sub
 :errors/map
 (fn [db _]
   (:errors/map db {})))

(rf/reg-event-db 
 :errors/set
 (fn [db [_ errors]]
   (-> db
       (assoc :errors/map errors))))

(rf/reg-sub 
 :messages/loading?
 (fn [db _]
   (:messages/loading? db)))

(rf/reg-event-db
 :messages/set
 (fn [db [_ messages]]
   (-> db
       (assoc :messages/loading? false
              :messages/list messages))))
(rf/reg-sub
 :messages/list
 (fn [db _]
   (:messages/list db [])))

(rf/reg-event-db
 :message/add
 (fn [db [_ message]]
   (update db :messages/list conj message)))

(defn send-message! [fields]
  (if-let [validation-errors (validate-message @fields)]
    (rf/dispatch [:errors/set validation-errors])
    (POST "/message"
      {:format :json
       :headers {"Accept" "application/transit+json" "x-csrf-token" (.-value (.getElementById js/document "token"))}
       :params @fields
       :handler #(do
                   (rf/dispatch [:message/add (assoc @fields :timestamp (js/Date.))])
                   (rf/dispatch [:errors/set nil])
                   (reset! fields nil))
       :error-handler #(do
                         (.log js/console (str %))
                         (rf/dispatch [:errors/set (get-in % [:response :errors])]))})))

(defn get-messages []
  (GET "/messages"
    {:headers {"Accept" "application/transit+json"}
     :handler #(rf/dispatch [:messages/set (:messages %)])}))

(defn message-list [messages]
  (println messages)
  [:ul.messages
   (for [{:keys [timestamp message name]} @messages]
     ^{:key timestamp}
     [:li
      [:time (.toLocaleString timestamp)]
      [:p message]
      [:p " - " name]])])

(defn errors-component [errors id]
  (when-let [error (id @errors)]
    [:div.notification.is-danger (string/join error)]))

(defn message-form []
    (let [fields (r/atom {})
          errors (rf/subscribe [:errors/map])]
        (fn []
          (println "errors in message form" errors)
          [:div
           [:div.field
            [:label.label {:for :name} "Name"]
            [errors-component errors :name]
            [:input.input
             {:type :text
              :name :name
              :on-change #(swap! fields assoc :name (-> % .-target .-value))
              :value (:name @fields)}]]
           [:div.field
            [:label.label {:for :message} "Message"]
            [errors-component errors :message]
            [:textarea.textarea
             {:name :message
              :on-change #(swap! fields assoc :message (-> % .-target .-value))
              :value (:message @fields)}]]
           [:input.button.is-primary
            {:type :submit
             :on-click #(send-message! fields)
             :value "comment"}]
           [:p "Name: " (:name @fields)]
           [:p "Message: " (:message @fields)]
           [:p "Errors: " @errors]])))


(defn home []
  (let [messages (rf/subscribe [:messages/list])]
    (rf/dispatch [:app/initialize])
    (get-messages)
    (fn []
      (if @(rf/subscribe [:messages/loading?])
        [:div>div.row>div.span12>h3
         "Loading Messages..."]
        [:div.content>div.columns.is-centered>div.column.is-two-thirds
         [:div.columns>div.column
          [:h3 "Messages"]
          [message-list messages]
          [:div.columns>div.column
           [message-form]]]]))))

(r/render
 [home]
 (.getElementById js/document "content"))

(comment 
  (+ 2 2))
(ns guestbook.core
    (:require [reagent.core :as r]
              [ajax.core :refer [GET POST]]
              [clojure.string :as string]
              [guestbook.validation :refer [validate-message]]))

(defn send-message! [fields errors messages]
  (if-let [validation-errors (validate-message @fields)]
    (reset! errors validation-errors)
    (POST "/message"
    {:format :json
     :headers {"Accept" "application/transit+json" "x-csrf-token" (.-value (.getElementById js/document "token"))}
     :params @fields
     :handler #((do
                  (swap! messages conj (assoc @fields :timestamp (js/Date.)))
                  (.log js/console (str "response: " %))
                  (reset! errors nil)
                  (reset! fields nil)))
     :error-handler #(do
                       (.log js/console (str %))
                       (reset! errors (get-in % [:response :errors])))})))
(defn get-messages [messages]
  (GET "/messages"
    {:headers {"Accept" "application/transit+json"}
     :handler #(reset! messages (:messages %))}))

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

(defn message-form [messages]
    (let [fields (r/atom {})
          errors (r/atom nil)]
        (fn []
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
               :on-click #(send-message! fields errors messages)
               :value "comment"}]
             [:p "Name: " (:name @fields)]
             [:p "Message: " (:message @fields)]])))


(defn home []
  (let [messages (r/atom nil)]
    (get-messages messages)
    (fn []
      [:div.content>div.columns.is-centered>div.column.is-two-thirds
       [:div.columns>div.column
        [:h3 "Messages"]
        [message-list messages]
       [:div.columns>div.column
        [message-form messages]]]])))

(r/render
 [home]
 (.getElementById js/document "content"))

(comment 
  (+ 2 2))
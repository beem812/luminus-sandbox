(ns guestbook.core
    (:require [reagent.core :as r]))

(r/render
    [:div {:id "hello", :class "content"}
     [:h1 "Hello, Reagent"]]
    (.getElementById js/document "content"))

(defn message-form []
    (let [fields (r/atom {})]
        (fn []
            [:div
             [:div.field
              [:label.label {:for :name} "Name"]
              [:input.input
               {:type :text
                :name :name
                :on-change #(swap! fields assoc :name (-> % .-target .-value))
                :value (:name @fields)}]]
             [:div.field
              [:label.label {:for :message} "Message"]
              [:textarea.textarea
               {:name :message
                :value (:massage @fields)
                :on-change #(swap! fields assoc :message (-> % .-target .-value))}]]
             [:input.button.is-primary
              {:type :submit
               :value "comment"}]])))

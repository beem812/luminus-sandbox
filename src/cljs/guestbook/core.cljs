(ns guestbook.core
    (:require [reagent.core :as r]
              [reagent.dom :as rd]
              [re-frame.core :as rf]
              [ajax.core :refer [GET]]
              [clojure.string :as string]
              [mount.core :as mount]
              [guestbook.websockets :as ws]
              [guestbook.validation :refer [validate-message]]))

(rf/reg-event-fx 
 :app/initialize
 (fn [_ _]
   {:db {:messages/loading? true}}))

(rf/reg-event-db 
 :form/set-field
 [(rf/path :form/fields)]
 (fn [fields  [_ id value]]
   (assoc fields id value)))

(rf/reg-event-db
 :form/clear-fields
 [(rf/path :form/fields)]
 (fn [_ _]
   {}))

(rf/reg-sub
 :form/fields
 (fn [db _]
   (:form/fields db)))

(rf/reg-sub
 :form/field
 :<- [:form/fields]
 (fn [fields [_ id]]
   (get fields id)))

(rf/reg-event-db 
 :form/set-server-errors
 [(rf/path :form/server-errors)]
 (fn [_ [_ errors]]
   errors))

(rf/reg-sub
 :form/server-errors
 (fn [db _]
   (:form/server-errors db)))

(rf/reg-sub
 :form/validation-errors
 :<- [:form/fields]
 (fn [fields _]
   (validate-message fields)))

(rf/reg-sub
 :form/validation-errors?
 :<- [:form/validation-errors]
 (fn [errors _]
   (not (empty? errors))))

(rf/reg-sub
 :form/errors
 :<- [:form/validation-errors]
 :<- [:form/server-errors]
 (fn [[validation server] _]
   (merge validation server)))

(rf/reg-sub
 :form/error
 :<- [:form/errors]
 (fn [errors [_ id]]
   (get errors id)))

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

(rf/reg-event-fx 
 :message/send!-called-back
 (fn [_ [_ {:keys [success errors]}]]
   (if success
     {:dispatch [:form/clear-fields]}
     {:dispatch [:form/set-server-errors errors]})))

(rf/reg-event-fx 
 :message/send!
 (fn [{:keys [db]} [_ fields]]
   {:db (dissoc db :form/server-errors)
    :ws/send! {:message [:message/create! fields]
               :timeout 10000
               :callback-event [:message/send!-called-back]}}))

(rf/reg-event-fx 
 :app/initialize
 (fn [_ _]
   {:db {:messages/loading? true}
    :dispatch [:messages/load]}))

(rf/reg-event-fx 
 :messages/load
 (fn [{:keys [db]} _]
   (GET "/api/messages"
     {:headers {"Accept" "application/transit+json"}
      :handler #(rf/dispatch [:messages/set (:messages %)])})
   {:db (assoc db :messages/loading? true)}))

(defn handle-response! [response]
  (println "Response: " response)
  (if-let [errors (:errors response)]

    (rf/dispatch [:form/set-server-errors errors])
    (do
      (rf/dispatch [:message/add response])
      (rf/dispatch [:form/clear-fields response]))))

(defn reload-messages-button []
  (let [loading? (rf/subscribe [:messages/loading?])]
    [:button.button.is-info.is-fullwidth
     {:on-click #(rf/dispatch [:messages/load])
      :disabled @loading?}
     (if @loading?
       "Loading Messages"
       "Refresh Messages")]))

(defn message-list [messages]
  [:ul.messages
   (for [{:keys [timestamp message name]} @messages]
     ^{:key timestamp}
     [:li
      [:time (.toLocaleString timestamp)]
      [:p message]
      [:p " - " name]])])

(defn errors-component [id]
  (when-let [error @(rf/subscribe [:form/error id])]
    [:div.notification.is-danger (string/join error)]))

(defn message-form []
  (fn []
    [:div
     [errors-component :server-error]
     [:div.field
      [:label.label {:for :name} "Name"]
      [errors-component :name]
      [:input.input
       {:type :text
        :name :name
        :on-change #(rf/dispatch
                     [:form/set-field
                      :name
                      (.. % -target -value)])
        :value @(rf/subscribe [:form/field :name])}]]
     [:div.field
      [:label.label {:for :message} "Message"]
      [errors-component :message]
      [:textarea.textarea
       {:name :message
        :on-change #(rf/dispatch [:form/set-field
                                  :message
                                  (.. % -target -value)])
        :value @(rf/subscribe [:form/field :message])}]]
     [:input.button.is-primary
      {:type :submit
       :disabled @(rf/subscribe [:form/validation-errors?])
       :on-click #(rf/dispatch
                   [:message/send!
                    @(rf/subscribe
                      [:form/fields])])
       :value "comment"}]]))

(defn home []
  (let [messages (rf/subscribe [:messages/list])]
    (rf/dispatch [:app/initialize])
    (fn []
      (if @(rf/subscribe [:messages/loading?])
        [:div>div.row>div.span12>h3
         "Loading Messages..."]
        [:div.content>div.columns.is-centered>div.column.is-two-thirds
         [:div.columns>div.column
          [:h3 "Messages"]
          [message-list messages]]
         [:div.columns>div.column
          [reload-messages-button]]
         [:div.columns>div.column
          [message-form]]]))))

(defn ^:dev/after-load mount-components []
  (.log js/console "Mounting Components...")
  (rd/render [#'home] (.getElementById js/document "content"))
  (.log js/console "Components Mounted!"))

(defn init! []
  (.log js/console "Initializing App...")
  (mount/start)
  (rf/dispatch [:app/initialize])
  (mount-components))

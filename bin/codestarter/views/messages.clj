(ns codestarter.views.messages
  (:use [codestarter.views.common :exclude (header)]
        codestarter.views.users
        hiccup.core
        hiccup.form-helpers
        [hiccup.page-helpers :exclude (url)]
        korma.core
        korma.db)
  (:require [clojure.string :as string]
            [noir.validation :as vali]))

(def messages* "WITH RECURSIVE parents AS (
SELECT messages.id, messages.parent, messages.time, messages.message, 
users.id as user_id, users.username, users.name, users.display,
users.github_display, users.google_display, users.twitter_display, users.facebook_display,
recipient, recipients.username as r_username, recipients.name as r_name, recipients.display as r_display,
recipients.github_display as r_github_display, 
recipients.google_display as r_google_display, 
recipients.twitter_display as r_twitter_display, 
recipients.facebook_display as r_facebook_display,
array[-messages.id, messages.id] as path, 0 as level
FROM messages 	
left outer join users on user_id = users.id
left outer join users as recipients on recipient = recipients.id
WHERE (user_id = ? or recipient = ?) and parent is null
UNION ALL
SELECT messages.id, messages.parent, messages.time, messages.message,
users.id, users.username, users.name, users.display,
users.github_display, users.google_display, users.twitter_display, users.facebook_display,
messages.recipient, recipients.username as r_username, recipients.name as r_name, recipients.display as r_display,
recipients.github_display as r_github_display, 
recipients.google_display as r_google_display, 
recipients.twitter_display as r_twitter_display, 
recipients.facebook_display as r_facebook_display,
path || -messages.id || messages.id, parents.level + 2 as level FROM messages
JOIN parents ON messages.parent = parents.id
left outer join users on messages.user_id = users.id
left outer join users as recipients on messages.recipient = recipients.id)
SELECT * FROM parents order by path;")

(defn index [id {:keys [params] :as request}]
  (body* [user (select-one users (where {:id id}))]
          (= id (get-user request :id))
          request
          (let [reply? (re-find #"/messages/\d+/create/" (request :uri))]
            {:session (assoc (request :session) :user 
                             (update users (set-fields {:messages (timestamp)}) (where {:id id})))
             :body
             (html
               (header user request (url "users" id "messages"))
               [:ul#messages.items
                (if (pos? (get-count messages (or {:user_id id} {:recipient id}))) 
                  (map #(vector
                          :li {:id (% :id) :style (format "margin-left: %sem" (% :level))}
                          [:div 
                           (if (= (% :user_id) id) [:em "you"] 
                             (link-to (url "users" (% :user_id)) (display %))) 
                           " to " 
                           (if (= (% :recipient) id) [:em "you"] 
                             (link-to (url "users" (% :recipient)) 
                                      (display {:username (% :r_username)
                                                :name (% :r_name)
                                                :display (% :r_display)
                                                :github_display (% :r_github_display)
                                                :google_display (% :r_google_display)
                                                :twitter_display (% :r_twitter_display)
                                                :facebook_display (% :r_facebook_display)}))) 
                           [:span.muted (pretty-time (% :time))]
                           (link-to (str (url "users" id "messages" (% :id)) "#" (% :id)) "link") 
                           (if (% :parent) 
                             (html " | " (link-to (str (url "users" id "messages" (% :parent)) "#" (% :parent)) "parent")))]
                          (markdown (% :message))
                          (let [message-id (params :message-id)
                                message? (= (% :id) (and message-id (read-string message-id)))]
                            (html
                             (if (and (not message?) (not= (% :user_id) (get-user request :id)))
                               (link-to {:class "button secondary tiny"} 
                                        (str (url "users" id "messages" (% :id) "create") "#" (% :id)) "reply"))
                             (if (and reply? message?)
                               (form-to [:post (str "#" (% :id))]
                                        (csrf)
                                        (wmd params)
                                        [:button.button {:name "reply"} "reply"])))))
                       (exec-raw [messages* [id id]] :results))
                       [:em "No messages"])])})))

(defn create [id message-id {:keys [params] :as request}]
  (body* [user (select-one users (where {:id id}) (with messages (where {:id message-id})))]
         (let [user-id (get-user request :id)]
               (and user-id (= user-id (user :id))) (not= user-id (user :user-id)))
         request
         (if message-id
           (assoc (redirect "") :session (assoc (request :session) :errors vali/*errors*))
           (html
             (header user request)
             [:h2 "create message"]
             (form-to [:post "."]
                      (csrf)
                      (control :recipient (text-field {:disabled true} :recipient (display user)))
                      (wmd params :big true)
                      [:button.button "send message"])))
         (required params :wmd-input)
         "sent!"
         (let [recipient (if message-id
                           (select-one messages (with users) (where {:id message-id})))
               recipient (if recipient (assoc recipient :id (recipient :id_2)) user)
               user (get-user request)
               message (insert messages (values {:parent message-id
                                                 :user_id (user :id) 
                                                 :recipient (recipient :id)
                                                 :time (timestamp)
                                                 :message (params :wmd-input)}))]
           (if (and (recipient :notify) (seq (recipient :email)))
             (email (str "[test] message from " (display user))
                    (html (markdown (message :message))
                          [:p "-----"]
                          [:p 
                           "You are receiving this email because you are subscribed to notifications for received messages. You can manage your subscriptions " 
                           (link-to (format "http:/localhost:3000/users/%s/subscriptions/" (recipient :id)) "here") "."])
                    [(recipient :email)]))
           (redirect
             (if message-id
               (string/replace (request :uri) #"\d+/create/" (str (message :id) "/#" (message :id)))
               (string/replace (request :uri) #"\d+/messages/create/" 
                               (str (message :user_id) "/messages/" (message :id) "/#" (message :id))))))))
(ns codestarter.views.subscriptions
  (:use codestarter.views.common 
        [codestarter.views.comments :only (comments*)]
        hiccup.core
        hiccup.form-helpers
        [hiccup.page-helpers :exclude (url)]
        korma.core
        korma.db)
  (:require clojure.set
            [clojure.string :as string]
            [noir.validation :as vali])
  (:import [com.sun.syndication.feed.synd SyndFeedImpl SyndEntryImpl SyndContentImpl]
           [com.sun.syndication.io SyndFeedOutput]))

(def keys* [:edit :application :comment :update :update_comment])

(defn edit [id {:keys [params] :as request}]
  (body* [project (select-one projects (where {:id id}))]
         true
         request
         (html
           (header project request)
           (let [subscription (select-one subscriptions 
                                          (where {:project_id id :user_id (:id (get-user request))})
                                          (with users))
                 subscription (merge subscription params)]
             (form-to [:post "."]
                      (csrf)
                      [:div.controls
                       (control 
                         :subscriptions
                         (html 
                           [:div
                            (map #(vector
                                    :label (check-box % (if subscription (subscription %))) 
                                    (string/replace (name %) "_" " "))
                                 keys*)]))]
                      (if (get-user request)
                        (control :email
                                 [:label.controls (check-box :email (subscription :email)) "receive notifications by email"]
                                 :label "")
                        (control :email (text-field :email (subscription :email))))
                      [:hr]
                      [:button.button "save changes"]
                      [:button.button.secondary {:name "get"} "get rss feed"])))
         (if (and (not (params :get)) (not (get-user request)))
           (required params :email))
         "Subscriptions updated."
         (if (params :get)
           (redirect (format "/projects/%s/rss/?%s" id 
                             (string/join "&" (map #(format "%s=%s" (name %) (params %)) (filter #(params %) keys*)))))
           (let [params (reduce #(assoc % %2 (read-string (or (% %2) "nil"))) params keys*)
                 user (get-user request)
                 where-clause {:project_id id :user_id (user :id)}
                 db-params (assoc (select-keys params keys*) :email (params :email))
                 subscriptions? (seq (filter #(params %) keys*))]
             (if (and subscriptions? (empty? (select subscriptions (where where-clause))))
               (insert subscriptions (values (merge {:project_id id :user_id (:id user)} db-params)))
               (if subscriptions?
                 (update subscriptions (set-fields db-params) (where where-clause))
                 (delete subscriptions (where where-clause))))
             (redirect "")))))

(defn rss [id {:keys [params] :as request}]
  (body* [project (select-one projects (where {:id id})
                              (with notifications 
                                (where {:type 
                                        [in (map name 
                                                 (clojure.set/intersection (set (keys params)) (set keys*)))]})
                                (with users)
                                (order :id :DESC)))]
         true
         request
         {:headers {"Content-Type" "application/rss+xml"}
          :body (-> (new SyndFeedOutput) 
                  (.outputString 
                    (doto (new SyndFeedImpl)
                      (.setFeedType "rss_2.0")
                      (.setTitle (project :name))
                      (.setLink (format "http://localhost:3000/projects/%s/rss/" id))
                      (.setDescription (project :description))
                      (.setEntries 
                        (map
                          #(doto (new SyndEntryImpl)
                             (.setTitle (format "%s%s by %s" 
                                                (string/replace (% :type) "update_comment" "comment") 
                                                (if (% :title) (str ": " (% :title)) "") (display %)))
                             (.setLink (str "http://localhost:3000" (% :link)))
                             (.setPublishedDate (% :time))
                             (.setDescription (doto (new SyndContentImpl) (.setType "text/html")
                                                (.setValue (% :notification)))))
                          (project :notifications))))))}))
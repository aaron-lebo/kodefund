(ns codestarter.views.comments
  (:use codestarter.views.common 
        hiccup.core
        hiccup.form-helpers
        [hiccup.page-helpers :exclude (url)]
        korma.core
        korma.db)
  (:require [clojure.string :as string]
            [noir.validation :as vali]))

(def sql
"WITH RECURSIVE parents AS (
SELECT comments.id, comments.%s, comments.parent, comments.time, comments.comment, 
users.id as id_2, users.username, users.name, users.display,
users.github_display, users.google_display, users.twitter_display, users.facebook_display,
array[-comments.id, comments.id] as path, 0 as level	
FROM comments 
join users on user_id = users.id
WHERE %s = ? and parent is null
  	UNION ALL
  	SELECT comments.id, comments.%s, comments.parent, comments.time, comments.comment,
  	users.id as id_2, users.username, users.name, users.display, 
    users.github_display, users.google_display, users.twitter_display, users.facebook_display,
  	path || -comments.id || comments.id, parents.level + 2 as level FROM comments
  		JOIN parents 
  			ON (comments.%s = parents.%s and comments.parent = parents.id)
  		join users on comments.user_id = users.id)
SELECT * FROM parents order by path;")

(defn comments* [id where-clause {:keys [comment-id] :as params} request]
  (let [comment-id (when comment-id (read-string comment-id))
        reply? (re-find #"/comments/\d+/create/" (request :uri))
        uri (string/replace (request :uri) #"comments(.+)" "")
        user-id (get-user request :id)]
    (html 
      (if user-id
        (form-to {:id 0} [:post (str (string/replace (request :uri) #"comments/(.*)" "") "comments/")]
                 (csrf)
                 (wmd params)
                 [:button.button "create comment"])
        [:p (link-to {:class "button"} (str "/users/login/?redirect-to=" (request :uri)) "login to comment")])
      [:ul#comments.items
       (if (pos? (get-count comments where-clause)) 
         (map
           #(vector 
              :li.item {:id (% :id) :style (format "margin-left: %sem" (% :level))}
              [:div 
               (link-to (url "users" (% :id_2)) (display %))
               [:span.muted (pretty-time (% :time))]
               (link-to (format "%scomments/%s/#%s" uri (% :id) (% :id)) "link") 
               (if (% :parent) 
                 (html " | " (link-to (format "%scomments/%s/#%s" uri (% :parent) (% :parent)) "parent")))]
              [:div (markdown (% :comment))]
              (if (or (not reply?) (not= (% :id) comment-id))
                (link-to {:class "button secondary tiny"} 
                         (format "%scomments/%s/create/#%s" uri (% :id) (% :id)) "reply"))
              (if user-id
                (if (and reply? (= (% :id) comment-id))
                  (form-to [:post (str "." "#" (% :id))]
                           (csrf)
                           (wmd params :n "-2")
                           [:button.button {:name "reply"} "reply"]))))
           (let [id (name (first (keys where-clause)))]
             (exec-raw 
               [(format sql id id id id id) [(first (vals where-clause))]] :results))))
       (html [:hr] [:em "No comments"])])))

(defn index [id {:keys [params] :as request}]
  (body* [project (select-one projects (where {:id id}))]
    true
    request
    (html
      (header project request :uri (url "projects" id "comments"))
      (comments* id {:project_id id} params request))))

(defn create [id content-n comment-id content-type {:keys [params] :as request}]
  (body* [_ true]
         (-> request :session :user)
         request
         (assoc (redirect "") :session (assoc (request :session) :errors vali/*errors*))
         (let [key (if (params :wmd-input-2) :wmd-input-2 :wmd-input)]
           (vali/rule (vali/has-value? (params key)) 
                      [key "Comment is required."]))
           "comment!"
           (let [content (if (= content-type :updates) 
                           [(select updates (where {:id content-n :project_id id})) :update_id :update_comment]
                           [(select projects (where {:id id})) :project_id :comment])
                 key (second content)
                 content-key (nth content 2)
                 content (-> content first first)
                 comment (or (params :wmd-input-2) (params :wmd-input))
                 user (get-user request)
                 comment (insert comments (values {key (content :id)  
                                                   :parent comment-id
                                                   :user_id (user :id)
                                                   :time (timestamp)
                                                   :comment comment}))]
             (notify id 
                     user 
                     (comment :time) 
                     content-key
                     (content :title)
                     (format "%s%s/#%s" (string/replace (request :uri) #"\d+/create/" "") (comment :id) (comment :id))
                     (comment :comment))
             (redirect 
               (format "%s%s/#%s" 
                       (string/replace (request :uri) #"\d+/create/" "") (comment :id) (comment :id))))))
(ns codestarter.views.updates
  (:use codestarter.views.common 
        [codestarter.views.comments :only (comments*)]
        hiccup.core
        hiccup.form-helpers
        [hiccup.page-helpers :exclude (url)]
        korma.core
        korma.db)
  (:require [noir.validation :as vali]))

(defn index [id {:keys [params] :as request}]
  (body* [project (select-one projects (where {:id id}) (with updates (with users) (order :id :DESC)))]
        true
        request
        (html
          (header project request)
          (if (= (project :user_id) (:id (get-user request)))
            [:p (link-to {:class "button"} "create/" "create update")])
          (if (empty? (project :updates)) 
            (html [:hr] [:em "No updates"])
            [:ul#updates.items
             (map
               #(vector :li
                        [:h3 (% :title)
                         [:small
                          [:span (link-to (url "users" (% :user_id)) 
                                          (display %))] 
                          [:span.muted (pretty-time (% :time))]]]
                        [:div (markdown (% :update))]
                        [:div (link-to (str (% :id) "/") 
                                       (get-count comments {:update_id (% :id)}) " comments")])
               (project :updates))]))))

(defn show [id update-id {:keys [params] :as request}]
  (body* [update (select-one updates (where {:id update-id})
                            (with projects)
                            (with users)
                            (with comments
                              (with users)))]
        true
        request
        (html
          (header (assoc update :id (update :id_2)) request :uri (url "projects" id "updates"))
          [:h2 (update :title)
           [:small
            [:span (link-to (url "users" (update :user_id)) (display update))] 
            [:span.muted (pretty-time (update :time))]]]
          [:div (markdown (update :update))]
          [:hr]
          [:h3 "comments"]
          (comments* id {:update_id update-id} params request))))

(defn create [id {:keys [params] :as request}]
  (body* [project (select-one projects (where {:id id}))]
        (= (project :user_id) (:id (get-user request)))
        request
        (html
          (header project request :uri (url "projects" id "updates"))
          [:h2 "create update"]
          (form-to [:post "."]
                   (csrf)
                   (control :title (text-field :title (params :title)))
                   (wmd params :big true)
                   [:button.button "create update"]))
        (let [{:keys [title wmd-input]} params]
          (required params :title)
          (vali/rule (vali/has-value? wmd-input) 
                     [:wmd-input "Update is required."])
          (vali/rule (empty? (select updates (where {:project_id id :title title})))
                     [:title "Title must be unique."]))
        "update!"
        (let [user (get-user request)
              update (insert updates (values {:project_id id
                                              :time (timestamp)
                                              :user_id (user :id) 
                                              :title (params :title)
                                              :update (params :wmd-input)}))]
          (notify id user (update :time) :update (update :title) 
                  (url "projects" id "updates" (update :id)) (update :update))
          (redirect (format "/projects/%s/updates/" id)))))
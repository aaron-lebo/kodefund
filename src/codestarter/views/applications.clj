(ns codestarter.views.applications
  (:use codestarter.views.common 
        hiccup.core
        hiccup.form-helpers
        [hiccup.page-helpers :exclude (url)]
        [korma.core :exclude (select* delete*)]
        korma.db)
  (:require [clojure.data :as data] 
            [clojure.string :as string]
            [noir.validation :as vali]))

(defn index [id {:keys [params] :as request}]
  (body* [project (select-one projects (where {:id id}) (with applications (with users)))]
         (= (project :type) "new [funding, lead]")
         request
         (html
           (header project request)
           (let [user-id (:id (get-user request))
                 application (select-one applications 
                                         (where {:project_id id :user_id user-id})
                                         (with users))]
             (html
               (if (and (not application) (not= (project :user_id) user-id))
                 [:p (link-to {:class "button"} "create/" "create application")])
               (if (empty? (project :applications)) 
                 (html [:hr]
                       [:div.item [:em "No applications"]])
                 [:ul#applications.items
                  (let [user-id? #(= (% :user_id) user-id)
                        item #(vector 
                                :li.item
                                [:h4 (link-to (url "users" (% :id_2)) (display %))]
                                [:div (markdown (% :application))]
                                (if (user-id? %) 
                                  (link-to {:class "button secondary tiny"} 
                                           (url "projects" id "applications" (% :id) "delete") "delete"))
                                (if (= user-id (project :user_id))
                                  (link-to {:class "button secondary tiny"} 
                                           (url "projects" id "applications" (% :id) "select") "select as lead")))]
                    (html (if application (item application))
                          (map item (filter #(not (user-id? %)) (project :applications)))))]))))))

(defn create [id {:keys [params] :as request}]
  (body* [project (select-one projects (where {:id id}))]
         (if-let [user-id (get-user request :id)] 
           (and
             (= (project :type) "new [funding, lead]")
             (not= (project :user_id) user-id)
             (empty? (select applications (where {:project_id id :user_id user-id}))))) 
         request
         (html
           (header project (assoc request :uri (url "projects" id "applications")))
           (form-to [:post "."]
                    (csrf)
                    (warning params)
                    [:h4 "why should you lead this project?"] 
                    (wmd params :big true)
                    [:hr]
                    [:button.button "create application"]))
         (do
           (required params :accept?)
           (vali/rule (vali/has-value? (params :wmd-input)) 
                      [:wmd-input "... is required."]))
         "..."
         (let [user (get-user request)
               application (insert applications (values {:project_id id
                                                         :user_id (user :id) 
                                                         :time (timestamp)
                                                         :application (params :wmd-input)}))]
           (notify id 
                   user
                   (application :time)
                   :application 
                   nil
                   (format "/projects/%s/applications/" id)
                   (application :application))
           (redirect (format "/projects/%s/applications/" id)))))

(defn delete* [id application-id {:keys [params] :as request}]
  (body* [application (select-one applications (where {:id application-id}) (with projects))]
         (and (= (application :type) "new [funding, lead]")
              (= (application :user_id) (get-user request :id)))
         request
         (html
           (header application request :uri (url "projects" id "applications"))
           (html 
             (form-to [:post "."]
                      (csrf)
                      [:div.alert-box.secondary
                       [:h4 "Warning!"]
                       [:p "You are about to delete your application."]
                       (control :accept?
                                [:label.controls (check-box :accept? (params :accept?)) "I understand and accept the responsibilties and consequences of this project."]
                                :label "")]
                      [:button.button "delete"])))
         (required params :accept?)
         "boom!"
         (do
           (delete applications (where {:id application-id}))
           ;;(send-email id 
           ;;            :applications 
           ;;            (application :application)
           ;;            id
           ;;            (application :id)
           ;;            (application :application)
           ;;            (application :application)))
           (redirect (format "/projects/%s/applications/" id)))))

(defn select* [id application-id {:keys [params] :as request}]
  (body* [application (select-one applications (where {:id application-id}) (with projects))]
         (and (= (application :type) "new [funding, lead]")
              (= (application :user_id) (get-user request :id)))
         request
         (html
           (header application request :uri (url "projects" id "applications"))
           (html 
             (form-to [:post "."]
                      (csrf)
                      [:div.alert-box.secondary
                       [:h4 "Warning!"]
                       [:p "You are about to select a lead for this project. By doing so, you will remain the project founder, but all control and responsibility will shift from you to the person you are selecting to be the lead."]
                      (control :accept?
                               [:label.controls (check-box :accept? (params :accept?)) "I understand and accept the responsibilties and consequences of this project."]
                               :label "")]
                      [:div
                       [:h4 (link-to (url "users" (application :id)) (display application))]
                       [:div (markdown (application :application))]]
                      [:hr]
                      [:button.button "select as lead"])))
         (required params :accept?)
         "boom!"
         (let [project (update projects (set-fields {:edit_time (timestamp)
                                                     :user_id (application :user_id )
                                                     :type "new [funding]"
                                                     :changes "lead"})
                               (where {:id id}))]
           (delete applications (where {:id application-id}))
           (notify id 
                   (project :user_id)
                   (project :edit_time)
                   :edit 
                   (project :changes)
                   (url "projects" id)
                   (project :changes))
           (redirect (url "projects" id)))))
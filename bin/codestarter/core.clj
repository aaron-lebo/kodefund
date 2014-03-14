(ns codestarter.core
  (:use codestarter.views.common
        codestarter.csrf
        compojure.core
        [compojure.route :as route]
        [compojure.handler :as handler]
        korma.core
        korma.db)
  (:require (codestarter.views 
              [users :as users]
              [messages :as messages]
              [projects :as projects]
              [updates :as updates]
              [comments :as comments]
              [pledges :as pledges]
              [applications :as applications]
              [subscriptions :as subscriptions])
            noir.validation))
  
(defroutes main-routes 
  (GET "/" request (projects/index request))
  
  (GET "/oauth2/" request (users/oauth2 request))
  (GET "/twitter/" request (users/twitter* request))
  
  (context "/users" request
           (ANY "/register/" request (users/register request))
           (ANY "/login/" request (users/login request))
           (GET "/logout/" request 
                (redirect "/" :request request :session {} :flash "Successfully logged out.")))
  (context "/users/:id" [id :as request]   
           (GET "/" [] 
                (users/show (read-string id) request))
           (ANY "/edit/" []  
                (users/edit (read-string id) request))
           (ANY "/edit-password/" [] 
                (users/edit-password (read-string id) request))
           (context "/messages" []  
                    (GET "/" [] 
                         (messages/index (read-string id) request))
                    (POST "/" [] 
                          (messages/create (read-string id) nil request))
                    (ANY "/create/" [] 
                         (messages/create (read-string id) nil request))
                    (GET "/:message-id/" [message-id]  
                         (messages/index (read-string id) request))
                    (GET "/:message-id/create/" [id message-id :as request]  
                         (messages/index (read-string id) request))
                    (POST "/:message-id/create/" [id message-id :as request]  
                          (messages/create (read-string id) (read-string message-id) request)))
           (GET "/notifications/" [] 
                (users/notifications* (read-string id) request))
           (GET "/projects/" [] 
                (users/projects* (read-string id) request))
           (ANY "/subscriptions/" [] 
                (users/subscriptions* (read-string id) request))
           (GET "/send-confirmation/" [] 
                (users/send-confirmation* (read-string id) request))
           (GET "/confirm/:key/" [key] 
                (users/confirm (read-string id) key request)))
  (context "/projects" request
           (ANY "/create/" [] 
                (projects/create request)))
  (context "/projects/:id" [id :as request]
           (GET "/" [] 
                (projects/show (read-string id) request))
           (ANY "/edit/" []
                (projects/edit (read-string id) request))
           (GET "/pledges/" []
                (pledges/index (read-string id) request))
           (ANY "/pledges/create/" []
                (pledges/create (read-string id) request))
           (ANY "/pledges/:pledge-id/edit/" [id pledge-id :as request]
                (pledges/edit (read-string id) (read-string pledge-id) request))
           (ANY "/pledges/:pledge-id/delete/" [id pledge-id :as request]
                 (pledges/delete* (read-string id) (read-string pledge-id) request))
           (GET "/applications/" []
                (applications/index (read-string id) request))
           (ANY "/applications/create/" []
                (applications/create (read-string id) request))
           (ANY "/applications/:application-id/delete/" [id application-id :as request]
                 (applications/delete* (read-string id) (read-string application-id) request))
           (ANY "/applications/:application-id/select/" [id application-id :as request]
                 (applications/select* (read-string id) (read-string application-id) request))
           (ANY "/:number/edit/" [number]
                (projects/edit (read-string id) (read-string number) request))
           (context "/comments" []  
                    (GET "/" [] 
                         (comments/index (read-string id) request))
                    (POST "/" [] 
                          (comments/create (read-string id) nil nil :projects request))
                    (GET "/:comment-id/" [comment-id]  
                         (comments/index (read-string id) request))
                    (GET "/:comment-id/create/" [id comment-id :as request]  
                         (comments/index (read-string id) request))
                    (POST "/:comment-id/create/" [id comment-id :as request]  
                          (comments/create (read-string id) nil (read-string comment-id) 
                                           :projects 
                                           request)))
           (context "/updates" []
                    (GET "/" [] 
                         (updates/index (read-string id) request))
                    (ANY "/create/" [] 
                         (updates/create (read-string id) request))
                    (GET "/:number/" [number] 
                         (updates/show (read-string id) (read-string number) request)))
           (context "/updates/:number/comments" [number]
                    (GET "/" [number] 
                         (updates/show (read-string id) (read-string number) request))
                    (POST "/" [] 
                          (comments/create (read-string id) (read-string number) nil
                                           :updates
                                           request))
                    (GET "/:comment-id/" [comment-id] 
                         (updates/show (read-string id) (read-string number) request))
                    (GET "/:comment-id/create/" [id number comment-id :as request] 
                         (updates/show (read-string id) (read-string number) request))
                    (POST "/:comment-id/create/" [id number comment-id :as request] 
                          (comments/create (read-string id) (read-string number) (read-string comment-id) 
                                           :updates
                                           request)))
           (context "/subscriptions" []
                    (ANY "/" [] 
                         (subscriptions/edit (read-string id) request)))
           (GET "/rss/" [] 
                  (subscriptions/rss (read-string id) request)))
  (route/resources "/"))

(def app 
  (->
    main-routes
    (wrap-anti-forgery)
    (#(fn [request]
        (binding [*user* (-> request :session :user)]
          (% request))))
    (#(fn [request]
        (if-let [errors (-> request :session :errors)]
          (binding [noir.validation/*errors* errors] 
            (assoc (% request) :session (dissoc (request :session) :errors)))
          (% request))))
    (noir.validation/wrap-noir-validation)
    (handler/site)))
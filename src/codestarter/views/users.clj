(ns codestarter.views.users
  (:use [codestarter.views.common :exclude (header)] 
        [codestarter.csrf :only (*anti-forgery-token*)] 
        hiccup.core
        [hiccup.page-helpers :exclude (url)]
        hiccup.form-helpers
        korma.core
        korma.db
        oauthentic.core)
  (:require [clojure.string :as string]
            cemerick.friend.credentials
            clj-http.client
            [noir.validation :as vali]
            [oauth.client :as oauth]
            ring.util.codec
            twitter)
  (:import org.mindrot.jbcrypt.BCrypt
           [org.apache.commons.mail HtmlEmail]))

(def consumer (oauth/make-consumer "59AyJdSafXvCKYHmIh6ig"  "fOnIShj7fCoyUplo3oZ0fufO43iiwqnD37SyRuOKA"
                                   "https://api.twitter.com/oauth/request_token"
                                   "https://api.twitter.com/oauth/access_token"
                                   "https://api.twitter.com/oauth/authenticate"
                                   :hmac-sha1))

(def token (atom {}))

(defn request-token [consumer csrf id redirect-to]
  (let [request-token (oauth/request-token 
                        consumer 
                        (format "https://localhost/twitter/?csrf=%s&id=%s&redirect-to=%s" 
                                (ring.util.codec/url-encode csrf) id redirect-to))]
    (reset! token request-token)
    request-token))
  
(def sites [[:github
             #(build-authorization-url "https://github.com/login/oauth/authorize" 
                                      {:client-id "84bd709a3738b609d1b3"
                                       :state %})
             "http://www.github.com/"]
            [:google
             #(build-authorization-url "https://accounts.google.com/o/oauth2/auth" 
                                       {:client-id "1091576414416.apps.googleusercontent.com"
                                        :redirect-uri "https://localhost/oauth2/?site=google"
                                        :scope "https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email"
                                        :state %})
             "http://plus.google.com/"] 
            [:twitter 
             #(oauth/user-approval-uri consumer
                                       (:oauth_token (apply request-token consumer (string/split % #" "))))
             "http://www.twitter.com/"]
            [:facebook 
             #(build-authorization-url "https://facebook.com/dialog/oauth/" 
                                       {:client-id "332903696792203"
                                        :redirect-uri "https://localhost/oauth2/?site=facebook"
                                        :state %})
             "http://www.facebook.com/"]])

(defmacro csrf* [csrf request body]
  `(let [cookie-token# (get-in ~request [:cookies "__anti-forgery-token" :value])]
    (if (and ~csrf cookie-token# (= ~csrf cookie-token#))
      ~body
      {:status 403
       :headers {"Content-Type" "text/html"}
       :body (fluid ~request [:p "CSRF error"])})))

(defn send-confirmation [user]
  (if (and (user :confirmation) (seq (user :email)))
    (let [link (format "https://localhost/users/%s/confirm/%s/" (user :id) 
                       (ring.util.codec/url-encode (user :confirmation)))]
      (email "Please confirm your email address"
             (html
               [:p (link-to link "Visit this link to confirm your email address.")]
               [:p link]
               [:p "Thanks"])
             [(user :email)])))
    user)

(defn user [site user id redirect-to request]
  (let [id (read-string id)
        id-field {(keyword site) (str (user :id))}
        fields (merge id-field {(keyword (str site "_display")) (user (cond (= site "github") :login
                                                                            (= site "twitter") :screen_name
                                                                            :else :name))
                                :display site})
        user (select-one users (where id-field))]
    (if (and user (get-user request))
      (redirect redirect-to :errors 
                (do (vali/set-error 
                      (keyword site) 
                      (format "Your %s account is already linked to a different account on this site." site))
                  vali/*errors*))
      (let [[user* flash] 
            (cond user [user]
                  id [(update users (set-fields fields) (where {:id id})) "Profile updated."]
                  :else [(send-confirmation
                           (insert users (values (merge fields {:name (:name user) :email (:email user)
                                                                :confirmation *anti-forgery-token*}))))
                           
                         "Welcome."])]
        (redirect redirect-to :replace? true :request request :session {:user user*} :flash flash)))))

(defn username? [username]
  (vali/rule (or (string/blank? username) 
                 (and (< (count username) 16) 
                      (re-find #"\d[\w|\-|\.&&[\D]]+|([\w|\-|\.&&[\D]][\w|-]*)" username)))
             [:username "Username can contain letters, digits, underscores, dashes and periods, cannot only be a digit, and must be less than 16 characters."]))
  
(defn username-unique? [username]
  (empty? (select users (where {:username username}))))

(defn email? [email email-confirmation]
  (vali/rule (= email email-confirmation)
             [:email-confirmation "Email and email confirmation must match."]))

(defn header [{:keys [id] :as user} request & [uri]]
  (html 
    [:h1 (display user true)]
    [:dl.tabs
     (let [uri (or uri (request :uri))
           tab #(if % [:dd {:class (if (= %2 uri) "active" "")} (link-to %2 %3)])
           user? (= id (get-user request :id))]
       (html 
         (tab true (url "users" id) "profile")
         (tab user? (url "users" id "edit") "edit")
         (tab (and user? (user :password)) (url "users" id "edit-password") "edit password")
         (tab (not user?) (url "users" id "messages/create") "send message")
         (tab true (url "users" id "projects") "projects")
         (tab user? (url "users" id "messages") "messages")
         (tab user? (url "users" id "notifications") "notifications")
         (tab user? (url "users" id "subscriptions") "subscriptions")))]))

(defn register [{:keys [params] :as request}]
  (body* [_ true]
         true
         request
         (html
           [:h1 "register"]
           (form-to [:post (str "?redirect-to=" (params :redirect-to))]
                    (csrf)
                    (control :username (text-field :username (params :username)))
                    (control :password (password-field :password (params :password)))
                    (control :password-confirmation (password-field :password-confirmation
                                                                     (params :password-confirmation))
                             :label "confirmation password")
                    (control :email (text-field :email (params :email)))
                    (control :email-confirmation (password-field :email-confirmation
                                                                 (params :email-confirmation))
                             :label "confirmation email")
                    [:div.row [:div.columns.ten.offset-by-two [:button.button "register"]]]))
         (let [{:keys [username password password-confirmation email email-confirmation]} params]
           (required params :username :password :password-confirmation :email :email-confirmation)
           (vali/rule (username-unique? username) 
                      [:username "That username already exists."])
           (username? username)
           (vali/rule (= password password-confirmation)
                      [:password-confirmation "Password and confirmation password must match."])
           (email? email email-confirmation))
         "Welcome!"
         (redirect (params :redirect-to) :replace? true :request request 
                   :session {:user (send-confirmation 
                                     (insert users (values {:username (params :username) 
                                                            :password (cemerick.friend.credentials/hash-bcrypt 
                                                                        (params :password)) 
                                                            :email (params :email)
                                                            :confirmation *anti-forgery-token*})))})))

(defn login [{:keys [params] :as request}]
  (let [{:keys [username password redirect-to]} params]
    (body* [_ true]
           true
           request
           (html
             [:h1 "login"]
             [:div.row
              [:div#login.columns.six
               (form-to [:post (str "?redirect-to=" redirect-to)]
                        (csrf)
                        (control :username (text-field :username username))
                        (control :password (password-field :password password))
                        [:div.row [:div.columns.ten.offset-by-two [:button.button "login"]  
                                   (link-to {:class "button secondary login"}
                                            (str "/users/register/?redirect-to=" redirect-to)
                                            "register")]])]
              [:div.columns.six
               [:ul.none
                (map #(vector 
                        :li
                        (let [[key link] %] 
                          (link-to {:class "button secondary login"}
                                   (link (str *anti-forgery-token* " nil " (params :redirect-to))) 
                                   [:img {:src (format "/%s_16.png" (name key))}] key)))
                        sites)]]])
           (do
             (required params :username :password)
             (vali/rule (select-one users (where {:username username}))
                        [:username "Username does not exist."])
             (vali/rule (and (vali/has-value? username) (vali/has-value? password)
                             (let [user (select-one users (where {:username username}))]
                               (BCrypt/checkpw (or password "test") (if user (user :password) ""))))
                        [:password "Incorrect password."]))
           "Welcome back."
           (redirect redirect-to :replace? true :request request
                     :session {:user (select-one users (where {:username username}))}))))

(defn show [id request]
  (body* [user (select-one users (where {:id id}))]
        true
        request
        (html 
          (header user request)
          [:div#dl.row
           (map #(let [[site _ link] %
                       site* (name site)]
                   (if (user site)
                     [:div.row
                      [:div.two.columns.offset-by-one [:img {:src (format "/%s_16.png" site*)}]
                       site*]
                      [:div.nine.columns 
                       (link-to (str link (user site) "/") (user (keyword (str  site* "_display"))))]]))
                   sites)]
           (if (user :about) 
             (markdown (user :about))))))

(defn edit [id {:keys [params] :as request}]
  (body* [user (select-one users (where {:id id}))]
        (= id (get-user request :id))
        request
        (html 
          (header user request)
          (let [user (merge user {:title (display user)
                                  :username? (user :username)
                                  :email-confirmation (user :email)
                                  :wmd-input (user :about)})
                user (merge user params)]
            (form-to [:post "."]
                     (csrf)
                     [:h3 "general information"]
                     (if (or (user :username?))
                       (control :username (text-field {:disabled true} :username (user :username)))
                       (html
                         (control :username (text-field :username (user :username)))
                         (control :username-confirmation (text-field :username-confirmation (user :username-confirmation))
                                  :label "username confirmation")
                         (control :password (password-field :password (params :password)))
                         (control :password-confirmation 
                                  (password-field :password-confirmation (params :password-confirmation))
                                  :label "password confirmation")))
                     (control :name (text-field :name (user :name)))
                     (control :email (text-field :email (user :email)))
                     (control :email-confirmation (text-field :email-confirmation (user :email-confirmation))
                              :label "email confirmation")
                     [:hr]
                     [:h3 "sites"]
                     [:p "connecting"]
                     (map (fn [[site link]]
                            [:div.row         
                             [:div.columns.eleven 
                              (control 
                                site
                                (text-field 
                                  {:disabled true} site 
                                  (if (user site) 
                                    (format "%s (%s)" (user site) 
                                            (user (keyword (str (name site) "_display")))))))]
                             [:div.columns.one
                              (if (user site)
                                [:button.button.secondary.tiny {:name "delete" :value site} "delete"]
                                (link-to {:class "button button secondary tiny"} 
                                         (link 
                                           (string/join " " [*anti-forgery-token* id (request :uri)]))
                                         "add"))]])
                          sites)
                     (if (some identity (map #(user (first %)) sites))
                       (control :display
                                (html 
                                  (map #(let [site (first %)]
                                          (if (user site)
                                            [:label 
                                             (radio-button {:disabled (user :username?)}
                                                           :display (if (user :username?) false 
                                                                      (= (name site) (user :display)))
                                                           (name site)) site]))
                                       sites))
                                :help [:p "the main account other users see"]))
                     [:hr]
                     [:h3 "about"]
                     (wmd user :big true)
                     [:hr]
                     [:button.button "save changes"])))
        (let [{:keys [delete username username-confirmation password password-confirmation 
                      email email-confirmation display]} params]
            (if delete
              (vali/rule (or (user :username) 
                             (some identity (map #(user (first %)) 
                                                 (filter #(not= (first %) (keyword delete)) sites))))
                         [(keyword delete) "Username or at least one linked account required."])
              (do 
                (vali/rule (or (string/blank? username) (username-unique? username))
                           [:username "That username already exists."])
                (username? username)
                (vali/rule (or (string/blank? username) (= username username-confirmation))
                           [:username-confirmation "Username and username confirmation must match."])
                (vali/rule (or (string/blank? username) (and username (vali/has-value? password)))
                           [:password "Password required."])
                (vali/rule (or (string/blank? username) (= password password-confirmation))
                           [:password-confirmation "Password and password confirmation must match."])
                (vali/rule (or (string/blank? email) (vali/is-email? email)) 
                           [:email "Must supply a valid email."])
                (vali/rule (= email email-confirmation) 
                           [:email-confirmation "Email and email confirmation must match."])
                (vali/rule (or (user :username) (vali/has-value? display)) 
                           [:display "Display is required."]))))
        "Profile updated."
        (let [user (if (params :delete)
                     (update users (set-fields {(params :delete) nil}) (where {:id id}))
                     (let [fields {:name (params :name) :email (params :email) :display (params :display) :about (params :wmd-input)}
                           fields (if (not= (params :email) (user :email)) 
                                    (assoc fields :confirmation *anti-forgery-token*) fields)
                           fields (if (and (params :username) (not (string/blank? (params :username)))) 
                                    (merge fields {:username (params :username) 
                                                   :password (cemerick.friend.credentials/hash-bcrypt (params :password))}) 
                                    fields)]
                       (send-confirmation (update users (set-fields fields) (where {:id id})))))]
          (redirect "" :request request :session {:user user}))))

(defn edit-password [id {:keys [params] :as request}]
  (body* [user (select-one users (where {:id id}))]
        (= id (-> request :session :user :id))
        request
        (html 
          (header user request)
          (form-to [:post "."]
                   (csrf)
                   (control :password (password-field :password (params :password)))
                   (control :password-confirmation (password-field :password-confirmation 
                                                                   (params :password-confirmation))
                            :label "password confirmation")
                   [:hr]
                   [:button.button "save changes"]))
        (let [{:keys [password password-confirmation email]} params]
          (required params :password :password-confirmation)
          (vali/rule (= password password-confirmation)
                     [:password-confirmation "Password and password confirmation must match."]))
        "Password updated."
        (let [user (update users (set-fields {:password (cemerick.friend.credentials/hash-bcrypt (params :password))}) (where {:id id}))]
          (redirect ""))))

(defn projects* [id {:keys [params] :as request}]
  (body* [user (select-one users (where {:id id}))]
        true
        request
        (let [projects (select projects (where {:user_id id}))]
          (html 
            (header user request)
            (if (empty? projects)
              [:em "No projects"]
              [:table.table.table-striped.table-condensed
               [:thead
                [:tr 
                 [:th "project"]
                 [:th "supporter?"]
                 [:th "founder?"]
                 [:th "lead?"]]]
               (map
                 #(vector :tr 
                          [:td (link-to (url "projects" (% :id)) (% :name))]
                          [:td]
                          [:td (if (= (user :id) (% :founder)) [:i.icon-ok])]
                          [:td (if (= (user :id) (% :lead)) [:i.icon-ok])])
                 projects)])))))

(defn notifications* [id {:keys [params] :as request}]
  (body* [user (select-one users (where {:id id}))]
          (= id (get-user request :id))
          request
          (let [notifications (exec-raw [(str "select link, notifications.type, title, users.*, project_id, notifications.id, notifications.time, projects.name as p_name, notifications.user_id from notifications "
                                                      "join users on user_id = users.id "
                                                      "join projects on project_id = projects.id "
                                                      (string/join " or " (where-notifications id))
                                                      "order by notifications.id desc")] :results)]
            {:session (assoc (request :session) :user 
                             (update users (set-fields {:notifications (timestamp)}) (where {:id id})))
             :body
             (html
               (header user request)
               (if (empty? notifications)
                 [:em "No notifications"]
                 [:table#table
                  [:thead
                   [:tr 
                    [:th "info"]
                    [:th "user"]
                    [:th "project"]
                    [:th "time"]]]
                  [:tbody
                   (map #(vector :tr 
                                 [:td (link-to (% :link) 
                                               (string/replace (% :type) "update_comment" "comment")
                                               (if (% :title) (str ": " (% :title))))]
                                 [:td (link-to (url "users" (% :user_id)) (display % 16))]
                                 [:td (link-to (url "projects" (% :project_id)) (% :p_name))]
                                 [:td (pretty-time (% :time))])
                        notifications)]]))})))

(defn subscriptions* [id {:keys [params] :as request}]
  (let [subscription-keys [:edit :application :comment :update :update_comment]]
    (body* [user (select-one users (where {:id id}) (with subscriptions (with projects) (order :project_id :DESC)))]
           (= id (get-user request :id))
           request
           (let [user (if (params :subscriptions) 
                        (assoc user :subscriptions (vals (params :subscriptions)))
                        user)]
             (html 
               (header user request)
               (form-to [:post "."]
                        (csrf)
                        [:label.controls (check-box :notify (user :notify)) 
                         "receive email notification when you receive a message"]
                        (if (empty? (user :subscriptions))
                          (html [:i "No subscriptions"] [:hr])
                          [:table#table
                           [:thead
                            [:tr 
                             [:th "project"]
                             [:th "notify by email"]
                             (map #(vector :th %) subscription-keys)]]
                           (map 
                             #(vector :tr 
                                      [:td (link-to (url "projects" (% :id_2)) (% :name))
                                       (hidden-field (format "subscriptions[%s][name]" (% :id)) 
                                                     (% :name))
                                       (hidden-field (format "subscriptions[%s][id]" (% :id)) 
                                                     (% :id))]
                                      [:td (check-box (format "subscriptions[%s][email]" (% :id)) 
                                                               (% :email))] 
                                      (map (fn [key]
                                             [:td (check-box (format "subscriptions[%s][%s]" (% :id) (name key)) 
                                                             (% key))])
                                           subscription-keys))
                             (user :subscriptions))])
                        [:button.button "save changes"])))
           true
           "Subscriptions updated."
           (do
             (update users (set-fields {:notify (read-string (or (params :notify) "false"))}))
             (doseq [subscription (params :subscriptions)]
               (let [fields (select-keys (second subscription) subscription-keys)
                     fields (reduce #(assoc % %2 (read-string (or (% %2) "false"))) 
                                    fields 
                                    subscription-keys)
                     subscription-id (read-string (first subscription))]
                 (if (= (set (vals fields)) #{false})
                   (delete subscriptions (where {:id subscription-id}))
                   (update subscriptions (set-fields (assoc fields :email (:email (second subscription)))) 
                           (where {:id subscription-id})))))
             (redirect "")))))

(defn oauth2 [{:keys [params] :as request}]
  (let [[csrf id redirect-to] (string/split (-> request :params (get :state "nil /")) #" ")]
    (csrf* 
      csrf request
      (let [site (params :site)
            site* (cond (= site "github")
                        ["https://github.com/login/oauth/access_token" 
                         "84bd709a3738b609d1b3"
                         "a78ee3f3b34d0e5bf43b595bc8d7859c12ea437e"
                         "https://api.github.com/user"]
                        (= site "google")
                        ["https://accounts.google.com/o/oauth2/token" 
                         "1091576414416.apps.googleusercontent.com"
                         "F1Ze-PzSLEzE_boYGI2TE2zY"
                         "https://www.googleapis.com/oauth2/v1/userinfo"]
                        :else
                        ["https://graph.facebook.com/oauth/access_token" 
                         "332903696792203"
                         "a467c8cefa802a77f26ccb4c0a408426"
                         "https://graph.facebook.com/me"])
            redirect-uri (str "https://localhost/oauth2/?site=" site)
            token (if (= site "facebook")
                    (second (re-find #"access_token=(.+)&expires" 
                                     (:body (clj-http.client/get (first site*) 
                                                                 {:query-params
                                                                  {:client_id (second site*)
                                                                   :client_secret (nth site* 2)
                                                                   :code (-> request :params :code)
                                                                   :redirect_uri redirect-uri}}))))
                    (:access-token (fetch-token (first site*) {:client-id (second site*)
                                                               :client-secret (nth site* 2)
                                                               :code (-> request :params :code)
                                                               :redirect-uri redirect-uri})))
            user* (:body (clj-http.client/get (nth site* 3) {:query-params {:access_token token} :as :json}))]
        (user site user* id redirect-to request)))))

(defn twitter* [{:keys [params] :as request}]
  (csrf* (params :csrf) request
         (let [token (oauth/access-token consumer @token (params :oauth_verified))
               user* (twitter/with-oauth consumer (token :oauth_token) (token :oauth_token_secret)
                                         (twitter/show-user-by-id (token :user_id)))]
           (user "twitter" user* (params :id) (params :redirect-to) request))))

(defn confirm [id key request]
  (body* [user (select-one users (where {:id id :confirmation key}))]
         (= id (get-user request :id))
         request
         (redirect "/" :request request :session 
                   {:user (update users (set-fields {:confirmation nil}) (where {:id id :confirmation key}))}
                   :flash "Thanks for confirming your email address.")))

(defn send-confirmation* [id request]
  (body* [user (select-one users (where {:id id}))]
         (= id (get-user request :id))
         request
         (let [user (update users (set-fields {:confirmation *anti-forgery-token*}) (where {:id id}))]
           {:session (assoc (request :session) :user (send-confirmation user))
            :body (html [:h1 "Confirmation email sent"] 
                 [:p (format "You should be receiving an email at %s asking you to confirm the address." (user :email))])})))
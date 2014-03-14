(ns codestarter.views.common
  (:use codestarter.csrf
        noir.core
        hiccup.core
        hiccup.form-helpers
        [hiccup.page-helpers :exclude (url)]
        korma.core
        korma.db
        clj-time.format
        [cemerick.friend.credentials :only (hash-bcrypt)]
        [clj-time.core :exclude (extend)]
        [clj-time.coerce])
  (:require [clojure.string :as string]
            [noir.validation :as vali]
            ring.util.codec
            ring.util.response)
  (:import [org.mozilla.javascript Context ScriptableObject]
           [org.ocpsoft.pretty.time PrettyTime]
           [org.apache.commons.mail HtmlEmail]))

(declare ^:dynamic *user*)

(defdb db (postgres {:db "codestarter"
                    :user "postgres"
                    :password "pg"}))
(declare users)
(defentity cards
  (belongs-to users {:fk :user_id}))
(defentity messages
  (belongs-to users {:fk :user_id}))
(declare subscriptions)
(defentity users
  (has-many messages {:fk :user_id})
  (has-many subscriptions {:fk :user_id}))
(declare projects)
(defentity notifications
  (belongs-to projects {:fk :project_id})
  (belongs-to users {:fk :user_id}))
(defentity pledges
  (belongs-to users {:fk :user_id})
  (belongs-to projects {:fk :project_id}))
(defentity comments
  (belongs-to projects {:fk :project_id})
  (belongs-to users {:fk :user_id}))
(defentity updates
  (belongs-to projects {:fk :project_id})
  (belongs-to users {:fk :user_id})
  (has-many comments {:fk :update_id}))
(defentity subscriptions
  (belongs-to projects {:fk :project_id})
  (belongs-to users {:fk :user_id}))
(defentity applications
  (belongs-to projects {:fk :project_id})
  (belongs-to users {:fk :user_id}))
(defentity goals)
(defentity projects
  ;;(belongs-to users {:fk :founder})
  (belongs-to users {:fk :user_id})
  (has-many goals {:fk :project_id})
  (has-many pledges {:fk :project_id})
  (has-many comments {:fk :project_id})
  (has-many updates {:fk :project_id})
  (has-many applications {:fk :project_id})
  (has-many notifications {:fk :project_id}))

(defn get-user [request & [key]]
  (let [user (-> request :session :user)]
    (if key (key user) user)))

(defn url [& args]
  (str "/" (string/join "/" args) "/"))

(defn redirect [uri & {:keys [replace? request session flash errors]}]
  (let [redirect* (ring.util.response/redirect 
                    (if replace? (string/replace uri #"(\d+)/new/" "$1/new/#$1") uri))
        session (cond (empty session) session 
                      (not-empty session) (merge (request :session) session))
        redirect* (if session (assoc redirect* :session (assoc session :errors errors)) redirect*)
        redirect* (if flash (assoc redirect* :flash flash) redirect*)]
    redirect*))

(defn display [{:keys [username name display] :as user} & [img]]
  (html
    (str (or username (user (keyword (str display "_display")))) 
         (if (and (seq name) (not (some #(= % display) ["google" "facebook"]))) (str " (" name ")")))
    (if (and img (not username)) [:img.site {:src (format "/%s_%s.png" display img)}])))

(defn get-count [table values]
  (:count (first (select table (aggregate (count :id) :count) (where values)))))

(defn required [params & keys]
  (doseq [key keys] 
    (vali/rule (vali/has-value? (params key)) 
               [key (format "%s is required." key)])))

(defn pretty-time [time]
  (. (PrettyTime.) format (to-date time)))

(defn timestamp [] 
  (to-timestamp (now)))

(defn $->cents [amount]
  (* amount 100))

(defn cents->$ [amount]
  (format "%.2f" (/ (float amount) 100)))

(defn where-notifications [id] 
  (let [subscriptions (select subscriptions (where {:user_id id}))
        where* (map #(vector (% :project_id) (filter (fn [key] (% (keyword key))) 
                                                     ["edit" "application" "comment" "update" "update_comment"]))
                    subscriptions)]
    (if (seq subscriptions)
      (map #(format "where (project_id = %s and notifications.type in (%s))" (first %)
                    (string/join ", " (map (fn [type] (format "'%s'" type)) (second %)))) where*))
    ""))

(defn fluid [request page & [error]]
  (let [user (get-user request)]
    (html
      "<!-- paulirish.com/2008/conditional-stylesheets-vs-css-hacks-answer-neither/ -->"
      "<!--[if lt IE 7]> <html class=\"no-js lt-ie9 lt-ie8 lt-ie7\" lang=\"en\"> <![endif]-->"
      "<!--[if IE 7]>    <html class=\"no-js lt-ie9 lt-ie8\" lang=\"en\"> <![endif]-->"
      "<!--[if IE 8]>    <html class=\"no-js lt-ie9\" lang=\"en\"> <![endif]-->"
      "<!--[if gt IE 8]><!--> <html class=\"no-js\" lang=\"en\"> <!--<![endif]-->"
      [:head
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
       [:title "test"]
       (include-css "/foundation3/foundation3/stylesheets/foundation.css" 
                    "/foundation3/foundation3/stylesheets/app.css"
                    "/foundation3/foundation3/stylesheets/presentation.css"
                    "/bootstrap.css") 
       "<!--[if lt IE 9]>"
       (include-css "/foundation3/foundation3/stylesheets/ie.css")
       "<![endif]-->"
       (include-js "/foundation3/foundation3/javascripts/modernizr.foundation.js")
       "<!--[if lt IE 9]>"
       "<script src=\"http://html5shim.googlecode.com/svn/trunk/html5.js\"></script>"
       "<![endif]-->"
       (include-js "/foundation3/foundation3/javascripts/jquery.min.js")
       (if (re-find #"/pledges/(create|\d+/edit)/" (request :uri))
         (html
           (include-js "https://js.stripe.com/v1/")
           [:script {:type "text/javascript"} "
Stripe.setPublishableKey('pk_Y9Q4B7FrfV9viNOK382AN4NpHtfGV');

function stripeResponseHandler(status, response) {
  $('.row').removeClass('error');
  $('#pledge small').remove();

  var error = false;

  var amount = parseFloat($('#amount').val());
  if (!amount || amount <= .99) {
    var parent = $('#amount').parent();
    parent.parent().addClass('error');
    parent.append('<small>must be a dollar amount greater than $0.99</small>');
    error = true;
  }
  if (($('input[name=card]:checked').val() == 'other' || $('#number').val() || $('#cvc').val() || $('#exp_month').val() || $('#exp_year').val()) && response.error) {
    var parent = $('#' + response.error.param).parent();
    parent.parent().addClass('error');
    parent.append('<small>' + response.error.message + '</small>');
    error = true;
  } else if (!response.error) {
    $('#pledge').append(\"<input type='hidden' name='stripe' value='\" + response['id'] + \"'/>\");
    var card = response.card;
    $('#pledge').append(\"<input type='hidden' name='details' value='\" + card.type + ' ' + card.last4 + \"'/>\");      
  }
  if (error) {
    $('#submit-button').removeAttr('disabled');
  } else {
    $('#pledge').get(0).submit();
  }
}

$(document).ready(function() {
  $('#pledge').submit(function(event) {
    $('#submit-button').attr('disabled', 'disabled');

    Stripe.createToken({
      number: $('#number').val(),
      cvc: $('#cvc').val(),
      exp_month: $('#exp_month').val(),
      exp_year: $('#exp_year').val()}, 
      stripeResponseHandler
    );
    
    return false;
  });
});
"]))]
      [:body
       [:div.row {:style "padding: 11px 0; background: rgb(63, 63, 63)"}
        [:div.columns.four 
         [:div.name {:style "margin: 0"} 
          (link-to {:style "color: rgb(255, 255, 255);"} "/" "Project name")]]
        [:div.eight.columns 
         [:ul.link-list.right {:style "margin: 0"}
          (if user
            (html 
              [:li (link-to (url "users" (user :id)) (display user))]
              [:li 
               (link-to (url "users" (user :id) "messages")
                        "messages "
                        (let [n-messages (get-count messages {:recipient (user :id) :time ['> (user :messages)]})]
                          (if (pos? n-messages) [:span.round.label n-messages])))]
              [:li 
               (link-to (url "users" (user :id) "notifications")
                        "notifications "
                        (let [where (where-notifications (user :id))
                              where (if (seq where) (str (string/join " or " where) " and ") "")
                              n-notifications (:count (first (exec-raw [(str "select count(*) from notifications where "
                                                                             where
                                                                             "time > '" (user :notifications) "'")] :results)))] 
                          (if (pos? n-notifications) [:span.round.label n-notifications])))]
              [:li (link-to "/users/logout/" "logout")])
            [:li(link-to (str "/users/login/?redirect-to=" (request :uri)) "login | register")])]]]
       [:div.row 
        [:div.twelve.columns
         (if user
           (if (user :confirmation) 
             [:div.alert-box.alert 
              "Please confirm your email address. "
              (if (seq (user :email))
                (html 
                  (format "An email was sent to %s. " (user :email))
                  (link-to (url "users" (user :id) "send-confirmation") "resend")
                  " | "))
              (link-to (url "users" (user :id) "edit") "change email address")]))
         (if (request :flash) [:div.alert-box.success (request :flash)])
         (if error [:div.alert-box.alert "Please fix the errors."])
         page]]
     [:hr]
     [:footer
      [:p "&copy; Company 2012"]]
     (include-js
       "/foundation3/foundation3/javascripts/jquery.reveal.js"
       "/foundation3/foundation3/javascripts/jquery.orbit-1.4.0.js"
       "/foundation3/foundation3/javascripts/jquery.customforms.js"
       "/foundation3/foundation3/javascripts/jquery.placeholder.min.js"
       "/foundation3/foundation3/javascripts/jquery.tooltips.js"
       "/foundation3/foundation3/javascripts/app.js"
       "/pagedown/Markdown.Converter.js"
       "/pagedown/Markdown.Sanitizer.js"
       "/pagedown/Markdown.Editor.js"
       "/bootstrap.js")])))
  
(defmacro body* [binding if* request get & [valid? message post]]
  `(if-let ~binding
     (if ~if* 
       (let [post?# (= (~request :request-method) :post)
             valid?# (and post?# (do ~valid? (not (vali/errors?))))]
         (if valid?# 
           (update-in ~post [:flash] #(or % ~message))
           (let [get# ~get]
             (cond (string? get#) (fluid ~request get# (and 
                                                         (not (-> ~request :params :add-goal))
                                                         (not (-> ~request :params :delete-goal))
                                                         post?#))
                   (= (-> get# :headers (get "Content-Type")) "application/rss+xml") get#
                   :else (update-in get# [:body] #(fluid ~request % post?#))))))
       (if (-> ~request :session :user)
         {:status 401 :body "not allowed"}
         (redirect (str "/users/login/?redirect-to=" (~request :uri)))))))

(defn on-error [id]
  (vali/on-error id (fn [[first-error]] [:small first-error])))

(defn csrf [] 
  (hidden-field "__anti-forgery-token" *anti-forgery-token*))

(defn control [id input & {:keys [label help skip-errors?]}]
  [:div {:class (str "row" (if (vali/errors? id) " error"))}
   [:div.two.columns [:label.right.inline {:for id} (or label (string/replace (name id) "-" " "))]]
   [:div.ten.columns input (if skip-errors? "" (on-error id)) help]])

(defn wmd [params & {:keys [n big] :or {n ""}}] 
  (let [key* (keyword (str "wmd-input" n))]
    (html
      [:div {:class (str "wmd-panel" (if (vali/errors? key*) " error"))}
       [:div {:id (str "wmd-button-bar" n)}]
       (text-area {:class (if big "big" "")} key* (params key*))
       (on-error key*)
       [:p "formatting: " (link-to "http://daringfireball.net/projects/markdown/" "Markdown")]
       [:div#wmd-preview.wmd-panel.wmd-preview.panel {:id (str "wmd-preview" n)}]])))

(defmacro select-one [& body]
  `(first (select ~@body)))

(defn tab 
  ([display uri id] (tab display uri id nil))
  ([display uri id table]
  [:dd {:class (if (= uri (format "/projects/%s/%s/" id display)) "active" "")} 
   (link-to (format "/projects/%s/%s/" id display) display 
            (if table (html " " [:span.round.label (get-count table {:project_id id})])))]))

(defn header [{:keys [id] :as project} request & {:keys [uri]}]
  (let [uri (or uri (request :uri))]
    (html
      [:h1 (project :name) " " [:small (project :description)]]
      [:dl.tabs
       [:dd {:class (if (= uri (format "/projects/%s/" id)) "active" "")} (link-to (format "/projects/%s/" id) "project")]
       (if (= (project :user_id) (:id (get-user request))) (tab "edit" uri id))
       (tab "pledges" uri id pledges)
       (if (= (project :type) "new [funding, lead]") (tab "applications" uri id applications))
       (tab "comments" uri id comments)
       (tab "updates" uri id updates)
       (let [uri* (url "projects" id "subscriptions")]
         [:dd {:class (if (= uri uri*) "active" "")} 
          (link-to uri* (html (if (select-one subscriptions 
                                              (where {:project_id id :user_id (:id (get-user request))}))
                                "edit " )
                              "subscriptions "
                              [:img {:src "/email_16.png"}] " " [:img {:src "/rss_16.png"}]))])])))
(defn warning [params]
  [:div.alert-box.secondary
   [:h4 "Warning!"]
   [:p "You are responsible for any project you lead."]
   [:p "Failure to deliver functionality for which you have received funding or fraudulent activity will impact your reputation, and in extreme cases, legal action may be necessary."]
   [:p "This should not be an issue as long as you are informed, diligent, and honest about your project."]
   [:ul 
    [:li "further information about how this site works"]
    [:li "example projects"]]
   (control :accept?
            [:label.controls (check-box :accept? (params :accept?)) "I understand and accept the responsibilties and consequences of this project."]
            :label "")])

(defn markdown [text]
  (let [context (Context/enter)
        scope (.initStandardObjects context)
        input (Context/javaToJS text scope)
        script (str 
                 (slurp "/codestarter/resources/public/pagedown/Markdown.Converter.js") 
                 "new Markdown.Converter().makeHtml(input);")]
    (try 
      (ScriptableObject/putProperty scope "input" input)
      (let [result (.evaluateString context scope script "<cmd>" 1 nil)]
        (Context/toString result))
      (finally (Context/exit)))))

(defn email [subject message recipients]
  (let [email (doto (HtmlEmail.)
                (.setHostName "smtp.gmail.com")
                (.setSslSmtpPort "465")
                (.setSSL true)
                (.setFrom "fx@fxdirect.net")
                (.setSubject subject)
                (.setHtmlMsg message)
                (.setAuthentication "aaron.m.lebo@gmail.com" "jackson218"))]
    (doseq [recipient recipients] (.addTo email recipient))
    (.send email)))

(defn notify [id user time type title link text]
  (insert notifications (values {:project_id id
                                 :user_id (user :id)
                                 :time time
                                 :type (name type)
                                 :title title
                                 :link link
                                 :notification text}))
  (.start (Thread. 
            #(let [subscriptions (select subscriptions (where {:project_id id type true}) 
                                         (with users) (with projects))]
               (if (empty? subscriptions)
                 ""
                 (let [subscription (first subscriptions)]
                   (email
                     (format "[test - %s] %s%s by %s" (subscription :name_2) 
                             (string/replace (name type) "update_comment" "comment") 
                             (if title (str ": " title) "") (display user))
                     (html
                       [:h1 title]
                       (markdown text)[:br]
                       (link-to (str "http://localhost:3000" link) "link")[:br]
                       [:p "-----"]
                       [:p
                        "You are receiving this email because you are subscribed to notifications for this project. You can change your subscriptions for it "
                        (link-to (format "http://localhost:3000/projects/%s/subscriptions/" id) "here") "."
                        (if (subscription :user_id_2) 
                          (html " You can manage all of your subscriptions " 
                                (link-to (format "http:/localhost:3000/users/%s/subscriptions/" (subscription :user_id_2)) "here") "."))])
                     (map (fn [subscription] (let [email* (subscription :email)]
                                               (if (=  email* "true") (subscription :email_2) email*)))
                          subscriptions))))))))
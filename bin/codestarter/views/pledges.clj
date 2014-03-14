(ns codestarter.views.pledges
  (:use codestarter.views.common 
        hiccup.core
        hiccup.form-helpers
        [hiccup.page-helpers :exclude (url)]
        [korma.core :exclude (select* delete*)]
        korma.db
        [clj-time.core :only (days plus)]
        [clj-time.format :only (formatter unparse)]
        [clj-time.coerce :only (from-sql-date)])
  (:require [clojure.data :as data] 
            [clojure.string :as string]
            [noir.validation :as vali]))

(defn form [project id params {:keys [uri] :as request}]
  (let [create? (re-find #"create" (request :uri))]
    (html
      (header project (assoc request :uri (url "projects" id "pledges")))
      (form-to {:id "pledge"} [:post "."]
               (csrf)
               [:div.alert-box.secondary
                [:h4 "Notice"]
                [:p (format "Your credit card be charged on %s only if the project reaches one or more funding goals."
                            (unparse (formatter "MMMM d, y") (plus (from-sql-date (project :time)) (days 30))))]
                [:p "You can edit or delete your pledge up to that time."]]
               [:hr]
               [:h4 "pledge amount"]
               [:div.row.collapse 
                [:div.one.columns [:span.prefix "$"]] 
                [:div.eleven.columns 
                 (text-field :amount (if-let [amount (params :amount)] (cents->$ amount)))]]
               [:hr]
               [:h4 "credit card information"]
               (control :card
                        (html (map #(vector :label 
                                            (radio-button :card (= (% :id) (params :card_id)) (% :id))
                                            [:span (% :details)])
                                   (select cards (where {:user_id (*user* :id)})))
                              [:label 
                               (radio-button :card create? "other")
                               [:span "other - fill in the details below"]]))
               (control :number (text-field {:name nil} :number))
               (control :cvc (text-field {:name nil} :cvc (params :cvc)) :label "CVC")
               [:div.row
                [:div.two.columns
                 [:label.right.inline "expiration (mm/yyyy)"]]
                [:div.columns.two (text-field {:name nil} :exp_month)]
                [:div.columns.eight (text-field {:name nil} :exp_year)]]
               [:hr]
               [:button#submit-button.button (format "%s pledge" (if create? "create" "update"))]
               (if (not create?) (link-to {:class "button secondary"} (string/replace uri #"edit" "delete") "delete"))))))

(defn index [id {:keys [params] :as request}]
  (body* [project (select-one projects (where {:id id}) (with pledges (with users) (order :id :DESC)))]
         true
         request
         (html
           (header project request)
           (let [pledge (select-one pledges  (where {:project_id id :user_id (*user* :id)}) (with users))]
             (html
               (if pledge
                 [:div#dl.row
                  [:div.two.columns.offset-by-one "current pledge"]
                  [:div.nine.columns
                   [:span.radius.success.label (format "$%s" (cents->$ (pledge :amount)))]
                   (link-to {:class "button secondary tiny"} (str (pledge :id) "/edit/") "edit")
                   (link-to {:class "button secondary tiny"} (str (pledge :id) "/delete/") "delete")]]
                 [:div (link-to {:class "button"} "create/" "create pledge") [:hr]])
               (if (empty? (project :pledges))
                 [:em "No pledges"]
                 [:table#table
                  [:thead [:tr [:th "user"] [:th "amount"]]]
                  [:tbody
                     (map #(vector :tr 
                                   [:td (link-to (url "users" (% :user_id)) (display % 16))]
                                   [:td (format "$%s" (cents->$ (% :amount)))]) 
                          (project :pledges))]]))))))

(defn create [id {:keys [params uri] :as request}]
  (body* [project (select-one projects (where {:id id}))]
         *user*
         request
         (let [pledge (select-one pledges (where {:user_id (*user* :id) :project_id id}))]
           (if pledge
             (redirect (string/replace uri #"create" (str (pledge :id) "/edit")))
             (form project id params request)))
         true;;(required params :accept?)
         "Pledge created."
         (let [fields {:user_id (*user* :id) :stripe (params :stripe) :details (params :details)}
               card (or (select-one cards (where (or {:id (read-string (params :card))} fields)))
                        (insert cards (values fields)))]
           (insert pledges 
                   (values {:user_id (*user* :id) :card_id (card :id) :project_id id 
                            :amount ($->cents (read-string (params :amount)))}))
           #_(notify id 
                   user
                   (pledge :time)
                   :pledge 
                   nil
                   (format "/projects/%s/pledges/" id)
                   (pledge :pledge))
           (redirect (format "/projects/%s/" id)))))

(defn edit [id pledge-id {:keys [params] :as request}]
  (body* [project (select-one projects (where {:id id}) (with pledges (where {:id pledge-id})))]
         (= (-> project :pledges first :user_id) (*user* :id))
         request
         (form project id (-> project :pledges first) request)
         true
         "Pledge updated."
         (let [fields {:user_id (*user* :id) :stripe (params :stripe) :details (params :details)}
               card (or (select-one cards (where (or {:id (read-string (params :card))} fields)))
                        (insert cards (values fields)))]
           (update pledges (set-fields {:card_id (card :id) :amount ($->cents (read-string (params :amount)))})
                   (where {:id pledge-id}))
           (redirect (format "/projects/%s/pledges/" id)))))

(defn delete* [id pledge-id {:keys [params] :as request}]
  (body* [project (select-one projects (where {:id id}) (with pledges (where {:id pledge-id})))]
         (= (-> project :pledges first :user_id) (*user* :id))
         request
         (html
           (header project request :uri (url "projects" id "pledges"))
           (html 
             (form-to [:post "."]
                      (csrf)
                      [:div.alert-box.secondary
                       [:h4 "Notice"]
                       [:p "You are about to delete your pledge."]]
                      [:button.button "delete"])))
         true
         "Pledge deleted."
         (do
           (delete pledges (where {:id pledge-id}))
           (redirect (format "/projects/%s/pledges/" id)))))
(ns codestarter.views.projects
  (:use codestarter.views.common 
        [clojure.contrib.seq :only (indexed)]
        [clojure.data :only (diff)] 
        [clojure.set :only (intersection)]
        hiccup.core
        hiccup.form-helpers
        [hiccup.page-helpers :exclude (url)]
        korma.core
        korma.db
        [clj-time.core :exclude (extend)]
        clj-time.format
        [clj-time.coerce])
  (:require [clojure.string :as string]
            [codestarter.views.users :as users]
            [noir.validation :as vali])
  (:import [org.mozilla.javascript Context ScriptableObject]))

(let [date-format "MM/dd/yyyy"]
  (def date-formatter (formatter date-format))
  (def date-formatter* (formatter (str date-format " hh:mm:ss a"))))

(let [popular (map #(list (frequencies (string/split (% :languages) #", "))
                         (frequencies (string/split (% :licenses) #", ")))
                  (select projects))
      popular #(string/join ", " (keys (reverse (sort (apply merge-with + (map % popular))))))]
  (def popular-languages (popular first))
  (def popular-licenses (popular second)))

(defn clean [items]
  (filter #(not= % "") (string/split (string/trim items) #",\s")))

(defn get-n [n]
  (if (and n (string? n)) (read-string n) (or n 2)))

(defn goals->vec [goals]
  (if (vector? goals) goals (vec (vals goals))))

(defn valid? [{:keys [accept? id name description type deadline languages licenses wmd-input n-goals goals] :as params}]  
  (if (not id)
    (do
      (required params :accept? :name)
      (vali/rule (<= (count name) 100)
                 [:name "Project name must be less than or equal to 100 characters."])
      (vali/rule (zero? (count (select projects (where {:name name}))))
                 [:name "A project with this name already exists."])))
  (required params :description :type)
  (vali/rule (vali/has-value? wmd-input)
             [:wmd-input "Plan is required."])
  (vali/rule (<= (count description) 240)
             [:description "Description must be less than or equal to 240 characters in length."])
  (vali/rule (some #{type} ["new [funding]" "new [funding, lead]" "proprietary [funding]"])
               [:type "Type must be one of \"new [funding]\", \"new [funding, lead]\", \"proprietary [funding]\"."])
  
  (doseq [[field key items] [["language" :languages languages]
                             ["license" :licenses licenses]]]
    (let [items (clean items)]
      (vali/rule (not (empty? items)) 
                 [key (format "At least one %s is required." field)])
      (doseq [counts (frequencies items)]
        (vali/rule (< (second counts) 2)
                   [key (format "Cannot have duplicate %ss." field)]))))
  
  (vali/rule (not (params :add-goal)) [])
  (vali/rule (not (params :delete-goal)) [])

  (let [goals (goals->vec goals)]
    (vali/rule (seq (filter #(not= (vals %) '("" "" "")) goals))
               ["goals[0]" "At least one goal is required."])
    
    (loop [n 0
           n-goals (get-n n-goals)
           last-amount 0
           last-deadline (now)]
      (if (< n n-goals)
        (let [goal (get goals n)
              valid-amount (and (re-matches #"[0-9]*\.?[0-9]+" (goal :amount)) 
                                (pos? (read-string (goal :amount))))
              amount (if valid-amount (read-string (goal :amount)))
              valid-deadline (and (re-matches #"(0[1-9]|1[012])/(0[1-9]|[12][0-9]|3[01])/(19|20)\d\d" (get goal :deadline ""))
                                  (within? (interval (now) (plus (now) (months 18))) (parse (formatter "MM/dd/yyyy") (goal :deadline))))
              deadline (if valid-deadline (parse (formatter "MM/dd/yyyy") (goal :deadline)))]
          (if (not= (vals goal) '("" "" ""))
            (do
              (vali/rule (vali/has-value? (goal :amount))
                         [(format "goals[%s][amount]" n) "Amount is required."])
              (vali/rule valid-amount
                         [(format "goals[%s][amount]" n) "Amount must be a number greater than 0."])
              (vali/rule (and valid-amount (> amount last-amount))
                         [(format "goals[%s][amount]" n) "Amount must be greater than the amounts of preceeding goals."])
              (vali/rule (vali/has-value? (goal :deadline))
                         [(format "goals[%s][deadline]" n) "Deadline is required."])
              (vali/rule valid-deadline
                         [(format "goals[%s][deadline]" n) "Deadline must be a date in the format mm/dd/yyyy that is within the next 18 months."])
              (vali/rule (and valid-deadline (not (before? deadline last-deadline)))
                         [(format "goals[%s][deadline]" n) "Deadline must be at or after the deadlines of preceeding goals."])
              (vali/rule (vali/has-value? (goal :description))
                         [(format "goals[%s][description]" n) "Description is required."])
              (vali/rule (<= (count (goal :description)) 500)
                         [(format "goals[%s][description]" n) "Description must be less than or equal to 500 characters in length."])))
          (recur (inc n) n-goals (if valid-amount amount last-amount) (if valid-deadline deadline last-deadline)))))))
  
(defn form [request params & [project]]  
  (let [params (if (params :goals) (assoc params :goals (goals->vec (params :goals))) params)
        params (cond 
                 (params :add-goal) (update-in params [:n-goals] #(inc (read-string %)))
                 (params :delete-goal) (let [goals (params :goals)
                                             deleted (read-string (params :delete-goal))
                                             params (assoc params :goals
                                                           (vec (concat (subvec goals 0 deleted) 
                                                                        (subvec goals (inc deleted)))))]
                                         (update-in params [:n-goals] #(dec (read-string %))))
                 :else params)
        params (if project (merge project params) params)]
    (form-to [:post "."]
             (csrf)
             (if (not project) (warning params))
             [:h4 "general information"]
             (control :name (text-field (if project {:disabled ""} {}) :name (params :name))
                      :label "project name")
             (control :description (text-area :description (params :description)))
             (control :lead 
                      (html (text-field {:disabled true} :lead 
                                        (display (if project 
                                                   (assoc params :name (params :name_2))
                                                   (get-user request))))
                            (if (= (params :type) "new [funding, lead]")
                              [:span {:style "padding-left: 1em"} 
                               (link-to (url "projects" (params :id) "applications") 
                                        (str (get-count applications {:project_id (params :id)}) " applications"))])))
             (control :type 
                      (html 
                        (for [[type help] [["new [funding]" "project which you will implement"] 
                                           ["new [funding, lead]" "project which you want someone else to implement"] 
                                           ["proprietary [funding]" "software you have the rights to which you want to open source"]]]
                          [:label (radio-button :type (= (params :type) type) type)
                           [:span (str type " - " help)]])))
             (control :languages 
                      (text-area :languages (params :languages))
                      :help (html [:p "Comma-seperated list of the languages which your project uses."]
                                  [:p "popular: " popular-languages]))
             (control :licenses 
                      (text-area :licenses (params :licenses))
                      :help (html [:p "Comma-seperated list of the licenses which your project uses."]
                                  [:p "popular: " popular-licenses]))
             [:hr]
             [:h4 "plan"]
             [:p "The details of your project. It is important to describe your project accurately and in as much detail as possible."]
             [:div {:class (str "wmd-panel" (if (vali/errors? :wmd-input) " error"))}  
              (text-area {:class "big"} :wmd-input (params :wmd-input))
              (on-error :wmd-input)
              [:p "formatting: " (link-to "http://daringfireball.net/projects/markdown/" "Markdown")]
              [:div#wmd-preview.wmd-panel.wmd-preview.panel]]
             [:hr]
             [:h4 "goals"]
             [:p "The funding goals for your project. Each goal is a subset of functionality that will be implemented if the funding amount is met."]
             [:p "For example, in a new project, $100 of funding would mean Python support, $200 of funding would mean Clojure support. Or, when releasing proprietary software, $150 of funding would mean the release of code, $200 would mean the release of art assets."]
             [:p {:style "padding-bottom: 1em"} "You should describe your goals in more detail in the project's plan."]
             (let [n-goals (get-n (params :n-goals))]
               (html
                 (hidden-field "n-goals" n-goals)
                 (for [n (range n-goals)
                       :let [id (format "goals[%s]" n)
                             amount (format "%s[amount]" id)
                             deadline (format "%s[deadline]" id)
                             description (format "%s[description]" id)
                             amount-value (-> (params :goals) (get n) :amount)
                             amount-value (if (= (type amount-value) java.lang.Double)
                                            (format "%.2f" amount-value)
                                            amount-value)]]
                   [:fieldset
                    [:legend (str "goal " (inc n)) 
                     " "
                     (if (pos? n) [:button.button.tiny {:name "delete-goal" :value n} "delete"])] 
                    [:div
                     (on-error id)
                     (control amount
                              [:div.row.collapse 
                               [:div.one.columns [:span.prefix "$"]] 
                               [:div.eleven.columns (text-field {:name amount} amount amount-value)
                                  (on-error amount)]]
                              :label "amount"
                              :help [:p "The amount of funding needed to implement this goal. Be realistic with costs."]
                              :skip-errors? true)
                     (control deadline 
                              (text-field {:name deadline} deadline (-> (params :goals) (get n) :deadline))
                              :label "expected date"
                              :help [:p "The expected date (up to 18 months from now) that the goal will be completed if funding is met. When this date is reached, individuals who have funded your project can rate whether you have completed the goal."])
                     (control description 
                                (text-area {:name description} description 
                                           (-> (params :goals) (get n) :description))
                                :label "description"
                                :help [:p "Description of what functionality will be implemented if the goal is met."])]])))
             [:div.eight.columns.centered [:button.button.secondary {:name "add-goal"} "add another goal"]]
             [:hr]
             [:button.button (if project "save changes" "create project")])))
  
(defn index [request]
  (fluid
    request
    (html
     [:div.panel [:h3 "Test"]]
     [:div
      [:span [:h2 "current projects"]]
      [:span
       [:a {:href "/projects/create/"} "create project"]]]
     [:table#table
      [:thead
       [:tr
        [:th "id"]
        [:th "name"]
        [:th "description"]
        [:th "languages"]]]
      [:tbody
       (for [project (select projects (order :id :DESC))]
         [:tr 
          [:td (project :id)]
          [:td (link-to (format "/projects/%s/" (project :id)) (project :name))]
          [:td (project :description)]
          [:td (project :languages)]])]])))

(defn show [id request]
  (body* [project (select-one projects (where {:id id}) (with users) (with goals) 
                              (with pledges (aggregate (sum :amount) :total)))]
         true
         request
         (html
           (header project request)
           [:div.row
            [:div.dl.eight.columns
             [:div.row
              [:div.two.columns.offset-by-one "deadline"]
              [:div.nine.columns
               [:span.radius.alert.label
                (let [minutes (in-minutes 
                                (interval (now) (plus (from-sql-date (project :time)) (days 30))))
                      remainder (rem minutes 1440)]
                  (format "%s days %s hours %s minutes" (quot minutes 1440) (quot remainder 60) 
                          (rem remainder 60)))]]]
             [:div.row
              [:div.two.columns.offset-by-one "lead"]
              [:div.nine.columns
               (if (= (project :type) "new [funding, lead]")
                 [:div.panel
                  [:p (link-to (url "users" (project :id_2)) (display (assoc project :name (project :name_2)) true)) 
                   [:span.radius.secondary.label "temporary"]]
                  (if (not= (project :user_id) (*user* :id))
                    (html 
                      [:p "This project needs someone to lead it. If you are interested and capable, consider applying. If accepted, you take on the rewards and responsibilies."]
                      (if-let [application (select-one applications (where {:project_id id :user_id (*user* :id)}))]
                        (link-to {:class "button secondary"} 
                                 (url "projects" (project :id) "applications" (application :id) "delete") 
                                 "delete application")
                        (link-to {:class "button success"} 
                                 (url "projects" (project :id) "applications" "create") 
                                 "apply"))))]
                 (link-to (format "/user/%s/" (project :id_2)) 
                          (display (assoc project :name (project :name_2)))))]]
             [:div.row
              [:div.two.columns.offset-by-one "languages"]
         [:div.nine.columns (project :languages)]]
             [:div.row
              [:div.two.columns.offset-by-one "licenses"]
              [:div.nine.columns (project :licenses)]]
             [:hr]
             (markdown (project :plan))]
            [:div#funding.dl.four.columns
             (let [total (-> project :goals last :amount)
                   pledges-$ (-> project :pledges first :total)
                   total-percentage (cents->$ (* (/ total pledges-$) 100))
                   amount (-> project :goals first :amount)
                   percentage (cents->$ (* (/ amount pledges-$) 100))
                   pledge (select-one pledges (where {:user_id (*user* :id) :project_id id}))]
               [:div.panel
                [:h4 "funding"]
                 [:div.row
                  [:div.four.columns "pledged"]
                  [:div.eight.columns
                   [:span.radius.success.label (format "$%s" (cents->$ pledges-$))] ]]
                 [:div.row
                  [:div.four.columns "status"]
                  [:div.eight.columns
                   (format "0 / %s goals met" (count (project :goals)))]]
                 [:div.row
                  [:div.four.columns "goal 1"]
                  [:div.eight.columns
                   (format "$%s (%s%%)" (cents->$ amount) percentage)]]
                 [:div.progress [:div.bar {:style (format "width: %s" percentage)}]]
                 [:div.row
                  [:div.four.columns "overall"]
                  [:div.eight.columns
                   (format "$%s (%s%%)" (cents->$ total) total-percentage)]]
                 [:div.progress [:div.bar {:style (format "width: %s" total-percentage)}]]
                 (if pledge
                   [:div.row
                    [:div.four.columns "current pledge"]
                    [:div.eight.columns
                     [:span.radius.success.label (format "$%s" (cents->$ (pledge :amount)))]
                     (link-to {:class "button secondary tiny"} (str (pledge :id) "/edit/") "edit")
                     (link-to {:class "button secondary tiny"} (str (pledge :id) "/delete/") "delete")]]
                   (link-to {:class "button success"} (str "pledges/create/") "pledge"))])
             [:div.panel
              [:h4 "goals"]
              (map-indexed
                #(vector :div.goal
                         [:div.row
                          [:div.four.columns (format "goal %s" (inc %))]
                          [:div.eight.columns (format "$%s" (cents->$(%2 :amount)))]]
                         [:div.row
                          [:div.eight.columns.offset-by-four 
                           [:span.radius.secondary.label "delivery date"]
                           (unparse date-formatter (from-sql-date (%2 :deadline)))]]
                         (%2 :description)(if (< (inc %) (count (project :goals))) [:hr]))
                (project :goals))]]])))

(defn create [{:keys [params] :as request}]
  (body* [params params]
         (get-user request)
         request
         (html
           [:h1 "create project"]
           (form request params))
         (valid? params)
         "Project created."
         (let [user (get-user request)
               project (insert projects (values {:user_id (user :id)
                                                 :founder (user :id)
                                                 :time (to-timestamp (now))
                                                 :name (params :name) 
                                                 :description (params :description)
                                                 :type (params :type)
                                                 :languages (string/join ", " (clean (params :languages)))
                                                 :licenses (string/join ", " (clean (params :licenses)))
                                                 :plan (params :wmd-input)}))]
           (map 
             #(insert goals (values {:project_id (project :id)
                                     :amount (read-string (% :amount)) 
                                     :deadline (to-timestamp (parse date-formatter (% :deadline)))  
                                     :description (% :description)}))
             (filter #(not= (vals %) '("" "" "")) (goals->vec (params :goals))))
           (insert subscriptions (values {:project_id (project :id) 
                                          :user_id (user :id)
                                          :edit true
                                          :comment true
                                          :update true
                                          :update_comment true
                                          :email (user :email)}))
           (redirect (url "projects" (project :id))))))

(defn edit [id {:keys [params] :as request}]
  (body* [project (select-one projects (where {:id id}) (with users) (with goals))]
        (= (project :user_id) (get-user request :id))
        request
        (html
          (header project request)
          (form request (assoc params :id id) 
                (merge project {:wmd-input (project :plan)
                                :n-goals (count (project :goals))
                                :goals (vec (map
                                              #(assoc % :deadline 
                                                      (unparse date-formatter 
                                                               (from-sql-date (% :deadline))))
                                              (project :goals)))})))
        (valid? params)
        "Project updated."
        (let [project* (update projects (set-fields {:edit_time (to-timestamp (now))
                                                     :description (params :description)
                                                     :type (params :type)
                                                     :languages (string/join ", " (clean (params :languages)))
                                                     :licenses (string/join ", " (clean (params :licenses)))
                                                     :plan (params :wmd-input)})
                               (where {:id id}))
              goals* (delete goals (where {:project_id id})) 
              goals* (map #(insert goals (values {:project_id id 
                                                  :amount (read-string (% :amount)) 
                                                  :deadline (to-timestamp 
                                                              (parse date-formatter (% :deadline))) 
                                                  :description (% :description)}))
                          (filter #(not= (vals %) '("" "" "")) (goals->vec (params :goals))))
              changes (diff (assoc project* :goals goals*) project)
              changes (merge (second changes) (first changes))
              hash #(apply hash-map (apply concat (map % %2)))
              goals-changes (hash #(vector (inc (first %)) (intersection (set (keys (second %))) 
                                                                         (set [:amount :deadline :description])))
                                  (indexed (changes :goals)))
              n-goals* (count goals*)
              n-goals (count (project :goals))
              n-changed-goals (- n-goals* n-goals)
              goals-changes (merge goals-changes 
                                   (cond (neg? n-changed-goals)
                                         (hash #(vector (inc %) "deleted") (range n-goals* n-goals))
                                         (pos? n-changed-goals)
                                         (hash #(vector % "added") (range n-goals* (+ n-goals* n-changed-goals)))))
              changed-keys (intersection (set (keys changes)) 
                                         (set [:description :lead :deadline :languages :licenses :plan]))
              changes (string/join ", " (map name changed-keys))
              goals-changes (string/join ", " 
                                         (for [change goals-changes 
                                               :let [details (second change)]
                                               :when (seq details)]
                                           (format "goal %s %s" (first change) 
                                                   (if (string? details) (second details)
                                                     (format "[%s]" (string/join ", " (map name details)))))))
              changes (str changes (if (or (string/blank? changes) (string/blank? goals-changes)) "" ", ") 
                           goals-changes)
              project* (update projects (set-fields {:changes changes}) (where {:id id}))]
          (if (string/blank? (project* :changes))
            (redirect "" :flash "Nothing changed.")
            (do 
              (notify id 
                      (get-user request)
                      (project* :edit_time)
                      :edit
                      (project* :changes)
                      (url "projects" id)
                      (project* :changes))
              (redirect ""))))))
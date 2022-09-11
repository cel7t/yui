(defn random-response [user]
  (str (mention-user user) " " (rand-nth (:responses config)) \!))

(defn affirm []
  (str (rand-nth (:affirm config))))

;; reminder
;; coming soon!

;; is mod?
(defn mod? [user-id]
  (= user-id "")) ; admin user ID

(defmacro when-mod [user-id & body]
  (list 'if (mod? user-id)
        (cons 'do body)))

;; store for spending kudos

;; serve food/drinks

;; duckduckgo search

(defn search [query]
  (:body (client/get (str "https://api.duckduckgo.com/?q=" query "&format=json&pretty=1") {:as :json})))

(defn yui-search [ch-id query]
  (let [result (search query)]
    (if (not (= (:AbstractText result) ""))
      (prompt ch-id (str "**From " (:AbstractSource result)
                         ",**\n" (:AbstractText result)
                         "\n" (:AbstractURL result)))
      (let [extra-result (:Text (first (:RelatedTopics result)))]
        (if extra-result
          (prompt ch-id extra-result)
          (prompt ch-id (str "No description found;\n")))))))
;    (prompt ch-id (str "https://duckduckgo.com/"
;                       (:Image result)))))

;; eval code

(defn live-repl [ch-id content]
  (let [code (detokenize (remove (fn [x] (and (< 2 (count x))
                                              (= "```" (subs x 0 3))))
                                 content))]
    (prompt ch-id (str (eval (read-string code))))))

;; dice roll
(defn roll 
  ([ch-id]
   (prompt ch-id (str ":game_die: You rolled a " (inc (rand-int 6)) "! :game_die:"))
   (prompt ch-id "https://tenor.com/view/girl-waiting-anime-chill-rolling-gif-15974128"))
  ([ch-id number]
   (prompt ch-id (str ":game_die: You rolled a " (inc (rand-int number)) "! :game_die:"))
   (prompt ch-id "https://tenor.com/view/girl-waiting-anime-chill-rolling-gif-15974128"))
  ([ch-id die number]
   (let [die-roll-values (repeatedly die #(inc (rand-int number)))]
   (prompt ch-id (str ":game_die: You rolled a " (reduce #'+ die-roll-values) "! :game_die:"))
   (prompt ch-id (str "individual values are " (pr-str die-roll-values)))
   (prompt ch-id "https://tenor.com/view/girl-waiting-anime-chill-rolling-gif-15974128"))))

(defn pain [ch-id]
  (prompt ch-id "https://tenor.com/view/k-on-yui-hirasawa-pain-gif-23894830"))

(defn ghot [ch-id]
  (prompt ch-id "https://tenor.com/view/k-on-yui-studying-homework-anime-gif-16480285"))

(defn predict [ch-id]
  (prompt ch-id (str "My calculations say the chances are **" (rand-int 101) "%**.")))

(defn say-x [ch-id text]
  (prompt ch-id text))

(defn add-todo [ch-id text]
  (prompt (:todo-channel config) text)
  (prompt ch-id (str \" text \" "was added to TODO!")))

;; TODO: test it out!
;; also add :new, :old and :all roles
;; (defn announce [ch-id text]
;;   (let [ann (cdr (cdr text))
;;         ann-txt (if (re-find #"(?i)tt" (car ann))
;;                   (cdr ann) ann)
;;         grp (car (cdr text))]
;;     (prompt ch-id ; 927956576680173570
;;             (str
;;               (case (subs grp 0 3)
;;                  "new" (mention-role (:new (:roles config)))
;;                  "old" (mention-role (:old (:roles config)))
;;                  "all" (mention-role (:all (:roles config)))
;;                  "eve" (mention-role (:all (:roles config)))
;;                  "")
;;               ann-txt))))

(defn say-hi [ch-id user]
  (prompt ch-id (str (rand-nth (:say-hi config)) ", " (mention-user user) \!)))

(defn say-bye [ch-id user]
  (prompt ch-id (str (rand-nth (:say-bye config)) ", " (mention-user user) \!)))

(defn goodbye [ch-id]
  (prompt ch-id "Bye!")
  (prompt ch-id "https://tenor.com/view/yui-x-azusa-yui-azusa-azunyan-anime-gif-21336402")
  (c/disconnect-bot! (:gateway @state)))

(defn say-error [ch-id com]
  (prompt ch-id (str "I don't know how to " com ", baka!")))

(defn revoke-membership [ch-id guild mention]
  (m/remove-guild-member-role! (:rest @state) guild mention (:member-role config))
  (prompt ch-id "Sad to see an impostor amogus..."))

(defn grant-membership [ch-id guild mention]
  (m/add-guild-member-role! (:rest @state) guild (:id mention) (:member-role config))
  (prompt ch-id (str "Welcome to the ghot club, " (mention-user mention) \!)))

(defn grant-role [ch-id guild author role role-name]
  (m/add-guild-member-role! (:rest @state) guild (:id author) role)
  (prompt ch-id (str "You were given the " role-name " role!")))

(defn remove-role [ch-id guild author role role-name]
  (m/remove-guild-member-role! (:rest @state) guild (:id author) role)
  (prompt ch-id (str role-name " role was removed!")))

(defn register [ch-id author]
  (let [score-message (car @(m/get-pinned-messages! (:rest @state) 944508939435917333))
        score (tokenize (:content score-message))]
    (if (not (belongs (:id author) score))
      (do
        (m/edit-message! (:rest @state)
                         944508939435917333
                         944600368766132234
                         :content
                         (s/join " "
                                 (conj score
                                       (:id author)
                                       (str 0))))
        (let [daily-message (car @(m/get-pinned-messages! (:rest @state) 944511119098261504))
              dailies (tokenize (:content daily-message))]
          (m/edit-message! (:rest @state)
                           944511119098261504
                           944615057906610297
                           :content
                           (s/join " "
                                   (conj dailies
                                         (:id author)
                                         (str 0)))))
        (prompt ch-id "Registration successful. Type `yui daily` to get daily kudos!"))
      (prompt ch-id "Already registered."))))

(defn kudos [ch-id author mention] ; TODO: add optional clause for nicknames, they're contained in :member
  ;; get person giving kudos
  ;; get score of person giving kudos
  ;; get person getting kudos
  ;; add 1 kudos to person getting kudos
  ;; subtract 1 kudos from person giving kudos
  (let [score-message (car @(m/get-pinned-messages! (:rest @state) 944508939435917333))
        score (tokenize (:content score-message))]
    (if (and (belongs (:id author) score)
             (belongs (:id mention) score)) ;; IDs are much larger than feasible scores
      (do (m/edit-message! (:rest @state)
                           944508939435917333
                           944600368766132234
                           :content
                           (s/join " "
                                   (update
                                     (update score
                                             (inc (.indexOf score (:id author)))
                                             #(str (dec (Integer/parseInt %))))
                                     (inc (.indexOf score (:id mention)))
                                     #(str (inc (Integer/parseInt %))))))
          (prompt ch-id (str (if (:nick mention)
                               (:nick mention)
                               (:username mention))
                             " was given kudos by "
                             (if (:nick author)
                               (:nick author)
                               (:username author)))))
      (do
        (prompt ch-id "Not registered, registering...")
        (register ch-id mention)
        (register ch-id author)
        (kudos ch-id author mention)))))

(defn check-score [user n] ; codes: 0 - not reg, 1 - not enough, 2 - sufficient
  (let [score-message (car @(m/get-pinned-messages! (:rest @state) 944508939435917333))
        score (tokenize (:content score-message))]
    (if (belongs (:id user) score)
      (if (<= n
              (nth score (inc (.indexOf score (:id user)))))
        2
        1)
      0)))

(defn user-score [ch-id user]
  (let [score-message (car @(m/get-pinned-messages! (:rest @state) 944508939435917333))
        score (tokenize (:content score-message))]
    (if (belongs (:id user) score)
      (prompt ch-id (str (if (:nick user)
                           (:nick user)
                           (:username user))
                         " has "
                         (nth score (inc (.indexOf score (:id user))))
                         " kudos."))
      (do
        (prompt ch-id "Not registered, registering...")
        (register ch-id user)
        (user-score ch-id user)))))

(defn give-kudos [ch-id n user]
  (let [score-message (car @(m/get-pinned-messages! (:rest @state) 944508939435917333))
        score (tokenize (:content score-message))]
    (if (belongs (:id user) score)
      (m/edit-message! (:rest @state)
                       944508939435917333
                       944600368766132234
                       :content
                       (s/join " "
                               (update score
                                       (inc (.indexOf score (:id user)))
                                       #(str (+ n (Integer/parseInt %))))))
      (do
        (prompt ch-id "Not registered, registering...")
        (register ch-id user)
        (give-kudos ch-id n user)))))

(defn caption [ch-id file ctext]
  (clojure.java.io/copy
    (:body (client/get (str (:url file)) {:as :stream}))
    (java.io.File. "/tmp/img"))
  (prompt ch-id "Processing...")
  (let [width (Integer/parseInt (:out (apply shell/sh (tokenize "identify -format %w /tmp/img"))))
        fmt (s/lower-case (:out (apply shell/sh (tokenize "identify -format %m /tmp/img"))))]
    (apply shell/sh (concat (tokenize (str "bash -c convert /tmp/img -background none -font 'Upright' -fill white -stroke black -strokewidth " (int (/ width 200)) " -size " width "x" (int (/ width 2)) " -gravity center"))
                            (list (str "caption:" ctext))
                            (tokenize (str "-composite /tmp/img." fmt))))
    (m/create-message! (:rest @state) ch-id :file (java.io.File. (str "/tmp/img." fmt)))))

(defn daily [ch-id author]
  ;; check redeemed list
  ;; if last-redeemed != today
  ;; add 4 kudos to author
  (let [zone-id "Asia/Calcutta"
        today (t/day
                (.toLocalDate
                  (t/to-time-zone
                    (t/now)
                    (t/time-zone-for-id zone-id))))]
    (let [daily-message (car @(m/get-pinned-messages! (:rest @state) 944511119098261504))
          dailies (tokenize (:content daily-message))]
      (if (belongs (:id author) dailies) ;; IDs are much larger than days (1-31)
        (if (not (= (nth dailies
                         (inc (.indexOf dailies (:id author))))
                    (str today)))
          (do
            (m/edit-message! (:rest @state)
                             944511119098261504
                             944615057906610297
                             :content
                             (s/join " "
                                     (update dailies
                                             (inc (.indexOf dailies (:id author)))
                                             (fn [x] (str today)))))
            (give-kudos ch-id 4 author)
            (prompt ch-id "You got **4 kudos**!"))
          (prompt ch-id "You have already redeemed your daily kudos!"))
        (do
          (prompt ch-id "Not registered, registering...")
          (register ch-id author)
          (daily ch-id author))))))


(defn pin-message [ch-id ref-msg] ;; broken
  (if ref-msg
    (do
      (m/add-channel-pinned-message! (:rest @state) ch-id ref-msg)
      (prompt ch-id "Pinned the message!"))
    (prompt ch-id "No message mentioned, baka!")))

(defn delete-message [ch-id ref-msg]
  (if ref-msg
    (do
      (m/delete-message! (:rest @state) ch-id ref-msg)
      (prompt ch-id "Deleted the message!"))
    (prompt ch-id "No message mentioned, baka!")))

(defn edit-call [ch-id msg-id]
  (m/edit-message! (:rest @state)
                   ch-id
                   msg-id
                   :content
                   "Edited by Yui <3"))

(defn not-a-mod [ch-id]
  (prompt ch-id "You are not a mod, you sussy baka!"))

(defn say-help [ch-id]
  (prompt ch-id (:help-string config)))

(defn kill-person [ch-id user]
  (prompt ch-id (str "_" (rand-nth (:kill-methods config)) " " (:username user) "_")))

(defn dm-privacy [ch-id user]
  (prompt (m/create-dm! (:rest @state) (:id user))
          (:privacy-string config)))

(defn command-audio [ch-id com]
  (case com
    "play" (prompt ch-id "nothingburger")))


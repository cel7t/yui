(defn random-response []
  (str (rand-nth (:responses config))))

(defn affirm []
  (str (rand-nth (:affirm config))))

(defn unspace [str]
  (s/replace str #" " "%20"))

;; is mod?
(defn mod? [user-id]
  (= user-id (:admin-id config)))

(defmacro when-mod [user-id & body]
  (list 'if (mod? user-id)
        (cons 'do body)))

(defn search-ai [user-name mmap query]
  (reply mmap
         (str
           (:content
             (:message
               (first
                 (:choices
                   (parse-string
                     (:body
                       (client/post "http://127.0.0.1:5000/v1/chat/completions"
                                    {:form-params
                                     {
                                      :mode "chat"
                                      :character "Yui"
                                      :instruction_template "ChatML"
                                      :messages [{:role "user"
                                                  :content query}]
                                      }
                                     :content-type :json
                                     :accept :json})) true))))))))


(def search-engine "https://search.zeroish.xyz/api.php?q=")
(def fallback-search-engine "https://searx.tuxcloud.net/search?q=")

(defn search [query]
  (or (:body (client/get (str search-engine query) {:as :json}))
      (:body (client/get (str fallback-search-engine query) {:as :json}))))

(defn search-image [query]
  (or (:body (client/get (str search-engine query "&category_images=") {:as :json}))
      (:body (client/get (str fallback-search-engine query "&t=1") {:as :json}))))

(defn search-yt [query]
  (:body (client/get (str "https://inv.tux.pizza/api/v1/search?q=" query "&pretty=1&fields=videoId,title") {:as :json})))

(defn yui-image-search [ch-id query]
  (if (not (nil? (re-find #"(?i)child" query)))
    (prompt ch-id "This search query has been reported to the authorities. Please refrain from searching for illegal topics.")
    (let [result (search-image query)]
      (if (:thumbnail (first result))
        (prompt ch-id (str (:thumbnail (first result))))
        (prompt ch-id "No results found!")))))

(defn yui-yt-search [ch-id query]
  (if (not (nil? (re-find #"(?i)child" query)))
    (prompt ch-id "This search query has been reported to the authorities. Please refrain from searching for illegal topics.")
    (let [result (search-yt query)]
      (if (:title (first result))
        (and
         (prompt ch-id (str "**Here's the video you requested:**\n" (:title (first result))))
         (prompt ch-id (str "https://youtu.be/" (:videoId (first result)))))
        (prompt ch-id "No results found!")))))

(defn yui-search [ch-id query]
  (if (not (nil? (re-find #"(?i)child" query)))
    (prompt ch-id "This search query has been reported to the authorities. Please refrain from searching for illegal topics.")
    (let [result (search query)]
      (if (first result)
        (if (:title (first result))
          (prompt ch-id (str "**" (:title (first result)) "**\n"
                             (:description (first result)) "\n"
                             "\nFrom: " (unspace (:url (first result)))))
          (prompt ch-id (str (:response (:special_response (first result))) "\n"
                             "\nFrom: " (unspace (:source (:special_response (first result)))))))
        (prompt ch-id "No results found!")))))

(defn man-page [mmap cmd]
  (let [qqq (:out (shell/sh "curl" "-s" (str "https://cht.sh/" (first cmd) "?qT&style=bw")))]
    (reply mmap (subs qqq (s/index-of qqq "\n") (min (count qqq) 2000)))))

;; reminders

;; time.edn contains all our reminders in a map
;; all reminders are sorted
;; we have a function running every 10 minutes checking if any is in 10min range
;; if it is, it creates an async fun that triggers a message when it's reached
;; the bot then replies to the message creating the timer

(def time-file "~/.yui/time.edn")

(def yui-time-format (jtf/formatter "dd/MM/yyyy-HH:mm:ss"))

(defn call-reminder [reminder]
  (go
    (println "Reminder Spawned.")
    (let [secs (max 0
                    (jt/as (jt/duration
                             (jt/local-date-time)
                             (jt/local-date-time "dd/MM/yyyy-HH:mm:ss" (first reminder))) :seconds))]
      (println (format "Seconds left: %s" secs))
      (<! (timeout (* secs 1000)))
      (reply (first (next reminder))
             (first (next (next reminder)))))))

(defn ten-minute-check []
  (let [next-10-min (jt/plus (jt/local-date-time)
                             (jt/minutes 10))]
    (println "Doing a ten-minute-check.")
    (loop [remind []
           timings (edn/read-string (slurp time-file))]
      (if (first remind) (call-reminder (first remind)))
      (if (and (first timings)
               (jt/before? (jt/local-date-time
                            "dd/MM/yyyy-HH:mm:ss" (first (first timings))) ; [(<time> messg reply-to ...) ...]
                           (jt/local-date-time next-10-min)))
        (recur (conj remind (first timings))
               (next timings))
        (binding [*print-length* -1]
          (prn-str (spit time-file
                         (or timings []))))))))

(defn parse-duration [time-str]
  ;; regex match
  (loop [time (jt/local-date-time)
         ent (s/split time-str #",")]
    (println (format "Parsing Duration %s" (first ent)))
    (if (and (first ent)
             (re-matches #"(\w+)([hdms])" (first ent)))
      (let [DUR ((fn [x] (cons (Integer/parseInt (first (next x)))
                              (rest (rest x))))
                 (re-matches #"(\w+)([hdms])" (first ent)))]
        (case (first (next DUR))
          "d"
          (recur (jt/plus time (jt/days (first DUR)))
                 (next ent))
          "h"
          (recur (jt/plus time (jt/hours (first DUR)))
                 (next ent))
          "m"
          (recur (jt/plus time (jt/minutes (first DUR)))
                 (next ent))
          "s"
          (recur (jt/plus time (jt/seconds (first DUR)))
                 (next ent))
          "ERROR"))
      time)))

(defn parse-date [time-str]
  (println "Parsing Date")
  (if (re-find #"-" time-str)
    ;; full regex
    (let [time-arr (re-matches #"(\d+)/(\d+)/?(\d+)?-(\d+):(\d+):?(\d+)?" time-str)
          day (nth time-arr 1)
          month (nth time-arr 2)
          year (nth time-arr 3)
          hour (nth time-arr 4)
          minutes (nth time-arr 5)
          seconds (nth time-arr 6)]
      (jt/local-date-time (if year (Integer/parseInt year) 2023)
                          (Integer/parseInt month)
                          (Integer/parseInt day)
                          (Integer/parseIntger/parseInt hour)
                          (Integer/parseIntger/parseInt minutes)
                          (if seconds (Integer/parseInt seconds) 0)))
    (if (re-find #"/" time-str)
      ;; date regex
      (let [time-arr (re-matches #"(\d+)/(\d+)/?(\d+)?" time-str)
            day (nth time-arr 1)
            month (nth time-arr 2)
            year (nth time-arr 3)]
        (jt/local-date-time (if year (Integer/parseInt year) 2023)
                            (Integer/parseInt month)
                            (Integer/parseInt day)))
      ;; time regex
      (let [time-arr (re-matches #"(\d+):(\d+):?(\d+)?" time-str)
            hour (nth time-arr 1)
            minutes (nth time-arr 2)
            seconds (nth time-arr 3)]
        (jt/local-date-time (jt/as (jt/local-date-time) :year)
                            (jt/as (jt/local-date-time) :month-of-year)
                            (jt/as (jt/local-date-time) :day-of-month)
                            (Integer/parseIntger/parseInt hour)
                            (Integer/parseIntger/parseInt minutes)
                            (if seconds (Integer/parseInt seconds) 0))))))

(defn remind [in-or-at time-str text mmap]
  (println "Triggered Reminder fn.")
  (let [time (case in-or-at
               in (parse-duration time-str)
               at (parse-date time-str))]
    (println "Finished Parsing Time String.")
    (if (= time "ERROR")
      (reply mmap "Bad reminder formatting. Use dd/mm[/yy][-]HH:mm[:ss] or n[d][h][m][s][,]")
      (do
        (println "Reading time-file...")
        (reply mmap (str "Set reminder for " (jtf/format yui-time-format time))) 
        (let [timings (edn/read-string (slurp time-file))]
          (println "Read time-file.")
          (binding [*print-length* -1]
            (prn-str (spit time-file
                           (sort
                             #(jt/duration
                                (jt/local-date-time "dd/MM/yyyy-HH:mm:ss" (first %))
                                (jt/local-date-time "dd/MM/yyyy-HH:mm:ss" (first %)))
                             (conj timings
                                   [(jtf/format yui-time-format time) ; exact time
                                    mmap ; replying to
                                    text])))))) ; message
        (ten-minute-check)))))

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

(defn random-gif [mmap]
  (let [gif-file (edn/read-string (slurp "~/.yui/gifs.edn"))]
    (reply mmap (rand-nth (:gifs gif-file)))))

(defn add-gif [mmap file]
  (let [gif-file (edn/read-string (slurp "~/.yui/gifs.edn"))]
    (if (not (belongs (:url file) (:gifs gif-file)))
      (and
       (spit "~/.yui/gifs.edn"
             (binding [*print-length* -1] (prn-str (assoc-in gif-file 
                                                             [(keyword "gifs")] 
                                                             (concat ((keyword "gifs") gif-file)
                                                                     [(:url file)])))))
       (reply mmap "Added file to collection!"))
      (reply mmap "That file already exists in the collection! BAKA!!"))))

(defn predict [ch-id]
  (prompt ch-id (str "My calculations say the chances are **" (rand-int 101) "%**.")))

(defn say-x [ch-id text]
  (prompt ch-id text))

(defn add-todo [ch-id text]
  (prompt (:todo-channel config) text)
  (prompt ch-id (str \" text \" " was added to TODO!")))

(defn say-hi []
  (str (rand-nth (:say-hi config))))

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
  (prompt ch-id (str "Welcome to the server, " (mention-user mention) \!)))

(defn associated-key [user]
  (let [keyword-file (edn/read-string (slurp "keys.edn"))]
    ((keyword (str (:id user))) keyword-file)))

(defn caption [ch-id file ctext]
  (clojure.java.io/copy
    (:body (client/get (str (:url file)) {:as :stream}))
    (java.io.File. "/tmp/img"))
  (prompt ch-id "Processing...")
  (let [width (Integer/parseInt (:out (apply shell/sh (tokenize "identify -format %w /tmp/img"))))
        fmt (s/lower-case (:out (apply shell/sh (tokenize "identify -format %m /tmp/img"))))]
    (let [answer (:out (apply shell/sh (concat (tokenize (str "convert /tmp/img -background none -font Upright -fill white -stroke black -strokewidth " (int (/ width 200)) " -size " width "x" (int (/ width 2)) " -gravity center"))
                                               (list (str "caption:" ctext))
                                               (tokenize (str "-composite /tmp/img." fmt)))))]
      (prompt ch-id "Processing done!"))
    (m/create-message! (:rest @state) ch-id :file (java.io.File. (str "/tmp/img." fmt)))))

(defn subscribe [mmap author msg]
  (let [sub-file (edn/read-string (slurp "~/.yui/subs.edn"))]
    ;; yui sub create name
    ;; -> creates sub group
    (if (= (first msg) "create")
      (if (not (first (next msg)))
        (reply mmap "Enter a name for the new group!")
      (let [group-name (apply str (re-seq #"\w" (first (next msg))))]
        (if ((keyword group-name) sub-file)
          (reply mmap "A sub group with the same name already exists!")
          (do
            (spit "~/.yui/subs.edn"
                  (binding [*print-length* -1] (prn-str (assoc-in sub-file 
                                                                  [(keyword group-name)]
                                                                  (list (:id author))))))
            (reply mmap "Sub group created!")))))
      ;; yui sub name
      ;; -> joins sub group
      (let [group-name (apply str (re-seq #"\w" (first msg)))]
        (spit "~/.yui/subs.edn"
              (binding [*print-length* -1] (prn-str (assoc-in sub-file 
                                                              [(keyword group-name)] 
                                                              (concat ((keyword group-name) sub-file)
                                                                      [(:id author)])))))
        (reply mmap (str "Joined the sub group " group-name))))))

(defn unsubscribe [ch-id msg]) ;; TODO

(defn sub-ping [mmap msg]
  (let [sub-file (edn/read-string (slurp "~/.yui/subs.edn"))]
    ;; yui ping name
    ;; pings sub group
    (let [group-name (apply str (re-seq #"\w" msg))]
      (if (not ((keyword group-name) sub-file))
        (reply mmap "Sub group does not exist!")
        (reply mmap (detokenize (mapv mention-user ((keyword group-name) sub-file))))))))


(defn image-name [img]
  (let [image-file (edn/read-string (slurp "~/.yui/images.edn"))]
    ((keyword img) image-file)))

(defn image-listing [mmap]
  (let [image-file (edn/read-string (slurp "~/.yui/images.edn"))]
    (reply mmap (s/join ", " (map (fn [str] (s/replace str #"-" " "))
                                    (next (s/split (apply str (keys image-file)) #":")))))))

(defn counter-add [ch-id f-author]
  (let [author (:id f-author)
        score (edn/read-string (slurp "~/.yui/score.edn"))]
    (if (not ((keyword author) score))
      (spit "~/.yui/score.edn"
            (binding [*print-length* -1] (prn-str (assoc-in score
                                                            [(keyword author)] 
                                                            1))))
      (let* [count (inc ((keyword author) score))
             new-score (assoc-in score
                                 [(keyword author)] 
                                 count)]
        (spit "~/.yui/score.edn"
              (binding [*print-length* -1] (prn-str new-score)))))))

(defn leaderboard [ch-id gd-id]
  (def number (atom 0))
  (let [score (edn/read-string (slurp "~/.yui/score.edn"))]
    (prompt ch-id
            (str "# POGGERS LEADERBOARD\n"
                 (apply str
                        (map (fn [[user-id val]]
                               (let* [user (m/get-user! (:rest @state)
                                                        (Long/parseLong (subs (str user-id) 1)))
                                      name (:username @user)]
                                 (if (not (s/blank? name))
                                   (str (swap! number inc) ". " name ": " val "\n"))))
                             (into (sorted-map-by
                                    (fn [key1 key2]
                                      (<= (key2 score)
                                          (key1 score))))
                                   score)))))))
  
;; loop over it with get-guild-member! 

(defn balance [ch-id f-author f-mention]
  (let* [author (:id f-author)
         mention (:id f-mention)
         cash-file (edn/read-string (slurp "~/.yui/cash.edn"))
         amt (- (or ((keyword mention) ((keyword author) cash-file)) 0)
                (or ((keyword author) ((keyword mention) cash-file)) 0))]
    (cond
      (> amt 0) (prompt ch-id (str "Current balance is: you owe " amt " YuiCoin:tm: to " (:username f-mention)))
      (< amt 0) (prompt ch-id (str "Current balance is: " (:username f-mention) " owes you " amt " YuiCoin:tm:"))
      (= amt 0) (prompt ch-id (str "Current balance is 0! No debts!")))))


(defn settle [ch-id f-author f-mention amount]
  (let [author (:id f-author)
        mention (:id f-mention)
        cash-file (edn/read-string (slurp "~/.yui/cash.edn"))]
    (if (not (every? #(Character/isDigit %) amount))
      (prompt ch-id "Format: yui settle <int> @mention")
      (let [amt (* -1 (Integer/parseInt amount))]
        (prompt ch-id (str "Amount paid is: " (* -1 amt) " by " (:username f-mention) " to " (:username f-author)))
        (if (or (not ((keyword mention) cash-file))
                (not ((keyword author) ((keyword mention) cash-file))))
          (spit "~/.yui/cash.edn"
                (binding [*print-length* -1] (prn-str (assoc-in cash-file 
                                                                [(keyword mention) (keyword author)] 
                                                                amt))))
          (spit "~/.yui/cash.edn"
                (binding [*print-length* -1] (prn-str (assoc-in cash-file 
                                                                [(keyword mention) (keyword author)] 
                                                                (+ ((keyword author) 
                                                                    ((keyword mention) cash-file))
                                                                   amt))))))
        (balance ch-id f-author f-mention)))))

(defn debt [ch-id f-author f-mention amount]
  (let [author (:id f-author)
        mention (:id f-mention)
        cash-file (edn/read-string (slurp "~/.yui/cash.edn"))]
    (if (not (every? #(Character/isDigit %) amount))
      (prompt ch-id "Format: yui iou <positive int> @mention")
      (let [amt (Integer/parseInt amount)]
        (prompt ch-id (str "Amount owed is: " amt " by " (:username f-author) " to " (:username f-mention)))
        (if (not ((keyword mention) ((keyword author) cash-file)))
          (spit "~/.yui/cash.edn"
                (binding [*print-length* -1] (prn-str (assoc-in cash-file 
                                                                [(keyword author) (keyword mention)] 
                                                                amt))))
          (spit "~/.yui/cash.edn"
                (binding [*print-length* -1] (prn-str (assoc-in cash-file 
                                                                [(keyword author) (keyword mention)] 
                                                                (+ amt
                                                                   ((keyword mention) 
                                                                    ((keyword author) cash-file))))))))
        (prompt ch-id "Debt entry made! Enjoy wageslaving~")
        (balance ch-id f-author f-mention)))))


(defn add-image [mmap key-name file]
  (let [image-file (edn/read-string (slurp "~/.yui/images.edn"))]
    (if (not ((keyword key-name) image-file))
      (let* [stamp (str (quot (System/currentTimeMillis) 1000))
             image (clojure.java.io/copy
                     (:body (client/get (str (:url file)) {:as :stream}))
                     (java.io.File. (str "~/.yui/file_" stamp)))
             fmt (s/lower-case (s/trim-newline (first (s/split (:out (apply shell/sh (tokenize (str "file -b --extension ~/.yui/file_" stamp)))) #"/"))))
             final-image (:out (apply shell/sh 
                                      (tokenize 
                                        (str "mv ~/.yui/file_" stamp 
                                             " ~/.yui/file_" stamp "." fmt))))]
        (spit "~/.yui/images.edn"
              (binding [*print-length* -1] (prn-str (merge image-file 
                                                           {(keyword key-name) 
                                                            (str "file_" stamp "." fmt)}))))
        (reply mmap "Keyword registered with the given file!"))
      (reply mmap "Keyword already exists!"))))

(defn update-image [mmap key-name file]
  (let [image-file (edn/read-string (slurp "~/.yui/images.edn"))]
    (if ((keyword key-name) image-file)
      (let* [stamp (str (quot (System/currentTimeMillis) 1000))
             image (clojure.java.io/copy
                     (:body (client/get (str (:url file)) {:as :stream}))
                     (java.io.File. (str "~/.yui/file_" stamp)))
             fmt (s/lower-case (s/trim-newline (first (s/split (:out (apply shell/sh (tokenize (str "file -b --extension ~/.yui/file_" stamp)))) #"/"))))
             final-image (:out (apply shell/sh 
                                      (tokenize 
                                        (str "mv ~/.yui/file_" stamp 
                                             " ~/.yui/file_" stamp "." fmt))))]
        (spit "~/.yui/images.edn"
              (binding [*print-length* -1] (prn-str (merge image-file 
                                                           {(keyword key-name) 
                                                            (str "file_" stamp "." fmt)}))))
        (reply mmap "Keyword updated with the given file!"))
      (reply mmap "Keyword does not exist!"))))

(defn yui-show [mmap img]
  (m/create-message! (:rest @state)
                     (:channel_id mmap)
                     :message-reference mmap
                     :file (java.io.File. (str "~/.yui/" (image-name img)))))

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


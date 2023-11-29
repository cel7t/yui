(ns yui.core
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [clojure.core.async :refer [chan close! go <! timeout]]
            [discljord.cdn :as cdn]
            [discljord.messaging :as m]
            [discljord.connections :as c]
            [discljord.formatting :refer [mention-user user-mention]]
            [discljord.events :refer [message-pump!]]
            [clj-http.client :as client]
            [java-time.api :as jt]
            [java-time.format :as jtf]
            [cheshire.core :refer :all]
            [cheshire.generate :refer [encode-str]]
            [clojure.java.shell :as shell]
            [clojure.set]
            [clojure.xml :as xml])
  (:gen-class))

(def state (atom nil))
(def bot-id (atom nil))
(def config (edn/read-string (slurp "config.edn")))

(defn belongs [elem arr]
  (some #(= elem %) arr))

(defn tokenize [content]
  (s/split content #"\s+"))

(defn detokenize [content]
  (s/join " " content))

(defmulti handle-event (fn [type _data] type))

(defmacro prompt [ch-id content]
  `(m/create-message! (:rest @state) ~ch-id :content ~content))

(defmacro reply [msg-map content]
  `(m/create-message! (:rest @state)
                      (:channel_id ~msg-map)
                      :message-reference ~msg-map
                      :content ~content))

(defmacro cond-action
  "Takes a key to access in config-file and code to execute if command belongs."
  [command config-file & clauses]
  (when clauses
    (list
     'if `(or (belongs ~command ((keyword ~(first clauses)) ~config-file))
              (= (keyword ~(first clauses)) :else))
     (if (next clauses)
       (second clauses)
       (throw (IllegalArgumentException.
               "cond requires an even number of forms")))
     `(cond-action ~command ~config-file ~@(next (next clauses))))))

(load "functions")

(defmethod handle-event :message-create
  [_ {:keys [id channel-id author mentions content guild-id referenced-message member attachments] :as _data}]
  (go
  (cond
  (and (not (:bot author))
             (some #{@bot-id} (map :id mentions)))
    (search-ai author
     {:guild_id guild-id
                :channel_id channel-id
                :message_id id}
               content)
  (and (not (:bot author)) ;; fix for the recursive mention bug 
             (< 2 (count content)))
    (if (some #(= (subs content 0 3) %) (:names config)) ;; checks if bot was mentioned
      (let [mess0 (next (tokenize content))
            mess (remove #{"please" "Please" "PLEASE"} mess0)
            com (first mess)
            mmap {:guild_id guild-id
                  :channel_id channel-id
                  :message_id id}]
        (if (not (first mess)) (reply mmap "Wut.")
            (cond-action com config
                         :names
                         (reply mmap (rand-nth (:fun config)))
                         :hello
                         (reply mmap (say-hi))
                         :bye
                         (say-bye channel-id author)
                         :todo
                         (if (mod? (:id author))
                           (add-todo channel-id (s/join " " (next mess))) (not-a-mod channel-id))
                         :say
                         (say-x channel-id (s/join " " (next mess)))
                         :search
                         (if (= (first (next mess)) "image")
                           (yui-image-search channel-id (s/join "+" (next (next mess))))
                           (if (or (= (first (next mess)) "yt")
                                   (= (first (next mess)) "youtube"))
                             (yui-yt-search channel-id (s/join "+" (next (next mess))))
                             (yui-search channel-id (s/join "+" (next mess)))))
                         :man
                         (man-page mmap (next mess))
                         :remind
                         (let [cmd (if (= "me" (first (next mess)))
                                     (first (next (next mess)))
                                     (first (next mess)))
                               text (if (= "me" (first (next mess)))
                                     (next (next (next mess)))
                                     (next (next mess)))]
                           (remind (case cmd
                                     "in" 'in ; duration

                                     ("on" "at") 'at ; exact time

                                     'in)
                                   (if (or (= cmd "in") (= cmd "on") (= cmd "at"))
                                     (first text)
                                     cmd)
                                   (if (or (= cmd "in") (= cmd "on") (= cmd "at"))
                                     (detokenize (next text))
                                     (detokenize text))
                                   mmap))
                         :show
                         (let [cmd (first (next mess))]
                           (case cmd
                             "add" (if (first attachments)
                                     (add-image mmap (first (next (next mess))) (first attachments))
                                     (if (first (:attachments referenced-message))
                                       (add-image mmap 
                                                  (s/join "-" (next mess))
                                                  (first (:attachments referenced-message)))
                                       (reply mmap "No file specified!")))
                             "update" (if (first attachments)
                                        (update-image mmap (first (next (next mess))) (first attachments))
                                        (if (first (:attachments referenced-message))
                                          (update-image mmap 
                                                        (s/join "-" (next mess))
                                                        (first (:attachments referenced-message)))
                                          (reply mmap "No file specified!")))
                             "list" (image-listing mmap)
                             (yui-show mmap cmd)))
                         :disconnect
                         (if (mod? (:id author)) (do
                                                   (reply mmap (affirm))
                                                   (reply mmap "https://tenor.com/view/yui-x-azusa-yui-azusa-azunyan-anime-gif-21336402")
                                                   ;; doesn't wait for some reason
                                                   (goodbye channel-id))
                             (not-a-mod channel-id))
                         :pin
                         (pin-message channel-id (:id referenced-message))
                         :edit
                         (if (mod? (:id author)) (do
                                                   (reply mmap (affirm))
                                                   (edit-call channel-id (:id referenced-message)))
                             (not-a-mod channel-id))
                         :delete
                         (if (mod? (:id author)) (do
                                                   (reply mmap (affirm))
                                                   (delete-message channel-id (:id referenced-message)))
                             (not-a-mod channel-id))
                         :repl
                         (if (mod? (:id author)) (do
                                                   (reply mmap (affirm))
                                                   (live-repl channel-id (next mess))))
                         :sub
                         (if (next mess)
                           (subscribe mmap author (next mess))
                           (reply mmap "Enter the sub group's name!"))
                         :ping
                         (if (next mess)
                           (sub-ping mmap (first (next mess)))
                           (reply mmap "Mention a sub group you dolt!"))
                         :leaderboard
                         (leaderboard channel-id guild-id)
                         :debt
                         (if (:id (first mentions))
                           (debt channel-id author (first mentions) (first (next mess)))
                           (reply mmap "No one mentioned!"))
                         :settle
                         (if (:id (first mentions))
                           (settle channel-id author (first mentions) (first (next mess)))
                           (reply mmap "No one mentioned!"))
                         :balance
                         (if (:id (first mentions))
                           (balance channel-id author (first mentions))
                           (reply mmap "No one mentioned!"))
                         :kill
                         (kill-person channel-id (first mentions))
                         :caption
                         (if (first attachments)
                           (caption channel-id (first attachments) (s/join " " (next mess)))
                           (if (first (:attachments referenced-message))
                             (caption channel-id (first (:attachments referenced-message)) (s/join " " (next mess)))
                             (reply mmap "No attachments given!")))
                         :help
                         (say-help channel-id)
                         :privacy
                         (dm-privacy channel-id author)
                         :roll
                         (if (not (next mess)) 
                           (roll channel-id)
                           (let [roll-val (first (next mess))
                                 alpha (re-find #"[a-zA-Z]" roll-val)]
                             (if (not alpha)
                               (roll channel-id (read-string roll-val))
                               (if (= alpha "d")
                                 (if (re-find #"\d+d" roll-val)
                                   (roll channel-id
                                         (read-string (re-find #"\d+" roll-val))
                                         (read-string (subs (re-find #"d\d+" roll-val) 1)))
                                   (roll channel-id
                                         1
                                         (read-string (subs (re-find #"d\d+" roll-val) 1))))
                                 (roll channel-id)))))
                         :pain
                         (pain channel-id)
                         :predict
                         (predict channel-id)
                         :play
                         (reply mmap "||the game||")
                         :cry
                         (reply mmap (rand-nth (:cry-gif config)))
                         :fun-things-are-fun
                         (reply mmap "https://i.kym-cdn.com/photos/images/newsfeed/001/360/233/ee2.jpg")
                         :meme
                         (reply mmap (rand-nth (:memes config)))
                         :gif
                         (let [cmd (first (next mess))]
                           (if (not (= cmd "add"))
                             (random-gif mmap)
                             (if (first attachments)
                               (add-gif mmap (first attachments))
                               (if (first (:attachments referenced-message))
                                 (add-gif mmap (first (:attachments referenced-message)))
                                 (reply mmap "Specify a file to add, baka!")))))
                         :smug
                         (reply mmap "https://tenor.com/view/lenny-face-kon-yui-yui-hirasawa-anime-gif-21814399")
                         :shrug
                         (reply mmap "https://i.redd.it/8ahxkbdcwn251.png")
                         :will
                         (if (= (first (next mess))
                                "you")
                           (reply mmap "No, I won't.")
                           (reply mmap (rand-nth (:possible? config))))
                         :nod
                         (reply mmap "https://tenor.com/view/anime-yui-yui-hirasawa-anime-rhythm-anime-music-gif-18534548")
                         :think
                         (reply mmap "https://tenor.com/view/yui-yui-hirasawa-anime-anime-girl-anime-mad-gif-18254321")
                         :eat
                         (reply mmap "nom nom nom\nhttps://tenor.com/view/anime-eat-burger-yui-k-on-gif-20746611")
                         :pray
                         (reply mmap "https://tenor.com/view/k-on-yui-hirasawa-mio-akiyama-tsumugi-kotobuki-ritsu-tainaka-gif-22123107")
                         :fuwa
                         (reply mmap "https://www.youtube.com/watch?v=FqkLjGS1IXE")
                         :drink
                         (reply mmap "schlop schlop schlop")
                         :laugh
                         (reply mmap "https://tenor.com/view/k-on-yui-hirasawa-laughing-anime-gif-16038492")
                         :ver
                         (reply mmap (str "My version is ***" (:version config) "***"))
                         :uwu
                         (reply mmap "OwO")
                         :owo
                         (reply mmap "UwU")
                         :else
                         (search-ai author mmap content))))
      (or
       (when (not-empty (clojure.set/intersection (set (tokenize content)) (set (:names config))))
        (search-ai author
                   {:guild_id guild-id
                    :channel_id channel-id
                    :message_id id}
                   content))
      (when (re-find #"(?i)pog" content)
        (counter-add channel-id author)))))))

(defmethod handle-event :ready
  [_ _]
  (c/status-update! (:gateway @state) :activity (c/create-activity :name (:playing config))))

(defmethod handle-event :default [_ _])

(defn start-bot! [token & intents]
  (let [event-channel (chan 100)
        gateway-connection (c/connect-bot! token event-channel :intents (set intents))
        rest-connection (m/start-connection! token)]
    {:events  event-channel
     :gateway gateway-connection
     :rest    rest-connection}))

(defn stop-bot! [{:keys [rest gateway events] :as _state}]
  (m/stop-connection! rest)
  (c/disconnect-bot! gateway)
  (close! events)
  (println "Exited successfuly!"))

(defn set-interval [callback ms]
  (future (while true (do (Thread/sleep ms) (callback)))))

(def cnt (atom 0))

(defn update-alive-message []
  (m/edit-message! (:rest @state)
                   ; alive channel ID
                   ; alive channel message
                   :content
                   (str "Alive at "
                        (reset! cnt (mod (inc @cnt) 6))
                        "0 seconds")))

(if (:replit? config)
  (def job (set-interval #(update-alive-message) 30000)))

(defn checkthread []  
  (go
    (while true
    (ten-minute-check)
    (<! (timeout (* 600 1000))))))

(defn -main [& args]
  (reset! state (start-bot! (:token config) :guild-messages))
  (reset! bot-id (:id @(m/get-current-user! (:rest @state))))
    (checkthread)
  (try
    (message-pump! (:events @state) handle-event)
    (finally (stop-bot! @state))))


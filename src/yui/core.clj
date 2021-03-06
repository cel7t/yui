(ns yui.core
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [clj-time.core :as t]
            [clojure.core.async :refer [chan close!]]
            [discljord.messaging :as m]
            [discljord.connections :as c]
            [discljord.formatting :refer [mention-user user-mention]]
            [discljord.events :refer [message-pump!]]
            ;; experimental feature!!!
            [clj-http.client :as client]
            [cheshire.core])
  (:gen-class))

(def car first) (def cdr next)
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

;; todo: fix else
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
  [_ {:keys [channel-id author mentions content guild-id referenced-message member] :as _data}]
  (when (some #{@bot-id} (map :id mentions))
    (m/create-message! (:rest @state) channel-id :content (random-response author)))
  (when (and (not (:bot author))
             (< 2 (count content))
             (belongs (subs content 0 2) ["hi" "Hi" "he" "He"]))
    (let [mess (cdr (tokenize content))]
      (if (belongs (subs (car mess) 0 3) (:names config))
        (say-hi channel-id author))))
  (when (and (not (:bot author))
             (< 2 (count content))
             (some #(= (subs content 0 3) %) (:names config))) ;; checks if bot was mentioned
    (let [mess (cdr (tokenize content))
          com (car mess)]
      (if (not (car mess)) (prompt channel-id "Wut.")
        (cond-action com config
                     :names
                     (prompt channel-id (rand-nth (:fun config)))
                     :hello
                     (say-hi channel-id author)
                     :bye
                     (say-bye channel-id author)
                     :todo
                     (if (mod? (:id author))
                       (add-todo channel-id (s/join " " (cdr mess))) (not-a-mod channel-id))
                     :say
                     (say-x channel-id (s/join " " (cdr mess)))
                     :search
                     (yui-search channel-id (s/join "+" (cdr mess)))
                     :disconnect
                     (if (mod? (:id author)) (do
                                               (prompt channel-id (affirm))
                                               (prompt channel-id "https://tenor.com/view/yui-x-azusa-yui-azusa-azunyan-anime-gif-21336402")
                                               ;; doesn't wait for some reason
                                               (goodbye channel-id))
                       (not-a-mod channel-id))
                     :pin
                     (pin-message channel-id (:id referenced-message))
                     :edit
                     (if (mod? (:id author)) (do
                                               (prompt channel-id (affirm))
                                               (edit-call channel-id (:id referenced-message)))
                       (not-a-mod channel-id))
                     :delete
                     (if (mod? (:id author)) (do
                                               (prompt channel-id (affirm))
                                               (delete-message channel-id (:id referenced-message)))
                       (not-a-mod channel-id))
                     :revoke
                     (if (mod? (:id author)) (do
                                               (prompt channel-id (affirm))
                                               (revoke-membership channel-id guild-id (:id (car mentions))))
                       (not-a-mod channel-id))
                     :grant
                     (if (mod? (:id author)) (do
                                               (prompt channel-id (affirm))
                                               (grant-membership channel-id guild-id (car mentions)))
                       (not-a-mod channel-id))
                     :repl
                     (if (mod? (:id author)) (do
                                               (prompt channel-id (affirm))
                                               (live-repl channel-id (cdr mess))))
                     :daily
                     (daily channel-id author)
                     :register
                     (register channel-id author)
                     :coins
                     (coins channel-id author (car mentions))
                     :kill
                     (kill-person channel-id (car mentions))
                     :score
                     (user-score channel-id (if (car mentions) (car mentions) author))
                     :help
                     (say-help channel-id)
                     :play
                     (command-audio channel-id com)
                     :privacy
                     (dm-privacy channel-id author)
                     :roll
                     (roll channel-id)
                     :ghot
                     (pain channel-id)
                     :predict
                     (predict channel-id)
                     :cry
                     (prompt channel-id (rand-nth (:cry-gif config)))
                     :fun-things-are-fun
                     (prompt channel-id "https://i.kym-cdn.com/photos/images/newsfeed/001/360/233/ee2.jpg")
                     :meme
                     (prompt channel-id (rand-nth (:memes config)))
                     :smug
                     (prompt channel-id "https://tenor.com/view/lenny-face-kon-yui-yui-hirasawa-anime-gif-21814399")
                     :shrug
                     (prompt channel-id "https://i.redd.it/8ahxkbdcwn251.png")
                     :will
                     (if (= (car (cdr mess))
                            "you")
                       (prompt channel-id "No, I won't.")
                       (prompt channel-id (rand-nth (:possible? config))))
                     :who
                     (if (= (subs (car (cdr mess)) 0 5)
                            "asked")
                       (prompt channel-id "https://tenor.com/view/yui-kon-who-asked-gif-23363342")
                       (prompt channel-id "Me."))
                     :nod
                     (prompt channel-id "https://tenor.com/view/anime-yui-yui-hirasawa-anime-rhythm-anime-music-gif-18534548")
                     :think
                     (prompt channel-id "https://tenor.com/view/yui-yui-hirasawa-anime-anime-girl-anime-mad-gif-18254321")
                     :eat
                     (prompt channel-id "nom nom nom\nhttps://tenor.com/view/anime-eat-burger-yui-k-on-gif-20746611")
                     :drink
                     (prompt channel-id "schlop schlop schlop")
                     :laugh
                     (prompt channel-id "https://tenor.com/view/k-on-yui-hirasawa-laughing-anime-gif-16038492")
                     :add
                     (if (belongs (str (:member-role config)) (:roles member))
                       (if (not (or (belongs (str (:role-1 config)) (:roles member))
                                    (belongs (str (:role-2 config)) (:roles member))
                                    (belongs (str (:role-3 config)) (:roles member))))
                         (let [role (subs (last mess) 0 3)]
                           (cond
                             (re-find #"(?i)xxx" role)
                             (grant-role channel-id guild-id author (:role-1 config) "XXX")
                             (re-find #"(?i)yyy" role)
                             (grant-role channel-id guild-id author (:role-2 config) "YYY")
                             (re-find #"(?i)zzz" role)
                             (grant-role channel-id guild-id author (:role-3 config) "ZZZ")
                             :else
                             (prompt channel-id "Role not found, baka!")))
                         (prompt channel-id "You already have a role!"))
                       (prompt channel-id "You are not a member"))
                     :remove
                     (if (or (belongs (str (:role-1 config)) (:roles member))
                             (belongs (str (:role-2 config)) (:roles member))
                             (belongs (str (:role-3 config)) (:roles member)))
                       (let [role (subs (last mess) 0 3)]
                         (cond
                           (re-find #"(?i)xxx" role)
                           (remove-role channel-id guild-id author (:role-1 config) "XXX")
                           (re-find #"(?i)yyy" role)
                           (remove-role channel-id guild-id author (:role-2 config) "YYY")
                           (re-find #"(?i)zzz" role)
                           (remove-role channel-id guild-id author (:role-3 config) "ZZZ")
                           :else
                           (prompt channel-id "Role not found, baka!")))
                       (prompt channel-id "You do not belong to any role!"))
                     :uwu
                     (prompt channel-id "OwO")
                     :owo
                     (prompt channel-id "UwU")
                     :else
                     (say-error channel-id com))))))

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

(defn -main [& args]
  (reset! state (start-bot! (:token config) :guild-messages))
  (reset! bot-id (:id @(m/get-current-user! (:rest @state))))
  (try
    (message-pump! (:events @state) handle-event)
    (finally (stop-bot! @state))))


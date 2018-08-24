(ns coup-cli.core
  (:gen-class))

(defrecord Action [name cost blockers challengeable identity target])
(def identities ["Duke" "Captian" "Ambassador" "Contessa" "Assassin"])
(def actions [(Action. "Coup" 7 [] false nil false)
              (Action. "Income" 0 [] false nil false)
              (Action. "Foreign Aid" 0 ["Duke"] false nil false)
              (Action. "Tax" 0 [] true "Duke" false)
              (Action. "Exchange" 0 [] true "Ambassador" false)
              (Action. "Assassinate" 3 ["Contessa"] true "Assassin" true)
              (Action. "Steal" 0 ["Captain" "Ambassador"] true "Captain" true)])

(defrecord Player [number identities revealed-identities coins])

(defn available-actions
  [coins]
  (vec (if (>= coins 10)
         (take 1 actions)
         (filter #(<= (:cost %) coins) actions))))

(defn print-player
  [player]
  (println (str "Player " (:number player)))
  (println (str "You have " (:coins player) " coins."))
  (println "Select an action:")
  (doseq [[action number] (map vector (available-actions (:coins player))  (range))]
    (print (str (inc number) ". " (:name action)))
    (if (> (:cost action) 0)
      (print (str " (" (:cost action) ")")))
    (if (:identity action)
      (if (get (set (:identities player)) (:identity action))
        (print " (Truth)")
        (print " (Lie)")))
    (println)))

(defn deal-card
  ([court-deck]
   (let [deck (shuffle court-deck)]
     [(first deck) (rest deck)])))

(defn deal-cards
  [n court-deck]
  (split-at n (shuffle court-deck)))

(def default-deck (apply concat (repeat 3 identities)))

(defn make-new-game
  [n-players]
  (let [[player-cards court-deck] (deal-cards (* n-players 2) default-deck)]
    {:players (mapv #(Player. %1 (vec %2) [] (if (and (= 2 n-players) (= 0 %)) 1 2)) (range n-players) (partition 2 player-cards))
     :court-deck court-deck
     :turn 0}))

(defn alive?
  [player]
  (not (empty? (:identities player))))

(defn play-game
  [state]
  (if (> (count (filter alive? (:players state))) 1)
    ;; There are living players
    (do
      (print-player (get (:players state) (:turn state)))
      (let [player (get (:players state) (:turn state))
            choice (dec (try (Integer/parseInt (read-line)) (catch Exception _ 0)))
            targets (concat
                           (take (:turn state) (:players state))
                           (drop (inc (:turn state)) (:players state)))]
        (if-not (< -1 choice (count (available-actions (:coins player))))
          (do
            (println "Invalid choice")
            (recur state))
          (let [action (get (available-actions (:coins player)) choice)]
            (if (:target action)
              (do (println "Choose a target:")
                  (doseq [[target number] (map vector targets (range))]
                    (println (str number ". " (:number target))))))))))
    ;; The game is over, declare last player winner
    :default
    ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (play-game (make-new-game 2)))

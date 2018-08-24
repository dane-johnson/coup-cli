(ns coup-cli.core
  (:gen-class))

(defrecord Action [name cost blockers challengeable identity])
(def identities ["Duke" "Captian" "Ambassador" "Contessa" "Assassin"])
(def actions [(Action. "Coup" 7 [] false nil)
              (Action. "Income" 0 [] false nil)
              (Action. "Foreign Aid" 0 ["Duke"] false nil)
              (Action. "Tax" 0 [] true "Duke")
              (Action. "Exchange" 0 [] true "Ambassador")
              (Action. "Assassinate" 3 ["Contessa"] true "Assassin")
              (Action. "Steal" 0 ["Captain" "Ambassador"] true "Captain")])

(defrecord Player [number identities revealed-identities coins])

(defn available-actions
  [coins]
  (if (>= coins 10)
    (take 1 actions)
    (filter #(<= (:cost %) coins) actions)))

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
    {:players (map #(Player. %1 (vec %2) [] (if (and (= 2 n-players) (= 0 %)) 1 2)) (range n-players) (partition 2 player-cards))}))

(defn play-game
  [state]
  ())

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

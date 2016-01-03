; http://adventofcode.com/day/22
(ns aoc.day22)


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

; --- Day 22: Wizard Simulator 20XX ---

; Little Henry Case decides that defeating bosses with swords and stuff is
; boring. Now he's playing the game with a wizard. Of course, he gets stuck on
; another boss and needs your help again.

; In this version, combat still proceeds with the player and the boss taking
; alternating turns. The player still goes first. Now, however, you don't get
; any equipment; instead, you must choose one of your spells to cast. The first
; character at or below 0 hit points loses.

; Since you're a wizard, you don't get to wear armor, and you can't attack
; normally. However, since you do magic damage, your opponent's armor is
; ignored, and so the boss effectively has zero armor as well. As before, if
; armor (from a spell, in this case) would reduce damage below 1, it becomes 1
; instead - that is, the boss' attacks always deal at least 1 damage.

; On each of your turns, you must select one of your spells to cast. If you
; cannot afford to cast any spell, you lose. Spells cost mana; you start with
; 500 mana, but have no maximum limit. You must have enough mana to cast a
; spell, and its cost is immediately deducted when you cast it. Your spells are
; Magic Missile, Drain, Shield, Poison, and Recharge.

;   - Magic Missile costs 53 mana. It instantly does 4 damage.
;   - Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit
;     points.
;   - Shield costs 113 mana. It starts an effect that lasts for 6 turns. While
;     it is active, your armor is increased by 7.
;   - Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the
;     start of each turn while it is active, it deals the boss 3 damage.
;   - Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At
;     the start of each turn while it is active, it gives you 101 new mana.

; Effects all work the same way. Effects apply at the start of both the
; player's turns and the boss' turns. Effects are created with a timer (the
; number of turns they last); at the start of each turn, after they apply any
; effect they have, their timer is decreased by one. If this decreases the
; timer to zero, the effect ends. You cannot cast a spell that would start an
; effect which is already active. However, effects can be started on the same
; turn they end.

; For example, suppose the player has 10 hit points and 250 mana, and that the
; boss has 13 hit points and 8 damage:

; -- Player turn --
; - Player has 10 hit points, 0 armor, 250 mana
; - Boss has 13 hit points
; Player casts Poison.

; -- Boss turn --
; - Player has 10 hit points, 0 armor, 77 mana
; - Boss has 13 hit points
; Poison deals 3 damage; its timer is now 5.
; Boss attacks for 8 damage.

; -- Player turn --
; - Player has 2 hit points, 0 armor, 77 mana
; - Boss has 10 hit points
; Poison deals 3 damage; its timer is now 4.
; Player casts Magic Missile, dealing 4 damage.

; -- Boss turn --
; - Player has 2 hit points, 0 armor, 24 mana
; - Boss has 3 hit points
; Poison deals 3 damage. This kills the boss, and the player wins.

; Now, suppose the same initial conditions, except that the boss has 14 hit
; points instead:

; -- Player turn --
; - Player has 10 hit points, 0 armor, 250 mana
; - Boss has 14 hit points
; Player casts Recharge.

; -- Boss turn --
; - Player has 10 hit points, 0 armor, 21 mana
; - Boss has 14 hit points
; Recharge provides 101 mana; its timer is now 4.
; Boss attacks for 8 damage!

; -- Player turn --
; - Player has 2 hit points, 0 armor, 122 mana
; - Boss has 14 hit points
; Recharge provides 101 mana; its timer is now 3.
; Player casts Shield, increasing armor by 7.

; -- Boss turn --
; - Player has 2 hit points, 7 armor, 110 mana
; - Boss has 14 hit points
; Shield's timer is now 5.
; Recharge provides 101 mana; its timer is now 2.
; Boss attacks for 8 - 7 = 1 damage!

; -- Player turn --
; - Player has 1 hit point, 7 armor, 211 mana
; - Boss has 14 hit points
; Shield's timer is now 4.
; Recharge provides 101 mana; its timer is now 1.
; Player casts Drain, dealing 2 damage, and healing 2 hit points.

; -- Boss turn --
; - Player has 3 hit points, 7 armor, 239 mana
; - Boss has 12 hit points
; Shield's timer is now 3.
; Recharge provides 101 mana; its timer is now 0.
; Recharge wears off.
; Boss attacks for 8 - 7 = 1 damage!

; -- Player turn --
; - Player has 2 hit points, 7 armor, 340 mana
; - Boss has 12 hit points
; Shield's timer is now 2.
; Player casts Poison.

; -- Boss turn --
; - Player has 2 hit points, 7 armor, 167 mana
; - Boss has 12 hit points
; Shield's timer is now 1.
; Poison deals 3 damage; its timer is now 5.
; Boss attacks for 8 - 7 = 1 damage!

; -- Player turn --
; - Player has 1 hit point, 7 armor, 167 mana
; - Boss has 9 hit points
; Shield's timer is now 0.
; Shield wears off, decreasing armor by 7.
; Poison deals 3 damage; its timer is now 4.
; Player casts Magic Missile, dealing 4 damage.

; -- Boss turn --
; - Player has 1 hit point, 0 armor, 114 mana
; - Boss has 2 hit points
; Poison deals 3 damage. This kills the boss, and the player wins.

; You start with 50 hit points and 500 mana points. The boss's actual stats are
; in your puzzle input. What is the least amount of mana you can spend and
; still win the fight? (Do not include mana recharge effects as "spending"
; negative mana.)

(def boss-stats {:hp 58 :damage 9})
(def player-stats {:hp 50 :mana 500 :armor 0})

(def effect-spells {:shield {:mana 113 :turns 6 :armor-gain 7}
                     :poison {:mana 173 :turns 6 :damage 3}
                     :recharge {:mana 229 :turns 5 :mana-gain 101}})
(def dd-spells {:missile {:mana 53 :damage 4}
                 :drain {:mana 73 :damage 2}})
(def all-spells (conj effect-spells dd-spells))

(defn add-effect
  "Add the given effect to the boss and/or player stats."
  [player boss effects [effect stats]]
  (let [effects (assoc effects effect stats)
        player (update player :mana - (:mana stats))]
    (if (= effect :shield)
      ; We want to update the player's armor only once, when effect is added.
      [(update player :armor + (:armor-gain stats)) boss effects]
      [player boss effects])))

(assert (= (add-effect player-stats boss-stats {}
                         [:shield (:shield effect-spells)])
           [{:hp 50 :mana 387 :armor 7}
            boss-stats
            {:shield (:shield effect-spells)}]))
(assert (= (add-effect player-stats boss-stats {}
                         [:poison (:poison effect-spells)])
           [{:hp 50 :mana 327 :armor 0}
            boss-stats
            {:poison (:poison effect-spells)}]))

(defn apply-effect
  "Apply the given effect to the boss and/or player stats."
  [player boss effects [effect stats]]
  ; Decrement the remaining turns for this effect.
  (let [effects (update-in effects [effect :turns] dec)]
   (condp = effect
    :poison [player (update boss :hp - (:damage stats )) effects]
    :recharge [(update player :mana + (:mana-gain stats)) boss effects]
    :shield [player boss effects]))) ; Deal with this in add/remove effect.

(assert (= (apply-effect player-stats boss-stats
                         {:poison (:poison effect-spells)}
                         [:poison (:poison effect-spells)])
           [player-stats
            {:hp 55 :damage 9} ; Poison did 3 damage.
            ; Number of turns for poison has decreased.
            {:poison {:mana 173 :turns 5 :damage 3}}]))

(defn apply-effects
  "Apply all the current effects to the boss and/or player stats."
  [player boss effects]
  (reduce (fn [[player boss effects] effect]
            (apply-effect player boss effects effect))
          [player boss effects]
          effects))

(assert (= (apply-effects player-stats boss-stats
                            {:shield (:shield effect-spells)
                             :poison (:poison effect-spells)})
           [player-stats ; Nothing changed on the player.
            {:hp 55 :damage 9} ; 3 hp down from the poison.
            ; All effects' turns have decreased by 1.
            {:shield {:mana 113 :turns 5 :armor-gain 7}
             :poison {:mana 173 :turns 5 :damage 3}}]))

(defn remove-effect
  "Remove the given effect from the boss and/or player stats."
  [player boss effects [effect stats]]
  (let [effects (dissoc effects effect)]
    (if (= effect :shield)
      ; We want to return the player's armor back to its original value.
      [(update player :armor - 7) boss effects]
      [player boss effects])))

(assert (= (remove-effect (assoc player-stats :armor 7) boss-stats
                            {:shield (:shield effect-spells)
                             :poison (:poison effect-spells)}
                            [:shield (:shield effect-spells)])
           [player-stats ; Player's armor back to its original value.
            boss-stats ; No incidence on the boss stats.
            {:poison (:poison effect-spells)}])) ; The "shield" effect is gone.

(defn remove-effects
  "Remove all the current effects if their number of turns left is 0."
  [player boss effects]
  (reduce (fn [[player boss effects] [effect stats]]
            (if (= 0 (:turns stats))
              (remove-effect player boss effects [effect stats])
              [player boss effects]))
          [player boss effects]
          effects))

(assert (= (remove-effects (assoc player-stats :armor 7) boss-stats
                             (assoc-in {:shield (:shield effect-spells)
                                        :poison (:poison effect-spells)}
                                       [:shield :turns] 0))
           [player-stats ; Player's armor back to its original value.
            boss-stats ; No incidence on the boss stats.
            ; The "shield" effect is gone.
            {:poison (:poison effect-spells)}]))

(def apply-remove-effects (fn [player boss effects]
                            (apply remove-effects
                                   (apply-effects player boss effects))))

(defn apply-dd
  "Apply the given direct damage to the boss and/or player stats."
  [player boss effects [spell stats]]
  (let [player (update player :mana - (:mana stats))]
    (condp = spell
      :missile [player (update boss :hp - (:damage stats )) effects]
      :drain [(update player :hp + (:damage stats))
              (update boss :hp - (:damage stats))
              effects])))

(assert (= (apply-dd player-stats boss-stats {}
                     [:missile (:missile dd-spells)])
           [{:hp 50 :mana 447 :armor 0} {:hp 54 :damage 9} {}]))
(assert (= (apply-dd player-stats boss-stats {} [:drain (:drain dd-spells)])
           [{:hp 52 :mana 427 :armor 0} {:hp 56 :damage 9} {}]))

(defn next-spell-list
  "Given the current active effects, return the list of available spells."
  [player effects]
  (let [current-mana (:mana player)
        remaining-mana (if (:recharge effects)
                         ; If there is an active recharge, then the player has
                         ; some more mana before casting a spell (effects apply
                         ; before the turn).
                         (+ current-mana
                            (get-in all-spells [:recharge :mana-gain]))
                         current-mana)
        ; Only keep spells that don't cost too much.
        available-spells (filter (fn [[spell stats]]
                                   (> remaining-mana (:mana stats)))
                                 all-spells)]
    (map first
         (filter
           (fn [[spell stats]]
             ; Remove effects that will still be active on the player turn.
             (or (nil? (spell effects)) ; Not active.
                 (>= 1 (:turns (spell effects))))) ; Will wear off next turn.
           available-spells))))

(assert (= (next-spell-list player-stats {:shield (:shield all-spells)
                                          :poison (:poison all-spells)})
           [:recharge :missile :drain]))
(assert (= (next-spell-list player-stats
                            {:shield {:mana 113 :turns 1 :armor-gain 7}
                             :poison (:poison all-spells)})
           [:shield :recharge :missile :drain]))
(assert (= (next-spell-list (assoc player-stats :mana 60)
                            {:shield (:shield all-spells)
                             :poison (:poison all-spells)})
           [:missile]))
(assert (= (next-spell-list (assoc player-stats :mana 6)
                            {:shield (:shield all-spells)
                             :poison (:poison all-spells)})
           []))
(assert (= (next-spell-list (assoc player-stats :mana 6)
                            {:shield (:shield all-spells)
                             :poison (:poison all-spells)
                             :recharge (:recharge all-spells)})
           [:missile :drain]))

; Player turn:
; - apply-effects
; - remove-effects
; - choose the spell (if it's an effect, call add-effect)
; Boss turn:
; - apply-effects
; - remove-effects
; - do the damage

(defn gameover? [player boss]
  (if (>= 0 (:hp player)) ; Player dead.
    :boss
    (if (>= 0 (:hp boss)) ; Boss dead.
      :player)))

(assert (= (gameover? player-stats boss-stats)
           nil))
(assert (= (gameover? (assoc player-stats :hp 0) boss-stats)
           :boss))
(assert (= (gameover? player-stats (assoc boss-stats :hp 0))
           :player))

(defn boss-turn
  "Apply the damage if possible."
  [player boss effects]
  (let [[player boss effects] (apply-remove-effects player boss effects)]
    (if-let [winner (gameover? player boss)]
      [player boss effects winner]
      (let [player (update player :hp - (- (:damage boss) (:armor player)))]
        [player boss effects (gameover? player boss)]))))

(assert (= (boss-turn player-stats boss-stats {})
           [{:hp 41 :mana 500 :armor 0} boss-stats {} nil]))
(assert (= (boss-turn (assoc player-stats :armor 7) boss-stats {})
           [{:hp 48 :mana 500 :armor 7} boss-stats {} nil]))
(assert (= (boss-turn (assoc player-stats :hp 7) boss-stats {})
           [{:hp -2 :mana 500 :armor 0} boss-stats {} :boss]))

(defn ^:dynamic player-turn
  "Cast the spell if the boss is still alive, and return the mana used."
  [player boss effects spell]
  (let [[player boss effects] (apply-remove-effects player boss effects)]
   (if-let [winner (gameover? player boss)]
     [player boss effects 0 winner]
     (if-let [[spell-key stats] spell] ; There is a spell that can be casted.
      (let [[player boss effects]
              (if (spell-key dd-spells)
                (apply-dd player boss effects spell) ; Direct damage.
                (add-effect player boss effects spell))] ; Effect.
          [player boss effects (:mana stats) (gameover? player boss)])
      [player boss effects 0 :boss])) ; No spell because no mana left.
    )
  )

(assert (= (player-turn player-stats boss-stats {}
                        [:missile (:missile dd-spells)])
           [{:hp 50 :mana 447 :armor 0} {:hp 54 :damage 9} {} 53 nil]))
(assert (= (player-turn player-stats boss-stats {} nil)
           [{:hp 50 :mana 500 :armor 0} {:hp 58 :damage 9} {} 0 :boss]))
(assert (= (player-turn player-stats boss-stats {}
                        [:shield (:shield effect-spells)])
           [{:hp 50 :mana 387 :armor 7}
            {:hp 58 :damage 9}
            {:shield (:shield effect-spells)}
            113
            nil]))
(assert (= (player-turn player-stats (assoc boss-stats :hp 7)
                        {:poison (:poison effect-spells)}
                        [:missile (:missile dd-spells)])
           [{:hp 50 :mana 447 :armor 0}
            {:hp 0 :damage 9}
            {:poison {:mana 173 :turns 5 :damage 3}}
            53
            :player]))
(assert (= (player-turn player-stats (assoc boss-stats :hp 3)
                        {:poison (:poison effect-spells)}
                        [:missile (:missile dd-spells)])
           [{:hp 50 :mana 500 :armor 0}
            {:hp 0 :damage 9}
            {:poison {:mana 173 :turns 5 :damage 3}}
            0 ; The poison effect killed the boss, no need for the missile.
            :player]))

(defn play-two-turns
  "Play the player turn then the boss turn. Return the winner if any."
  [player boss effects spell]
  ; Player turn first.
  (let [[player boss effects mana-used winner]
          (player-turn player boss effects spell)]
    (if winner
      [player boss effects mana-used winner]
      ; Boss turn.
      (let [[player boss effects winner] (boss-turn player boss effects)]
        [player boss effects mana-used winner]))))

(assert (= (play-two-turns player-stats boss-stats {}
                           [:missile (:missile dd-spells)])
           [{:hp 41 :mana 447 :armor 0} {:hp 54 :damage 9} {} 53 nil]))
(assert (= (play-two-turns (assoc player-stats :hp 8) boss-stats {}
                             [:missile (:missile dd-spells)])
           [{:hp -1 :mana 447 :armor 0} {:hp 54 :damage 9} {} 53 :boss]))
(assert (= (play-two-turns player-stats (assoc boss-stats :hp 4) {}
                           [:missile (:missile dd-spells)])
           [{:hp 50 :mana 447 :armor 0} {:hp 0 :damage 9} {} 53 :player]))
(assert (= (play-two-turns player-stats (assoc boss-stats :hp 3)
                             {:poison (:poison effect-spells)}
                             [:missile (:missile dd-spells)])
           ; Boss dead from poison before player needed to cast.
           [{:hp 50 :mana 500 :armor 0} {:hp 0 :damage 9}
            {:poison {:mana 173 :turns 5 :damage 3}} 0 :player]))

(defn play-all-spells
  "Play all the turns using the list of spells."
  [player boss spells]
  (reduce (fn [[player boss effects total-mana-used winner] spell]
            (let [stats (spell all-spells)
                  [player boss effects mana-used winner]
                    (play-two-turns player boss effects [spell stats])
                  total-mana-used (+ total-mana-used mana-used)]
              (if winner
                (reduced [player boss effects total-mana-used winner])
                [player boss effects total-mana-used nil])))
          [player boss {} 0 nil]
          spells))

(assert (= (play-all-spells player-stats boss-stats [:missile])
           [{:hp 41 :mana 447 :armor 0} {:hp 54 :damage 9} {} 53 nil]))
(assert (= (play-all-spells {:hp 10 :mana 250 :armor 0} {:hp 13 :damage 8}
                            [:poison :missile])
           [{:hp 2 :mana 24 :armor 0} {:hp 0 :damage 8}
            {:poison {:mana 173 :turns 3 :damage 3}} 226 :player]))
(assert (= (play-all-spells {:hp 10 :mana 250 :armor 0} {:hp 14 :damage 8}
                              [:recharge :shield :drain :poison :missile])
           [{:hp 1 :mana 114 :armor 0} {:hp -1 :damage 8}
            {:poison {:mana 173 :turns 3 :damage 3}} 641 :player]))

; This is really not optimized: we explore all the games (play them through)
; for each new spell, instead of memorizing the current situation.
(defn explore-combinations [player boss]
  "Explore all the possible games."
  (let [finished-games (atom [])
        min-mana-to-win (atom Integer/MAX_VALUE)]
    (loop [combinations (map vector (keys all-spells))]
      (dbg (count combinations))
      (if (empty? combinations)
        @finished-games ; We explored everything.
        ; Explore all combinations, and recur with the new combinations.
        (recur (reduce
                 (fn [new-combinations spells]
                   (let [[player boss effects mana-used winner]
                           (play-all-spells player boss spells)]
                     (if winner
                       ; We found a winner for this game: this one is finished.
                       (do (swap! finished-games conj {:winner winner
                                                       :mana-used mana-used
                                                       :spells spells})
                           (if (and (= winner :player)
                                    (< mana-used @min-mana-to-win))
                             ; Update the min amount of mana to win.
                             (reset! min-mana-to-win mana-used))
                           new-combinations) ; Don't add any new combination.
                       ; No winner yet: create a new combination for each of
                       ; the next available spells.
                       (if (> mana-used @min-mana-to-win)
                         ; Optimization: if we already used more mana than a
                         ; winned game, stop exploring.
                         new-combinations
                         ; Otherwise, go on, and add the new combinations.
                         (if-let [next-spells (next-spell-list player effects)]
                           ; For each next spell, add a new combination of the
                           ; current spells followed by the next spell. Those
                           ; are the new combinations to try.
                           (reduce conj new-combinations
                                   (map #(conj spells %) next-spells))
                           ; No more mana: is player going to win thanks to
                           ; effects before he needs to cast a spell?
                           (conj new-combinations (conj spells nil)))))))
                 []
                 combinations))))))

(assert (= (apply min
                  (map :mana-used
                       (filter #(= (:winner %) :player)
                               (explore-combinations
                                 {:hp 10 :mana 250 :armor 0}
                                 {:hp 13 :damage 8}))))
           226))
(assert (= (apply min
                  (map :mana-used
                       (filter #(= (:winner %) :player)
                               (explore-combinations
                                 {:hp 10 :mana 250 :armor 0}
                                 {:hp 14 :damage 8}))))
           641))

; The following takes less than a minute on a recent (2015) mbp.
(defn part1 []
  (println
    "The least amount of mana to still win is:"
    (apply min
           (map :mana-used
                (filter #(= (:winner %) :player)
                        (explore-combinations player-stats boss-stats))))))


; --- Part Two ---

; On the next run through the game, you increase the difficulty to hard.

; At the start of each player turn (before any other effects apply), you lose 1
; hit point. If this brings you to or below 0 hit points, you lose.

; With the same starting stats for you and the boss, what is the least amount
; of mana you can spend and still win the fight?

(def simpler-player-turn player-turn)

(defn harder-player-turn [player boss effects spell]
  (simpler-player-turn (update player :hp dec) boss effects spell))

; The following takes less than a minute on a recent (2015) mbp.
(defn part2 []
  (println
    "The least amount of mana to still win the hard difficulty is:"
    (binding [player-turn harder-player-turn] ; Override "player-turn".
      (apply min
             (map :mana-used
                  (filter #(= (:winner %) :player)
                          (explore-combinations player-stats boss-stats)))))))

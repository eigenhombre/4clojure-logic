(ns clojic.zebra
  (:require [clojure.core.logic :refer [lvar defne conde all run firsto membero ==]])
  (:require [clojure.tools.macro :as macro]))

;; Solution based on https://github.com/swannodette/logic-tutorial:


;; defne is a macro which is like defn but which allows you to specify
;; patterns which succeed.  righto succeeds if y is to the right of x
;; at the beginning of the list, or if they are buried deeper in the
;; list and are in the correct order there.
(defne righto [x y l]
  ([_ _ [x y . ?r]])
  ([_ _ [_ . ?r]] (righto x y ?r)))

(defn nexto
  "
  If x and y are next to each other, then x is to the right of y, or y
  is to the right of x.
  "
  [x y l]
  (conde
   ((righto x y l))
   ((righto y x l))))

;; symbol-macrolet lets you define a symbol which behaves as a
;; function call for the body of that expression! This is a crazy
;; beautiful feature of the tools.macro library.
(macro/symbol-macrolet [_ (lvar)]
  [_ _ _])
;;=>
'[<lvar:27919> <lvar:27920> <lvar:27921>]

(defn zebrao [hs]
  (macro/symbol-macrolet [_ (lvar)]
    (all
     ;; Milk is drunk in the middle house (h3):
     ;;  h1 h2       h3      h4 h5
     ;;  |  |        |        | |
     ;;  |  | --------------- | |
     ;;  v  v v             v v v
     (== [_ _ [_ _ 'milk _ _] _ _] hs)
     ;; The Norwegian lives on the first house on the left:
     (firsto hs ['norwegian _ _ _ _])
     ;; The Norwegian lives next to the blue house:
     (nexto ['norwegian _ _ _ _] [_ _ _ _ 'blue] hs)
     ;; The ivory house is just to the right of the green house:
     (righto [_ _ _ _ 'ivory] [_ _ _ _ 'green] hs)
     ;; The Englishman lives in the red house:
     (membero ['englishman _ _ _ 'red] hs)
     ;; The person in the yellow house smokes Kools:
     (membero [_ 'kools _ _ 'yellow] hs)
     ;; The Spaniard has a dog:
     (membero ['spaniard _ _ 'dog _] hs)
     ;; The coffee drinker's house is green:
     (membero [_ _ 'coffee _ 'green] hs)
     ;; The Ukranian drinks tea:
     (membero ['ukrainian _ 'tea _ _] hs)
     ;; The Lucky Strikes smoker drinks Orange Juice:
     (membero [_ 'lucky-strikes 'oj _ _] hs)
     ;; The Japanese person smokes Parliaments:
     (membero ['japanese 'parliaments _ _ _] hs)
     ;; The Old Golds smoker keeps snails:
     (membero [_ 'oldgolds _ 'snails _] hs)
     ;; The Kools smoker lives next to the house whose owner keeps a
     ;; horse:
     (nexto [_ _ _ 'horse _] [_ 'kools _ _ _] hs)
     ;; The Fox owner lives next to the house where Chesterfields are
     ;; smoked:
     (nexto [_ _ _ 'fox _] [_ 'chesterfields _ _ _] hs))))

(run 1 [q] (zebrao q))
;;=>
'([[norwegian kools _0 fox yellow]
   [ukrainian chesterfields tea horse blue]
   [englishman oldgolds milk snails red]
   [spaniard lucky-strikes oj dog ivory]
   [japanese parliaments coffee _1 green]])


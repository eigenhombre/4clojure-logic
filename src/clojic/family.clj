(ns clojic.family
  (:require [clojure.core.logic
             :refer [all
                     fresh
                     run
                     run*]]
            [clojure.core.logic.pldb
             :refer [db
                     db-fact
                     db-rel
                     with-db]]))

(db-rel person x)
(def facts (-> (db [person 'Bob]
                   [person 'Judy]
                   [person 'Delores])))

(with-db facts
  (run* [q] (person q)))

(db-rel child x y)

(def facts (-> facts
               (db-fact child 'Bob 'Judy)
               (db-fact child 'Delores 'Bob)))

(with-db facts
  (run* [q] (child q 'Bob)))
;;=>
'(Delores)

(defn parent [x y]
  (all
   (person x)
   (person y)
   (child y x)))

(defn grandparent [x y]
  (fresh [z]
    (all
     (person x)
     (person y)
     (person z)
     (parent x z)
     (parent z y))))

(defn grandchild [x y]
  (all
   (person x)
   (person y)
   (grandparent y x)))

(with-db facts
  (run* [q r]
    (grandchild q r)))

;; Exercise 1: add more people to fill out the family tree (or change
;; it to mirror your own family's).  Write more relations and try more
;; queries.

;; Exercise 2: add a second relation, with data and sample queries.
;; Example: gender + find great grandmothers of person X; find the cousins of Janet's grandson; etc.
;; Example: professions + knowledge-of-person.  E.g. find all
;; carpenters who know any of Janet's uncles.

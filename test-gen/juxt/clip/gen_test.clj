(ns juxt.clip.gen-test
  (:require
   [clojure.test.check.clojure-test :refer [defspec]]
   [com.gfredericks.test.chuck.generators :as gen']
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [juxt.clip.impl.core :as impl]
   [clojure.edn :as edn]
   [clojure.test.check :as check]))

;; test: impl/sccs produces a valid topoligical sort for a graph with no cycles
;; Generate a topological sorting of a graph (here a list of unique numbers) (a)
;; generate a matching graph (b) out of the previous generatete list (a)
;; use juxt.clip.impl.core/sccs to calcualte a topological sorting (c) of the graph (b)
;; match the calculated topoligical sort (c) with the generated graph (b)

(def topo-list
  (gen/such-that not-empty (gen/vector-distinct gen/nat)))

(defn subgraphf [nodelist]
  (gen'/for [sub (gen'/subset (rest nodelist))]
    {(first nodelist) sub}))

(def subgraph
  (gen'/for [nodelist topo-list
             sub (gen'/subset (rest nodelist))]
    {(first nodelist) sub}))

#_(def graph
  (gen'/for [subnodelists (gen/fmap #(take-while seq (iterate next %)) topo-list)
             subgraphs (gen/bind subnodelists #(apply gen/tuple (map subgraphf %)))
             dependecy-graph (gen/fmap #(apply merge-with clojure.set/union %) subgraphs)]
    dependecy-graph))


(def all
  (gen/fmap #(apply merge-with clojure.set/union %) (gen/bind (gen/fmap #(take-while seq (iterate next %)) topo-list) #(apply gen/tuple (map subgraphf %)))))

(defn exists-and-has-no-incomming? [node graph]
  (and (contains? graph node) (not (contains? (apply clojure.set/union (vals graph)) node))))

(defn topo-graph-match? [topo graph]
  (if (empty? topo)
    (empty? graph) ;; both empty => true
    (let [node (first topo)]
      (if (exists-and-has-no-incomming? node graph)
        (recur (rest topo) (dissoc graph node))
        {:does-not-exist-or-has-incomming graph :topo topo}))))

(def topo-prop
  (prop/for-all [g all]
                (topo-graph-match? (reverse (map first (impl/sccs g))) g)))

(defn -main
  [& args]
  (let [[num-tests & args] (map edn/read-string args)
        res (apply check/quick-check num-tests topo-prop args)]
    (prn res)
    (System/exit (if (:pass? res) 0 1))))

(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map (fn [x] (value-at board [x col]))
            (range (count board)))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [top-left (fn [[row col]]
                   [(- row (mod row 3))
                    (- col (mod col 3))])]
    (let [[tl-row tl-col] (top-left coord)]
      (set
        (for [r (range tl-row (+ 3 tl-row))
              c (range tl-col (+ 3 tl-col))]
          (value-at board [r c]))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (set/union
        (row-values board coord)
        (col-values board coord)
        (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (for [r (range 9)]
    (row-values board [r 0])))

(defn valid-helper [board func]
  (reduce (fn [x y] (and x y))
          (map empty?
               (for [rs (func board)]
                 (clojure.set/difference all-values rs)))))

(defn valid-rows? [board]
  (valid-helper board rows))

(defn cols [board]
  (for [c (range 9)]
    (col-values board [0 c])))

(defn valid-cols? [board]
  (valid-helper board cols))

(defn blocks [board]
  (for [r [0 3 6]
        c [0 3 6]]
    (block-values board [r c])))

(defn valid-blocks? [board]
  (valid-helper board blocks))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (cond
      (> row 8)
        nil
      (> col 8)
        (recur (inc row) 0)
      (not (has-value? board [row col]))
        [row col]
      :else
        (recur row (inc col)))))

(defn solve-helper [board empty-coord values]
  (cond
    (nil? empty-coord)
      board
    (empty? values)
      board
    :else
      (let [new-board (set-value-at board
                                    empty-coord
                                    (first values))
            new-empty-coord (find-empty-point new-board)
            valid-values (if (nil? new-empty-coord)
                           #{}
                           (valid-values-for new-board
                                             new-empty-coord))
            solution (solve-helper new-board
                                   new-empty-coord
                                   valid-values)]
        (if (valid-solution? solution)
          solution
          (solve-helper board 
                        empty-coord 
                        (rest values))))))

(defn solve [board]
  (let [empty-coord (find-empty-point board)]
    (solve-helper board empty-coord
                  (valid-values-for board empty-coord))))


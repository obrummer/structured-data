(ns structured-data
  (:require [authors-books :refer :all]))

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
  (+ first third)))

(defn cutify [v]
  (let [item "<3"]
(conj v item)
))

(defn spiff-destructuring [v]
  (let [[first second third] v]
  (+ first third)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (== (- x2 x1)(- y2 y1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (* (- x2 x1)(- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle 
        [x y] point]
  (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
  (and (contains-point? (rectangle [x1 y1] [x2 y2]) (point x3 y3))
       (contains-point? (rectangle [x1 y1] [x2 y2]) (point x4 y4)))       
))

(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (not (== (count (:authors book)) 1)))

(defn add-author [book new-author]
  (let [authors (:authors book)] 
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (let [year (:death-year author)] 
    (if year false true))
  )

(defn get-length [element]
  (count element)
  )

(defn element-lengths [collection]
  (map get-length collection))

(defn second-elements [collection]
   (let [second-element (fn [x] (get x 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))


(defn stars [n]
   (let [amount (repeat n "*")] 
     (apply str amount)))


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
  (assoc book :authors (set authors))))

(defn has-author? [book author]
  (let [new-book (old-book->new-book book)]
  (contains? (:authors new-book) author)))

(defn get-authors [book]
  (let [authors (:authors book)]
  (set authors)))

(defn get-author-names [book]
  (let [authors (:authors book)]
  (set authors)))


(defn authors [books]
    (apply clojure.set/union (map get-authors books)))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (if (> (count (str birth-year)) 0) 
      (str author-name " (" birth-year " - " death-year ")") 
      (str author-name))))


(defn authors->string [authors]
  (apply str (interpose ", " (set (map author->string authors)))))

(defn book->string [book]
  (let [book-title (:title book)
        book-authors (:authors book)]
    (str book-title ", written by " (authors->string book-authors))))

(defn books->string [books]
  (let [book-amount (count books)] 
   (if (== book-amount 0) 
    (str "No books.") 
    (str (str book-amount) 
         (if (== book-amount 1) (str " book. ") (str " books. ")) 
         (apply str (interpose ". " (set (map book->string books))))
         "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [authors-by-name 
        (apply clojure.set/union (filter (fn [x] (= name (:name x))) authors))]
    (if (== (count authors-by-name) 0) 
      nil 
      (apply clojure.set/union (filter (fn [x] (= name (:name x))) authors)))
  ))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))


(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true))

(has-a-living-author? wild-seed)



(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%

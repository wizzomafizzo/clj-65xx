(ns clj-65xx.addressing-test
  (:require [clojure.test :refer :all]
            [clj-65xx.addressing :refer :all])
  (:use clj-65xx.cpu))

(def test-cpu
  (-> (load-to-mem cpu 0 [1 2 3 4 5 0 0xff 0xff 0 1 0 2])
      (set-a 1)
      (set-x 2)
      (set-y 3)))

;; TODO: what happens at end of mem?
(deftest test-common
  (testing "post-addr"
    (is (= (post-addr test-cpu) 1))
    (is (= (-> test-cpu (set-pc 1) post-addr) 2)))
  (testing "post-mem"
    (is (= (take 2 (post-mem test-cpu)) '(2 3)))
    (is (= (take 2 (-> test-cpu (set-pc 1) post-mem)) '(3 4))))
  (testing "post-byte"
    (is (= (post-byte test-cpu) 2))
    (is (= (-> test-cpu (set-pc 1) post-byte) 3)))
  (testing "post-short"
    (is (= (post-short test-cpu) 770))
    (is (= (-> test-cpu (set-pc 1) post-short) 1027))))

(deftest test-implicit
  (is (= (read-implicit test-cpu) [nil nil])))

(deftest test-accumulator
  (is (= (read-accumulator test-cpu) [1 nil])))

(deftest test-immediate
  (is (= (read-immediate test-cpu) [2 1]))
  (is (= (-> test-cpu (set-pc 1) read-immediate) [3 2])))

(deftest test-zero-page
  (is (= (read-zero-page test-cpu) [3 2])))

(deftest test-zero-page-x
  (is (= (read-zero-page-x test-cpu) [5 4])))

(deftest test-zero-page-y
  (is (= (read-zero-page-y test-cpu) [0 5])))

(deftest test-relative
  (is (= (read-relative test-cpu) [2 1])))

(deftest test-absolute
  (is (= (read-absolute test-cpu) [0 0x302])))

(deftest test-absolute-x
  (is (= (read-absolute-x test-cpu) [0 0x304])))

(deftest test-absolute-y
  (is (= (read-absolute-y test-cpu) [0 0x305])))

(deftest test-indirect
  (is (= (read-indirect test-cpu) [1 0])))

(deftest test-indexed-indirect
  (is (= (read-indexed-indirect test-cpu) [0 5])))

(deftest test-indirect-indexed
  (is (= (read-indirect-indexed test-cpu) [0 0x406])))

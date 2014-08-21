(ns clj-65xx.cpu-test
  (:require [clojure.test :refer :all]
            [clj-65xx.cpu :refer :all]))

(deftest test-common
  (testing "to-byte"
    (is (= (to-byte 1) 1))
    (is (= (to-byte 0xff) 0xff))
    (is (= (to-byte 256) 0))
    (is (= (to-byte -129) 127)))
  (testing "to-short"
    (is (= (to-short 1) 1))
    (is (= (to-short 0xffff) 0xffff))
    (is (= (to-short 65536) 0))
    (is (= (to-short -32769) 32767)))
  (testing "to-signed-byte"
    (is (= (to-signed-byte 256) 0))
    (is (= (to-signed-byte 127) 127))
    (is (= (to-signed-byte 1) 1))
    (is (= (to-signed-byte 384) -128)))
  (testing "to-le-short"
    (is (= (to-le-short 0xff) [0xff 0]))
    (is (= (to-le-short 0xffff) [0xff 0xff]))
    (is (= (to-le-short 0) [0 0]))
    (is (= (to-le-short 0x200) [0 2])))
  (testing "from-le-short"
    ;; TODO: what if they're over a byte? less than 2 items?
    (is (= (from-le-short [0 0]) 0))
    (is (= (from-le-short [0xff 0xff]) 0xffff))
    (is (= (from-le-short [0 1]) 0x100))
    (is (= (from-le-short [1 0]) 1)))
  (testing "write-mem"
    (is (= (get-in (write-mem cpu 0 0xff) [:memory 0]) 0xff))
    (is (= (get-in (write-mem cpu 0xffff 0xff) [:memory 0xffff]) 0xff))
    (is (= (get-in (write-mem cpu 0x10000 0xff) [:memory 0]) 0xff))
    (is (= (get-in (write-mem cpu 0 256) [:memory 0]) 0)))
  (testing "load-to-mem"
    (is (= (take 5 (:memory (load-to-mem cpu 0 [1 2 3 4 5]))) '(1 2 3 4 5)))
    (is (= (take 5 (:memory (load-to-mem cpu 0xfffe [1 2 3 4 5]))) '(3 4 5 0 0)))
    (is (= (take 2 (:memory (load-to-mem cpu 0 []))) '(0 0)))))

(deftest test-registers
  (testing "set-a"
    (is (= (get-in (set-a cpu 0) [:registers :a]) 0))
    (is (= (get-in (set-a cpu 0xff) [:registers :a]) 0xff))
    (is (= (get-in (set-a cpu 256) [:registers :a]) 0)))
  (testing "set-x"
    (is (= (get-in (set-x cpu 0) [:registers :x]) 0))
    (is (= (get-in (set-x cpu 0xff) [:registers :x]) 0xff))
    (is (= (get-in (set-x cpu 256) [:registers :x]) 0)))
  (testing "set-y"
    (is (= (get-in (set-y cpu 0) [:registers :y]) 0))
    (is (= (get-in (set-y cpu 0xff) [:registers :y]) 0xff))
    (is (= (get-in (set-y cpu 256) [:registers :y]) 0)))
  (testing "set-s"
    (is (= (get-in (set-s cpu 0) [:registers :s]) 0))
    (is (= (get-in (set-s cpu 0xff) [:registers :s]) 0xff))
    (is (= (get-in (set-s cpu 256) [:registers :s]) 0)))
  (testing "set-p"
    (is (= (get-in (set-p cpu 0) [:registers :p]) 2r00100000))
    (is (= (get-in (set-p cpu 0xff) [:registers :p]) 0xff))
    (is (= (get-in (set-p cpu 256) [:registers :p]) 2r00100000)))
  (testing "set-pc"
    (is (= (get-in (set-pc cpu 0) [:registers :pc]) 0))
    (is (= (get-in (set-pc cpu 0xffff) [:registers :pc]) 0xffff))
    (is (= (get-in (set-pc cpu 65536) [:registers :pc]) 0))))

(deftest test-flags
  (testing "zero-flag?"
    (is (zero-flag? 0))
    (is (not (zero-flag? 1))))
  (testing "overflow-flag?"
    (is (overflow-flag? -1))
    (is (overflow-flag? 256))
    (is (not (overflow-flag? 1))))
  (testing "negative-flag?"
    (is (negative-flag? -1))
    (is (not (negative-flag? 1)))
    (is (not (negative-flag? 0)))
    (is (not (negative-flag? -129))))
  (testing "set-flag"
    (let [cpu (set-p cpu 2r00100000)]
      (is (= (get-in (set-flag cpu :carry true) [:registers :p]) 2r00100001))
      (is (= (get-in (set-flag cpu :zero true) [:registers :p]) 2r00100010))
      (is (= (get-in (set-flag cpu :interrupt true) [:registers :p]) 2r00100100))
      (is (= (get-in (set-flag cpu :decimal true) [:registers :p]) 2r00101000))
      (is (= (get-in (set-flag cpu :break true) [:registers :p]) 2r00110000))
      (is (= (get-in (set-flag cpu :overflow true) [:registers :p]) 2r01100000))
      (is (= (get-in (set-flag cpu :negative true) [:registers :p]) 2r10100000))
      (is (= (get-in (set-flag cpu :negative false) [:registers :p]) 2r00100000))))
  (testing "get-flag"
    (is (= (get-flag cpu :negative) false)))
  (testing "set-flag helpers"
    (is (= (get-in (zero-flag cpu 0) [:registers :p]) 2r00100010))
    (is (= (get-in (overflow-flag cpu 256) [:registers :p]) 2r01100000))
    (is (= (get-in (negative-flag cpu -1) [:registers :p]) 2r10100000)))
  (testing "carry-bit"
    (is (= (carry-bit cpu) 0))))

(deftest test-stack
  (testing "abs-stack-addr"
    (is (= (abs-stack-addr cpu) 0x1ff))
    (is (= (abs-stack-addr (set-s cpu 0)) 0x100))
    (is (= (abs-stack-addr (set-s cpu 256)) 0x100))
    (is (= (abs-stack-addr (set-s cpu 0x10)) 0x110)))
  (testing "push-stack-byte"
    (is (= ((:memory (push-stack-byte cpu 1)) 0x1ff) 1))
    (is (= ((:memory (-> (push-stack-byte cpu 1) (push-stack-byte 2))) 0x1fe) 2))
    (is (= ((:registers (-> (push-stack-byte cpu 1) (push-stack-byte 2))) :s) 0xfd)))
  (testing "push-stack-short"
    (is (= ((:memory (push-stack-short cpu 1)) 0x1fe) 1))
    (is (= ((:memory (-> (push-stack-short cpu 1) (push-stack-short 2))) 0x1fc) 2))
    (is (= ((:registers (-> (push-stack-short cpu 1) (push-stack-short 2))) :s) 0xfb)))
  (testing "pull-stack-byte"
    (is (= ((pull-stack-byte (-> (push-stack-byte cpu 1) (push-stack-byte 2))) 1) 2)))
  (testing "pull-stack-short"
    (is (= ((pull-stack-short (-> (push-stack-short cpu 1) (push-stack-short 2))) 1) 2))))

;; TODO: check s on all
;; TODO: combinations
;; TODO: general edge cases, exceptions

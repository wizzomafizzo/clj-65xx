(ns clj-65xx.core
  (:use clj-65xx.cpu
        clj-65xx.addressing
        clj-65xx.instructions
        clj-65xx.reader)
  (:require [clojure.java.io :refer [file input-stream]])
  (:gen-class)) ;; FIXME: do i need this?

;; TODO: change all :use to :require

(def test-app
  [0xa9 0x01 0x8d 0x00 0x02 0xa9 0x05 0x8d 0x01 0x02 0xa9 0x08 0x8d 0x02 0x02])

; (require 'clojure.tools.namespace.repl)
; (clojure.tools.namespace.repl/refresh)

;;;; printing cpu status

(defn pretty-p
  [p]
  (let [binary-str (Integer/toString p 2)
        pad-amount (- 8 (count binary-str))]
    (str (clojure.string/join (repeat pad-amount \0)) binary-str)))

(defn pretty-registers
  [registers]
  (format (str "A: 0x%02x // X: 0x%02x // Y: 0x%02x%n%n"
               "S: 0x%02x // PC: 0x%04x%n%n"
               "   NV-BDIZC%nP: %s")
          (:a registers) (:x registers) (:y registers)
          (:s registers) (:pc registers) (pretty-p (:p registers))))

(defn pprint-memory
  [memory from to]
  (printf "0x%04x->0x%04x:%n" from to)
  (loop [coll (subvec memory from (inc to))]
    (println (reduce (fn ([s] s) ([s m] (str s (format "%02x " m))))
                     "" (take 16 coll)))
    (if (> (count coll) 16)
      (recur (subvec coll 17)))))

(defn pprint-cpu
  [cpu mem-from mem-to]
  (println (pretty-registers (:registers cpu)))
  (println)
  (println "vv")
  (println (reduce (fn ([s] s) ([s m] (str s (format "%02x " m))))
           "" (take 16 (subvec (:memory cpu) (get-in cpu [:registers :pc])))))
  (println "^^")
  (println)
  (pprint-memory (:memory cpu) mem-from mem-to)
  cpu)

(defn dump-cpu
  [cpu]
  (spit "last-crash.log" cpu))

(defn read-rom
  [path]
  (let [f (file path)]
    (with-open [in (input-stream f)]
      (let [buf (byte-array (.length f))
            _ (.read in buf)]
        (into [] buf)))))

(defn step-cpu
  [cpu program pc mem-low mem-high]
  (let [loaded (-> (load-to-mem cpu 10 program) (set-pc pc))]
    (loop [cpu loaded]
      (pprint-cpu cpu mem-low mem-high)
      (let [input "" #_(read-line)]
        (cond ; (get-flag (:registers cpu) :break) (println "BREAK")
              (= input "q") (println "Bye!")
              :else (recur (do-next cpu)))))))

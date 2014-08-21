(ns clj-65xx.cpu)

;; TODO: cycle counter
(def cpu
  {:registers {:a 0                     ; accumulator / 1b
               :x 0                     ; x / 1b
               :y 0                     ; y / 1b
               :p 2r00100000            ; processor status / 1b
               :s 0xff                  ; stack pointer / 1b
               :pc 0}                   ; program counter / 2b
   :memory (into [] (repeat 0x10000 0)) ; 64kb
   :cycles 0})

(def flag-map
  {:carry 0      ; C
   :zero 1       ; Z
   :interrupt 2  ; I
   :decimal 3    ; D
   :break 4      ; B
                 ; - / always set
   :overflow 6   ; V
   :negative 7}) ; NV-BDIZC

(def memory-map
  {:zero-page 0                ; ends 0xff
   :stack 0x100                ; ends 0x1ff
   :interrupt-handler 0xfffe}) ; 2b le address

(defn to-byte
  "Wrap long to 1 byte length."
  [x]
  (bit-and 0xff x))

(defn to-short
  "Wrap long to 2 byte length."
  [x]
  (bit-and 0xffff x))

(defn to-signed-byte
  "Wrap long to 1 byte signed."
  [x]
  (long (unchecked-byte x)))

(defn to-le-short
  "Convert long to little endian short, 2 bytes of memory."
  [x]
  (let [s (to-short x)
        h (bit-shift-right s 8)
        l (bit-and 0xff s)]
    [l h]))

(defn from-le-short
  "Read little endian short from block of memory."
  [mem]
  (bit-or (bit-shift-left (second mem) 8)
          (first mem)))

(defn write-mem
  "Write single byte to address in memory."
  [cpu address x]
  (assoc-in cpu [:memory (to-short address)] (to-byte x)))

(defn load-to-mem
  "Write vector of bytes to address in memory."
  [cpu address [x & xs]]
  (if x
    (recur (write-mem cpu address x) (inc address) xs)
    cpu))

;;;; registers

(defn set-a
  "Set accumulator, wraps byte."
  [cpu n]
  (assoc-in cpu [:registers :a] (to-byte n)))

(defn set-x
  "Set x register, wraps byte."
  [cpu n]
  (assoc-in cpu [:registers :x] (to-byte n)))

(defn set-y
  "Set y register, wraps byte."
  [cpu n]
  (assoc-in cpu [:registers :y] (to-byte n)))

(defn set-s
  "Set stack pointer, wraps byte."
  [cpu n]
  (assoc-in cpu [:registers :s] (to-byte n)))

(defn set-p
  "Set processor status, wraps byte. Bit 5 always set."
  [cpu n]
  (assoc-in cpu [:registers :p] (bit-set (to-byte n) 5)))

(defn set-pc
  "Set program counter, wraps short."
  [cpu n]
  (assoc-in cpu [:registers :pc] (to-short n)))

;;; processor status flags

(defn overflow?
  [x]
  (not (<= 0 x 0xff)))

(defn negative?
  [x]
  (bit-test x 7))

(defn set-flag
  [{r :registers :as cpu} flag status]
  (let [bit-f (if status bit-set bit-clear)]
    (set-p cpu (bit-f (:p r) (flag flag-map)))))

(defn get-flag
  [{r :registers :as cpu} flag]
  (bit-test (:p r) (flag flag-map)))

(defn carry-bit
  [cpu]
  (if (get-flag cpu :carry) 1 0))

(defn zero-flag
  [cpu n]
  (set-flag cpu :zero (zero? n)))

(defn overflow-flag
  [cpu n]
  (set-flag cpu :overflow (overflow? n)))

(defn negative-flag
  [cpu n]
  (set-flag cpu :negative (negative? n)))

;;;; stack

(defn abs-stack-addr
  [{r :registers :as cpu}]
  (+ (:stack memory-map) (:s r)))

(defn push-stack-byte
  [{r :registers :as cpu} n]
  (-> (set-s cpu (dec (:s r)))
      (write-mem (abs-stack-addr cpu) n)))

(defn push-stack-short
  [{r :registers :as cpu} n]
  (-> (set-s cpu (- (:s r) 2))
      (load-to-mem (dec (abs-stack-addr cpu))
                   (to-le-short n))))

(defn pull-stack-byte
  [{r :registers m :memory :as cpu}]
  (let [stack-addr (inc (abs-stack-addr cpu))]
    [(-> (set-s cpu (inc (:s r)))
         (write-mem stack-addr 0))
     (m stack-addr)]))

(defn pull-stack-short
  [{r :registers m :memory :as cpu}]
  (let [stack-addr (inc (abs-stack-addr cpu))]
    [(-> (set-s cpu (+ (:s r) 2))
         (load-to-mem stack-addr [0 0]))
     (from-le-short (subvec m stack-addr))]))

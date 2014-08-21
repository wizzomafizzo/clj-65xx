(ns clj-65xx.addressing
  (:use clj-65xx.cpu))

;; all these functions work under the assumption that the pc register is
;; pointing to the *current instruction* and is offset from there as appropriate
;;
;; each addressing function returns a vector containing the value of the
;; retrieved address and, if possible, the memory address of that value

(defn post-addr
  "Return address to first byte after the current instruction."
  [{r :registers :as cpu}]
  (to-short (inc (:pc r))))

(defn post-mem
  "Return bytes after the current instruction."
  [{m :memory :as cpu}]
  (subvec m (post-addr cpu)))

(defn post-byte
  "Return byte argument to current instruction."
  [cpu]
  (first (post-mem cpu)))

(defn post-short
  "Return short argument to current instruction."
  [cpu]
  (from-le-short (post-mem cpu)))

;; TODO: cycle count support

;; no argument required
(defn read-implicit
  [cpu]
  [nil nil])

;; accumulator used as value, no memory address
;; functions writing to the accumulator need to handle this specially, but
;; there are only a few cases of this
(defn read-accumulator
  [cpu]
  [(get-in cpu [:registers :a]) nil])

;; return the byte immediately after the instruction and its address
(defn read-immediate
  [cpu]
  [(post-byte cpu) (post-addr cpu)])

;; first byte contains a zero page address, this address is looked up as the
;; value (and returned address)
(defn read-zero-page
  [{m :memory :as cpu}]
  (let [addr (post-byte cpu)]
    [(m addr) addr]))

(defn- read-zero-page--
  [reg-key {r :registers m :memory :as cpu}]
  (let [address (to-byte (+ (post-byte cpu) (r reg-key)))]
    [(m address) address]))

;; same as zero-page, but the x register is added to the lookup address before
;; the value is read
(defn read-zero-page-x
  [cpu]
  (read-zero-page-- :x cpu))

;; same as above, but using the y register
(defn read-zero-page-y
  [cpu]
  (read-zero-page-- :y cpu))

;; first byte argument is read as a signed byte (so it can be negative)
;; this is used mainly by branching instruction which can jump forwards and
;; backwards
;;
;; NOTE: during(after?) execution the pc is incremented by 2 affecting the
;; final location a jump will go to
;; TODO: is this set up correct re: ordering of pc update??
(defn read-relative
  [cpu]
  [(to-signed-byte (post-byte cpu)) (post-addr cpu)])

;; short argument is read, this address is used as the final lookup for value
;; and address
(defn read-absolute
  [{m :memory :as cpu}]
  (let [address (post-short cpu)]
    [(m address) address]))

(defn- read-absolute--
  [reg-key {r :registers m :memory :as cpu}]
  (let [address (to-short (+ (post-short cpu) (r reg-key)))]
    [(m address) address]))

;; same as absolute, but the x register is added to the final address before
;; lookup
(defn read-absolute-x
  [cpu]
  (read-absolute-- :x cpu))

;; same as above, but using the y register
(defn read-absolute-y
  [cpu]
  (read-absolute-- :y cpu))

;; read the short argument, lookup that address and use the short at that
;; address as the final value. only jmp uses this
(defn read-indirect
  [{m :memory :as cpu}]
  (let [initial-addr (post-short cpu)
        final-addr (from-le-short (subvec m initial-addr))]
    [(m final-addr) final-addr]))

;; a zero page address is created using the first byte argument plus the x
;; register. a short at that address is then lookup up, and the value at that
;; address is used as the final value
(defn read-indexed-indirect
  [{r :registers m :memory :as cpu}]
  (let [initial-addr (to-byte (+ (post-byte cpu) (r :x)))
        final-addr (from-le-short (subvec m initial-addr))]
    [(m final-addr) final-addr]))

;; a zero page address is looked up using the first byte argument, from that a
;; short is read. the y register is added to that short, and then that address
;; is looked up as the final value
(defn read-indirect-indexed
  [{r :registers m :memory :as cpu}]
  (let [initial-addr (from-le-short (subvec m (post-byte cpu)))
        final-addr (to-short (+ initial-addr (r :y)))]
    [(m final-addr) final-addr]))

;; mode-type => [reader-function byte-len]
;; byte-len is used to increment the pc correctly
(def address-modes
  {:implicit [read-implicit 0]                   ; no arg
   :accumulator [read-accumulator 0]             ; no arg
   :immediate [read-immediate 1]                 ; #10
   :zero-page [read-zero-page 1]                 ; $00
   :zero-page-x [read-zero-page-x 1]             ; $00,X
   :zero-page-y [read-zero-page-y 1]             ; $00,Y
   :relative [read-relative 1]                   ; $00
   :absolute [read-absolute 2]                   ; $0000
   :absolute-x [read-absolute-x 2]               ; $0000,X
   :absolute-y [read-zero-page-y 2]              ; $0000,Y
   :indirect [read-indirect 2]                   ; ($0000)
   :indexed-indirect [read-indexed-indirect 1]   ; ($00,X)
   :indirect-indexed [read-indirect-indexed 1]}) ; ($00),Y

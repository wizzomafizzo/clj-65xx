(ns clj-65xx.instructions
  (:use clj-65xx.cpu)) ;; TODO: use require

(defmacro defop
  [name & body]
  (let [name (symbol (str "op-" name))
        args [{'r :registers 'm :memory :as 'cpu}
              'addr-mode
              ['n 'n-addr]]]
    `(defn ~name ~args ~@body)))

;; load
(defn- op-ld-
  [set-f cpu n]
  (-> (zero-flag cpu n)
      (negative-flag n)
      (set-f n)))

(defop lda (op-ld- set-a cpu n))
(defop ldx (op-ld- set-x cpu n))
(defop ldy (op-ld- set-y cpu n))

;;; store
(defn- op-st-
  [reg-key {r :registers :as cpu} address]
  (write-mem cpu address (reg-key r)))

(defop sta (op-st- :a cpu n-addr))
(defop stx (op-st- :x cpu n-addr))
(defop sty (op-st- :y cpu n-addr))

;;; transfer
(defn- op-t--
  [from-reg-key to-set-f {r :registers :as cpu}]
  (let [from (from-reg-key r)]
    (-> (zero-flag cpu from)
        (negative-flag from)
        (to-set-f from))))

(defop tax (op-t-- :a set-x cpu))
(defop tay (op-t-- :a set-y cpu))
(defop txa (op-t-- :x set-a cpu))
(defop tya (op-t-- :y set-a cpu))

;; transfer stack pointer
(defop tsx (op-t-- :s set-x cpu))
(defop txs (set-s cpu (:x r)))

;;; push stack
(defn- op-ph-
  [reg-key {r :registers :as cpu}]
  (push-stack-byte cpu (reg-key r)))

(defop pha (op-ph- :a cpu))
(defop php (op-ph- :p cpu))

;;; pull stack
(defop pla
  (let [[cpu n] (pull-stack-byte cpu)]
    (-> (negative-flag cpu n)
        (zero-flag n)
        (set-a n))))

(defop plp (apply set-p (pull-stack-byte cpu)))

;;; logical
(defn- op-logic-
  [logic-f {r :registers :as cpu} n]
  (let [result (logic-f (:a r) n)]
    (-> (zero-flag cpu result)
        (negative-flag result))))

(defop and (op-logic- bit-and cpu n))
(defop ora (op-logic- bit-or cpu n))
(defop eor (op-logic- bit-xor cpu n))

(defop bit
  (-> (zero-flag cpu (bit-and (:a r) n))
      (negative-flag n)
      (set-flag :overflow (bit-test n 6))))

;;; arithmetic
(defop adc
  (let [result (+ (:a r) n (carry-bit cpu))]
    (-> (set-a cpu result)
        (set-flag :carry (> result 0xff))
        (negative-flag result)
        (zero-flag result)
        (overflow-flag result)))) ;; TODO: check this

(defop sbc
  (let [result (- (:a r) n (- 1 (carry-bit cpu)))]
    (-> (set-a cpu result)
        (set-flag :carry (not (< result 0)))
        (negative-flag result)
        (zero-flag result)
        (overflow-flag result))))

;;; inc/dec
(defn- op-inc-or-dec-
  [inc-or-dec {r :registers m :memory :as cpu} addr]
  (let [result (inc-or-dec (m addr))]
    (-> (negative-flag cpu result)
        (zero-flag result)
        (write-mem addr result))))

(defn- op-inc-or-dec-reg--
  [inc-or-dec reg-key set-f {r :registers :as cpu}]
  (let [result (inc-or-dec (reg-key r))]
    (-> (zero-flag cpu result)
        (negative-flag result)
        (set-f result))))

(defop inc (op-inc-or-dec- inc cpu n-addr))
(defop inx (op-inc-or-dec-reg-- inc :x set-x cpu))
(defop iny (op-inc-or-dec-reg-- inc :y set-y cpu))

(defop dec (op-inc-or-dec- dec cpu n-addr))
(defop dex (op-inc-or-dec-reg-- dec :x set-x cpu))
(defop dey (op-inc-or-dec-reg-- dec :y set-y cpu))

;;; compare
(defn- op-comp-
  [reg-key {r :registers :as cpu} n]
  (-> (set-flag cpu :carry (<= (reg-key r) n))
      (set-flag :zero (= (reg-key r) n))
      (set-flag :negative (> (reg-key r) n))))

(defop cmp (op-comp- :a cpu n))
(defop cpx (op-comp- :x cpu n))
(defop cpy (op-comp- :y cpu n))

;;; shifts
(defn- op-shift-
  [carry-bit shift-f cpu addr-mode [n n-addr]]
  (let [carry (bit-test n carry-bit)
        shift (to-byte (shift-f n 1))
        cpu (-> (set-flag cpu :carry carry)
                (zero-flag shift)
                (negative-flag shift))]
    (if (= addr-mode :accumulator)
      (set-a cpu shift)
      (write-mem cpu n-addr shift))))

(defop asl (op-shift- 7 bit-shift-left cpu addr-mode [n n-addr]))
(defop lsr (op-shift- 0 bit-shift-right cpu addr-mode [n n-addr]))

(defn set-bit
  [x n status]
  (if status
    (bit-set x n)
    (bit-clear x n)))

(defn- op-rotate-
  [in-bit out-bit shift-f cpu addr-mode [n n-addr]]
  (let [rotate-in (get-flag cpu :carry)
        rotate-out (bit-test n out-bit)
        shift (set-bit (to-byte (shift-f n 1)) in-bit rotate-in)
        cpu (-> (set-flag cpu :carry rotate-out)
                (zero-flag shift)
                (negative-flag shift))]
    (if (= addr-mode :accumulator)
      (set-a cpu shift)
      (write-mem cpu n-addr shift))))

(defop rol (op-rotate- 0 7 bit-shift-left cpu addr-mode [n n-addr]))
(defop ror (op-rotate- 7 0 bit-shift-right cpu addr-mode [n n-addr]))

(defop jmp (set-pc cpu n-addr))

(defop jsr
  (-> (push-stack-short cpu (:pc r))
      (set-pc n-addr)))

;; TODO: check var name in other pull stacks, no n???
(defop rts
  (apply set-pc (pull-stack-short cpu)))

(defn- op-branch-
  [{r :registers :as cpu} flag status n]
  (if (= (get-flag cpu flag) status)
    (set-pc cpu (to-short (+ (:pc r) n)))
    cpu))

(defop bcc (op-branch- cpu :carry false n))
(defop bcs (op-branch- cpu :carry true n))
(defop beq (op-branch- cpu :zero true n))
(defop bne (op-branch- cpu :zero false n))
(defop bmi (op-branch- cpu :negative true n))
(defop bpl (op-branch- cpu :negative false n))
(defop bvc (op-branch- cpu :overflow false n))
(defop bvs (op-branch- cpu :overflow true n))

;;; set/clear

(defop clc (set-flag cpu :carry false))
(defop cld (set-flag cpu :decimal false))
(defop cli (set-flag cpu :interrupt false))
(defop clv (set-flag cpu :overflow false))

(defop sec (set-flag cpu :carry true))
(defop sed (set-flag cpu :decimal true))
(defop sei (set-flag cpu :interrupt true))

(defop brk
  (let [ih-loc (subvec m (:interrupt-handler memory-map))
        ih-addr (from-le-short ih-loc)]
    (-> (set-flag cpu :break true)
        (set-pc ih-addr)
        (push-stack-byte (:p r))
        (push-stack-short (:pc r)))))

(defop rti
  (let [[pc-cpu pc] (pull-stack-short cpu)
        [p-cpu p] (pull-stack-byte pc-cpu)]
    (-> (set-p p-cpu p)
        (set-pc pc))))

(defop nop cpu)

;; op-byte => [op-function address-mode]
(def opcode-map
  {0x00 [op-brk :implicit]
   0x01 [op-ora :indexed-indirect]
   0x05 [op-ora :zero-page]
   0x06 [op-asl :zero-page]
   0x08 [op-php :implicit]
   0x09 [op-ora :immediate]
   0x0a [op-asl :accumulator]
   0x0d [op-ora :absolute]
   0x0e [op-asl :absolute]
   0x10 [op-bpl :relative]
   0x11 [op-ora :indirect-indexed]
   0x15 [op-ora :zero-page-x]
   0x16 [op-asl :zero-page-x]
   0x18 [op-clc :implicit]
   0x19 [op-ora :absolute-y]
   0x1d [op-ora :absolute-x]
   0x1e [op-asl :absolute-x]
   0x20 [op-jsr :absolute]
   0x21 [op-and :indexed-indirect]
   0x24 [op-bit :zero-page]
   0x25 [op-and :zero-page]
   0x26 [op-rol :zero-page]
   0x28 [op-plp :implicit]
   0x29 [op-and :immediate]
   0x2a [op-rol :accumulator]
   0x2c [op-bit :absolute]
   0x2d [op-and :absolute]
   0x2e [op-rol :absolute]
   0x30 [op-bmi :relative]
   0x31 [op-and :indirect-indexed]
   0x35 [op-and :zero-page-x]
   0x36 [op-rol :zero-page-x]
   0x38 [op-sec :implicit]
   0x39 [op-and :absolute-y]
   0x3d [op-and :absolute-x]
   0x3e [op-rol :absolute-x]
   0x40 [op-rti :implicit]
   0x41 [op-eor :indexed-indirect]
   0x45 [op-eor :zero-page]
   0x46 [op-lsr :zero-page]
   0x48 [op-pha :implicit]
   0x49 [op-eor :immediate]
   0x4a [op-lsr :accumulator]
   0x4c [op-jmp :absolute]
   0x4d [op-eor :absolute]
   0x4e [op-lsr :absolute]
   0x50 [op-bvc :relative]
   0x51 [op-eor :indirect-indexed]
   0x55 [op-eor :zero-page-x]
   0x56 [op-lsr :zero-page-x]
   0x58 [op-cli :implicit]
   0x59 [op-eor :absolute-y]
   0x5d [op-eor :absolute-x]
   0x5e [op-lsr :absolute-x]
   0x60 [op-rts :implicit]
   0x61 [op-adc :indexed-indirect]
   0x65 [op-adc :zero-page]
   0x66 [op-ror :zero-page]
   0x68 [op-pla :implicit]
   0x69 [op-adc :immediate]
   0x6a [op-ror :accumulator]
   0x6c [op-jmp :indirect]
   0x6d [op-adc :absolute]
   0x6e [op-ror :absolute]
   0x70 [op-bvs :relative]
   0x71 [op-adc :indirect-indexed]
   0x75 [op-adc :zero-page-x]
   0x76 [op-ror :zero-page-x]
   0x78 [op-sei :implicit]
   0x79 [op-adc :absolute-y]
   0x7d [op-adc :absolute-x]
   0x7e [op-ror :absolute-x]
   0x81 [op-sta :indexed-indirect]
   0x84 [op-sty :zero-page]
   0x85 [op-sta :zero-page]
   0x86 [op-stx :zero-page]
   0x88 [op-dey :implicit]
   0x8a [op-txa :implicit]
   0x8c [op-sty :absolute]
   0x8d [op-sta :absolute]
   0x8e [op-stx :absolute]
   0x90 [op-bcc :relative]
   0x91 [op-sta :indirect-indexed]
   0x94 [op-sty :zero-page-x]
   0x95 [op-sta :zero-page-x]
   0x96 [op-stx :zero-page-y]
   0x98 [op-tya :implicit]
   0x99 [op-sta :absolute-y]
   0x9a [op-txs :implicit]
   0x9d [op-sta :absolute-x]
   0xa0 [op-ldy :immediate]
   0xa1 [op-lda :indexed-indirect]
   0xa2 [op-ldx :immediate]
   0xa4 [op-ldy :zero-page]
   0xa5 [op-lda :zero-page]
   0xa6 [op-ldx :zero-page]
   0xa8 [op-tay :implicit]
   0xa9 [op-lda :immediate]
   0xaa [op-tax :implicit]
   0xac [op-ldy :absolute]
   0xad [op-lda :absolute]
   0xae [op-ldx :absolute]
   0xb0 [op-bcs :relative]
   0xb1 [op-lda :indirect-indexed]
   0xb4 [op-ldy :zero-page-x]
   0xb5 [op-lda :zero-page-x]
   0xb6 [op-ldx :zero-page-y]
   0xb8 [op-clv :implicit]
   0xb9 [op-lda :absolute-y]
   0xba [op-tsx :implicit]
   0xbc [op-ldy :absolute-x]
   0xbd [op-lda :absolute-x]
   0xbe [op-ldx :absolute-y]
   0xc0 [op-cpy :immediate]
   0xc1 [op-cmp :indexed-indirect]
   0xc4 [op-cpy :zero-page]
   0xc5 [op-cmp :zero-page]
   0xc6 [op-dec :zero-page]
   0xc8 [op-iny :implicit]
   0xc9 [op-cmp :immediate]
   0xca [op-dex :implicit]
   0xcc [op-cpy :absolute]
   0xcd [op-cmp :absolute]
   0xce [op-dec :absolute]
   0xd0 [op-bne :relative]
   0xd1 [op-cmp :indirect-indexed]
   0xd5 [op-cmp :zero-page-x]
   0xd6 [op-dec :zero-page-x]
   0xd8 [op-cld :implicit]
   0xd9 [op-cmp :absolute-y]
   0xdd [op-cmp :absolute-x]
   0xde [op-dec :absolute-x]
   0xe0 [op-cpx :immediate]
   0xe1 [op-sbc :indexed-indirect]
   0xe4 [op-cpx :zero-page]
   0xe5 [op-sbc :zero-page]
   0xe6 [op-inc :zero-page]
   0xe8 [op-inx :implicit]
   0xe9 [op-sbc :immediate]
   0xea [op-nop :implicit]
   0xec [op-cpx :absolute]
   0xed [op-sbc :absolute]
   0xee [op-inc :absolute]
   0xf0 [op-beq :relative]
   0xf1 [op-sbc :indirect-indexed]
   0xf5 [op-sbc :zero-page-x]
   0xf6 [op-inc :zero-page-x]
   0xf8 [op-sed :implicit]
   0xf9 [op-sbc :absolute-y]
   0xfd [op-sbc :absolute-x]
   0xfe [op-inc :absolute-x]})

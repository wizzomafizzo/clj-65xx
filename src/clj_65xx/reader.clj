(ns clj-65xx.reader
  (:use clj-65xx.cpu
        clj-65xx.addressing
        clj-65xx.instructions))

(defn print-current-op
  [pc op op-f addr-mode am-len args]
  (println (format ">>> 0x%04x:" pc)
           (str op-f) addr-mode (str am-len "b")
           (format "[Val: %d, Addr: 0x%04x]" (args 0) (args 1)))
  (println))

(defn do-next
  [{r :registers m :memory :as cpu}]
  (let [op (m (:pc r))
        [op-f addr-mode] (opcode-map op)]
    (if op-f
      (let [[am-f am-len] (address-modes addr-mode)
            addr-result (am-f cpu)
            next-cpu (-> (op-f cpu addr-mode addr-result)
                         (set-pc (+ (:pc r) 1 am-len)))]
        (print-current-op (:pc r) op op-f addr-mode am-len addr-result)
        next-cpu)
      (throw (Exception. (str "Opcode " op " has no mapping."))))))

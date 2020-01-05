(ns multihash.varint
  (:require
    [alphabase.bytes :as bytes])
  #?(:clj
     (:import (java.nio ByteBuffer)
              (java.io ByteArrayInputStream InputStream))))


(defn varint-size [x]
  (loop [result 1
         i      x]
    (let [next-x (bit-shift-right i 7)]
      (if (= 0 next-x)
        result
        (recur (+ 1 result) next-x)))))

(defn byte-array->varint
  ([^bytes src]
   (byte-array->varint src 0))
  ([^bytes src start-offset]
   (let [src-length (alength src)]
     (loop [offset start-offset
            result 0
            shift  0]
       (when (>= offset src-length)
         (throw #?(:clj  (IndexOutOfBoundsException. "invalid varint array")
                   :cljs (js/Error. "invalid varint array"))))
       (when (>= shift 32)
         (throw #?(:clj  (IndexOutOfBoundsException. "varint too long")
                   :cljs (js/Error. "varint too long"))))

       (let [bits            (aget src offset)
             has-leading-bit (not (= 0 (bit-and 0x80 bits)))
             next-result     (bit-or result (bit-shift-left (bit-and 0x7f bits) shift))]
         (if has-leading-bit
           (recur (+ 1 offset) next-result (+ 7 shift))
           next-result))))))

#?(:clj
   (defn input-stream->varint
     [^InputStream src]
     (loop [result 0
            shift  0]
       (when (>= shift 32)
         (throw #?(:clj  (IndexOutOfBoundsException. "varint too long")
                   :cljs (js/Error. "varint too long"))))

       (let [bits            (.read src)
             has-leading-bit (not (= 0 (bit-and 0x80 bits)))
             next-result     (bit-or result (bit-shift-left (bit-and 0x7f bits) shift))]
         (if has-leading-bit
           (recur next-result (+ 7 shift))
           next-result)))))

(defn put-varint
  ([value ^bytes dst]
   (put-varint value dst 0))
  ([value ^bytes dst start-offset]
   (loop [result value
          offset start-offset]
     (let [bits        (bit-and result 0x7f)
           next-value  (bit-shift-right result 7)
           next-byte   (+ bits (if (= 0 next-value) 0 0x80))
           next-offset (+ 1 offset)]
       (bytes/set-byte dst offset next-byte)
       (if (= 0 next-value)
         next-offset
         (recur next-value next-offset))))))

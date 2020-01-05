(ns multihash.varint-test
  (:require
    #?(:clj  [clojure.test :refer :all]
       :cljs [cljs.test :refer-macros [deftest is testing]])
    [alphabase.bytes :as bytes]
    [multihash.core]
    [multihash.varint :as varint])
  #?(:clj (:import (java.io ByteArrayInputStream))))

(defn to-byte-array
  [arr]
  #?(:clj  (bytes/byte-array arr)
     :cljs (let [src    (to-array arr)
                 length (alength src)
                 dst    (bytes/byte-array length)]
             #?(:clj  (bytes/copy src 0 dst 0 length)
                :cljs (dotimes [i length] (aset dst i (aget src i))))
             dst)))

(deftest varint-size-test
  (is (= 1 (varint/varint-size 0)))
  (is (= 1 (varint/varint-size 1)))
  (is (= 1 (varint/varint-size 127)))
  (is (= 2 (varint/varint-size 128)))
  (is (= 2 (varint/varint-size 256)))
  (is (= 2 (varint/varint-size 16383)))
  (is (= 3 (varint/varint-size 16384)))
  (is (= 4 (varint/varint-size 2097152)))
  (is (= 5 (varint/varint-size 268435456)))
  (is (= 5 (varint/varint-size 0x7fffffff))))

(deftest byte-array->varint-test
  (is (= 1 (varint/byte-array->varint (to-byte-array [1]))))
  (is (= 1 (varint/byte-array->varint (to-byte-array [1 2]))))
  (is (= 127 (varint/byte-array->varint (to-byte-array [127]))))
  (is (= 128 (varint/byte-array->varint (to-byte-array [128 1]))))
  (is (= 0 (varint/byte-array->varint (to-byte-array [128 128 0]))))
  (is (= 2080768 (varint/byte-array->varint (to-byte-array [128 128 127]))))
  (is (= 266338304) (varint/byte-array->varint (to-byte-array [128 128 128 127]))))

#?(:clj
   (deftest input-stream->varint-test
     (is (= 1 (varint/input-stream->varint (ByteArrayInputStream. (bytes/byte-array [1])))))
     (is (= 1 (varint/input-stream->varint (ByteArrayInputStream. (bytes/byte-array [1 2])))))
     (is (= 127 (varint/input-stream->varint (ByteArrayInputStream. (bytes/byte-array [127])))))
     (is (= 128 (varint/input-stream->varint (ByteArrayInputStream. (bytes/byte-array [128 1])))))
     (is (= 0 (varint/input-stream->varint (ByteArrayInputStream. (bytes/byte-array [128 128 0])))))
     (is (= 2080768 (varint/input-stream->varint (ByteArrayInputStream. (bytes/byte-array [128 128 127])))))
     (is (= 266338304 (varint/input-stream->varint (ByteArrayInputStream. (bytes/byte-array [128 128 128 127])))))
     (let [stream (ByteArrayInputStream. (bytes/byte-array [1]))]
       (is (= 1 (varint/input-stream->varint stream)))
       (is (thrown-with-msg? IndexOutOfBoundsException #"^varint too long$"
             (varint/input-stream->varint stream))))))

(defn- bar->seq [array] (map #(aget array %) (range (alength array))))

(deftest put-varint-test
  (let [value  3
        bar    (bytes/byte-array 1)
        offset (varint/put-varint value bar)]
    (is (= 1 offset))
    (is (= [value] (bytes/byte-seq bar))))
  (let [value  256
        bar    (bytes/byte-array 2)
        offset (varint/put-varint value bar)]
    (is (= 2 offset))
    ;TODO in clj the (vec bar) returns [-128 2], but there is no vec in cljs. I tried using bytes/byte-seq, but it returns (128 2) so the `-` sign is getting lost
    (is (= (bar->seq (to-byte-array [-128 2])) (bar->seq bar))))
  ;TODO can we have different behavior between clj and cljs?
  #?(:clj
     (is (thrown-with-msg? ArrayIndexOutOfBoundsException #"^1$"
           (varint/put-varint 256 (bytes/byte-array 1))))))

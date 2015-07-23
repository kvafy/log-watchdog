(ns log-watchdog.util-test
  (:require [clojure.test :refer :all]
            [log-watchdog.util :as util]))

(deftest merge-recursive-test
  (testing "empty merges"
    (is (= {}
           (util/merge-recursive {})))
    (is (= {}
           (util/merge-recursive {} {})))
    (is (= {}
           (util/merge-recursive {} {} {})))
    (is (= {}
           (util/merge-recursive {} nil)))
    (is (= {}
           (util/merge-recursive {} nil nil)))
    (is (= {:a "a"}
           (util/merge-recursive {:a "a"} {})))
    (is (= {:a "a"}
           (util/merge-recursive {:a "a"} {} {})))
    (is (= {:a "a"}
           (util/merge-recursive {:a "a"} nil)))
    (is (= {:a "a"}
           (util/merge-recursive {:a "a"} nil nil))))
  (testing "one-level merge"
    (is (= {:a "a" :b "b" :c "c"}
           (util/merge-recursive {:a "a"} {:b "b"} {:c "c"})))
    (is (= {:a "new-a" :b "new-b"}
           (util/merge-recursive {:a "old-a" :b "old-b"} {:a "new-a" :b "new-b"})))
    (is (= {:a "new-a" :b "new-b"}
           (util/merge-recursive {:a "old-a" :b "old-b"} {:a "new-a"} {:b "new-b"}))))
  (testing "two-level merge"
    (is (= {:root {:a "a" :b "b"}}
           (util/merge-recursive {:root nil} {:root {:a "a"}} {:root {:b "b"}})))
    (is (= {:root {:a "new-a" :b "new-b"}}
           (util/merge-recursive {:root {:a "a" :b "b"}} {:root {:a "new-a"}} {:root {:b "new-b"}}))))
  (testing "mixed-level merge"
    (is (= {:flat-value "flat-value" :root {:a "a" :b "b"}}
           (util/merge-recursive {} {:flat-value "xxx"} {:flat-value "flat-value"} {:root nil} {:root {:a "a"}} {:root {:b "b"}})))))

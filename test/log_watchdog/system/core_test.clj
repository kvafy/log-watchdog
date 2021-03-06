(ns log-watchdog.system.core-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [log-watchdog.system.core :as core]
            [log-watchdog.config :as config]
            [log-watchdog.validators :as validators]
            [log-watchdog.utils :as utils]
            [log-watchdog.config-test :as config-test]))


;; definition of a testing system

(def config-id (utils/uuid))
(def file1-id (utils/uuid))
(def file2-id (utils/uuid))
(def file-group1-id (utils/uuid))
(def file-group-default-id (utils/uuid))
(def alert1-id (utils/uuid))
(def alert2-id (utils/uuid))
(def alert3-id (utils/uuid))
(def alert4-id (utils/uuid))
(def notifications-id (utils/uuid))

(def system-orig
  { config-id
      { :type :configuration
        :check-enabled true
        :check-interval-ms 5000
        :nagging-interval-ms 60000}

    file1-id
      { :type :watched-file
        :file (java.io.File. "file-path-1")
        :line-regex #".*"
        :last-check-failed false
        :file-last-size-b nil
        :file-last-modified-ms nil
        :watched-file-group-id file-group1-id
        :always-check-override false
        :never-seek-override false}
    alert1-id
      { :type :alert
        :matched-line "line-1"
        :acknowledged false
        :watched-file-id file1-id}
    alert2-id
      { :type :alert
        :matched-line "line-2"
        :acknowledged true
        :watched-file-id file1-id}

    file2-id
      { :type :watched-file
        :file (java.io.File. "file-path-2")
        :line-regex #".*"
        :last-check-failed true
        :file-last-size-b nil
        :file-last-modified-ms nil
        :watched-file-group-id file-group-default-id
        :always-check-override false
        :never-seek-override false}
    alert3-id
      { :type :alert
        :matched-line "line-3"
        :acknowledged true
        :watched-file-id file2-id}
    alert4-id
      { :type :alert
        :matched-line "line-4"
        :acknowledged false
        :watched-file-id file2-id}

    file-group1-id
      { :type :watched-file-group
        :name "group1"}
    file-group-default-id
      { :type :watched-file-group
        :name config/default-watched-file-group-name}

    notifications-id
      { :type :notifications
        :last-notification-timestamp 0N}
  })


(deftest system-validator-test
  (testing "valid system is accepted by the validator (failure means either the system is invalid or the validator is invalid)"
    (validators/validate-system system-orig)))


(deftest create-entity-test
  (testing "Minimalistic entity has type only"
    (let [min-entity (core/create-entity :my-type)]
      (is (vector? min-entity))
      (is (= 2 (count min-entity)))
      (let [[id data] min-entity]
        (is (map? data))
        (is (= 1 (count data)))
        (is (= :my-type (:type data))))))
  (testing "Entity with other properties"
    (let [entity (core/create-entity :rich-entity
                                     :str-property "string"
                                     :int-property 42)]
      (is (vector? entity))
      (is (= 2 (count entity)))
      (let [[id data] entity]
        (is (map? data))
        (is (= 3 (count data)))
        (is (= :rich-entity (:type data)))
        (is (= "string" (:str-property data)))
        (is (= 42 (:int-property data)))))))


(deftest create-system-test
  (let [created-system (core/create-system config-test/valid-configuration)]
    (testing "creating a system based on valid configuration yields a structurally valid system"
      (validators/validate-system created-system))))


(deftest query-by-id-test
  (testing "Existing entity is found"
    (let [entity (core/query-by-id system-orig config-id)]
      (is (not (nil? entity)))
      (is (= config-id (first entity)))))
  (testing "Non-existing entity is not found"
    (is (nil? (core/query-by-id system-orig "IDs are not strings")))))


(deftest query-test
  (testing "Impossible predicate results in no entity being found."
    (let [result (core/query system-orig (core/entity-pred :type (partial = :alert)
                                                           :type (partial = :watched-file)))]
      (is (seq? result))
      (is (empty? result))))
  (testing "Empty predicate matches all entities (query yields the whole system)."
    (let [all-entities (core/query system-orig (core/entity-pred))]
      (is (seq? all-entities))
      (is (= system-orig
             (apply merge (map (fn [[e-id e-data]] {e-id e-data}) all-entities))))))
  (testing "Fetch files"
    (let [files (core/query system-orig (core/entity-pred :type (partial = :watched-file)))]
      (is (seq? files))
      (let [file-ids (map first files)]
        (is (= #{file1-id file2-id} (set file-ids))))))
  (testing "Fetch alerts"
    (let [alerts (core/query system-orig (core/entity-pred :type (partial = :alert)))]
      (is (seq? alerts))
      (let [alert-ids (map first alerts)]
        (is (= #{alert1-id alert2-id alert3-id alert4-id} (set alert-ids)))))))


(deftest query-singleton-test
  (testing "Impossible predicate results in no entity being found."
    (is (nil? (core/query-singleton system-orig (core/entity-pred :type (partial = :undefined-entity-type))))))
  (testing "Existing singleton entity is found"
    (let [entity (core/query-singleton system-orig (core/entity-pred :type (partial = :configuration)))]
      (is (not (nil? entity)))
      (is (= config-id (first entity)))))
  (testing "Ambiguous entity predicate yields nil."
    (let [entity (core/query-singleton system-orig (core/entity-pred :type (partial = :alert)))]
      (is (nil? entity)))))


(deftest add-entity-test
  (testing "Adding entity to an empty system"
    (let [system-zero {}
          entity (core/create-entity :first-entity)
          system-one (core/add-entity system-zero entity)]
      (is (map? system-one))
      (is (= 1 (count system-one)))))
  (testing "Update semantics of add, if entity already exists"
    (let [entity-id alert1-id
          [_ entity-data-orig] (core/query-by-id system-orig entity-id)
          entity-data-new (assoc entity-data-orig :matched-line "this property has changed")
          system-new (core/add-entity system-orig [entity-id entity-data-new])]
      ; number of entities stays the same
      (is (= (count system-orig) (count system-new)))
      ; the updated entity changes
      (is (not= (core/query-by-id system-orig entity-id)
                (core/query-by-id system-new entity-id))))))

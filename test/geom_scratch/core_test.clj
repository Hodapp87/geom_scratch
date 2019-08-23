(ns geom-scratch.core-test
  (:require [clojure.test :refer :all]
            [geom-scratch.core :refer :all]))

(defn parse-args [args]
  (into {} (map (fn [[k v]] [(keyword (.replace k "--" "")) v])
                (partition 2 args))))

(deftest pairs-of-values
   (let [args ["--server" "localhost"
               "--port" "8080"
               "--environment" "production"]]
      (is (= {:server "localhost"
              :port "8080"
              :environment "production"}
             (parse-args args)))))

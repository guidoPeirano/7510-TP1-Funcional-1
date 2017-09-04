(ns logical-interpreter
  (:require [parser :refer :all]))

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (def database_array (parse-with-characters database #"[.\n]"))

  nil)

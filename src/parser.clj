(ns parser)


(defn process-database-initial-input
  "Processes initial input parsing with '\n' and '.'"
  [input]
  (parse-with-pattern input #"[\.|\n]")
  )

(defn parse-with-pattern
  "Parses an input with a determined pattern"
  [input pattern]
  (remove empty? (clojure.string/split input pattern))
  )

(defn parse-database-statement
  [statement]
  (parse-with-pattern statement #"\W")
  )

(defn parse-database-rule
  [rule]
  (def processed_rule (clojure.string/replace rule #" " ""))
  (def processed_rule (clojure.string/replace processed_rule #"\)," ")|"))
  (parse-with-pattern processed_rule #":-")
  )

(defn process-database-statements
  "Process the database array obtaining all the logic statements."
  [database]
  (def statements (filter string-has-no-upper-case database))
  (map (fn[x] (parse-database-statement x )) statements)
  )

(defn process-database-rules
  "Process the database array obtaining all the logic rules"
  [database]
  (def rules (remove string-has-no-upper-case database))
  (map parse-database-rule rules)
  )

(defn string-has-no-upper-case
  "Determines wether or not a string has any upper case letter"
  [string]
  (empty? (filter #(Character/isUpperCase %) string))
  )
(defn query-is-statement
  "Determines if a query is part of the statements"
  [query database_statements]
  (not-empty (filter (fn [x] (= x query)) database_statements))
  )
(defn query-is-a-rule
  "Determines if a query is part of a logic rule"
  [query database_rules]
  (not-empty (filter (fn [x] (query-matches-rule query (nth x 0))) database_rules))
  )
(defn query-matches-rule
  "Determine if a query has the same format as a specific rule"
  [query rule]
  (def rule_format (parse-with-pattern rule #"[(,)]"))
  (if (and (= (count query) (count rule_format)) (= (nth query 0) (nth rule_format 0)))
    true
    false
    )
  )
(defn query-processor
  "Processes a input query and determines its value"
  [input statements rules]
  (def query (parse-with-pattern input #"\W"))
  (if (query-is-statement query statements)
    true
    (if (query-is-a-rule query rules)
      (determine-rule-match query rules statements)
      false))
  )

(defn determine-rule-match
  "Determines if a rule match is true"
  [query database_rules database_statements]
  (let [rule_name 0 conditions 1 step 0 clojure_bug 0]
    (def rule_matched (nth (query-is-a-rule query database_rules) 0))
    (check-statements-matches query (parse-with-pattern (nth rule_matched rule_name) #"\W")
     (parse-with-pattern (nth rule_matched conditions) #"\|")
                              database_statements step))
 )

(defn check-statements-matches
  "Checks statements matches"
  [query rule_header conditions statement_database step]
  (let [ condition_len (count conditions)]
    (println step condition_len)
    (if (= step condition_len) true
      (and
       (check-this-match query rule_header (parse-with-pattern (nth conditions step) #"\W") statement_database)
       (check-statements-matches query rule_header conditions statement_database (+ step 1))))
    )
  )

(defn check-this-match
  "Checks a specific match"
  [query rule_format checking_statement statement_database]
  (let [statement_name 0 first_argument 1]
    (println "this match")
    (def query_statement
      (concat [(nth checking_statement statement_name)]
              (get-statement-participants query rule_format checking_statement first_argument))
      )
    (println query_statement)
    (not-empty (filter (fn[x] (= x query_statement)) statement_database)))
  )

(defn get-statement-participants
  [query rule_format statement_format step_number]
  (println "state partic" query rule_format statement_format step_number)
  (cond (= step_number (count statement_format)) []
      :else
    (concat [(nth query (.indexOf rule_format (nth statement_format step_number)))]
              (get-statement-participants query rule_format statement_format (+ step_number 1))))
  )

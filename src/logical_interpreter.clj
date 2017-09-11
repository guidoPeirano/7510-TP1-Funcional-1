(ns logical-interpreter
  )

(defn parse-with-pattern
  "Parses an input with a determined pattern"
  [input pattern]
  (remove empty? (clojure.string/split input pattern))
  )


(defn process-database-initial-input
  "Processes database initial input parsing with '\n' and '.'"
  [input]
  (parse-with-pattern input #"[\.|\n]")
  )



(defn parse-database-statement
  [statement]
  "Process database statements and returns an array with processed statements"
  (parse-with-pattern (clojure.string/trim statement) #"\W")
  )

(defn parse-database-rule
  [rule]
  "Process database rules and returns an array with processed rules and their corresponding conditions"
  (def processed_rule (clojure.string/replace rule #" " ""))
  (def processed_rule (clojure.string/replace processed_rule #"\)," ")|"))
  (parse-with-pattern processed_rule #":-")
  )

(defn string-has-no-upper-case
  "Determines wether or not a string has any upper case letter"
  [string]
  (empty? (filter #(Character/isUpperCase %) string))
  )

(defn process-database-statements
  "Process the database array obtaining all the logic statements."
  [database]
  (def statements (filter string-has-no-upper-case database))
  (map (fn[x] (parse-database-statement x )) statements)
  )

(defn process-database-rules
  "Process the database array obtaining all the logic rules and their conditions"
  [database]
  (def rules (remove string-has-no-upper-case database))
  (map parse-database-rule rules)
  )


(defn query-is-statement
  "Determines if a query is part of the statements"
  [query database_statements]
  (not-empty (filter (fn [x] (= x query)) database_statements))
  )

(defn query-matches-rule
     "Determine if a query matches the format of a given rule"
     [query rule]
     (def rule_format (parse-with-pattern rule #"[(,)]"))
     (if (and (= (count query) (count rule_format)) (= (nth query 0) (clojure.string/trim (nth rule_format 0))))
         true
       false     )
     )

(defn query-is-a-rule
  "Determines if a query matches the format of any rule"
  [query database_rules]
  (not-empty (filter (fn [x] (query-matches-rule query (nth x 0))) database_rules))
  )

(defn get-statement-participants
  [query rule_format statement_format step_number]
  "Given a query and a condition of a rule, it returns the participants of the condition"
  (cond (= step_number (count statement_format)) []
    :else
    (concat [(nth query (.indexOf rule_format (nth statement_format step_number)))]
            (get-statement-participants query rule_format statement_format (+ step_number 1))))
  )

(defn check-this-match
  "Checks if a query meets a condition in a given rule"
  [query rule_format checking_statement statement_database]
  (let [statement_name 0 first_argument 1]
    (def query_statement
      (concat [(nth checking_statement statement_name)]
              (get-statement-participants query rule_format checking_statement first_argument))
      )

    (not-empty (filter (fn[x] (= x query_statement)) statement_database)))
  )


(defn check-statements-matches
  "Checks if a given query meets the conditions for the rule to be true"
  [query rule_header conditions statement_database step]
  (let [ condition_len (count conditions)]
    (if (= step condition_len) true
      (and
       (check-this-match query rule_header (parse-with-pattern (nth conditions step) #"\W") statement_database)
       (check-statements-matches query rule_header conditions statement_database (+ step 1))))
    )
  )

(defn determine-rule-match
  "Given a query that matches a rule, it checks if it meets with the conditions for the rule to be true"
  [query database_rules database_statements]
  (let [rule_name 0 conditions 1 step 0 clojure_bug 0]
    (def rule_matched (nth (query-is-a-rule query database_rules) 0))
    (check-statements-matches query (parse-with-pattern (nth rule_matched rule_name) #"\W")
                              (parse-with-pattern (nth rule_matched conditions) #"\|")
                              database_statements step))
  )

(defn query-processor
  "Processes a input query and determines its value"
  [input statements rules]
  (def query (parse-with-pattern input #"\W"))
  (if (query-is-statement query statements)
    true
    (if (query-is-a-rule query rules)
      (if (determine-rule-match query rules statements)
        true
        false)
      false))
  )

(defn evaluate-database
  [database]
  (def database_array (process-database-initial-input database ))
  (def statement_pattern #"(\s)*\w+\(\w+(,\s\w+)*\)")
  (def rule_pattern (re-pattern (str  statement_pattern " :- " statement_pattern (str "("", " statement_pattern ")*"))))
  (def first_step (remove (fn[x] (re-matches statement_pattern (str x))) database_array))
  (def step2 (remove (fn[x] (re-matches rule_pattern (str x))) first_step))
  (empty? step2)
  )

(defn evaluate-query-format
  [query]
  (def statement_pattern #"(\s)*\w+\(\w+(,\s\w+)*\)")
  (re-matches statement_pattern query)
  )

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (def database_array (process-database-initial-input database ))
  (def statements (process-database-statements database_array))
  (def rules (process-database-rules database_array))
  (if (and (evaluate-database database) (evaluate-query-format query)) (query-processor query statements rules) nil)
  )

(ns parser)

(defn parse-with-characters
  "Parses the input into a list separating elements by a determined character"
  [string separation_characters]
  (let [special_character "*" special_separation_character "[*]"]
    (def single_line_string (clojure.string/replace string #"\n" " "))
    (def spaced_string (clojure.string/replace single_line_string separation_characters special_character))
    (def trimed_string (clojure.string/trim spaced_string))
    (def array (clojure.string/split trimed_string (re-pattern special_separation_character))))
    array
  )

(defn process-database-statements
  "Process the database array obtaining all the logic statements."
  [database]
  (def statements (filter string-has-no-upper-case database))
  (map (fn[x] (parse-with-characters x #"[(,)]")) statements)
  )

(defn process-database-rules
  "Process the database array obtaining all the logic rules"
  [database]
  (def statements (remove string-has-no-upper-case database))
  (def statements (map (fn [x] (clojure.string/replace x #"\)," ")|")) statements))
  (def statements (map (fn [x] (clojure.string/replace x #" " "")) statements))
  (map (fn [x] (clojure.string/split x #":-")) statements)
  )

(defn string-has-no-upper-case
  "Determines wether or not a string has any upper case letter"
  [string]
  (empty? (filter #(Character/isUpperCase %) string))
  )
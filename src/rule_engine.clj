(ns rule_engine)




(defn statement-input-stream-to-array
  "Parses the input of a new statement and returns an array with the inputs"
  [string]
  (def spaced_string (clojure.string/replace string #"[(,)]." " "))
  (def input_array (clojure.string/split spaced_string #" "))
  input_array
  )


(defn functional-input-array-processor
  "Adds new data from the input array in a functional way"
  [arguments]
  (let [array (nth arguments 0) data (nth arguments 1)]
    (if (input-es-valido arguments) (concat data [array]) data))
  )


(defn input-es-valido
  "Verifica que un nuevo input sea valido"
  [arguments]
  (let [name 0 sex 1 input_type 0 father 1 son 2 error 0 ok 1 input (nth arguments 0) data (nth arguments 1)]
    (if (= (nth input input_type) "padre")
      (let [father_name (nth input 1)]
        (if (or (empty? (filter (fn [x] (and  (= (nth x name) father_name)  (= (nth x sex) "varon"))) data))
                (empty? (filter (fn [x] (= (nth x name) (nth input son)))  data)))
        error
        ok))
      ok))
  )


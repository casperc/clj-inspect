(ns clj-inspect.core
  (:require [clojure.pprint :as pprint]
            [clojure.tools.namespace.find :as ns]
            [clojure.java.classpath :as cp]))

;; Record a unit test based on defining which functions should be "recorded"

(defmacro ^:private time-expr
  "Times a function and returns the time the expression took to execute and the return value in a tupple: [time res]"
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     [(- (. System (nanoTime)) start#) ret#]))

(defn- add-recorded-value [m key value]
  (let [m1 (if-let [old-vals (m key)]
              (update-in m [key] conj value)
              (assoc m key [value]))]
    (if (seq (pop (:current m1)))
      (update-in m1 [:current] pop)
      (dissoc m1 :current))))

(defn- push-current [m current-fn-name]
  (if-let [old (m :current)]
    (update-in m [:current] conj current-fn-name)
    (assoc m :current [current-fn-name])))

(defn record-wrap [record-atom func-var]
  (let [func (var-get func-var)] ; To avoid executing the function as it is after redefining it, leading to an infinite loop
    (fn [& args]
      (let [fn-name (str (:ns (meta func-var)) "/" (:name (meta func-var)))]
        (swap! record-atom push-current fn-name)
        (let [[t res] (time-expr (apply func args))]
          (swap! record-atom add-recorded-value fn-name [(peek (pop (:current @record-atom))) t args res])
          res)))))

(defmacro with-record-redefs
  "Takes a list of functions to be recorded and an atom to record their in and output in."
  [[record-atom func-vars] & body]
  `(let [bindings-map# (into {} (mapv #(vector % (record-wrap ~record-atom %)) ~func-vars))]
     (with-redefs-fn bindings-map# (fn [] ~@body))))

(defn replay-wrap [replay-atom func-var]
  (let [func (var-get func-var)] ; To avoid executing the function as it is after redefining it, leading to an infinite loop
    (fn [& args]
     (let [k (str (:ns (meta func-var)) "/" (:name (meta func-var)))
           [parent t in out] (first (@replay-atom k))]
       ;; Check that the input is the same as expected
       (assert (= in args) (str "Replay failed due to " k " not getting the expexted input. Expected input: " in ", Actual input: " args)) ;; TODO: Skriv besked så man ved hvilken funktion som fejlede
       ;; Remove the value from the atom
       (swap! replay-atom update-in [k] rest)
       out))))

(defmacro with-replay-redef
  "Takes a an atom with previously recorded values from functions and list of functions to replay the values from."
  [[replay-map func-vars bindings] & body]
  `(let [bindings-map# (merge (into {} (mapv #(vector % (replay-wrap (atom ~replay-map) %)) ~func-vars))
                              ~bindings)]
     (with-redefs-fn bindings-map# (fn [] ~@body))))

(defn- median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn- average [ns]
  (/ (apply + ns) (count ns)))

(defn- find-children [m fn-name]
  (mapcat #(filter (comp #{fn-name} first) %) (vals m)))

(defn- nanos->millis [time]
  (/ (double time) 1000000.0))

(defn print-timing-report
  "Prints a table containing the function name, number of calls to the function, the average time taken,
   the mean time taken, and the total time taken in that function"
  [m]
  (let [rows (map (fn [[k v]]
                    (let [times (seq (map second v))]
                      (when (not times) (prn "kv" k v))
                      {:name k
                       :n (count times)
                       :avg (nanos->millis (/ (apply + times) (count times)))
                       :median (nanos->millis (median times))
                       :min (nanos->millis (apply min times))
                       :max (nanos->millis (apply max times))
                       :total (nanos->millis (apply + times))
                       :adjusted-total (nanos->millis (- (apply + times) (apply + (mapv second (find-children m k)))))}))
                  m)]
    (pprint/print-table (sort-by :total > rows))))

(defn find-namespaces
  "Filter all the namespaces defined in the currently running repl (including libraries) by a regular expression."
  [re]
  (filter (comp #(re-find re %) str) (all-ns)))

(defn get-vars-in-namespaces
  "Takes a list of namespaces and returns a list of the namespace interns."
  [namespaces]
  (mapcat (comp vals ns-interns) namespaces))

(defn find-vars-in-namespaces 
  "Matches a regular expression against namespace names and returns the vars in the matched namespaces."
  [re]
  (get-vars-in-namespaces (find-namespaces re)))

(defn namespaces-in-src
  "Find all namespaces under the src foler in the current project"
  []
  (remove nil? (map find-ns (ns/find-namespaces (filter (comp #(re-find #"src$" %) str) (cp/classpath-directories))))))

(defn find-fns-in-namespaces
  "Takes a list of namespaces and returns a map between the namespace and its functions."
  [namespaces]
  (into {} (map (comp #(filter (comp fn? var-get) %) vals ns-interns) namespaces)))

(ns genuine-highlighter.renderer)

(declare render)

(defmulti render* :type)

(defmethod render* :whitespaces [x]
  (:raw x))

(defmethod render* :symbol [x]
  (let [code {:special 36, :macro 33, :var 35, :local 32}
        color (fn [x]
                (if-let [c (code (:type (:usage x)))]
                  (str "\033[" c "m" (:symbol x) "\033[39m")
                  (str (:symbol x))))]
    (color x)))

(defmethod render* :number [x]
  (str (:number x)))

(defmethod render* :list [x]
  (str \( (render (:nodes x)) \)))

(defmethod render* :vector [x]
  (str \[ (render (:nodes x)) \]))

(defmethod render* :map [x]
  (str \{ (render (:nodes x)) \}))

(defmethod render* :set [x]
  (str "#{" (render (:nodes x)) "}"))

(defn render [xs]
  (apply str (map render* xs)))

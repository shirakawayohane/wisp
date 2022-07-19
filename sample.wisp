(defn addTwoAnd30: i32 [a: i32, b: i32]
    (let [c 30]
        (+ c (+ a b))))

(export defn main []
    (addTwoAnd30 10 20))
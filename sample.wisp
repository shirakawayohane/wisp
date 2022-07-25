(defn addTwoAnd30: i32 [a: i32, b: i32]
    (let [c 30]
        (+ c (+ a b))))

(define GlobalNumber: i32
    10)

(defn middle : i32
    [a: i32 b: i32 c: i32]
    (if (> a b c)
        b
        (if (> a c b)
            c
            (if (> b a c)
                a
                (if (> b c a)
                    c
                    (if (> c a b)
                        a
                        (if (> c b a)
                            b
                            a)))))))

(export defn getIndexOf123:i32 
    [index: i32]
    (index [1,2,3]))

(export defn main []
    (addTwoAnd30 10 20))
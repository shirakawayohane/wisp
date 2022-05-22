(module hoge)

(import [console log] as (func (param i32)))

(defn add [a : i32, b: i32] : i32
    (+ a b))
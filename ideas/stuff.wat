i32
i64
  42
  -42
  +42

f32
f64
  42
  -42
  +42
  3.14
  inf
  nan

---

(module ...)

(import "imports" "name" (func $name (param type ...) (result type)))
(import "imports" "memory" (memory 1))

(data (type.const literal) literal)

(func $name (param $name type) ... (result type) body)
(call $name arg ...)
(return value)

(local.get $name)
(local.set $name)

(type.load (i32.const offset))
(type.store (i32.const offset) value)

(if type cond then else)

(type.const literal)
(type.eq a b)
(type.add a b)
...

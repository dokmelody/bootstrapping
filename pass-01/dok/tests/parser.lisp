;; SPDX-License-Identifier: MIT
;; Copyright (C) 2023 Massimo Zaniboni <mzan@dokmelody.org>

(defpackage :dok/parser/tests
  (:import-from :parachute #:define-test #:true)
  (:import-from :dok-parser #:parse-dok)
  (:use :cl
        :dok/parser))

(in-package :dok/parser/tests)

(define-test parse-tokens
  :serial nil
  (true (parse-dok "return \"hello world\""))
  (true (parse-dok "return 123"))
  (true (parse-dok "return 123.56"))
  (true (parse-dok "return my-id"))
  (true (parse-dok "return my-new-id"))
  (true (parse-dok "data A [
  slot x::Int

  fun f::Int {
    return 0
  }

  variant ../B [
    fun f::Int -override {
      return 1
    }
  ]
]"))

  (true (parse-dok "
data A [
  fun f::Int {
    return 0
  }

  variant ../B [
    fun f::Int -override {
      return 1
    }
  ]

  variant ../C [
    fun f::Int -override {
      return 2
    }
  ]
]

var b::A/B
assert b.f == 1

var a::A
assert a.f == 0

return a.f(1,2)
return a.f(a.g(a.f, 3 + 2))
return \"some string\"
return 3.5

")))

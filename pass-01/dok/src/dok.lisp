;; SPDX-License-Identifier: MIT
;; Copyright (C) 2023 Massimo Zaniboni <mzan@dokmelody.org>

(in-package :dok)

(defparameter *code1*
  "
data Int [

]

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
")

(defun test ()
  (let ((ast (dok-parser:parse-dok *code1*)))
    (walk1 ast nil)
    (walk2 ast)
    ast))


;; -*- geiser-scheme-implementation: stklos -*-

(require "slib")
(require 'array)

(require 'pretty-print)

(define a (make-array '#() 3 5))
(array-set! a "at2,4" 2 4)
(array-ref a 2 4)


;; (require 'syntax-case)
;; (require 'repl)
;; (repl:top-level macro:eval)

;; (require 'syntax-case)
;; (syncase:sanity-check)

;; for loops


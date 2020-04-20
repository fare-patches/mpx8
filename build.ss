#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/make)

(def build-spec
  '("mpx8"
    (exe: "mpx8" "-ld-options" "-lssl -lz -L/usr/local/lib/" "-cc-options" "-I/usr/local/include")))

(def build-spec-static
  '("mpx8"
    (static-exe: "mpx8"
                 "-ld-options" "-lssl -lz -L/usr/local/lib"
                 "-prelude" "(declare (not safe))")))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def (main . args)
  (match args
    (["static"]
     (make srcdir: srcdir
           bindir: srcdir
           optimize: #t
           static: #t
           prefix: "mpx8"
           build-spec-static))
    ([]
     (make srcdir: srcdir
           bindir: srcdir
           optimize: #t
           debug: 'env
           static: #t
           prefix: "mpx8"
           build-spec))))

#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("gerbil-mcp/lib"
    (exe: "gerbil-mcp/main" bin: "gerbil-mcp")))

(defsystem "download-dependencies"
  :description "A simple tool to download dependencies recursively."
  :depends-on ("uiop")
  :components ((:file "download-dependencies.lisp")))

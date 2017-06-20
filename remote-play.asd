;;;; remote-play.asd

(asdf:defsystem #:remote-play
  :description "Describe remote-play here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:live-remote :cepl.sdl2 :nineveh :skitter)
  :components ((:file "package")
               (:file "controls")
               (:file "remote-play")))

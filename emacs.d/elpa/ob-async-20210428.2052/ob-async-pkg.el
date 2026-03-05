;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "ob-async" "20210428.2052"
  "Asynchronous org-babel src block execution."
  '((async "1.9")
    (org   "9.0.1")
    (emacs "24.4")
    (dash  "2.14.1"))
  :url "https://github.com/astahlman/ob-async"
  :commit "9aac486073f5c356ada20e716571be33a350a982"
  :revdesc "9aac486073f5"
  :keywords '("tools")
  :authors '(("Andrew Stahlman" . "andrewstahlman@gmail.com"))
  :maintainers '(("Andrew Stahlman" . "andrewstahlman@gmail.com")))

<script type="application/racket" id="home-main">
#lang racket/base

;; This is an application element. It can define a new page.
(provide replace-page)

;; Use the 'project' link to access your project.
(require "project/vcomps.rkt"
         polyglot)

(define (replace-page page-tx)
  (page "My thoughts on Lisps" page-tx))
</script>

Sometimes, feelings about a thing can be a by-product of thinking deeply about it. Sometimes, it's the other way around. My relationship with Lisp is closer to the latter.

In Highschool, I decided to read [The Annotated Turing by Charles Petzold](https://www.amazon.com/Annotated-Turing-Through-Historic-Computability/dp/0470229055) for fun. After learning all about Turing Machines, After that, I got curious about Lambda Calculus - it seemed even simpler than a Turing Machine, and less intuitive by that logic. What happened after could happen to anyone. I don't fault myself for it. I got dragged into the world of functional programming. Some people get there through Haskell, which I also love. My first experience was with Racket.

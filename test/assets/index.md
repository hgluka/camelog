<script type="application/racket" id="home-main">
#lang racket/base

;; This is an application element. It can define a new page.
(provide replace-page)

;; Use the 'project' link to access your project.
(require "project/vcomps.rkt"
         polyglot)

(define (replace-page page-tx)
  (page "home" page-tx))
</script>

Greetings, earthling! I'm Luka Hadži-Đokić and I've been programmed to like programming language theory. I'm a student at the Faculty of Mathematics, University of Belgrade. You can find me on [twitter](https://twitter.com/hgluka) and [github](https://github.com/hgluka) and also [right here](#). Or you can read my [CV](resume.pdf). I might write in Serbian occasionally.

## blog
<script type="application/racket" id="post-list">
#lang racket/base
(provide replace-page)

(require "project/posts.rkt"
         polyglot)

(define (replace-page page-tx)
  (tx-replace-me page-tx
    (lambda (x) post-list)))
</script>

<script type="application/racket" id="home-main">
#lang racket/base

;; This is an application element. It can define a new page.
(provide replace-page)

;; Use the 'project' link to access your project.
(require "project/vcomps.rkt"
         polyglot)

(define (replace-page page-tx)
  (page "Island generation with Typed Racket" page-tx))
</script>

Last year I wrote a small procedural island generator in Racket called [procsland](https://github.com/hgluka/procsland). I know, the name sucks. I'm not very good at names. Aside from the name, though, I took a good look at the code this week and I realised I'm not very good at that either. 
The generator was really a cellular automaton with hexagonal cells. Each cell had 6 neighbours. If a cell with water is surrounded by too little or a too much land, it turned into land. Pretty straightforward stuff, aside from a bit of [hexagonal grid math](https://www.redblobgames.com/grids/hexagons/). The trouble is, I didn't really adopt a very functional style for the code. I used a straight list to represent the map, but that was about it. Lots of `list-ref`s and lots of awkward iteration. For some reason, a bunch of double quoted values popped up in the code (`''land` instead of `'land`) so there were snippets like this one:

```
(last (list-ref 
        tile-list
        (+ 
          (+ q (car dir))
          (* (+ r (last dir)) w))))
```

devised as a last ditch effort against myself. I still have leftover trauma from that. Anyway, you get the point.


I decided to rewrite it all. First, I ditched the list in favor of a 2D array. That made more sense considering all those `list-ref`s that I feel are necessary. And then, as per [the docs](https://docs.racket-lang.org/math/array.html), Typed Racket popped up as an option. While I am familiar with it, I've never _really_ used it. This was an opportunity to learn. 

It felt a lot cleaner this time around. `math/array` provides a lot of useful tools that fit the job. The drawing code naturally moved to `graphics.rkt`, which I should've done sooner. The end result is a 60 line file for the cellular automata code and another 60 line file for the graphics code. `main.rkt` remained largely the same.

One issue that I ran into was (I think) a bug in `typed/racket/gui`. For some reason, before I moved the drawing code, running `map.rkt`, a file with around 5 definitions (and no user-defined macros or top-level function application) took too much memory. Switching to `typed/racket/gui/base` didn't help. Splitting the code did. After that, both `map.rkt` and `graphics.rkt` ran quickly and used a lot less memory. At a high level, it was probably some weird interaction between my code and stuff provided by `typed/racket/gui`. It will take a bit more testing to pinpoint the exact issue.

In the process, I completely forgot that I was using hexagons instead of squares, so the cellular automaton is actually a regular square-based one, but the end result might look a bit better anyway. After 15 steps, all the land masses seem well-defined, although they might actually be peninsulas and not islands. Po-tay-to po-tah-to.

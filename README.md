# camelog

A static blog generator written in OCaml. It turns a directory of markdown files and HTML templates into a static site.

## Installing

Every push to `main` that passes the tests publishes a Linux x86_64 binary as the `latest` release:

```
curl -fL -o camelog https://github.com/hgluka/camelog/releases/download/latest/camelog-linux-x86_64
chmod +x camelog
```

It needs the Oniguruma library at run time (`libonig5` on Debian and Ubuntu). Sites with math also need pandoc, and sites with TikZ figures need `latex` and `dvisvgm`. To build from source instead, see [Building](#building).

## Usage

```
camelog INPUT_DIR OUTPUT_DIR [-verbose]
```

camelog renders each `.md` file into `OUTPUT_DIR` as `.html` and copies every other file as is. `-verbose` prints each page before building.

Files and folders whose names start with `_` are never rendered or copied. They hold what the site is built from rather than what it publishes:

```
source/
  _template.html          template for pages in this folder
  _grammars/racket.json   extra highlighting grammars
  _drafts/new-post.md     unfinished posts
  index.md
  posts/
    _template.html
    2020-03-31-island-generation-with-typed-racket.md
  styles/styles.css
```

## Templates

Every folder with markdown files needs a `_template.html`. A template is an HTML file with three placeholders:

- `inscribe:title` is the page title.
- `inscribe:content` is the rendered markdown.
- `inscribe:posts` is a list of links to all posts, newest first.

```html
<title>inscribe:title</title>
<div id="page_content">inscribe:content</div>
```

Any other name after `inscribe:` stops the build, so typos don't slip through.

## Posts

Posts are markdown files under `INPUT_DIR/posts/`. Name them `YYYY-MM-DD-some-title.md` and camelog sorts the post list by that date and shows it next to the title.

A page's title comes from its first line starting with `##`, without the `#`s. Without one, the file name is the title.

Unfinished posts go in `INPUT_DIR/_drafts/`. To publish one, move it to `posts/` with a date prefix.

Markdown is rendered with [cmarkit](https://erratique.ch/software/cmarkit). Raw HTML passes through.

## Links

Plain links are left as written. To have camelog find a file and write the right relative path, start the link with `seek:`:

- `seek:/styles/styles.css` is an exact path from `INPUT_DIR`.
- `seek:styles.css` is a file name. It must match exactly one file in the site.

Markdown files are linked by their source name, so `seek:2020-03-31-island-generation-with-typed-racket.md` links to the generated `.html`. `seek:` works in templates and in markdown, in `href` and `src` attributes, markdown links and images:

```html
<link rel="stylesheet" href="seek:styles.css">
<a href="seek:/index.md">home</a>
```

```markdown
See [the previous post](seek:2020-03-29-prijavite-se-za-plmw.md).
![screenshot](seek:procsland_screenshot.png)
```

The build stops with an error if a `seek:` link matches no file or more than one, or if it contains a `/` without starting with one.

## Code highlighting

Fenced code blocks are highlighted at build time with [hilite](https://github.com/patricoferris/hilite) and TextMate grammars, the format VS Code uses. No JavaScript is needed on the page.

hilite includes grammars for OCaml, dune, opam, shell and diff. For other languages, put TextMate grammar `.json` files in `INPUT_DIR/_grammars/`. [Shiki's grammar collection](https://github.com/shikijs/textmate-grammars-themes/tree/main/packages/tm-grammars) is a good source; keep each grammar's license next to it.

A block's language is matched against each grammar's `name`, then its `fileTypes`, so ```` ```racket ```` and ```` ```rkt ```` can both find a Racket grammar. Blocks in other languages are rendered as plain code.

Each token becomes a `<span>` whose class is the block's language followed by the token's TextMate scope, like `racket-keyword-control` or `ocaml-string-quoted-double`. A stylesheet can match scopes for every language at once:

```css
pre code [class*="-comment"] { color: #928374; }
pre code [class*="-keyword"] { color: #fb4934; }
```

## Math

`$...$` (inline), `$$...$$` (display) and ```` ```math ```` blocks are converted to MathML at build time by [pandoc](https://pandoc.org), which must be on `PATH` for sites that use math. Browsers render MathML natively, so pages need no math JavaScript or fonts.

pandoc understands most of LaTeX math, including `aligned`, matrices and `cases`. A formula it can't convert stops the build with pandoc's message and the page's path. To write a literal dollar sign in text, use `\$`.

## TikZ figures

```` ```tikz ```` blocks are compiled with `latex` and `dvisvgm` at build time and inlined as SVG, so figures need no JavaScript or images. The block holds the picture itself, for example a `tikzpicture` or a `tikz-cd` diagram:

````markdown
```tikz
\begin{tikzcd}
A \arrow[r, "f"] \arrow[d, "g"'] & B \arrow[d, "h"] \\
C \arrow[r, "k"'] & D
\end{tikzcd}
```
````

Each figure is wrapped in a document that loads `tikz`, `tikz-cd` and the `arrows.meta`, `positioning` and `automata` libraries. Put your own preamble in `INPUT_DIR/_tikz.tex` to replace it. It should select pgf's dvisvgm driver before loading TikZ, as the default does:

```latex
\documentclass{article}
\pagestyle{empty}
\def\pgfsysdriver{pgfsys-dvisvgm.def}
\usepackage{tikz}
```

dvisvgm crops each figure to what it draws, so the page layout doesn't matter.

Black is drawn as `currentColor`, so figures take the colour of the surrounding text. Each SVG sits in a `<div class="tikz">` for styling. A figure that doesn't compile stops the build with LaTeX's error lines. Sites without TikZ blocks don't need TeX at all.

With TeX Live, the needed packages are `scheme-basic pgf tikz-cd dvisvgm`. On Debian and Ubuntu, they are `texlive-latex-base texlive-pictures dvisvgm`.

## Building

camelog needs the Oniguruma C library and its headers (`oniguruma-devel` on Void, `libonig-dev` on Debian and Ubuntu).

```
dune build
dune runtest
```

`test/golden.t` builds `test/test_website` and compares the output. After an intended change to the output, check the diff from `dune runtest` and accept it with `dune promote`.

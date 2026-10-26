# camelog

A static blog generator written in OCaml. It turns a directory of markdown files and HTML templates into a static site.

## Usage

```
camelog INPUT_DIR TEMPLATE OUTPUT_DIR [-grammars DIR] [-verbose]
```

`TEMPLATE` is a file name, not a path. Every directory that contains markdown files needs a template with that name. camelog renders each `.md` file into `OUTPUT_DIR` as `.html` and copies every other file as is. `-verbose` prints each page before building.

Files whose names start with `WIP` are skipped, and so are the templates.

## Templates

A template is an HTML file with three placeholders:

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

hilite includes grammars for OCaml, dune, opam, shell and diff. For other languages, put TextMate grammar `.json` files in a directory outside `INPUT_DIR` and pass it with `-grammars DIR`. [Shiki's grammar collection](https://github.com/shikijs/textmate-grammars-themes/tree/main/packages/tm-grammars) is a good source; keep each grammar's license next to it.

A block's language is matched against each grammar's `name`, then its `fileTypes`, so ```` ```racket ```` and ```` ```rkt ```` can both find a Racket grammar. Blocks in other languages are rendered as plain code.

Each token becomes a `<span>` whose class is the block's language followed by the token's TextMate scope, like `racket-keyword-control` or `ocaml-string-quoted-double`. A stylesheet can match scopes for every language at once:

```css
pre code [class*="-comment"] { color: #928374; }
pre code [class*="-keyword"] { color: #fb4934; }
```

## Building

camelog needs the Oniguruma C library and its headers (`oniguruma-devel` on Void, `libonig-dev` on Debian and Ubuntu).

```
dune build
dune runtest
```

`test/golden.t` builds `test/test_website` and compares the output. After an intended change to the output, check the diff from `dune runtest` and accept it with `dune promote`.

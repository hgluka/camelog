# camelog

A static blog generator written in OCaml. It turns a directory of markdown files and HTML templates into a static site.

## Usage

```
camelog INPUT_DIR TEMPLATE OUTPUT_DIR [-verbose]
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

## Building

```
dune build
dune runtest
```

`test/golden.t` builds `test/test_website` and compares the output. After an intended change to the output, check the diff from `dune runtest` and accept it with `dune promote`.

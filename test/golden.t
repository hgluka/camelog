Build the test website and check the generated files.

  $ camelog test_website out > /dev/null
  $ find out -type f | sort
  out/index.html
  out/posts/2025-11-11-post-one.html
  out/posts/2025-11-11-post-two.html
  out/posts/2025-11-12-post-three.html
  out/posts/diagram.svg
  out/styles.css
  $ cat out/index.html
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="stylesheet" href="styles.css">
    <title>index</title>
  </head>
  <body>
    <a href="index.html">index.md</a>
    <a href="https://example.com/docs/index.html">docs</a>
    <h2>index</h2>
  <p>This is a test website.</p>
  <p>List:</p>
  <ul>
  <li>one</li>
  <li>two</li>
  <li>three</li>
  </ul>
  
    <ul>
      <li><a href="posts/2025-11-12-post-three.html">2025-11-12: Post Three &amp; &lt;Friends&gt;</a></li>
      <li><a href="posts/2025-11-11-post-two.html">2025-11-11: Post Two</a></li>
      <li><a href="posts/2025-11-11-post-one.html">2025-11-11: Post One</a></li>
    </ul>
  </body>
  </html>
  $ cat out/posts/2025-11-11-post-two.html
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="stylesheet" href="../styles.css">
    <title>Post Two</title>
  </head>
  <body>
    <a href="../index.html">index.md</a>
    <a href="https://example.com/docs/index.html">docs</a>
    <h2>Post Two</h2>
  <pre><code><span class='ocaml-keyword'>let</span><span class='ocaml-source'> </span><span class='ocaml-entity-name-function-binding'>x</span><span class='ocaml-source'> </span><span class='ocaml-keyword-operator'>=</span><span class='ocaml-source'> </span><span class='ocaml-constant-numeric-decimal-integer'>1</span><span class='ocaml-source'>
  </span></code></pre>
  <pre><code><span class='toy-keyword-control'>let</span><span class='toy-source'> greeting = </span><span class='toy-string-quoted-double'>&quot;hi&quot;</span><span class='toy-source'> </span><span class='toy-keyword-control'>in</span><span class='toy-source'> greeting </span><span class='toy-comment-line'>-- from _grammars</span><span class='toy-source'>
  </span></code></pre>
  <pre><code><span class='ty-keyword-control'>let</span><span class='ty-source'> x = </span><span class='ty-string-quoted-double'>&quot;found by file type&quot;</span><span class='ty-source'>
  </span></code></pre>
  <pre><code class="language-nosuchlang">plain &lt;code&gt; with href=&quot;seek:styles.css&quot; stays as is
  </code></pre>
  
    <ul>
      <li><a href="2025-11-12-post-three.html">2025-11-12: Post Three &amp; &lt;Friends&gt;</a></li>
      <li><a href="2025-11-11-post-two.html">2025-11-11: Post Two</a></li>
      <li><a href="2025-11-11-post-one.html">2025-11-11: Post One</a></li>
    </ul>
  </body>
  </html>
  $ cat out/posts/2025-11-12-post-three.html
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="stylesheet" href="../styles.css">
    <title>Post Three &amp; &lt;Friends&gt;</title>
  </head>
  <body>
    <a href="../index.html">index.md</a>
    <a href="https://example.com/docs/index.html">docs</a>
    <h3>Post Three &amp; <Friends></h3>
  
    <ul>
      <li><a href="2025-11-12-post-three.html">2025-11-12: Post Three &amp; &lt;Friends&gt;</a></li>
      <li><a href="2025-11-11-post-two.html">2025-11-11: Post Two</a></li>
      <li><a href="2025-11-11-post-one.html">2025-11-11: Post One</a></li>
    </ul>
  </body>
  </html>
  $ cat out/posts/2025-11-11-post-one.html
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="stylesheet" href="../styles.css">
    <title>Post One</title>
  </head>
  <body>
    <a href="../index.html">index.md</a>
    <a href="https://example.com/docs/index.html">docs</a>
    <h2>Post One</h2>
  <p><a href="2025-11-11-post-two.html">Post two</a>, <a href="../index.html">home</a> and <img src="diagram.svg" alt="diagram" >.</p>
  <img src="diagram.svg">
  <p>A plain <a href="../index.html">relative link</a> stays as written.</p>
  <pre><code class="language-html">&lt;a href=&quot;seek:styles.css&quot;&gt;code stays literal&lt;/a&gt;
  </code></pre>
  
    <ul>
      <li><a href="2025-11-12-post-three.html">2025-11-12: Post Three &amp; &lt;Friends&gt;</a></li>
      <li><a href="2025-11-11-post-two.html">2025-11-11: Post Two</a></li>
      <li><a href="2025-11-11-post-one.html">2025-11-11: Post One</a></li>
    </ul>
  </body>
  </html>

A trailing slash on the input directory gives the same output.

  $ camelog test_website/ out2 > /dev/null
  $ diff -r out out2

A seek: link that matches no file stops the build.

  $ mkdir broken
  $ printf '<link href="seek:missing.css">inscribe:content' > broken/_template.html
  $ printf '## Broken\n' > broken/index.md
  $ camelog broken broken_out > /dev/null
  camelog: broken/index.md: seek:missing.css matches no file
  [1]

A missing template names the file it looked for.

  $ mkdir no_template
  $ printf '## No template\n' > no_template/index.md
  $ camelog no_template no_template_out
  camelog: no_template/_template.html is not a regular file
  [1]

A site without subdirectories still creates the output directory.

  $ mkdir flat
  $ printf 'inscribe:content' > flat/_template.html
  $ printf '## Flat\n' > flat/index.md
  $ camelog flat flat_out
  $ cat flat_out/index.html
  <h2>Flat</h2>

An unknown inscribe: placeholder stops the build and names the template.

  $ mkdir typo
  $ printf 'inscribe:contnet' > typo/_template.html
  $ printf '## Typo\n' > typo/index.md
  $ camelog typo typo_out
  camelog: typo/_template.html: inscribe:contnet is not a placeholder, use inscribe:title, inscribe:content or inscribe:posts
  [1]

A grammar that doesn't parse stops the build.

  $ mkdir -p bad_grammars/_grammars
  $ printf 'inscribe:content' > bad_grammars/_template.html
  $ printf '## Bad grammars\n' > bad_grammars/index.md
  $ printf '{' > bad_grammars/_grammars/broken.json
  $ camelog bad_grammars bad_out
  camelog: bad_grammars/_grammars/broken.json: Line 1, bytes 0-1:
  Unexpected end of input
  [1]

Math becomes MathML, converted by pandoc. Code spans keep their dollars.

  $ mkdir math
  $ printf 'inscribe:content' > math/_template.html
  $ printf '## Math\n\nInline $\\omega$, display $$\\frac{1}{2}$$ and a block:\n\n```math\n\\sum_{i=0}^{n} i\n```\n\nCode keeps `$x$` as is.\n' > math/index.md
  $ camelog math math_out
  $ grep -o '<math display="[a-z]*"' math_out/index.html
  <math display="inline"
  <math display="block"
  <math display="block"
  $ grep -c 'camelog-math' math_out/index.html
  0
  [1]
  $ grep -o '<code>[^<]*</code>' math_out/index.html
  <code>$x$</code>

Math pandoc can't convert stops the build and names the page.

  $ mkdir bad_math
  $ printf 'inscribe:content' > bad_math/_template.html
  $ printf '$\\nosuchcommand$\n' > bad_math/index.md
  $ camelog bad_math bad_math_out 2>&1 | head -1
  camelog: bad_math/index.md: pandoc could not convert the math:

TikZ blocks become inline SVG. Black becomes currentColor, and each figure
gets its own id prefix so glyphs don't clash.

  $ mkdir tikz
  $ printf 'inscribe:content' > tikz/_template.html
  $ cat > tikz/index.md <<'MD'
  > ```tikz
  > \begin{tikzcd}
  > A \arrow[r, "f"] & B
  > \end{tikzcd}
  > ```
  > 
  > ```tikz
  > \begin{tikzpicture}
  > \node[state, initial] (q0) {$q_0$};
  > \end{tikzpicture}
  > ```
  > MD
  $ camelog tikz tikz_out
  $ grep -c '<div class="tikz"><svg' tikz_out/index.html
  2
  $ grep -o "id='tikz[0-9]-" tikz_out/index.html | sort -u
  id='tikz0-
  id='tikz1-
  $ grep -q currentColor tikz_out/index.html && echo uses currentColor
  uses currentColor
  $ grep -c '<?xml' tikz_out/index.html
  0
  [1]

A _tikz.tex preamble replaces the default one.

  $ mkdir tikz_preamble
  $ printf 'inscribe:content' > tikz_preamble/_template.html
  $ printf '\\documentclass[dvisvgm]{standalone}\n\\usepackage{tikz}\n\\newcommand{\\camelogdot}{\\fill (0,0) circle (1pt);}\n' > tikz_preamble/_tikz.tex
  $ printf '```tikz\n\\begin{tikzpicture}\\camelogdot\\end{tikzpicture}\n```\n' > tikz_preamble/index.md
  $ camelog tikz_preamble tikz_preamble_out
  $ grep -c '<svg' tikz_preamble_out/index.html
  1

A figure LaTeX can't compile stops the build with LaTeX's error lines.

  $ mkdir bad_tikz
  $ printf 'inscribe:content' > bad_tikz/_template.html
  $ printf '```tikz\n\\begin{tikzpicture}\\nosuchmacro\\end{tikzpicture}\n```\n' > bad_tikz/index.md
  $ camelog bad_tikz bad_tikz_out
  camelog: bad_tikz/index.md: LaTeX could not compile a TikZ figure:
  ! Undefined control sequence.
  l.6 \begin{tikzpicture}\nosuchmacro
  [1]

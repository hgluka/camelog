Build the test website and check the generated files.

  $ camelog test_website template.html out > /dev/null
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

  $ camelog test_website/ template.html out2 > /dev/null
  $ diff -r out out2

A seek: link that matches no file stops the build.

  $ mkdir broken
  $ printf '<link href="seek:missing.css">inscribe:content' > broken/template.html
  $ printf '## Broken\n' > broken/index.md
  $ camelog broken template.html broken_out > /dev/null
  camelog: broken/index.md: seek:missing.css matches no file
  [1]

A missing template names the file it looked for.

  $ mkdir no_template
  $ printf '## No template\n' > no_template/index.md
  $ camelog no_template template.html no_template_out
  camelog: no_template/template.html is not a regular file
  [1]

A site without subdirectories still creates the output directory.

  $ mkdir flat
  $ printf 'inscribe:content' > flat/template.html
  $ printf '## Flat\n' > flat/index.md
  $ camelog flat template.html flat_out
  $ cat flat_out/index.html
  <h2>Flat</h2>

An unknown inscribe: placeholder stops the build and names the template.

  $ mkdir typo
  $ printf 'inscribe:contnet' > typo/template.html
  $ printf '## Typo\n' > typo/index.md
  $ camelog typo template.html typo_out
  camelog: typo/template.html: inscribe:contnet is not a placeholder, use inscribe:title, inscribe:content or inscribe:posts
  [1]

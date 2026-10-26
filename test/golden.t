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
  $ printf '<link href="seek:missing.css">[content]' > broken/template.html
  $ printf '## Broken\n' > broken/index.md
  $ camelog broken template.html broken_out > /dev/null
  camelog: broken/index.md: seek:missing.css matches no file
  [1]

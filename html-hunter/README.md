Html Hunter
===========

html-hunter is a simple but flexible text extractor for HTML pages.

html-hunter can be used to parse web pages or local HTML files and to extract
the text out of various parts of a document. The parts to be extracted are
specified by XPath path expressions. There are some predefined extractor
functions, e.g. for text in the complete body of a document, for the text in
headlines and for the title text.

The extracted text is formated in a JSON format suitable for feeding it into
the hunt search engine system to perform a free text search over these
documents. Output can be written into files or/and can be directly send to a
hunt server.

Usage
-----

```
html-hunter version 0.0.0.1, (C) Uwe Schmidt 2014

html-hunter [OPTIONS] [FILES/URLS]
  a simple HTML extractor for the Hunt search engine system

Common flags:
  -c --contexts=CONTEXT[:XPATH]  Define a context and specify the parts of a
                                 document to be indexed by an XPath selector.
                                 Several contexts may be given. An example for
                                 selecting all text within the body-part of a
                                 document is "-c body:/html/body" or shorter
                                 "-c body://body". For the predefined contexts
                                 "body", "title", "headlines" the XPath
                                 expression can be omitted, e.g "-c headlines".
                                 If no contexts are given, all the predefined
                                 contexts are used.
  -o --output=FILE               Output file, "-" for stdout.
  -s --hunt-server=URL           Hunt server url for pushing the indexed data
                                 into a hunt server.
  -p --proxy=URL                 HTTP acces via proxy.
  -r --redirect                  Allow automatic redirects.
  -M --parse-by-mime-type        Parse document by mime type (default).
  -H --parse-html                Parse document with HTML parser, ignore
                                 warnings and errors.
  -X --parse-xml                 Parse document with XML parser.
  -t --trace-level=INT           Set trace level (sensible values: 0,1,2,3).
  -? --help                      Display help message
  -V --version                   Print version information
```

Examples
--------

Use pandoc to create a html page:

```pandoc -f markdown_github --title 'Html Hunter' -t html -s -o readme.html README.md```

Html Hunter with defaults:

```html-hunter readme.html```


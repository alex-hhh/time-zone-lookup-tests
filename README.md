This is the code for the [timezone lookup performance improvements][bp] and
[more timezone lookup][bp2] blog posts.  To run the code you will need to
download the GeoJSON timezone data from the
[timezone-boundary-builder][timezone-boundary-builder] project, place it in a
"data" sub-folder, than run each example using a "racket".

```
racket tzlookup2.rkt
```

The examples from 7 onwards require the data to be prepared in advance by a
corresponding "pack" program, to prepare the data run:

```
racket tzpack9.rkt
```

Than you can run the tests multiple times:

```
racket tzlookup9.rkt
```

[timezone-boundary-builder]: https://github.com/evansiroky/timezone-boundary-builder
[bp]: https://alex-hhh.github.io/2019/08/timezone-lookup.html
[bp2]: https://alex-hhh.github.io/2019/08/timezone-lookup-2.html


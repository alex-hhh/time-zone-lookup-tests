This is the code for the [timezone lookup performance improvements][bp] blog
post.  To run the code you will need to download the GeoJSON timezone data
from the [timezone-boundary-builder][timezone-boundary-builder] project, place
it in a "data" sub-folder, than run each example using a "raco test" command:

```
raco test tzlookup2.rkt
```

[timezone-boundary-builder]: https://github.com/evansiroky/timezone-boundary-builder
[bp]: https://alex-hhh.github.io/2019/08/timezone-lookup.html


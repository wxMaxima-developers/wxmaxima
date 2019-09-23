The english manual is written by hand. The manuals in the other languages
are generated from the .po files in ../locales/manual/

The approach with the .po files might look more labourious than needed.
But it has a big advantage over just translating the whole manual once:
.po files allow the build system to tell the translator which paragraphs
are new, which ones have changed and need re-visiting and which
paragraphs of the original file were never touched since the
translation took place.

td;lr using the .po files for translating the manual means extra work
for the initial translation. But it eases up the process of keeping the
translation up-to-date.


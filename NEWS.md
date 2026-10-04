# pharmaRTF 0.1.5

No user facing API changes. Compatibility fixes for huxtable 6.0.0, which
introduced a table level `breakable` property.

### Bug Fixes
- The table body is now explicitly marked breakable before it is handed to
  `huxtable::to_rtf()`. Under huxtable 6 the default (`breakable = FALSE`)
  emitted `\trkeepfollow` on every body row, which forced a multi-page table
  onto a single page. Column headers remain unbreakable, which is correct since
  pharmaRTF repeats them on each page.
- The `tf_from_file` vignette rebuilds its stored example huxtable under the
  installed huxtable version, rather than using an object serialized before
  huxtable's newer table level properties existed.
- `test_rtf-code-generators.r` no longer asserts on huxtable's internal RTF
  encoding when checking that titles are ordered correctly.

# pharmaRTF 0.1.4

No functionality changes. Bug fix for tibble 3.1.0 update identified in https://github.com/atorus-research/pharmaRTF/issues/6

# pharmaRTF 0.1.3

No functionality changes. Increment version number with updates for development version of R.

# pharmaRTF 0.1.2

No functionality changes. Increment version with new release of Huxtable

# pharmaRTF 0.1.1

### API Changes
This version is primarily used to adjust some internal functions to be
compatilbe with huxtable 5.0. No major changes to user facing functionailty was made.
Huxtable is now a hard dependancy on pharmaRTF.

### Enhansements
The `header_row` attribute can now be set to 0 to print an `rtf_doc` with no headers. If the 'header_row' attribute is set to 
0, only the titles and footnotes are repeated 

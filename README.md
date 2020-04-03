# pharmaRTF 
<Add Shields Here>

## Overview
Enhanced RTF wrapper written in R for use with existing R tables packages such as huxtable or GT

## Installation
pharma RTF is currently not on CRAN but when it is
``` r
# Get the latest version on CRAN
install.packages(pharmaRTF)
```

## Usage
``` r
library(pharmaRTF)

rtf <- rtf_doc(ASupportedTable, titles = titles_l, footnotes = footnote_l)

write_rtf(rtf)
```

## Contribution
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests/documentation as appropriate.

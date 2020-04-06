# pharmaRTF 
<!--https://img.shields.io/azure-devops/build/atorus/32a592e8-d9a9-4ccd-bd26-65bc1c2a634d/1-->



## Overview
Enhanced RTF wrapper written in R for use with existing R tables packages
such as huxtable or GT. This package fills a gap where certain packages could
be written out as RTF, but couldn't add certain metadata or features to the
document that are required/expected in a report for a regulatory submission

This package intends to provide a flexible and reliable framework to connect
R to a pharmaceutical reporting workflow.

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

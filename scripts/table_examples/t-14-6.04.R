# T-14-6.04


library(huxtable)
library(plyr)
library(dplyr)
library(glue)
library(tidyverse)
library(haven)
library(pharmaRTF)
library(tibble)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

## Chem
adlbc <- read_xpt(glue("{adam_lib}/adlbc.xpt")) %>%
  filter(SAFETY == "Y", BLTRFL != "")
adlbh <- read_xpt(glue("{adam_lib}/adlbh.xpt")) %>%
  filter(SAFETY == "Y", BLTRFL != "")
comb <- rbind(adlbc, adlbh)

#sort tests
comb$LBTEST <-ordered(comb$LBTEST, c(sort(unique(adlbc$LBTEST)), sort(unique(adlbh$LBTEST))))
comb$TRTP <- ordered(comb$TRTP, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
comb$BLTRFL <- ordered(comb$BLTRFL, c("N", "H"))
comb$VISIT <- ordered(comb$VISIT, c(
  "WEEK 2",
  "WEEK 4",
  "WEEK 6",
  "WEEK 8",
  "WEEK 12",
  "WEEK 16",
  "WEEK 20",
  "WEEK 24",
  "WEEK 26"
))

comb2 <- comb %>%
  filter(!is.na(VISIT))

comb3 <- ddply(comb2, "LBTEST", function(test_i){

  ddply(df1, "VISIT", function(visit_i) {

    #returned as a list so they can be cbinded
    df3 <- dlply(df2, "TRTP", function(trtp_i) {
      ns <- c(
        nrow(comb2[comb2$LBTEST == test_i &
                   comb2$VISIT == visit_i &
                   comb2$TRTP == trtp_i   &
                   comb2$BLTRFL == "N", ]),
        nrow(comb2[comb2$LBTEST == test_i &
                     comb2$VISIT == visit_i &
                     comb2$TRTP == trtp_i   &
                     comb2$BLTRFL == "H", ])
      )
      norms <- c(
        nrow(comb2[comb2$LBTEST == test_i   &
                     comb2$VISIT == visit_i &
                     comb2$TRTP == trtp_i   &
                     comb2$BLTRFL == "N"    &
                     comb2$LBTRFL == "N", ]),
        nrow(comb2[comb2$LBTEST == test_i   &
                     comb2$VISIT == visit_i &
                     comb2$TRTP == trtp_i   &
                     comb2$BLTRFL == "N"    &
                     comb2$LBTRFL == "H", ])
      )
      highs <- c(
        nrow(comb2[comb2$LBTEST == test_i   &
                     comb2$VISIT == visit_i &
                     comb2$TRTP == trtp_i   &
                     comb2$BLTRFL == "H"    &
                     comb2$LBTRFL == "N", ]),
        nrow(comb2[comb2$LBTEST == test_i   &
                     comb2$VISIT == visit_i &
                     comb2$TRTP == trtp_i   &
                     comb2$BLTRFL == "H"    &
                     comb2$LBTRFL == "H", ])
      )

      data.frame(
        `Normal at Baseline` = c(
          ns[1],
          norms[1],
          highs[1]
        ),
        `High at Baseline` = c(
          ns[2],
          norms[2],
          highs[2]
        ),
        check.names = FALSE
      )
    })


  })
})



library(plyr)
library(dplyr)
library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(huxtable)
library(pharmaRTF)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

# Read in the datasets
adsl <- read_xpt(glue("{adam_lib}/adsl.xpt"))

adsl_grp <- as.data.frame(adsl %>%
  group_by(SITEGRP, SITEID, TRTP, ITT, EFFICACY, COMPLT24) %>%
  summarise(n = n()))

df <- ddply(.data = adsl_grp, .variables = "SITEID", .fun = function(x) {
  siteid_i <- unique(x[, "SITEID"])
  sitegrp_i <- unique(x[, "SITEGRP"])
  data.frame(
    SITEGRP = sitegrp_i,
    PlaITT = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                        adsl_grp$TRTP == "Placebo" &
                        adsl_grp$ITT == "Y"), "n"]),
    PlaEff = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                         adsl_grp$TRTP == "Placebo" &
                         adsl_grp$EFFICACY == "Y"), "n"]),
    PlaCom = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                         adsl_grp$TRTP == "Placebo" &
                         adsl_grp$COMPLT24 == "Y"), "n"]),
    XanLowITT = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                             adsl_grp$TRTP == "Xanomeline Low Dose" &
                             adsl_grp$ITT == "Y"), "n"]),
    XanLowEff = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                             adsl_grp$TRTP == "Xanomeline Low Dose" &
                             adsl_grp$EFFICACY == "Y"), "n"]),
    XanLowCom = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                             adsl_grp$TRTP == "Xanomeline Low Dose" &
                             adsl_grp$COMPLT24 == "Y"), "n"]),
    XanHighITT = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                              adsl_grp$TRTP == "Xanomeline High Dose" &
                              adsl_grp$ITT == "Y"), "n"]),
    XanHighEff = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                              adsl_grp$TRTP == "Xanomeline High Dose" &
                              adsl_grp$EFFICACY == "Y"), "n"]),
    XanHighCom = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                              adsl_grp$TRTP == "Xanomeline High Dose" &
                              adsl_grp$COMPLT24 == "Y"), "n"]),
    TotITT = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                          adsl_grp$ITT == "Y"), "n"]),
    TotEff = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                          adsl_grp$EFFICACY == "Y"), "n"]),
    TotCom = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                          adsl_grp$COMPLT24 == "Y"), "n"]),
    check.rows = FALSE, stringsAsFactors = FALSE
  )
}, .inform = TRUE)
df[,c(1,2)] <- df[,c(2,1)]
#sort by siteid
df[1:nrow(df),] <- df[sort(df[,2], index.return = TRUE)$ix,]
#then by grp
df[1:nrow(df),] <- df[sort(df[,1], index.return = TRUE)$ix, ]

df[nrow(df) + 1,] <- c(
  "Total",
  "",
  unname(apply(df[,3:ncol(df)], 2, sum))
)

names(df) <- c(
  "Pooled\\lineId",
  "Site\\lineId",
  rep(c("ITT", "Eff", "Com"), 4)
)

ht <- df %>%
  huxtable::as_hux(add_colnames=TRUE)

### Add Headers
headers <- adsl %>%
  group_by(ARM) %>%
  summarise(N = n()) %>%
  mutate(labels = str_replace_all(str_wrap(glue('{ARM} (N={N})'), width=10), "\n", function(x) "\\line "))
headers[4,] <- list(
  ARM = "Total",
  N = nrow(adsl),
  labels = paste0("Total\\line(N=", nrow(adsl), ")")
)



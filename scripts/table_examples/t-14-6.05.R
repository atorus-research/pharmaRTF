## 14-6.05


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

pad_row <- function(df, r) {
  #df - dataframe to insert pad
  #r - row number to pad
  for(i in seq(along = r)) {
    if(r[i] + i - 1 < nrow(df)){
      df[seq(r[i] + i, nrow(df) + 1),] <- df[seq(r[i] + (i - 1), nrow(df)),]
      df[r[i] + (i - 1),] <- NA
    } else {
      df[r[i] + (i - 1),] <- NA
    }
  }
  df
}

n_pct <- function(n, pct, n_width=3, pct_width=3) {
  n <- unlist(n)
  pct <- unique(pct)
  # n (%) formatted string. e.g. 50 ( 75%)
  unlist(lapply(n, function(x) {
    if(x == 0) " 0      "
    else {
      as.character(
        # Form the string using glue and format
        glue('{format(x, width=n_width)}({format(round((x/pct) * 100), width=pct_width)}%)')
      )
    }
  }))
}

adlbc <- read_xpt(glue("{adam_lib}/adlbc.xpt")) %>%
  filter(SAFETY == "Y", BLTRFL != "")
adlbh <- read_xpt(glue("{adam_lib}/adlbh.xpt")) %>%
  filter(SAFETY == "Y", BLTRFL != "")
comb <- rbind(adlbc, adlbh) %>%
  filter(LBTRMXFL == "Y")

#sort tests
comb$LBTEST <-ordered(comb$LBTEST, c(sort(unique(adlbc$LBTEST)), sort(unique(adlbh$LBTEST))))
comb$TRTP <- ordered(comb$TRTP, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
comb$BLTRFL <- ordered(comb$BLTRFL, c("N", "H"))
comb$LBTRFL <- ordered(comb$LBTRFL, c("N", "H"))


total_bltrfl1 <- comb%>%
  filter(!is.na(TRTP), !is.na(BLTRFL), !is.na(LBTRFL)) %>%
  group_by(LBTEST, TRTP, BLTRFL) %>%
  complete(nesting(TRTP, BLTRFL)) %>%
  summarise(N = n())
total_bltrfl <- total_bltrfl1 %>%
  mutate(LBTRFL = ordered("T", c("T", "N", "H"))) %>%
  pivot_wider(id_cols = c(LBTEST, LBTRFL), names_from = c(TRTP, BLTRFL), values_from = N)

comb2 <- comb %>%
  filter( !is.na(TRTP), !is.na(BLTRFL), !is.na(LBTRFL)) %>%
  group_by(LBTEST, TRTP, BLTRFL, LBTRFL) %>%
  complete(nesting(BLTRFL, LBTRFL)) %>%
  summarise(N = n()) %>%
  arrange(LBTEST, BLTRFL, TRTP, LBTRFL)

pvalues <- list()
for(i in seq(nrow(comb2)/12)) {
  arr <- array(
    unlist(comb2[((i-1)*12 + 1):(i * 12), "N"]),
    dim = c(2,3,2)
  )

  if(all(arr[,,2] == 0)) {
    pvalues[i] <- ""
  } else {
    pvalues[i] <- pvalue(cmh_test(as.table(arr)))
  }

}


comb3 <- comb2 %>%
  mutate(n2 = n_pct(N, total_bltrfl1[total_bltrfl1$LBTEST == LBTEST &
                                       total_bltrfl1$TRTP == TRTP     &
                                       total_bltrfl1$BLTRFL == BLTRFL, "N"], n_width = 2)) %>%
  pivot_wider(id_cols = c(LBTEST, LBTRFL), names_from = c(TRTP, BLTRFL), values_from = n2)

comb4 <- comb3[!apply(comb3, 1, function(x) {
  all(x[4:8] ==  " 0      ") & all(x[2] == "H")
}), ]


comb2$LBTRFL <- ordered(comb2$LBTRFL, c("T", "N", "H"))

total_bltrfl$Placebo_N <- num_fmt(total_bltrfl$Placebo_N, size = 2, int_len = 2)
total_bltrfl$Placebo_H <- num_fmt(total_bltrfl$Placebo_H, size = 2, int_len = 2)
total_bltrfl$`Xanomeline Low Dose_N` <- num_fmt(total_bltrfl$`Xanomeline Low Dose_N`, size = 2, int_len = 2)
total_bltrfl$`Xanomeline Low Dose_H` <- num_fmt(total_bltrfl$`Xanomeline Low Dose_H`, size = 2, int_len = 2)
total_bltrfl$`Xanomeline High Dose_N` <- num_fmt(total_bltrfl$`Xanomeline High Dose_N`, size = 2, int_len = 2)
total_bltrfl$`Xanomeline High Dose_H` <- num_fmt(total_bltrfl$`Xanomeline High Dose_H`, size = 2, int_len = 2)

comb3 <- comb2 %>%
  rbind(total_bltrfl) %>%
  arrange(LBTEST, LBTRFL)

comb3$LBTRFL <- as.character(recode(comb3$LBTRFL,
                                    "T" = "n",
                                    "N" = "Normal",
                                    "H" = "High"))

names(comb3) <- c(
  "",
  "Shift\\line[1]",
  "Normal at Baseline",
  "High at Baseline",
  "Normal at Baseline",
  "High at Baseline",
  "Normal at Baseline",
  "High at Baseline"
)

comb3 <- comb3[!apply(comb3, 1, function(x) {
  all(x[4:9] ==  " 0      ") & all(x[3] == "High")
}), ]

p_val_tests <- unlist(comb3[unlist(comb3[, 2]) == "High", ""])

comb3[unlist(comb3[, 1]) == p_val_tests[1] & unlist(comb3[, 2]) == "n", ]

comb3[unlist(comb3[, 1]) == p_val_tests[1], ]



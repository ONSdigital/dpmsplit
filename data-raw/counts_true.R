library(dplyr)
library(dpmaccpf)

gl_subnat_report_out <- dpmaccpf::gl_subnat_report_out
gl_nat_report_emig <- dpmaccpf::gl_nat_report_emig

internal <- gl_subnat_report_out %>%
  mutate(
    age = cut(age,
      breaks = c(seq(0, 90, 5), Inf),
      right = FALSE,
      labels = c(
        paste(seq(0, 85, 5),
          seq(4, 89, 5),
          sep = "-"
        ),
        "90+"
      )
    ),
    time = cut(time,
      breaks = c(0, 2015, Inf),
      labels = c(2021, 2022)
    )
  ) %>%
  count(age, time, wt = count, name = "internal")

external <- gl_nat_report_emig %>%
  mutate(
    age = cut(age,
      breaks = c(seq(0, 90, 5), Inf),
      right = FALSE,
      labels = c(
        paste(seq(0, 85, 5),
          seq(4, 89, 5),
          sep = "-"
        ),
        "90+"
      )
    ),
    time = cut(time,
      breaks = c(0, 2015, Inf),
      labels = c(2021, 2022)
    )
  ) %>%
  count(age, time, wt = count, name = "external")

counts_true <- inner_join(internal, external, by = c("age", "time"))


saveRDS(counts_true, file = "counts_true.rds")

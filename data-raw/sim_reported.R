library(dplyr)
library(dpmaccpf)

set.seed(0)

counts_true <- readRDS("counts_true.rds")

sim_reported <- counts_true %>%
  mutate(
    internal = rbinom(n = n(), size = internal, prob = 0.95),
    external = rbinom(n = n(), size = external, prob = 0.7)
  )


save(sim_reported,
  file = "../data/sim_reported.rda",
  compress = "bzip2"
)

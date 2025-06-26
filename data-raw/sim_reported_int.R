library(dplyr)
library(dpmaccpf)

set.seed(0)

counts_true <- readRDS("counts_true.rds")

sim_reported_int <- counts_true %>%
  slice(rep(1:n(), each = 2)) %>%
  data.frame(region_orig = rep(c("a", "b"))) %>%
  data.frame(region_dest = rep(c("b", "a"))) %>%
  mutate(
    count = rbinom(n = n(), size = internal, prob = 0.95),
  ) %>%
  select(-external, -internal)

save(sim_reported_int,
     file = "../data/sim_reported_int.rda",
     compress = "bzip2"
)

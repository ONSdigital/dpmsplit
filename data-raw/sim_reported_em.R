library(dplyr)
library(dpmaccpf)

set.seed(2)

counts_true <- readRDS("counts_true.rds")

sim_reported_em <- counts_true %>%
  slice(rep(1:n(), each = 2)) %>%
  data.frame(region_orig = rep(c("a", "b"))) %>%
  data.frame(region_dest = rep("x")) %>%
  mutate(
    count = rbinom(n = n(), size = internal, prob = 0.7),
  ) %>%
  select(-external, -internal)

save(sim_reported_em,
     file = "../data/sim_reported_em.rda",
     compress = "bzip2"
)

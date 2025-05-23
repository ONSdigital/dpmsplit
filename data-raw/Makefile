
.PHONY: all
all: ../data/sim_totals.rda \
     ../data/sim_reported.rda

counts_true.rds: counts_true.R
	Rscript $<

../data/sim_totals.rda: sim_totals.R \
  counts_true.rds
	Rscript $<

../data/sim_reported.rda: sim_reported.R \
  counts_true.rds
	Rscript $<

.PHONY: clean
clean:
	rm -f ../data/sim_totals.rda
	rm -f ../data/sim_reported.rda


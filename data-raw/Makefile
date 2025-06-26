
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

../data/sim_totals_in.rda: sim_totals_in.R \
  counts_true.rds
	Rscript $<

../data/sim_totals_out.rda: sim_totals_out.R \
  counts_true.rds
	Rscript $<

../data/sim_reported_int.rda: sim_reported_int.R \
  counts_true.rds
	Rscript $<

../data/sim_reported_im.rda: sim_reported_im.R \
  counts_true.rds
	Rscript $<

../data/sim_reported_em.rda: sim_reported_em.R \
  counts_true.rds
	Rscript $<

.PHONY: clean
clean:
	rm -f ../data/sim_totals.rda
	rm -f ../data/sim_reported.rda
	rm -f ../data/sim_totals_in.rda
	rm -f ../data/sim_totals_out.rda
	rm -f ../data/sim_reported_int.rda
	rm -f ../data/sim_reported_im.rda
	rm -f ../data/sim_reported_em.rda


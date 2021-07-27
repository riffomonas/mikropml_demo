.SECONDARY:

raw_data/baxter.% :
	wget https://github.com/riffomonas/raw_data/archive/refs/tags/0.3.zip
	unzip 0.3.zip
	mv raw_data-0.3 raw_data
	rm 0.3.zip

SEEDS = $(shell seq 1 1 100)
L2_GENUS_RDS = $(patsubst %,processed_data/l2_genus_%.Rds,$(SEEDS))

$(L2_GENUS_RDS) : code/run_genus_split.R code/genus_process.R\
																raw_data/baxter.metadata.tsv\
																raw_data/baxter.cons.taxonomy\
																raw_data/baxter.subsample.shared
	./code/run_genus_split.R $@

processed_data/l2_genus_performance.tsv : code/combine_models.R $(L2_GENUS_RDS)
	$^



L2_GENUS_FIT_RDS = $(patsubst %,processed_data/l2_genus_fit_%.Rds,$(SEEDS))

$(L2_GENUS_FIT_RDS) : code/run_genus_fit_split.R code/genus_process.R\
											raw_data/baxter.metadata.tsv\
											raw_data/baxter.cons.taxonomy\
											raw_data/baxter.subsample.shared
	./code/run_genus_fit_split.R $@

processed_data/l2_genus_fit_performance.tsv : code/combine_models.R\
											$(L2_GENUS_FIT_RDS)
	$^

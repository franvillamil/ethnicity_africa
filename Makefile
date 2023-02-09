.PHONY: all input_data/ged201.csv input_data/epr_groupdata.csv input_data/eosv_group_year.csv input_data/gadm36_africa.shp
.DELETE_ON_ERROR:

# Variables -------------------------------------

dataset = dataset/output/data.csv
out_models = models/glm.Rout models_1525/glm_1525.Rout models_lm/lm.Rout models_by_cr/m_cr.Rout robust/rob.Rout models_multilevel/mlm.Rout
out_effects = effect_plots/effect_plots.Rout
out_desc = descriptives/descriptives.Rout

# Main recipes ----------------------------------

all: $(dataset) $(out_models) $(out_effects) $(out_desc)

# Dataset and related ---------------------------

input_data/ab.csv: |
	unzip input_data
	curl -L -O https://ucdp.uu.se/downloads/ged/ged201-csv.zip
	unzip ged201-csv.zip
	rm ged201-csv.zip
	mv ged201.csv input_data

$(out_desc): descriptives/descriptives.R initial_subset/subset.Rout input_data/gadm36_africa.shp input_data/eosv_group_year.csv $(dataset)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out
	find ./descriptives/output -name "map*" -exec pdfcrop {} {} \;

$(dataset): dataset/dataset.R initial_subset/subset.Rout exposure_to_violence/calculate_exposure.Rout ethnic_variables/epr_variables.Rout dist_nat_capital/dist_capital.Rout
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

ethnic_variables/epr_variables.Rout: ethnic_variables/epr_variables.R input_data/epr_groupdata.csv epr_match/epr_match.Rout eosv_list/eosv.Rout
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

exposure_to_violence/calculate_exposure.Rout: exposure_to_violence/calculate_exposure.R initial_subset/subset.Rout distance_matrix/distance_matrix.Rout
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

initial_subset/subset.Rout: initial_subset/subset.R input_data/ab.csv input_data/ged201.csv
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

epr_match/epr_match.Rout: epr_match/epr_match.R initial_subset/subset.Rout input_data/epr_groupdata.csv
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

eosv_list/eosv.Rout: eosv_list/eosv.R input_data/epr_groupdata.csv input_data/eosv_group_year.csv
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

distance_matrix/distance_matrix.Rout: distance_matrix/distance_matrix.R initial_subset/subset.Rout
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

dist_nat_capital/dist_capital.Rout: dist_nat_capital/dist_capital.R initial_subset/subset.Rout
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

# Analyses --------------------------------------

models/glm.Rout: models/glm.R func/estimate.R func/sim.R func/effect.R $(dataset)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

models_1525/glm_1525.Rout: models_1525/glm_1525.R func/estimate.R func/sim.R func/effect.R $(dataset)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

models_lm/lm.Rout: models_lm/lm.R func/estimate.R func/sim.R func/effect.R $(dataset)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

models_by_cr/m_cr.Rout: models_by_cr/m_cr.R $(dataset) func/estimate_cr.R
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

robust/rob.Rout: robust/rob.R $(dataset) func/estimate.R
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

models_multilevel/mlm.Rout: models_multilevel/mlm.R $(dataset)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

$(out_effects): effect_plots/effect_plots.R func/eff_plot.R $(out_models)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

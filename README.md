# Title: Evaluation of modeling approaches to account for hunting data variability across large spatial scales 
#### Submitted to: Journal of Applied Ecology
#### Date: 30/10/2025 (dd/mm/yyyy)
#### Authors: Not provided until the article is accepted for publishing.

The following files are provided in order to make reproducible the analysis developed:
- *models_predictions_code.R*: It is a script which contains all the modeling approaches developed in the research article. Note that for running the analysis is also needed data input as well as predicion layers. Models results obtained in this step will be used as input data for making the figures.
- *modelingSpData.csv* is a subset of the HYs dataset used for modeling. The information provide in this file is:
	prec: precipitation value of each territorial unit
	hfp: human footprint value of each territorial unit
	forest: percentage of forest cover of each territorial unit,
	bioreg: the bioreg that each territorial unit corresponds to
	Var1: x centroid coordinates in kilometers 
	Var2: y centroid coordinates in kilometers
	Ss: HYs data for Sus scrofa
 	Ce: HYs data for Cervus elaphus
 	Cc: HYs data for Capreolus capreolus
 	Vv: HYs data for Vulpes vulpes
- data_modeling_predict.gpkg comprises two layers for predicting at the different spatial resolutions. municipality level and at 2 x 2 km grid cell. 
	- Municipality spatial resolution: municipalities of Spain have been downloaded from ([IGN Spain](https://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=LILIM)). Spatial statistics of the covariates are already calculated on this layer. 
	- The 2 x 2 km grid cell is one fifth of the European Environment Agency 10 x 10 km grid cell available at: ([EEA](https://www.eea.europa.eu/en/datahub/datahubitem-view/3c362237-daa4-45e2-8c16-aaadfb1a003b?activeAccordion=1238)).
	
	Both files contain the same information: 
		prec: mean precipitation value of municipality/cell.
		hfp: mean of the human footprint value of each municipality/cell.
		forest: forest cover of each municipality/cell (fraction of total area, 0-1, not percentage).
		area: surface (kilometer squares) of the municipality/cell.
		bioreg: the bioreg that the municipality/cell corresponds to.
		Var1: x centroid coordinates in kilometers. 
		Var2: y centroid coordinates in kilometers.
	
 


There are also provided the scripts used for generating the figures:

- *Figure 2.R*: Data code used to reproduce figure 2. Data inputs are the Rdata model outputs for all species and approaches. All model results per species and data scenario might be in an independent folder. There are also provided the icons of the species for reproducibility.

- *Figure 3.R*: Data code used to reproduce figure 3. Data inputs are 1) RAW data and 2) the predictions of the results in both spatial resolution (municipality and grid).

- *Figure 4.R*: Data code used to reproduce figure 4. Data inputs are  1) RAW data and 2) the predictions of the results at municipality resolution scale.

## Citation
- Authors. (submitted, 2025). Evaluation of modeling approaches to account for hunting data variability across large spatial scales. *Journal of Applied Ecology.*
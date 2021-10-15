# Drug Screening App

Shiny app for drug screening analysis comparing one drug to a baterry of drugs.

## Input file:

A input file with results from drug screening platform is needed to run the app.

Input file format (csv) whith the following columns:

* Plate
* Well ID
* Well Row
* Well Column
* Compound
* Concentration
* Count of beads
* % live cells
* % dead cells
* absolute count of live cells per well
* absoute count of dead cells per well
* Median CellTrace FR (RL1-A) of lice cells

## IC50 calculation:

Estimate IC50 for every drug by plate.

* % Live Cells
* Median Cell trace/10e5

Summary table with the IC50 calculated by drug on uM units.
Drugs evaluated are on the rows, plates are on the columns.

## Synergy Analysis:

Synergy analysis based on the SynergyFinder Plus package in R from Shuyu Zheng et al. from Reserach Program in system Oncology, Faculty of Medicine, University of Helsinki.

[SynergyFinder](https://www.bioconductor.org/packages/release/bioc/html/synergyfinder.html)

4 synergy scoring are computed.

* __Highest Single Agent (HSA):__ states that the expected combination effect equals to the higher effect of individual drugs.
* __Bliss model (Bliss):__ assumes a stochastic process in which two drugs exert their effects independently, and the expected combination effect can be calculated based on the probability of independent events.
* __Loewe additivity model (Loewe):__ is based on the assumption that no compound interacts with itself and that two doses from different compounds having the same effect are equivalent.
* __Zero Interaction Potency (ZIP):__ calculates the expected effect of two drugs under the assumption that they do not potentiate each other, i.e. both the assumptions of the Loewe model and the Bliss model are met.

### Synergy plots:

* __a:__ Heatmap form the dose response matrix, columns are the concentration of the drug 1, rows the concentration of the drug 2. Numbers inside indicate the % of inhibition. Mean/Median indicate the mean/median percentage inhibition of all the possible combinations for the two drugs.
* __b:__ Heatmap form the Synergy Score (chose by user), columns are the concentration of the drug 1, rows the concentration of the drug 2. Numbers inside indicate the Synergy score.
* __c:__ Summary barplots:
  + concentration drug 1.
  + concentration drug 2.
  + % of inhibition.
  + ZIP score for every concentration drug combination.
  + Loewe score for every concentration drug combination.
  + HSA score for every concentration drug combination.
  + Bliss score for every concentration drug combination.
* __d:__ Barometer plot, barometer for given concentration combination (max ZIP synergy score by concentration 1 and 2) in a matrix. The needle of the barometer points to the observed response value. The expected responses from different models are marked as the ticks on the color bar. The observed response and the concentration of the combined drugs are tested at the center of the barometer.

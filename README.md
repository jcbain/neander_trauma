# Neanderthal Trauma
## a reanalysis of *Berger & Trinkaus (1995)*

This project is an extended analysis to see if the unique trauma patterns found on Neanderthals match a larger sampling base. Data come from the National Electronic Injury Surveillance System (NEISS) and has primarily been filtered down to injuries related to sports activities.
### Contents
+ `NEISS_data_cleanup.ipynb`: basic data carpentry of NEISS data.
+ `NEISS_data_configuration.ipynb`: notebook to return data in a workable format.
+ `exploratory.R`: Exploration of features within the data.
+ `body_part_combiner.ipynb`: Granularizes the "lower trunk" category into separate sub-categories (hip and lower back) for re-aggregation.
+ `body_part_joining.R`: Join into seven body part groups defined by *Berger & Trinkaus*.
+ `analysis.R`: Final analysis file, primarily dealing with several Chi Square tests.

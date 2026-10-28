# campsismisc 1.1.0

* Reference subject in forest plots (add caption) #31
* Refactor Campsismisc: OAT outputs -> NCA Analysis #68
* Update README #78
* Labeled covariates should be moved to Campsismod #79
* Parameter labels should be loaded from the Campsis model #80
* Prepare package for future submission on CRAN #82
* Review the use of Campsis settings in package #83
* Review ForestPlot and SensitivityAnalysis constructors #84
* Review the 'prepare' function (-> now 'run_analysis') #85
* Remove deprecated methods #86
* Create a JSON interface for the forest plots #87
* Outputs should not be stored in the forest plots or sensitivity analysis anymore #88
* Create a JSON interface for the sensitivity analyses #89
* Migrate package to campsisverse organisation #90
* Add 'arm_index' argument to 'run_analysis' method (+ logic) #94

# campsismisc 1.0.0

* Migrate campsismisc to snake_case #64
* Review package dependencies #65
* Review Campsismisc output function #66
* Remove filter argument of NcaMetricOutput #67
* Remove generic method postProcessScenarios #69
* Review list of exported functions #70
* Remove argument suffix from NcaMetricOutput #71
* Get rid of plyr #72
* Update NEWS #73
* Prepare package for CRAN submission #74
* Format code with Air #75

# campsismisc 0.6.0

* Update package due to campsisnca rework #62

# campsismisc 0.5.3

* The label.size argument of geom_label() is deprecated #58

# campsismisc 0.5.2

* Set up code coverage report with codecov.io #53
* Add logo and badges to README #54
* Add NEWS file to repository #55

# campsismisc 0.5.1

* Slowness detected on get_forest_plot/get_tornado_plot call #51

# campsismisc 0.5.0

* Sensitivity analysis change: add 'where' argument (equation in model or theta) #48
* Sensitivity analysis change: add 'log' argument #49

# campsismisc 0.4.4

* Review call to Campsis simulation #46

# campsismisc 0.4.3

* Slighly adapt the tornado plot item #44 (previous release version was not updated)

# campsismisc 0.4.2

* Slighly adapt the tornado plot item #44

# campsismisc 0.4.1

* Make tests run with both rxode2 and mrgsolve #39
* Show labels should be optional in tornado plots #40
* Add argument 'limits' to get_tornado_plot and get_forest_plot #41
* Deactivate progress for the simulation of the base scenario #42

# campsismisc 0.4.0

* NA in item when unit of continuous covariate is not specified #32
* Include GPLv3 license #33
* Issue when simulating only 1 item/scenario #34
* Remove level argument from calculate method #35

# campsismisc 0.3.0

* Parallel computation not working anymore #29
* Update package due to refactoring of campsisnca #27

# campsismisc 0.2.2

* Strange regression in R4.3.1 when sampling parameter uncertainty with Campsis/mvrnorm #21
* Rerun README #23
* Setup GitHub actions workflow #24
* Fix R CMD check #25

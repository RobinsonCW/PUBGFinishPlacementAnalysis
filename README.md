# PUBGFinishPlacementAnalysis

Created by [Chance Robinson](https://github.com/RobinsonCW), [Allison Roderick](https://github.com/allroderick) and [William Arnost](https://github.com/warnost)

## Purpose
Prediction of top 10% finish placement of PUBG players


## Analysis Files

* [PUBG Placement Analysis (html)](https://github.com/RobinsonCW/PUBGFinishPlacementAnalysis/blob/master/src/PUBG-Placement-Analysis.pdf): A paper covering the analysis. 
* [PUBG Placement Analysis (Rmd)](https://github.com/RobinsonCW/PUBGFinishPlacementAnalysis/blob/master/src/PUBG%20Placement%20Analysis.Rmd): An R-markdown file with the analysis.


## Codebook
The [Codebook](https://github.com/RobinsonCW/PUBGFinishPlacementAnalysis/blob/master/CodeBook.md) provides additional details regarding the session information of the environment.


## Data

The dataset [readme](https://github.com/RobinsonCW/PUBGFinishPlacementAnalysis/blob/master/data/README.md) provides a reference for the source files.

* [`pubg_solo_game_types.csv`](https://github.com/RobinsonCW/PUBGFinishPlacementAnalysis/blob/master/data/pubg_solo_game_types.csv): Filtered for solo only game types
* [`pubg_solo_game_types_test_full.csv`](https://github.com/RobinsonCW/PUBGFinishPlacementAnalysis/blob/master/data/pubg_solo_game_types_test_full.csv): Pre-split for test data
* [`pubg_solo_game_types_train_full.csv`](https://github.com/RobinsonCW/PUBGFinishPlacementAnalysis/blob/master/data/pubg_solo_game_types_train_full.csv): Pre-split for train data without downsampling for the unbalanced response variable
* [`pubg_solo_game_types_train_downsampled.csv`](https://github.com/RobinsonCW/PUBGFinishPlacementAnalysis/blob/master/data/pubg_solo_game_types_train_downsampled.csv): Pre-split for train data with downsampling for the unbalanced response variable


## Repo Structure
    .
    ├── data                                # Raw data for EDA and analysis questions
    ├── munge                               # Helper functions for src code
    ├── src                                 # Primary analysis files
    |    ├── exploratory_data_analysis      # R-markdown files for EDA
    |    ├── objectives                     # R-markdown files for primary analysis questions
    ├── CodeBook.md                         # Session information regarding the environment
    ├── LICENSE                             # All code and analysis is licensed under the MIT license.
# Script for assembling a Galactic Polymath interdisciplinary lesson plan
# ver 0.3.0

require(remotes)
install_github("galacticpolymath/galacticPubs")
install_github("galacticpolymath/galacticEdTools")
library(galacticPubs);library(galacticEdTools)

#Run the GP lesson editor
editor()
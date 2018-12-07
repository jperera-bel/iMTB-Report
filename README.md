iMTB Report Shiny app
=====================

This is the source code for the Shiny application "iMTB Report". 

The Shiny app relies on the method described in [Perera-Bel et al., 2018](https://doi.org/10.1186/s13073-018-0529-2), and uses the functions available at [MTB-Report repository](https://github.com/jperera-bel/MTB-Report). If you use this method, please cite our publication ([Perera-Bel et al., 2018](https://doi.org/10.1186/s13073-018-0529-2)).

The current release of "iMTB Report" Shiny app uses the latest version of MTB-Report (v1.1.0), which includes the following database versions:

- [Gene Drug Knowledge Database](https://www.synapse.org/#!Synapse:syn2370773): Dienstmann et al., Cancer discovery 5.2 (2015), v20.0
- [CIViC](https://civic.genome.wustl.edu/): Griffith et al., Nat Genet (2017), release 01-Dec-2018
- [TARGET](http://archive.broadinstitute.org/cancer/cga/target): Van Allen et al., Nat Med (2014), v3
- Meric-Bernstam et al., J Natl Cancer Inst. (2015)



Dependencies
------------
```r
install.packages("shiny")
install.packages("shinyjs")
install.packages("DT")
install.packages("shinydashboard")
install.packages("knitr")
install.packages("stringr")
install.packages("xtable")
install.packages("ggplot2")
install.packages("pander")
install.packages("timeSeries")
install.packages("tools")
devtools::install_github("mariodeng/FirebrowseR")
```

Requires LaTeX an Texinfo for the generation of PDF reports. In Linux, install them with:
```
sudo apt-get install texlive-full
sudo apt-get install texinfo
```


Installation
------------

If you are not familiar with R, you can visit our [iMTB Report webpage](http://www.ams.med.uni-goettingen.de:3838/iMTB-Report/app).

Alternatively, you can run it as an stand-alone application in your local computer. Be aware of the [dependencies](#dependencies).
There are many ways to download and run it:

```R
library(shiny)
library(shinyjs)

# Easiest way is to use runGitHub
runGitHub("iMTB-Report", "jperera-bel")

# Run a tar or zip file directly
runUrl("https://github.com/jperera-bel/iMTB-Report/archive/master.tar.gz")
runUrl("https://github.com/jperera-bel/iMTB-Report/archive/master.zip")
```

Or you can clone the git repository, then use `runApp()`:

```R
# First clone the repository with git. If you have cloned it into
# ~/iMTB-Report, first go to that directory, then use runApp().
setwd("~/iMTB-Report")
library(shiny)
library(shinyjs)
runApp()
```





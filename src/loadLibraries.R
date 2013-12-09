###Load Libraries 
require(plyr)
require(XML)
require(RCurl)
require(lubridate)
require(Zelig)
require(plm)
require(tseries)
require(lmtest)
require(MASS)
require(pscl)
require(xtable)
require(texreg)
require(glmmADMB)
require(qcc)

##glmmADMB has problems installing from cran directly, use this source
#install.packages("glmmADMB", 
#                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
#                        getOption("repos")),type="source")
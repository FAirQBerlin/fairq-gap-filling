################################################################################
# This script starts the ETL Process for gap fillling                          #
#                                                                              #
# Job Runs at kubernetes                                                       #
#                                                                              #
# Schedule: hourly                                                             #
#                                                                              #
# Author: Milan Flach                                                          #
# E-mail: milan.flach@inwt-statistics.de                                       #
################################################################################


# 00 Preparation ---------------------------------------------------------------
cat("System information:\n")
for (i in seq_along(sysinfo <- Sys.info()))
  cat("  ", names(sysinfo)[i], ":", sysinfo[i], "\n")
options(warn = 2)

library(fairqGapFilling)
library(methods)
sessionInfo()

# 01 Start ETL -----------------------------------------------------------------
# futile.logger::flog.threshold(futile.logger::DEBUG)
status <- main()

q(save = "no", status = status)

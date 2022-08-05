library(CommKern)

################################################################################
#         check that initial checks in the function results in errors          #

# too few spins
x <- tryCatch(hms(SBM_net, spins = 1), error = function(e) {e})
stopifnot(inherits(x, "simpleError"))
stopifnot(x$message == "Must provide a number of spins within [2,number of nodes in network]")

# too many spins
x <- tryCatch(hms(SBM_net, spins = 100), error = function(e) {e})
stopifnot(inherits(x, "simpleError"))
stopifnot(x$message == "Must provide a number of spins within [2,number of nodes in network]")


################################################################################
#                                 End of File                                  #
################################################################################


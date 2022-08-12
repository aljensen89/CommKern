library(CommKern)

# Verify expected error handles - too few spins
x <- tryCatch(hms(SBM_net, spins = 1), 
              error = function(e) {e})
stopifnot(inherits(x, "simpleError"))
stopifnot(x$message == "Must provide a number of spins within [2,number of nodes in network]")

# Verify expected error handles - too many spins
x <- tryCatch(hms(SBM_net, spins = 100), 
              error = function(e) {e})
stopifnot(inherits(x, "simpleError"))
stopifnot(x$message == "Must provide a number of spins within [2,number of nodes in network]")

# Verify expected error handles - coolfact too low
x <- tryCatch(hms(SBM_net,spins=2,coolfact=-0.5), 
              error = function(e) {e})
stopifnot(inherits(x, "simpleError"))
stopifnot(x$message == "Must provide a temperature cooling factor within (0,1)")

# Verify expected error handles - coolfact too high
x <- tryCatch(hms(SBM_net,spins=2,coolfact=1.09), 
              error = function(e) {e})
stopifnot(inherits(x, "simpleError"))
stopifnot(x$message == "Must provide a temperature cooling factor within (0,1)")

# Verify expected error handles - alpha is not strictly positive
x <- tryCatch(hms(SBM_net,spins=2,coolfact=0.985,alpha=-1), 
              error = function(e) {e})
stopifnot(inherits(x, "simpleError"))
stopifnot(x$message == "Must provide a strictly positive alpha value")

# Verify expected error handles - max layers is >=1
x <- tryCatch(hms(SBM_net,spins=2,coolfact=0.99,alpha=0,max_layers=0),
              error = function(e) {e})
stopifnot(inherits(x, "simpleError"))
stopifnot(x$message == "Must provide a max number of layers greater than one")

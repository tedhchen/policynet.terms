# Input is list or list of lists. 
# The single or each lower level list has two elements, 
# each a vector of attribute levels, for the sender and receiver, respectively.

InitErgmTerm.sendreceive_attr_combo <- function(nw, arglist, ...){
  ### Checking arguments
  a <- check.ErgmTerm(nw, arglist,
                      varnames = c('attr', 'combos'),
                      vartypes = c(ERGM_VATTR_SPEC, 'list'),
                      defaultvalues = list(NULL, NULL),
                      required = c(T, T)
                      )
  
  ### Initializing
  n <- network.size(nw)
  nodecov <- ergm_get_vattr(a$attr, nw)
  combospace <- ergm_attr_levels(NULL, nodecov, nw)
  combos <- a$combos
  
  # Checking if levels are properly specified
  if(any(!unique(unlist(combos)) %in% seq(1, length(combospace)))){
    stop("One or more specified level is outside of existing levels.",call. = FALSE)
  }
  
  # making sure single specified combo works with general form
  if(mode(combos[[1]]) != 'list'){
    combos <- list(combos)
  }
  
  ### Constructing matrix based on supplied node attributes and sender-receiver combinations
  
  # Function: for each pair of sender-receiver combination, check to see if each pair on the network satisfies it
  parse_list <- function(l){
    senders <- combospace[l[[1]]]
    receivers <- combospace[l[[2]]]
    mat <- outer(nodecov %in% senders, nodecov %in% receivers)
    mat
  }
  
  # Parse sender-receiver combinations
  pl <- lapply(combos, parse_list)
  
  # If any of the sender-receiver combinations is satisfied, set as 1
  xm <- ifelse(Reduce('+', pl) > 0, 1, 0)
  
  ### Construct the list to return (from the `ergm` package)
  if(!is.null(a$attr)) {
    # Note: the sys.call business grabs the name of the x object from the 
    # user's call.  Not elegant, but it works as long as the user doesn't
    # pass anything complicated.
    cn<-paste("sendreceive_attr_combo", as.character(a$attr), sep = ".")
  } else {
    cn<-paste("sendreceive_attr_combo", as.character(sys.call(0)[[3]][2]), sep = ".")
  }
  inputs <- c(NCOL(xm), as.double(xm))
  attr(inputs, "ParamsBeforeCov") <- 1
  list(name="edgecov", coef.names = cn, inputs = inputs, dependence=FALSE,
       minval = sum(c(xm)[c(xm)<0]),
       maxval = sum(c(xm)[c(xm)>0]),
       pkgname = "ergm"
  )
}
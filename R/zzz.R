.onAttach <- function(libname, pkgname) {
  desc<- packageDescription(pkgname, libname)
  packageStartupMessage(
    '======================\n',
    'Package:  policynet_terms\n', 
    'Version:  ', desc$Version, '\n', 
    'Date:     ', desc$Date, '\n', 
    'Author:   Ted Hsuan Yun Chen (Aalto University and University of Helsinki)\n'
  )
  packageStartupMessage("Parts of this package builds on 'statnet' project software (statnet.org). For license and citation information see statnet.org/attribution.")
}

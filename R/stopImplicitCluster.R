if (exists("stopImplicitCluster", envir = asNamespace("doParallel"), inherits = FALSE)) {
  doParallel::stopImplicitCluster()
}

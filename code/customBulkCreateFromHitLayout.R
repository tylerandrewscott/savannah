
customBulkCreateFromHITLayout <-
  function (hitlayoutid, input, annotation, verbose = FALSE, ...) 
{
  HITs <- list()
#  if (length(annotation) != 1) {
#    annotation <- rep(annotation[1], nrow(input))
#  }
  a <- list(...)
  if (!"hit.type" %in% names(a)) {
    hittypeargs <- unique(c(names(formals(MTurkR::RegisterHITType)), 
                            names(formals(MTurkR::request))))
    hittypeargs <- hittypeargs[hittypeargs != "..."]
    register <- do.call("RegisterHITType", a[names(a) %in% 
                                               hittypeargs])
    if (!as.logical(register$Valid)) {
      stop("Could not RegisterHITType(), check parameters")
    }
  }
  input[] <- lapply(input, as.character)
  for (i in 1:nrow(input)) {
    if (!"hit.type" %in% names(a)) {
      HITs[[i]] <- CreateHIT(hitlayoutid = hitlayoutid, 
                             hitlayoutparameters = GenerateHITLayoutParameter(names = names(input), 
                                                                              values = unlist(input[i, , drop = TRUE])), 
                             annotation = annotation[i], hit.type = register$HITTypeId, 
                             verbose = verbose, ...)
    }
    else {
      HITs[[i]] <- CreateHIT(hitlayoutid = hitlayoutid, 
                             hitlayoutparameters = GenerateHITLayoutParameter(names = NULL, 
                                                                              values = unlist(input[i, , drop = TRUE])), 
                             annotation = annotation, verbose = verbose, 
                             ...)
    }
  }
  return(HITs)
}
optimx <- function(par, fn, gr=NULL, hess=NULL, lower=-Inf, upper=Inf, 
            method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
            control=list(),
             ...) {

  optcfg <- optimx.setup(par=par, fn=fn, gr=gr, hess=hess, lower=lower, upper=upper, 
            method=method, itnmax=itnmax, hessian=hessian, control=control, ...)
# Parse and use optcfg
  if (optcfg$ctrl$starttests) {
    optchk <- optimx.check(par=par, ufn=optcfg$ufn, ugr=optcfg$ugr, uhess=optcfg$uhess, lower=lower,
           upper=upper, hessian=hessian, ctrl=optcfg$ctrl, have.bounds=optcfg$have.bounds,
           usenumDeriv=optcfg$usenumDeriv, ...)
  }
  optcfg$ctrl$have.bounds<-optcfg$have.bounds # to pass boundedness
  if (! is.null(control$trace) && control$trace > 1) {
    cat("optcfg:")
    print(optcfg)
  }
  ansout <- optimx.run(par=par, ufn=optcfg$ufn, optcfg$ugr, uhess=optcfg$uhess, lower=lower, upper=upper,
            method=optcfg$method, itnmax=itnmax, hessian=hessian, ctrl=optcfg$ctrl, ...)
  details <- attr(ansout, "details")
  attr(ansout, "details") <- NULL ## Not sure this necessary, but null here and replace below
  if (optcfg$ctrl$maximize) {
     if (optcfg$ctrl$trace>0) cat("Reversing sign on objective, gradient, & hessian\n")
     ansout$value <- - ansout$value
     nlist<-dim(details)[[1]]
     for (i in 1:nlist) {
        details[[i,"ngatend"]] <- - details[[i,"ngatend"]] 
        details[[i,"nhatend"]] <- - details[[i,"nhatend"]] 
        details[[i,"hev"]] <- - details[[i,"hev"]] 
     }
  }
  rownames(details) <- details[, "method"]
  ##JN -- don't remove method: 
  ##JN  details <- details[, colnames(details) != "method", drop=FALSE]
  # Fix kkt test output to logical
    ansout[ , "kkt1"] <- as.logical(ansout[ , "kkt1"])
    ansout[ , "kkt2"] <- as.logical(ansout[ , "kkt2"])

  answer <- structure(list(par=ansout, details = details, maximize = optcfg$ctrl$maximize,
            npar = optcfg$npar, follow.on=optcfg$ctrl$follow.on),
            class = c("optimx", "data.frame"))
  answer # requested by Gabor 1408
} ## end of optimx


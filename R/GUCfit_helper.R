norm_lse <- function(x) {
  # normalize a vector of log probabilities
  k <- max(x, na.rm = T)
  if (is.infinite(k)){
    z <- k
  } else {
    z <- log(sum(exp(x - k), na.rm=T)) + k
  }
  x - z
}


gucfit_nb <- function(nm_y, nm_x, nm_mc, dat,
                      ucs, gcs, alp = 0.1, ...){


  calc_ent <- function(ps){
    ps <- ps/sum(ps)
    -sum(ps*log(ps))
  }

  datY <-  factor(dat[, nm_y], ucs)
  datX <- dat[, nm_x]
  datMC <- dat[, nm_mc]


  # P(Y), H(Y)
  tab_y <- table(datY) + alp
  h_y <- calc_ent(tab_y)
  lp_y <- norm_lse(log(tab_y))
  lp_y <- t(as.matrix(lp_y))

  # P(X|Y)
  tab_x_y<- lapply(nm_x, function(m){
    table(datY, datX[,m]) + alp
  })
  lp_x_y <- lapply(tab_x_y, function(m){
    lpmat <- log(m)
    apply(lpmat, 1, norm_lse)
  })
  # Information gain: IG(Y, X) = H(Y) - (H(Y, X) - H(X))
  ig_x_y <- sapply(tab_x_y, function(m){
    h_y - (calc_ent(as.vector(m)) - calc_ent(as.vector(colSums(m))) )
  })
  names(lp_x_y) <- names(ig_x_y) <- nm_x

  # P(MC|Y)
  if(length(nm_mc) > 0){
    tmp <- lapply(nm_mc, function(m) table(datY, datMC[,m]) )
    tab_mc_y <- Reduce(`+`, tmp) + alp
    lp_mc_y <- apply(log(tab_mc_y), 1, norm_lse)

    # Information gain: IG(Y, X) = H(Y) - (H(Y, X) - H(X))
    ig_mc_y <- h_y - (calc_ent(as.vector(tab_mc_y)) - calc_ent(as.vector(colSums(tab_mc_y))) )
    names(ig_mc_y) <- "multi(.)"
    ig_x_y <- c(ig_x_y, ig_mc_y)
  } else {
    lp_mc_y <- NULL
  }

  nk <- length(ucs)
  if(! is.matrix(lp_y) | NROW(lp_y) != 1) stop("incorrect row dimension for lp_y")
  if(NCOL(lp_y) != nk) stop("incorrect col dimension for lp_y")
  if(! all(sapply(lp_x_y, is.matrix))) stop("lp_x_y are not all matrices")
  if(! all(sapply(lp_x_y, NCOL) == nk)) stop("incorrect col dimension for lp_x_y")
  if(! is.null(lp_mc_y)){
    if(NCOL(lp_mc_y) != nk) stop("incorrect col dimension for lp_mc_y")
  }

  structure(
    list(
      lp_ = list(
        lp_y = lp_y, lp_x_y = lp_x_y, lp_mc_y = lp_mc_y
      ),
      nms = list(
        nm_y = nm_y, nm_x = nm_x, nm_mc = nm_mc
      ),
      ig = ig_x_y, # information gain
      h_y = h_y, # marginal entropy
      nk = nk,
      alp = alp,
      dat = dat
    )
    , class = "gucfit_nb")
}


predict.gucfit_nb <- function(fit, newdata = NULL,
                              log.p = T){

  if(is.null(newdata)) newdata <- fit$dat

  lp_mat <- sapply(1:NROW(newdata), function(i){

    lpos <- t(fit$lp_$lp_y)

    lik_x <- sapply(fit$nms$nm_x, function(xv){
      fit$lp_$lp_x_y[[xv]][newdata[[xv]][i],]
    })

    mcs <- sapply(fit$nms$nm_mc, function(m) newdata[[m]][i])
    mcs <- as.character(mcs[!is.na(mcs)])

    if(length(mcs) > 0){
      lik_mc <- sapply(mcs, function(m){
        fit$lp_$lp_mc_y[m,]
      })
      lpos <- cbind(lpos, lik_x, lik_mc)
    } else{
      lpos <- cbind(lpos, lik_x)
    }
    norm_lse(rowSums(lpos))
  })

  if(!log.p) lp_mat <- exp(lp_mat)

  return(t(lp_mat))
}

summary.gucfit_nb <- function(fit){
  cat("Variable importance:\n")
  cat("\t Variable \t Info. gain \t Scaled (%) \n")
  cat("\t -------- \t ---------- \t ---------- \n")
  for(k in 1:length(fit$ig)) cat("\t",formatC(names(fit$ig)[k], width = 8),
                                 "\t", formatC(round(fit$ig[k], 3), width = 10),
                                 "\t", formatC(round(100*fit$ig[k]/fit$h_y, 2), width = 10), "\n")
}

#' @importFrom nnet multinom
gucfit_mlr <- function(nm_y, nm_x, nm_mc, dat,
                      ucs, gcs, maxit = 100, ...){

  dat[[nm_y]] <- factor(as.character(dat[[nm_y]]), levels = ucs)

  if(!is.null(nm_mc)){
    for(i in nm_mc) dat[[i]] <- as.character(dat[[i]])

    mclv <- unique(unlist(dat[, nm_mc]))
    mclv <- sort(mclv[!is.na(mclv)])

    mcdf <- sapply(1:NROW(dat), function(i){
      x <- structure(rep(0, length(mclv)), names = mclv)
      x[which(mclv %in% dat[i, nm_mc])] <- 1
      x
    })
    mcdf <- as.data.frame(t(mcdf))

    nm_mc_d <- paste0("mcd_", mclv)
    colnames(mcdf) <- nm_mc_d

    fl <- as.formula(paste(
      nm_y, " ~ ",
      paste0(nm_x, collapse = " + "), " + ",
      paste0(nm_mc_d, collapse = " + ")
    ))

    dat1 <- cbind(dat, mcdf)
  } else{
    mcdf <- mclv <- nm_mc_d <- NULL
    fl <- as.formula(paste(
      nm_y, " ~ ",
      paste0(nm_x, collapse = " + ")
    ))
    dat1 <- dat
  }

  fit <- nnet::multinom(fl,  data = dat1,
                        MaxNWts = Inf,
                        maxit = maxit, ...)
  structure(
    list(
      nnet_fit = fit,
      mclv = mclv,
      nms = list(
        nm_y = nm_y, nm_x = nm_x, nm_mc = nm_mc
      ),
      nk = length(ucs),
      dat = dat
    )
    , class = "gucfit_mlr")
}

predict.gucfit_mlr <- function(fit, newdata, log.p = T){

  if(missing(newdata)){

    p_mat <- fit$nnet_fit$fitted.values

  } else{

    if(!is.null(fit$mclv)){
      mclv <- fit$mclv
      nm_mc <- fit$nms$nm_mc

      mcdf <- sapply(1:NROW(newdata), function(i){
        x <- structure(rep(0, length(mclv)), names = mclv)
        x[which(mclv %in% newdata[i, nm_mc])] <- 1
        x
      })
      mcdf <- as.data.frame(t(mcdf))
      nm_mc_d <- paste0("mcd_", mclv)
      colnames(mcdf) <- nm_mc_d

      newdata1 <- cbind(newdata, mcdf)
    } else{
      mcdf <- NULL
      newdata1 <- newdata
    }


    p_mat <- predict(fit$nnet_fit, newdata = newdata1, type = "probs")
  }

  if(log.p) p_mat <- log(p_mat)

  return(p_mat)
  }

summary.gucfit_mlr <- function(fit){
}

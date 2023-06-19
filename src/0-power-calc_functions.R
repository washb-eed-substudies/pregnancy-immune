
#glm function
fit_glm <- function (d, Y, X, W = NULL, forcedW = NULL, V = NULL, id = "clusterid", family = "gaussian", pval = 0.2, print = TRUE){
  
  cat("\nNon-prescreened covariates: ", paste(forcedW, 
                                              sep = "", collapse = ", "), "\n")
  set.seed(12345)
  require(mgcv)
  require(dplyr)
  require(faraway)
  if (!is.null(V)) {
    require(lmtest)
  }
  if (!is.null(W)) {
    W <- subset(d, select = W)
  }
  Y <- subset(d, select = Y)
  colnames(Y) <- "Y"
  X <- subset(d, select = X)
  colnames(X) <- "X"
  id <- subset(d, select = id)
  colnames(id) <- "id"
  if (!is.null(V)) {
    Vvar <- subset(d, select = V)
    colnames(Vvar) <- "V"
  }
  else {
    Vvar <- data.frame(V = rep(1, nrow(d)))
  }
  collinear_vars <- NULL
  if (!is.null(W)) {
    gamdat <- data.frame(Y, X, id, Vvar, W)
  }
  else {
    gamdat <- data.frame(Y, X, id, Vvar)
  }
  if (!is.null(W)) {
    if (sum(is.na(forcedW)) != 0) {
      colnamesW <- names(W)
    }
    else {
      if (is.null(forcedW)) {
        Wnames <- names(W)
        forcedW <- c(Wnames[Wnames == "tr" | grepl("age_", 
                                                   Wnames) | grepl("agedays_", Wnames) | 
                              grepl("ageday_", Wnames)])
      }
      cat("\nNon-prescreened covariates: ", paste(forcedW, 
                                                  sep = "", collapse = ", "), "\n")
      colnamesW <- names(W)[!(names(W) %in% forcedW)]
    }
    screenW <- subset(gamdat, select = colnamesW)
  }
  else {
    screenW <- NULL
  }
  if (!is.null(screenW)) {
    if (print == TRUE) {
      cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    }
    suppressWarnings(Wscreen <- washb_prescreen(Y = gamdat$Y, 
                                                Ws = screenW, family = family, pval = pval, print = print))
    if (!is.null(forcedW)) {
      Wscreen <- c(as.character(Wscreen), as.character(forcedW))
    }
    W <- subset(gamdat, select = Wscreen)
    Wdf <- W
    Wdf$constant <- rep(1, nrow(gamdat))
    for (i in 1:ncol(W)) {
      tmp <- glm(constant ~ ., data = Wdf, family = family)
      todrop <- NULL
      todrop <- suppressWarnings(names(tmp$coefficients)[-1][as.vector(vif(tmp)) > 
                                                               10][1])
      if (!is.null(todrop) & !is.na(todrop)) {
        collinear_vars <- c(collinear_vars, todrop)
        Wdf <- Wdf[, colnames(Wdf) != todrop]
      }
    }
    to_keep <- colnames(W)[!(colnames(W) %in% collinear_vars)]
    if (length(to_keep) != length(colnames(W))) {
      cat("\nDropped for collinearity with other covariates:\n", 
          colnames(W)[!(colnames(W) %in% to_keep)])
    }
    W_processed <- W[which(colnames(W) %in% to_keep)]
    Wscreen <- colnames(W_processed)
    cat("\n\nCovariated included in model:\n", Wscreen)
  }
  else {
    Wscreen = NULL
  }
  if (!is.null(Wscreen)) {
    d <- subset(gamdat, select = c("Y", "X", 
                                   "id", "V", Wscreen))
  }
  else {
    d <- subset(gamdat, select = c("Y", "X", 
                                   "id", "V"))
  }
  fullrows <- nrow(d)
  d <- d %>% filter(!is.na(Y))
  Yrows <- nrow(d)
  cat("\nRows dropped due to missing outcome: ", fullrows - 
        Yrows, "\n")
  d <- d %>% filter(!is.na(X))
  Xrows <- nrow(d)
  cat("Rows dropped due to missing exposure: ", Yrows - 
        Xrows, "\n")
  if (!is.null(W) & length(Wscreen) > 0) {
    cat("Percent missingness by covariate:\n")
    print(sapply(d[, -c(1:3)], function(x) round(sum(is.na(x))/nrow(X) * 
                                                   100, 1)))
    d <- d[complete.cases(d), ]
    cat("\nRows dropped due to missing covariates: ", 
        Xrows - nrow(d), "\n")
  }
  cat("Final sample size: ", nrow(d), "\n")
  d$dummy <- 1
  if (!is.null(W) & length(Wscreen) > 0) {
    Ws <- subset(gamdat, select = c(Wscreen))
    W_factors <- colnames(Ws)[(grepl("factor", sapply(Ws, 
                                                      class)) | grepl("character", sapply(Ws, class)))]
    W_numeric <- colnames(Ws)[(grepl("integer", sapply(Ws, 
                                                       class)) | grepl("numeric", sapply(Ws, class)))]
    indicator_vec <- rep(TRUE, length(W_numeric))
    for (i in 1:length(W_numeric)) {
      N_unique <- length(unique(Ws[, W_numeric[i]]))
      if (N_unique > 20) {
        indicator_vec[i] <- FALSE
      }
    }
    W_indicator <- W_numeric[indicator_vec]
    W_continious <- W_numeric[!indicator_vec]
    if (length(W_continious) > 0) {
      eq_num <- paste0(W_continious, 
                       collapse = " + ")
    }
    else {
      eq_num = NULL
    }
    if (length(W_factors) + length(W_indicator) > 0) {
      eq_fact <- paste0(" + ", paste0(c(W_factors, 
                                        W_indicator), collapse = " + "))
    }
    else {
      eq_fact = NULL
    }
    if (length(unique(d$X)) > 2) {
      if (!is.null(V)) {
        form <- paste0("Y~X + V + X*V+", 
                       eq_fact, " +", eq_num)
        form <- gsub("+ +", "+", form, fixed = TRUE)
        equation <- as.formula(form)
        form_null <- paste0("Y~X + V + ", 
                            eq_fact, " +", eq_num)
        form_null <- gsub("+ +", "+", form_null, 
                          fixed = TRUE)
        equation_null <- as.formula(form_null)
      }
      else {
        form <- paste0("Y~X+", eq_fact, 
                       " +", eq_num)
        form <- gsub("+ +", "+", form, fixed = TRUE)
        equation <- as.formula(form)
      }
    }
    else {
      if (!is.null(V)) {
        form <- paste0("Y~X+ V + X*V +", eq_fact, 
                       " +", eq_num)
        form <- gsub("+ +", "+", form, fixed = TRUE)
        equation <- as.formula(form)
        form_null <- paste0("Y~X+ V + ", eq_fact, 
                            " +", eq_num)
        form_null <- gsub("+ +", "+", form_null, 
                          fixed = TRUE)
        equation_null <- as.formula(form_null)
      }
      else {
        form <- paste0("Y~X+", eq_fact, " +", 
                       eq_num)
        form <- gsub("+ +", "+", form, fixed = TRUE)
        equation <- as.formula(form)
      }
    }
    fit <- lm(formula = equation, data = d)
    #fit<-arm::standardize(fit)
    
    if (!is.null(V)) {
      fit_null <- lm(formula = equation_null, data = d)
      LRp <- lrtest(fit, fit_null)[2, 5]
    }
  }
  else {
    if (length(unique(d$X)) > 2) {
      if (!is.null(V)) {
        equation <- as.formula(paste0("Y~X + V + X*V + s(id,bs=\"re\",by=dummy)"))
        fit <- lm(formula = equation, data = d)
        equation_null <- as.formula(paste0("Y~X + V + s(id,bs=\"re\",by=dummy)"))
        fit_null <- lm(formula = equation_null, 
                              data = d)
        LRp <- lrtest(fit, fit_null)[2, 5]
      }
      else {
        fit <- lm(Y ~ X, data = d)
      }
    }
    else {
      if (!is.null(V)) {
        equation <- as.formula(paste0("Y~X + V + X*V + s(id,bs=\"re\",by=dummy)"))
        fit <- lm(formula = equation, data = d)
        equation_null <- as.formula(paste0("Y~X + V + s(id,bs=\"re\",by=dummy)"))
        fit_null <- lm(formula = equation_null, 
                              data = d)
        LRp <- lrtest(fit, fit_null)[2, 5]
      }
      else {
        fit <- lm(Y ~ X , data = d)
      }
    }
  }
  if (!is.null(V)) {
    cat("\nInteraction p-value: ", LRp, "\n")
    return(list(fit = fit, dat = d, int.p = LRp, covars = Wscreen, 
                collinear_vars = collinear_vars))
  }
  else {
    return(list(fit = fit, dat = d, covars = Wscreen, collinear_vars = collinear_vars))
  }
}

predict_glm_power <- function(fit, d, Xvar, Yvar){
  
  #fit <- arm::standardize(fit, standardize.y = TRUE)
  res <- summary(fit)
  R2 <- res$r.squared
  N_predictors <- nrow(res$coefficients)-1
  
  #res <- res$p.table[2,]
  res <- res$coefficients[2,]
  res <- data.frame(t(res))
  
  lb.diff <- res$Estimate - 1.96 * res$Std..Error
  ub.diff <- res$Estimate + 1.96 * res$Std..Error
  standardized_coef <- lm.beta(fit,complete.standardization=TRUE)$coefficients[2]
  

  res <- data.frame(Y = Yvar, X = Xvar, N = nrow(d), standardized_coef=standardized_coef, point.diff=res$Estimate, lb.diff = lb.diff, ub.diff = ub.diff, Pval = res$Pr...t.., R2=R2, N_predictors=N_predictors)
  res$power <- pwrss.z.reg(beta1 = res$standardized_coef, alpha = 0.05,
                           alternative = "not equal",
                           k = res$N_predictors, 
                           #r2 = res$R2,
                           n = res$N)$power
    
  return(list(res = res))
}

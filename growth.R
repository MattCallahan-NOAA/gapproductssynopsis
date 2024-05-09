fit_vb_mc <- function(dat,
                   sex = c("female", "male", "all"),
                   method = "tmb",
                   downsample = Inf,
                   chains = 4L,
                   iter = 1000L,
                   cores = parallel::detectCores(),
                   allow_slow_mcmc = FALSE,
                   est_method = median,
                   min_samples = 50L,
                   too_high_quantile = 1.0,
                   uniform_priors = FALSE,
                   ageing_method_codes = NULL,
                   usability_codes = c(0, 1, 2, 6),
                   check_convergence_tmb = TRUE,
                   tmb_inits = list(k = 0.5, linf = 40, log_sigma = log(0.1), t0 = -1),
                   ...) {
 
  
  # if (!is.null(usability_codes)) {
  #   dat <- filter(dat, .data$usability_code %in% usability_codes)
  # }
  # 
  
  # if (!is.null(ageing_method_codes)) {
  #   dat <- filter(dat, .data$ageing_method %in% ageing_method_codes)
  # }
  # dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]
  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length_mm), !is.na(.data$age))
  ql <- quantile(dat$length_mm, probs = too_high_quantile)
  dat <- filter(dat, length_mm <= ql)
  
  sex <- match.arg(sex)
  dat <- switch(sex,
                "female" = filter(dat, sex == 2L),
                "male" = filter(dat, sex == 1L),
                "all" = dat
  )
  
  if (nrow(dat) < min_samples) {
    return(list(
      predictions = tibble(
        ages = NA, length_mm = NA
      ),
      pars = list(k = NA, linf = NA, t0 = NA),
      data = dat,
      model = NA
    ))
  }
  
  if (nrow(dat) > downsample) {
    dat <- dat[sample(seq_len(nrow(dat)), downsample), , drop = FALSE]
  }

  
  if (method == "tmb") {
    dlls <- getLoadedDLLs()
    if (!any(vapply(dlls, function(x) x[["name"]] == "vb", FUN.VALUE = TRUE))) {
      lib.f <- system.file("tmb", "vb.cpp", package = "gfplot")
      .f <- "vb_gfplot.cpp"
      if (!file.exists(.f)) {
        file.copy(lib.f, to = .f)
      }
      TMB::compile(.f)
      dyn.load(TMB::dynlib("vb_gfplot"))
    }

    data <- list(len = dat$length_mm, age = dat$age)
    
    obj <- TMB::MakeADFun(data, tmb_inits, DLL = "vb_gfplot", silent = TRUE)
    opt <- stats::nlminb(obj$par, obj$fn, obj$gr)
    if (opt$convergence != 0L && check_convergence_tmb)
      stop("VB growth model did not converge!")
    pars <- as.list(opt$par)
    m <- obj
  }
  
  vb <- function(ages, linf, k, t0) linf * (1 - exp(-k * (ages - t0)))
  ages <- seq(min(dat$age), max(dat$age), length.out = 200L)
  pred <- vb(ages, linf = pars$linf, k = pars$k, t0 = pars$t0)
  
  list(
    predictions = tibble(age = ages, length_mm = pred),
    pars = pars, data = as_tibble(dat), model = m
  )
}

############ TESTING ###########
# I get an error with TMB::compile(.f) 
# Error in system(paste(MAKE, p1(paste("-f", shQuote(makefiles))), "compilers"),  : 
#                   'make' not found
# Moving to a loess smooth for now.

sink(tempfile())
vb_m<-fit_vb(dat=gap_specimen%>%
               mutate(usability_code=1,
                      length=length_mm),
             sex = "male", method = "tmb",
             too_high_quantile = 1, check_convergence_tmb = check_convergence_tmb,
             tmb_inits = tmb_init
)


# Growth

vb_m <- fit_vb_mc(dat=gap_specimen%>%
                    mutate(usability_code=1),
                  
                  sex = "male", method = "tmb",
                  too_high_quantile = 1, check_convergence_tmb = check_convergence_tmb,
                  tmb_inits = tmb_init
)
vb_f <- fit_vb_mc(dat$survey_samples,
               sex = "female", method = "tmb",
               too_high_quantile = 1, check_convergence_tmb = check_convergence_tmb,
               tmb_inits = tmb_init
)


################################################################################
#fit length weigth
################################################################################


fit_length_weight <- function(dat,
                              sex = c("female", "male", "all"),
                              downsample = Inf,
                              min_samples = 50L,
                              method = c("tmb", "rlm", "lm"),
                              df = 3,
                              too_high_quantile = 1.0,
                              usability_codes = c(0, 1, 2, 6),
                              scale_weight = 1 / 1000) {
  if ("species_common_name" %in% names(dat)) {
    if (length(unique(dat$species_common_name)) != 1L) {
      stop("Multiple species detected via the `species_common_name` column. ",
           "fit_length_weight() is for use with a single species. Filter the data yourself ",
           "first.",
           call. = FALSE
      )
    }
  }
  
  if (!is.null(usability_codes)) {
    dat <- filter(dat, .data$usability_code %in% usability_codes)
  }
  
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]
  
  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length), !is.na(.data$weight))
  ql <- quantile(dat$length, probs = too_high_quantile)
  qw <- quantile(dat$weight, probs = too_high_quantile)
  dat <- filter(dat, length <= ql, weight <= qw)
  
  dat <- switch(sex[[1]],
                "female" = filter(dat, sex == 2),
                "male" = filter(dat, sex == 1),
                "all" = dat,
                stop("`sex` argument must be 'female' or 'male' or 'all'.", call. = FALSE)
  )
  
  dat$weight <- dat$weight * scale_weight
  
  if (nrow(dat) < min_samples) {
    return(list(
      predictions = tibble(length = NA, weight = NA),
      pars = list(log_a = NA, b = NA),
      data = dat,
      model = NA
    ))
  }
  
  method <- match.arg(method)
  
  if (method == "rlm") {
    m <- MASS::rlm(log(weight) ~ log(length), data = dat)
    pars <- as.list(coef(m))
    pars <- stats::setNames(pars, c("log_a", "b"))
  }
  
  if (method == 'lm') {
    m <- stats::lm(log(weight) ~ log(length), data = dat)
    pars <- as.list(coef(m))
    pars <- stats::setNames(pars, c("log_a", "b"))
  }
  
  if (method == "tmb") {
    dlls <- getLoadedDLLs()
    if (!any(vapply(dlls, function(x) x[["name"]] == "lw", FUN.VALUE = TRUE))) {
      lib.f <- system.file("tmb", "lw.cpp", package = "gfplot")
      .f <- "lw_gfplot.cpp"
      if (!file.exists(.f)) {
        file.copy(lib.f, to = .f)
      }
      TMB::compile(.f)
      dyn.load(TMB::dynlib("lw_gfplot"))
    }
    data <- list(len = log(dat$length), weight = log(dat$weight), df = df)
    parameters <- list(log_a = 0, b = 0, log_sigma = 0)
    obj <- TMB::MakeADFun(data, parameters, DLL = "lw_gfplot", silent = TRUE)
    opt <- stats::nlminb(obj$par, obj$fn, obj$gr)
    if (opt$convergence != 0L)
      stop("Length-weight model did not converge!")
    pars <- as.list(opt$par)
    m <- obj
  }
  
  pred <- tibble(length = seq(min(dat$length), max(dat$length), length.out = 200L))
  pred$weight <- exp(pars$log_a + pars$b * log(pred$length))
  
  list(predictions = pred, pars = pars, data = as_tibble(dat), model = m)
}


#############################################################################
# https://danstich.github.io/we-r-nycafs/fishStats.html
library(FSA)

ageplot_m<-gap_specimen %>%
  filter(age>0 & length_mm >0 & sex == 1)

ageplot_f<-gap_specimen %>%
  filter(age>0 & length_mm >0 & sex == 2)

vbmod <- length_mm ~ Linf * (1 - exp(-K * (age - t0)))

startsm <- vbStarts(formula = length_mm ~ age, data = ageplot_m)
startsf <- vbStarts(formula = length_mm ~ age, data = ageplot_f)

age_modm <-nls(vbmod, data = ageplot_m, start = startsm)
age_modf <-nls(vbmod, data = ageplot_f, start = startsf)

predm <- predict(age_modm)
predf <- predict(age_modf)

ageplot_m$pred <-predm
ageplot_f$pred <-predf


length_at_age_plot<-ggplot()+
  geom_jitter(data=ageplot_dat, aes(x=age, y=length_mm), width = 0.1, alpha = 0.15, size = 2)+
  geom_line(data=ageplot_m, aes(x=age, y=pred), color=mcolor)+
  geom_line(data=ageplot_f, aes(x=age, y=pred), color=fcolor)+
  theme_bw()
length_at_age_plot

dEXPic <-
function (x, log = FALSE, ...) 
{
    if (!is.Surv(x)) 
        stop(paste("the x variable is not a Surv object"))
    dfun0 <- ifelse(x[, "status"] == 0, cdf(x[, 1], lower.tail = F, 
        log.p = T, ...), 0)
    dfun1 <- ifelse(x[, "status"] == 1, pdf(x[, 1], log = TRUE, 
        ...), 0)
    dfun2 <- ifelse(x[, "status"] == 2, cdf(x[, 1], log.p = T, 
        ...), 0)
    suppressWarnings(dfun3 <- ifelse(x[, "status"] == 3, log(cdf(x[, 
        2], ...) - cdf(x[, 1], ...)), 0))
    dfun <- dfun0 + dfun1 + dfun2 + dfun3
    dfun <- if (log == TRUE) 
        dfun
    else exp(dfun)
    dfun
}
dEXPlc <-
function (x, log = FALSE, ...) 
{
    if (!is.Surv(x)) 
        stop(paste("the x variable is not a Surv object"))
    dfun <- ifelse(x[, "status"] == 1, pdf(x[, 1], log = TRUE, 
        ...), log(cdf(x[, 1], ...)))
    dfun <- if (log == TRUE) 
        dfun
    else exp(dfun)
    dfun
}
dEXPrc <-
function (x, log = FALSE, ...) 
{
    if (!is.Surv(x)) 
        stop(paste("the x variable is not a Surv object"))
    dfun <- ifelse(x[, "status"] == 1, pdf(x[, 1], log = TRUE, 
        ...), log(1 - cdf(x[, 1], ...)))
    dfun <- if (log == TRUE) 
        dfun
    else exp(dfun)
    dfun
}
dLINic <-
function (x, log = FALSE, ...) 
{
    if (!is.Surv(x)) 
        stop(paste("the x variable is not a Surv object"))
    dfun0 <- ifelse(x[, "status"] == 0, cdf(x[, 1], lower.tail = F, 
        log.p = T, ...), 0)
    dfun1 <- ifelse(x[, "status"] == 1, pdf(x[, 1], log = TRUE, 
        ...), 0)
    dfun2 <- ifelse(x[, "status"] == 2, cdf(x[, 1], log.p = T, 
        ...), 0)
    suppressWarnings(dfun3 <- ifelse(x[, "status"] == 3, log(cdf(x[, 
        2], ...) - cdf(x[, 1], ...)), 0))
    dfun <- dfun0 + dfun1 + dfun2 + dfun3
    dfun <- if (log == TRUE) 
        dfun
    else exp(dfun)
    dfun
}
dLINlc <-
function (x, log = FALSE, ...) 
{
    if (!is.Surv(x)) 
        stop(paste("the x variable is not a Surv object"))
    dfun <- ifelse(x[, "status"] == 1, pdf(x[, 1], log = TRUE, 
        ...), log(cdf(x[, 1], ...)))
    dfun <- if (log == TRUE) 
        dfun
    else exp(dfun)
    dfun
}
dLINrc <-
function (x, log = FALSE, ...) 
{
    if (!is.Surv(x)) 
        stop(paste("the x variable is not a Surv object"))
    dfun <- ifelse(x[, "status"] == 1, pdf(x[, 1], log = TRUE, 
        ...), log(1 - cdf(x[, 1], ...)))
    dfun <- if (log == TRUE) 
        dfun
    else exp(dfun)
    dfun
}
EXPic <-
function (mu.link = "log") 
{
    mstats <- checklink("mu.link", "Exponential", substitute(mu.link), 
        c("inverse", "log", "sqrt", "identity"))
    structure(list(family = c("EXPic", "interval censored Exponential"
    ), parameters = list(mu = TRUE), nopar = 1, type = "Continuous", 
        mu.link = as.character(substitute(mu.link)), mu.linkfun = mstats$linkfun, 
        mu.linkinv = mstats$linkinv, mu.dr = mstats$mu.eta, dldm = function (y, 
            mu) 
        attr(gamlss::numeric.deriv(dEXPic(y, mu, log = TRUE), 
            "mu", delta = NULL), "gradient"), d2ldm2 = function(mu) (-1/mu^2), 
        G.dev.incr = function (y, mu, ...) 
        -2 * dEXPic(x = y, mu = mu, log = TRUE), rqres = expression(
            rqres(pfun = "pEXPic ", censored = "interval", type = "Continuous", 
                y = y, mu = mu)), mu.initial = expression(mu <- (y[, 
            1] + mean(y[, 1]))/2), mu.valid = function(mu) all(mu > 
            0), y.valid = function (y) 
        all(y[, 1] > 0), mean = function(mu) mu, variance = function(mu) mu^2), 
        class = c("gamlss.family", "family"))
}
EXPlc <-
function (mu.link = "log") 
{
    mstats <- checklink("mu.link", "Exponential", substitute(mu.link), 
        c("inverse", "log", "sqrt", "identity"))
    structure(list(family = c("EXPlc", "left censored Exponential"
    ), parameters = list(mu = TRUE), nopar = 1, type = "Continuous", 
        mu.link = as.character(substitute(mu.link)), mu.linkfun = mstats$linkfun, 
        mu.linkinv = mstats$linkinv, mu.dr = mstats$mu.eta, dldm = function (y, 
            mu) 
        attr(gamlss::numeric.deriv(dEXPlc(y, mu, log = TRUE), 
            "mu", delta = NULL), "gradient"), d2ldm2 = function(mu) (-1/mu^2), 
        G.dev.incr = function (y, mu, ...) 
        -2 * dEXPlc(x = y, mu = mu, log = TRUE), rqres = expression(
            rqres(pfun = "pEXPlc ", censored = "left", type = "Continuous", 
                y = y, mu = mu)), mu.initial = expression(mu <- (y[, 
            1] + mean(y[, 1]))/2), mu.valid = function(mu) all(mu > 
            0), y.valid = function (y) 
        all(y[, 1] > 0), mean = function(mu) mu, variance = function(mu) mu^2), 
        class = c("gamlss.family", "family"))
}
EXPrc <-
function (mu.link = "log") 
{
    mstats <- checklink("mu.link", "Exponential", substitute(mu.link), 
        c("inverse", "log", "sqrt", "identity"))
    structure(list(family = c("EXPrc", "right censored Exponential"
    ), parameters = list(mu = TRUE), nopar = 1, type = "Continuous", 
        mu.link = as.character(substitute(mu.link)), mu.linkfun = mstats$linkfun, 
        mu.linkinv = mstats$linkinv, mu.dr = mstats$mu.eta, dldm = function (y, 
            mu) 
        attr(gamlss::numeric.deriv(dEXPrc(y, mu, log = TRUE), 
            "mu", delta = NULL), "gradient"), d2ldm2 = function(mu) (-1/mu^2), 
        G.dev.incr = function (y, mu, ...) 
        -2 * dEXPrc(x = y, mu = mu, log = TRUE), rqres = expression(
            rqres(pfun = "pEXPrc ", censored = "right", type = "Continuous", 
                y = y, mu = mu)), mu.initial = expression(mu <- (y[, 
            1] + mean(y[, 1]))/2), mu.valid = function(mu) all(mu > 
            0), y.valid = function (y) 
        all(y[, 1] > 0), mean = function(mu) mu, variance = function(mu) mu^2), 
        class = c("gamlss.family", "family"))
}
LINic <-
function (mu.link = "log") 
{
    mstats <- checklink("mu.link", "Lindely", substitute(mu.link), 
        c("inverse", "log", "sqrt", "identity"))
    structure(list(family = c("LINic", "interval censored Lindley"
    ), parameters = list(mu = TRUE), nopar = 1, type = "Continuous", 
        mu.link = as.character(substitute(mu.link)), mu.linkfun = mstats$linkfun, 
        mu.linkinv = mstats$linkinv, mu.dr = mstats$mu.eta, dldm = function (y, 
            mu) 
        attr(gamlss::numeric.deriv(dLINic(y, mu, log = TRUE), 
            "mu", delta = NULL), "gradient"), d2ldm2 = function(mu) 1/(mu + 
            1)^2 - 2/mu^2, G.dev.incr = function (y, mu, ...) 
        -2 * dLINic(x = y, mu = mu, log = TRUE), rqres = expression(
            rqres(pfun = "pLINic ", censored = "interval", type = "Continuous", 
                y = y, mu = mu)), mu.initial = expression(mu <- rep((-(mean(y[, 
            1]) - 1) + sqrt((mean(y[, 1]) - 1)^2 + 8 * mean(y[, 
            1])))/(2 * mean(y[, 1])), length(y[, 1]))), mu.valid = function(mu) all(mu > 
            0), y.valid = function (y) 
        all(y[, 1] > 0), mean = function(mu) (mu + 2)/(mu * (mu + 
            1)), variance = function(mu) 2 * (mu + 3)/(mu^2 * 
            (mu + 1)) - ((mu + 2)/(mu * (mu + 1)))^2), class = c("gamlss.family", 
        "family"))
}
LINlc <-
function (mu.link = "log") 
{
    mstats <- checklink("mu.link", "Lindely", substitute(mu.link), 
        c("inverse", "log", "sqrt", "identity"))
    structure(list(family = c("LINlc", "left censored Lindley"
    ), parameters = list(mu = TRUE), nopar = 1, type = "Continuous", 
        mu.link = as.character(substitute(mu.link)), mu.linkfun = mstats$linkfun, 
        mu.linkinv = mstats$linkinv, mu.dr = mstats$mu.eta, dldm = function (y, 
            mu) 
        attr(gamlss::numeric.deriv(dLINlc(y, mu, log = TRUE), 
            "mu", delta = NULL), "gradient"), d2ldm2 = function(mu) 1/(mu + 
            1)^2 - 2/mu^2, G.dev.incr = function (y, mu, ...) 
        -2 * dLINlc(x = y, mu = mu, log = TRUE), rqres = expression(
            rqres(pfun = "pLINlc ", censored = "left", type = "Continuous", 
                y = y, mu = mu)), mu.initial = expression(mu <- rep((-(mean(y[, 
            1]) - 1) + sqrt((mean(y[, 1]) - 1)^2 + 8 * mean(y[, 
            1])))/(2 * mean(y[, 1])), length(y[, 1]))), mu.valid = function(mu) all(mu > 
            0), y.valid = function (y) 
        all(y[, 1] > 0), mean = function(mu) (mu + 2)/(mu * (mu + 
            1)), variance = function(mu) 2 * (mu + 3)/(mu^2 * 
            (mu + 1)) - ((mu + 2)/(mu * (mu + 1)))^2), class = c("gamlss.family", 
        "family"))
}
LINrc <-
function (mu.link = "log") 
{
    mstats <- checklink("mu.link", "Lindely", substitute(mu.link), 
        c("inverse", "log", "sqrt", "identity"))
    structure(list(family = c("LINrc", "right censored Lindley"
    ), parameters = list(mu = TRUE), nopar = 1, type = "Continuous", 
        mu.link = as.character(substitute(mu.link)), mu.linkfun = mstats$linkfun, 
        mu.linkinv = mstats$linkinv, mu.dr = mstats$mu.eta, dldm = function (y, 
            mu) 
        attr(gamlss::numeric.deriv(dLINrc(y, mu, log = TRUE), 
            "mu", delta = NULL), "gradient"), d2ldm2 = function(mu) 1/(mu + 
            1)^2 - 2/mu^2, G.dev.incr = function (y, mu, ...) 
        -2 * dLINrc(x = y, mu = mu, log = TRUE), rqres = expression(
            rqres(pfun = "pLINrc ", censored = "right", type = "Continuous", 
                y = y, mu = mu)), mu.initial = expression(mu <- rep((-(mean(y[, 
            1]) - 1) + sqrt((mean(y[, 1]) - 1)^2 + 8 * mean(y[, 
            1])))/(2 * mean(y[, 1])), length(y[, 1]))), mu.valid = function(mu) all(mu > 
            0), y.valid = function (y) 
        all(y[, 1] > 0), mean = function(mu) (mu + 2)/(mu * (mu + 
            1)), variance = function(mu) 2 * (mu + 3)/(mu^2 * 
            (mu + 1)) - ((mu + 2)/(mu * (mu + 1)))^2), class = c("gamlss.family", 
        "family"))
}
pEXPic <-
function (q, log = FALSE, ...) 
{
    if (!is.Surv(q)) 
        stop(paste("the q variable is not a Surv object"))
    pfun1 <- cdf(q[, 1], ...)
    pfun2 <- runif(length(q[, 1]), 0, pfun1)
    pfun0 <- runif(length(q[, 1]), pfun1, 1)
    suppressWarnings(pfun3 <- runif(length(q[, 1]), cdf(q[, 1], 
        ...), cdf(q[, 2], ...)))
    pfun0 <- ifelse(q[, "status"] == 0, pfun0, 0)
    pfun1 <- ifelse(q[, "status"] == 1, pfun1, 0)
    pfun2 <- ifelse(q[, "status"] == 2, pfun2, 0)
    pfun3 <- ifelse(q[, "status"] == 3, pfun3, 0)
    dfun <- pfun0 + pfun1 + pfun2 + pfun3
    dfun
}
pEXPlc <-
function (q, log = FALSE, ...) 
{
    if (!is.Surv(q)) 
        stop(paste("the q variable is not a Surv object"))
    pfun1 <- cdf(q[, 1], ...)
    pfun2 <- runif(length(q[, 1]), 0, pfun1)
    pfun <- ifelse(q[, "status"] == 1, pfun1, pfun2)
    pfun
}
pEXPrc <-
function (q, log = FALSE, ...) 
{
    if (!is.Surv(q)) 
        stop(paste("the q variable is not a Surv object"))
    pfun1 <- cdf(q[, 1], ...)
    pfun2 <- runif(length(q[, 1]), pfun1, 1)
    pfun <- ifelse(q[, "status"] == 1, pfun1, pfun2)
    pfun
}
pLINic <-
function (q, log = FALSE, ...) 
{
    if (!is.Surv(q)) 
        stop(paste("the q variable is not a Surv object"))
    pfun1 <- cdf(q[, 1], ...)
    pfun2 <- runif(length(q[, 1]), 0, pfun1)
    pfun0 <- runif(length(q[, 1]), pfun1, 1)
    suppressWarnings(pfun3 <- runif(length(q[, 1]), cdf(q[, 1], 
        ...), cdf(q[, 2], ...)))
    pfun0 <- ifelse(q[, "status"] == 0, pfun0, 0)
    pfun1 <- ifelse(q[, "status"] == 1, pfun1, 0)
    pfun2 <- ifelse(q[, "status"] == 2, pfun2, 0)
    pfun3 <- ifelse(q[, "status"] == 3, pfun3, 0)
    dfun <- pfun0 + pfun1 + pfun2 + pfun3
    dfun
}
pLINlc <-
function (q, log = FALSE, ...) 
{
    if (!is.Surv(q)) 
        stop(paste("the q variable is not a Surv object"))
    pfun1 <- cdf(q[, 1], ...)
    pfun2 <- runif(length(q[, 1]), 0, pfun1)
    pfun <- ifelse(q[, "status"] == 1, pfun1, pfun2)
    pfun
}
pLINrc <-
function (q, log = FALSE, ...) 
{
    if (!is.Surv(q)) 
        stop(paste("the q variable is not a Surv object"))
    pfun1 <- cdf(q[, 1], ...)
    pfun2 <- runif(length(q[, 1]), pfun1, 1)
    pfun <- ifelse(q[, "status"] == 1, pfun1, pfun2)
    pfun
}
qEXPic <-
function (p, mu = 1, lower.tail = TRUE, log.p = FALSE) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(p < 0) | any(p > 1)) 
        stop(paste("p must be between 0 and 1", "\n", ""))
    q <- qexp(p, rate = 1/mu, lower.tail = lower.tail, log.p = log.p)
    q
}
qEXPlc <-
function (p, mu = 1, lower.tail = TRUE, log.p = FALSE) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(p < 0) | any(p > 1)) 
        stop(paste("p must be between 0 and 1", "\n", ""))
    q <- qexp(p, rate = 1/mu, lower.tail = lower.tail, log.p = log.p)
    q
}
qEXPrc <-
function (p, mu = 1, lower.tail = TRUE, log.p = FALSE) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(p < 0) | any(p > 1)) 
        stop(paste("p must be between 0 and 1", "\n", ""))
    q <- qexp(p, rate = 1/mu, lower.tail = lower.tail, log.p = log.p)
    q
}
qLINic <-
function (p, mu, lower.tail = TRUE, log.p = FALSE) 
{
    stopifnot(mu > 0)
    if (lower.tail) {
        t1 <- 1 + mu
        t4 <- exp(-t1)
        t6 <- lambertWm1(t1 * (p - 1) * t4)
        qtf <- -(t6 + 1 + mu)/mu
    }
    else {
        t1 <- 1 + mu
        t3 <- exp(-t1)
        t5 <- lambertWm1(-p * t1 * t3)
        qtf <- -(t5 + 1 + mu)/mu
    }
    if (log.p) 
        return(log(qtf))
    else return(qtf)
}
qLINlc <-
function (p, mu, lower.tail = TRUE, log.p = FALSE) 
{
    stopifnot(mu > 0)
    if (lower.tail) {
        t1 <- 1 + mu
        t4 <- exp(-t1)
        t6 <- lambertWm1(t1 * (p - 1) * t4)
        qtf <- -(t6 + 1 + mu)/mu
    }
    else {
        t1 <- 1 + mu
        t3 <- exp(-t1)
        t5 <- lambertWm1(-p * t1 * t3)
        qtf <- -(t5 + 1 + mu)/mu
    }
    if (log.p) 
        return(log(qtf))
    else return(qtf)
}
qLINrc <-
function (p, mu, lower.tail = TRUE, log.p = FALSE) 
{
    stopifnot(mu > 0)
    if (lower.tail) {
        t1 <- 1 + mu
        t4 <- exp(-t1)
        t6 <- lambertWm1(t1 * (p - 1) * t4)
        qtf <- -(t6 + 1 + mu)/mu
    }
    else {
        t1 <- 1 + mu
        t3 <- exp(-t1)
        t5 <- lambertWm1(-p * t1 * t3)
        qtf <- -(t5 + 1 + mu)/mu
    }
    if (log.p) 
        return(log(qtf))
    else return(qtf)
}

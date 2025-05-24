pool.vector <- function(w, dfcom = Inf, custom.t = NULL,
                        rule = c("rubin1987", "reiter2003")) {
  # rubin1987: Rubin's rules for scalar estimates
  # reiter2003: Reiter's rules for partially synthetic data
  rule <- match.arg(rule)

  present <- hasName(w, c("estimate", "std.error"))
  if (!all(present)) {
    stop("Column(s) not found: ",
         paste(c("estimate", "std.error")[!present], collapse = ", "))
  }
  grp <- intersect(names(w),
                   c("term", "parameter", "contrast", "y.level", "component"))
  if (!length(grp)) {
    warning("No parameter names found. Add a column named `term`.")
  }
  dfcom <- ifelse(hasName(w, "df.residual"), w[["df.residual"]], dfcom)
  if (is.infinite(dfcom)) {
    warning("Large sample assumed.")
  }

  # Convert to factor to preserve ordering
  if (hasName(w, "term"))
    w$term <- factor(w$term, levels = unique(w$term))
  if (hasName(w, "parameter"))
    w$parameter <- factor(w$parameter, levels = unique(w$parameter))
  if (hasName(w, "contrast"))
    w$contrast <- factor(w$contrast, levels = unique(w$contrast))
  if (hasName(w, "y.level"))
    w$y.level <- factor(w$y.level, levels = unique(w$y.level))
  if (hasName(w, "component"))
    w$component <- factor(w$component, levels = unique(w$component))

  # Prefer using robust.se when tidy object contains it
  if (hasName(w, "robust.se"))
    w$std.error <- w$robust.se

  # There we go..
  if (rule == "rubin1987") {
    pooled <- w %>%
      group_by(!!!syms(grp)) %>%
      summarize(
        m = n(),
        qbar = mean(.data$estimate),
        ubar = mean(.data$std.error^2),
        b = var(.data$estimate),
        t = ifelse(is.null(custom.t),
                   .data$ubar + (1 + 1 / .data$m) * .data$b,
                   eval(parse(text = custom.t))),
        dfcom = dfcom,
        df = barnard.rubin(.data$m, .data$b, .data$t, .data$dfcom),
        riv = (1 + 1 / .data$m) * .data$b / .data$ubar,
        lambda = (1 + 1 / .data$m) * .data$b / .data$t,
        fmi = (.data$riv + 2 / (.data$df + 3)) / (.data$riv + 1)
      )
  }

  if (rule == "reiter2003") {
    pooled <- w %>%
      group_by(!!!syms(grp)) %>%
      summarize(
        m = n(),
        qbar = mean(.data$estimate),
        ubar = mean(.data$std.error^2),
        b = var(.data$estimate),
        t = ifelse(is.null(custom.t),
                   .data$ubar + (1 / .data$m) * .data$b,
                   eval(parse(text = custom.t))),
        dfcom = dfcom,
        df = (.data$m - 1) * (1 + (.data$ubar / (.data$b / .data$m)))^2,
        riv = (1 + 1 / .data$m) * .data$b / .data$ubar,
        lambda = NA_real_,
        fmi = NA_real_
      )
  }

  pooled <- data.frame(pooled)
  names(pooled)[names(pooled) == "qbar"] <- "estimate"
  return(pooled)
}

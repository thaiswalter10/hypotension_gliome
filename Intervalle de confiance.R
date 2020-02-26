# Calcul d'un interval de confiance pour un test glm 
confidence_interval <- function (estimate, sd, alpha = 0.975) {
  lower_limit = exp(estimate - qnorm(alpha) * sd)
  upper_limit = exp(estimate + qnorm(alpha) * sd)
  limit = c(lower_limit = lower_limit, upper_limit = upper_limit)
  return(limit)
}
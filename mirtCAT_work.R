library('mirtCAT')
options(stringsAsFactors = FALSE)

# define population IRT parameters
set.seed(1234)
nitems <- 120
itemnames <- paste0("Item.", 1:nitems)
a <- matrix(c(rlnorm(nitems/2, 0.2, 0.3), rnorm(nitems/4, 0, 0.3), numeric(nitems/2),
              rnorm(nitems/4, 0, 0.3), rlnorm(nitems/2, 0.2, 0.3)), nitems)
d <- matrix(rnorm(nitems))
pars <- data.frame(a, d)
colnames(pars) <- c("a1", "a2", "d")
trait_cov <- matrix(c(1, 0.5, 0.5, 1), 2, 2)

# create mirt_object
mod <- generate.mirt_object(pars, itemtype = "2PL", latent_covariance = trait_cov)

# math items definitions addition for one factor and multiplication for the other
questions <- answers <- character(nitems)
options <- matrix("", nitems, 5)
spacing <- floor(d - min(d)) + 1  #easier items have more variation

for (i in 1:nitems) {
  if (i < 31) {
    # addition
    n1 <- sample(1:100, 1)
    n2 <- sample(101:200, 1)
    ans <- n1 + n2
    questions[i] <- paste0(n1, " + ", n2, " = ?")
  } else if (i < 61) {
    # addition and multiplication
    n1 <- sample(1:50, 1)
    n2 <- sample(51:100, 1)
    m1 <- sample(1:10, 1)
    m2 <- sample(1:10, 1)
    ans <- n1 + n2 + m1 * m2
    questions[i] <- paste0(n1, " + ", n2, " + ", m1, " * ", m2, " = ?")
  } else if (i < 91) {
    # multiplication and addition
    n1 <- sample(1:10, 1)
    n2 <- sample(1:10, 1)
    m1 <- sample(1:25, 1)
    m2 <- sample(1:25, 1)
    ans <- n1 + n2 + m1 * m2
    questions[i] <- paste0(m1, " * ", m2, " + ", n1, " + ", n2, " = ?")
  } else {
    # multiplication
    m1 <- sample(1:50, 1)
    m2 <- sample(1:50, 1)
    ans <- n1 + n2 + m1 * m2
    questions[i] <- paste0(m1, " * ", m2, " = ?")
  }
  answers[i] <- as.character(ans)
  ch <- ans + sample(c(-5:-1, 1:5) * spacing[i, ], 5)
  ch[sample(1:5, 1)] <- ans
  options[i, ] <- as.character(ch)
}

# load list of items and their answers
df <- data.frame(Question = questions, Option = options, Answer = answers, Type = "radio")
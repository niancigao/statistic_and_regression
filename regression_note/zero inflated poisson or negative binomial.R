#https://cran.r-project.org/web/packages/pscl/pscl.pdf
#http://www.ats.ucla.edu/stat/r/dae/zipoisson.htm
#https://stats.idre.ucla.edu/r/dae/zip/   

require(ggplot2)
require(pscl)
require(boot)

zinb <- read.csv("http://www.ats.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)

ggplot(zinb, aes(count)) + geom_histogram() + scale_x_log10()

install.packages('pscl')
library('pscl')

summary(m11 <- zeroinfl(count ~ child + camper | persons, data = zinb))

summary(m1 <- zeroinfl(count ~ child + camper+persons | child+camper+persons, data = zinb))

summary(p1 <- glm(count ~ child + camper, family = poisson, data = zinb))

vuong(p1, m1)
vuong(m11, m1)




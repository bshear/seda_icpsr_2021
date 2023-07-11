#09mar2021
#AERA/ICPS PEERS SEDA Webinar
#Authors: Ben Shear, Erin Fahle, sean reardon, Andrew Ho
#https://www.icpsr.umich.edu/web/pages/peersdatahub/
#Sample code to accompany webinar


# !!!!! #
# Before proceeding, please complete the data use agreement here:
# https://edopportunity.org/get-the-data/
# !!!!! #


# Packages

library(tidyverse)
library(haven)
library(stargazer)

################################################################################
# Pooled data examples

# Load achievement data (district, CS, pooled)
ach <- read_dta(file = "https://stacks.stanford.edu/file/druid:db586ns4974/seda_geodist_pool_cs_4.0.dta")


# Load covariate data (district, pooled)
covs <- read_dta(file = "https://stacks.stanford.edu/file/druid:db586ns4974/seda_cov_geodist_pool_4.0.dta")


# Merge files together. This keeps only matched districts.
dat <- inner_join(ach, covs, by = c("sedalea", "fips"))


# Subset to the "all" subgroup estimates for all students, only districts
# with an estimate of average achievement, learning rate, and non-missing SES.
dat <- filter(dat,
              subgroup == "all",
              !is.na(cs_mn_avg_eb),
              !is.na(cs_mn_grd_eb),
              !is.na(sesavgall))
nrow(dat)


# Summary statistics and correlations
stargazer(data.frame(select(dat, cs_mn_avg_eb, cs_mn_grd_eb, sesavgall)),
          type="text")

round(cor(select(dat, cs_mn_avg_eb, cs_mn_grd_eb, sesavgall)), 2)


# How much variation is there in average test scores?
ggplot(aes(x=cs_mn_avg_eb), data = dat) +
  geom_histogram(color="black", fill="grey") +
  annotate("text", x=-1.5, y=1510, label = "Mean=0.022, SD=0.33",
           size=5, hjust=0, vjust=0) +
  xlab("Average Test Scores (Grade 5.5, CS Scale)") +
  ylab("Frequency")

# How much variation is there in average learning rates?
ggplot(aes(x=cs_mn_grd_eb), data = dat) +
  geom_histogram(color="black", fill="grey") +
  annotate("text", x=-0.2, y=1510, label = "Mean = 0.002, SD=0.04",
           size=5, hjust=0, vjust=0) +
  xlab("Average Learning Rates") +
  ylab("Frequency")

# How much variation is there in SES?
ggplot(aes(x=sesavgall), data = dat) +
  geom_histogram(color="black", fill="grey") +
  annotate("text", x=-4, y=1510, label = "Mean = 0.33, SD=0.85",
           size=5, hjust=0, vjust=0) +
  xlab("Average SES") +
  ylab("Frequency")


# What is the association between SES and average achievement?
r1 <- round(cor(dat$cs_mn_avg_eb, dat$sesavgall),2)
ggplot(aes(x=sesavgall, y=cs_mn_avg_eb), data = dat) +
  geom_point(alpha = 0.3, pch=21, color = "black", fill = "grey", aes(size = totenrl)) +
  geom_smooth(se=F, lwd=0.5, lty=2, method="lm", formula = y~poly(x,3)) +
  annotate("text", x=-4, y=1, hjust=1, vjust=0,
           label = paste0("r=",r1)) +
  scale_size(range = c(1,30)) +
  guides(size=FALSE) +
  ggtitle("What is the association between SES and average test scores?") +
  xlab("Average SES") +
  ylab("Average Test Scores (Grade 5.5, CS Scale)")


# How does the gradient compare in CA vs. KY?
dat_ca_ky <- filter(dat, stateabb %in% c("CA","KY"))

ggplot(aes(x=sesavgall, y=cs_mn_avg_eb), data = dat) +
  geom_point(alpha = 0.2, pch=21, color = "black", fill = "grey", aes(size = totenrl)) +
  geom_point(alpha=0.7, pch=21, color="black", aes(size=totenrl, fill=stateabb),
             data = dat_ca_ky) +
  geom_smooth(se=F, lwd=0.5, lty=2, method="lm", color="black", aes(fill=stateabb),
              data = dat_ca_ky) +
  scale_size(range = c(1,30)) +
  guides(size=FALSE, color=FALSE) +
  labs(fill = "State") +
  ggtitle("How does the SES-achievement gradient differ in CA and KY?") +
  xlab("Average SES") +
  ylab("Average Test Scores (Grade 5.5, CS Scale)")


# What is the association between SES and average learning rates?
r2 <- round(cor(dat$cs_mn_grd_eb, dat$sesavgall),2)
ggplot(aes(x=sesavgall, y=cs_mn_grd_eb), data = dat) +
  geom_point(alpha = 0.3, pch=21, color = "black", fill = "grey", aes(size = totenrl)) +
  geom_smooth(se=F, lwd=0.5, lty=2, method="lm", formula = y~poly(x,3)) +
  annotate("text", x=-4, y=.2, hjust=1, vjust=0,
           label = paste0("r=",r2)) +
  scale_size(range = c(1,30)) +
  guides(size=FALSE) +
  ggtitle("What is the association between SES and average learning rates?") +
  xlab("Average SES") +
  ylab("Average Test Learning Rates (CS Scale)")


################################################################################
# Long form data example

# Warning: this file is larger and takes longer to load.
longdat <- read_dta(file = "https://stacks.stanford.edu/file/druid:db586ns4974/seda_geodist_long_cs_4.0.dta")

# Subset to LAUSD as an example
longdat_lausd <- filter(longdat, sedalea==622710)
nrow(longdat_lausd)
with(longdat_lausd, table(grade, year, subject))

round(with(longdat_lausd,
           tapply(cs_mn_all,
                  list(Grade=grade,
                       Year=year,
                       Subject=subject), mean)), 2)


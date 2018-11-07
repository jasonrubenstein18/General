library(foreign)
library(plyr)
library(MASS)
library(dplyr)
library(lme4)
install.packages("Hmisc")
library(Hmisc)
library(stats)
cps.2014 <- read.dta("econ452_cps.dta")
cps.2014$age <- as.numeric(cps.2014$age)
cps.2014 <- plyr::rename(cps.2014, c("year" = "survey.year", "serial" = "household.serial.number", 
                                     "hwtsupp" = "household.weight.supplement", "region" = "region.and.division", 
                                     "statefip" = "state", "metro" = "metro.central.city.status", "metarea" = "metro.area", 
                                     "cbsasz" = "core.based.area.size", "individcc" = "individual.principal.city", "ownershp" = "ownership.of.dwelling", 
                                     "hhincome" = "total.hh.income", "proptax" = "annual.property.tax", "pubhous" = "live.in.public.housing", 
                                     "foodstmp" = "food.stamp.recipient", "stampno" = "number.of.persons.covered.by.food.stamps", 
                                     "stampmo" = "number.of.months.received.food.stamps", "stampval" = "total.value.of.food.stamps", 
                                     "lunchsub" = "gov.school.lunch.subsidy", "frelunch" = "number.of.children.with.gov.school.lunch.subsidy", 
                                     "pernum" = "person.number.in.sample.unit", "wtsupp" = "supplement.weight", "earnwt" = "earnings.weight", 
                                     "nchild" = "number.of.own.children.in.house", "nchlt5" = "number.of.own.children.under.5.in.house", 
                                     "relate" = "relationship.to.household.head", "asian" = "asian.subgroup", "marst" = "marital.status", 
                                     "popstat" = "adultcivilian.armedforces.or.child", "bpl" = "birthplace", "yrimmig" = "year.of.immigration", 
                                     "citizen" = "citizenship.status", "mbpl" = "mothers.birthplace", "fbpl" = "fathers.birthplace",
                                     "nativity" = "foreign.birthplace.or.parentage", "hispan" = "hispanic.origin", "educ" = "educational.attainment",
                                     "educ99" = "educational.attainment.1990", "schlcoll" = "school.or.college.attendance", "empstat" = "employment.status",
                                     "labforce" = "labor.force.status", "occ2010" = "occupation.2010", "ind1990" = "industry.1990", "occ1950" = "occupation.1950", 
                                     "ind1950" = "industry.1950", "classwkr" = "class.of.worker", "uhrsworkt" = "hours.worked.per.week.all.jobs", 
                                     "uhrswork1" = "hours.worked.per.week.main.job", "ahrsworkt" = "hours.worked.last.week", "wksunem1" = "weeks.uneployed.last year",
                                     "absent" = "absent.from.work.last.week", "durunemp" = "cont.weeks.unemployed", 
                                     "fullpart" = "full.or.part.timework", "nwlookwk" = "weeks.look.for.work.without.work",
                                     "hourwage" = "hourly.wage", "paidhour" = "paid.by.hour", "whyunemp" = "why.unemployed",
                                     "wantjob" = "want.regular.job.now", "whyptly" = "why.parttime.last.year", "whyptlwk" = "why.parttime.last.week",
                                     "usftptlw" = "usually.full.but.part.lw", "payifabs" = "paid.if.absent", "numemps" = "number.of.employers.ly",
                                     "wnftlook" = "when.last.worked.fulltime.two.consec.weeks", "wnlwnilf" = "when.last.worked.for.pay",
                                     "strechlk" = "stretches.of.looking.for.work.ly", "whynwly" = "reason.for.not.working.ly",
                                     "actnlfly" = "activity.when.not.working.ly", "ftotval" = "total.family.income", "inctot" = "total.personal.income",
                                     "incwage" = "wage.and.salary.income", "incss" = "social.security.income", "incwelfr" = "welfare.income",
                                     "incretir" = "retirement.income", "earnweek" = "weekly.earnings", "offpov" = "official.poverty.status",
                                     "adjginc" = "adjusted.gross.income", "eitcred" = "earned.income.tax.credit", "filestat" = "tax.filer.status",
                                     "margtax" = "fed.income.marginal.tax.rate", "taxinc" = "taxable.income.amount", "vetstat" = "veteran.status",
                                     "vet1" = "vet.most.recent.service.period", "vet2" = "vet.second.most.recent.service.period",
                                     "vet3" = "vet.third.most.recent.service.period", "vet4" = "vet.fourth.most.recent.service.period",
                                     "disabwrk" = "work.disability", "health" = "health.status", "migsta1" = "state.of.residence.one.year.ago",
                                     "whymove" = "reason.for.moving", "migrate1" = "migration.status.one.year.ago", "gotwic" = "received.WIC",
                                     "ftype" = "family.type", "wkstat" = "full.or.part.time.status", "workly" = "worked.last.year"
                                      ))
#str(cps.2014)

hist(cps.2014$age)
AgeDensity <- density(cps.2014$age)
plot(AgeDensity)

# Only keep working age population
hist(cps.2014$age[cps.2014$age >= 18 & cps.2014$age <= 65])

# Only keep full-time workers
hist(cps.2014$age[cps.2014$age >= 18 & cps.2014$age <= 65 & cps.2014$wkstat == "Employed full time"])









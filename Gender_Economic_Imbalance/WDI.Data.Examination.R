install.packages("pastecs")
install.packages("foreign")
install.packages("plyr")
install.packages("MASS")
install.packages("dplyr")
install.packages("lme4")
install.packages("Hmisc")
install.packages("stats")
install.packages("data.table")
install.packages("car")
install.packages("moments")
install.packages("apsrtable")
install.packages("zoo")
install.packages("sandwich")
install.packages("plm")
install.packages("stargazer")
install.packages("quantmod")
install.packages("lmtest")
library(pastecs)
library(foreign)
library(plyr)
library(MASS)
library(dplyr)
library(lme4)
library(Hmisc)
library(stats)
library(data.table)
library(car)
library(moments)
library(apsrtable)
library(zoo)
library(psych)
library(sandwich)
library(plm)
library(stargazer)
require(quantmod)
library(lmtest)

aid.set <- subset(econset.dac, Total.Bilateral.Aid.Flows < 0 & year <= 2011 & country != "World")
plot(aid.set$year, aid.set$total.net.bilateral.aid.flows.from.DAC, ylab = "aid received", xlab = "year")

econset$receiver <- ifelse(Total.Bilateral.Aid.Flows < 0, 1, 0)
plot(year, econset$receiver, ylab = "receive aid/not (1 = receive aid)", xlab = "year")
Plot <- plotly::plot_ly(y = econset$receiver, x = year ,type = "scatter", mode = "markers + lines", color = country, opacity = 1, data = econset, na.rm = TRUE)
Plot
plot(table(econset$year[year < 2011], econset$receiver[year < 2011], useNA = NULL))

# Summary: Install data - rename and subset variables to new data frame - create fem/mal ratios 
# create three year lag variables - send data to three year lags - create three year averages with rowmeans 
# subset to developing country group - run regressions
rm(wdi)
wdi <- unzip("econ452_wdi2013.dta.zip")
wdidat <- read.dta(wdi, warn.missing.labels = TRUE)
wdidat$wbcode <- as.factor(wdidat$wbcode)
#View(wdidat)

econset <- plyr::rename(wdidat, c("AG_AGR_TRAC_NO" = "wheel.and.crawler.trac.used.year", "AG_CON_FERT_PT_ZS" = "fertilizer.production%", 
                            "AG_CON_FERT_ZS" = "fertilizer.consumption", "AG_LND_AGRI_K2" = "agricultural.land.square.km", 
                            "AG_LND_AGRI_ZS" = "agricultural.land%", "AG_LND_ARBL_HA" = "temp.crops.land", 
                            "AG_LND_ARBL_HA_PC" = "temp.crops.land.per.person", "AG_LND_ARBL_ZS" = "temp.crops.land%", 
                            "AG_LND_CREL_HA" = "harvested.area", "AG_LND_CROP_ZS" = "permanent.cropland%",
                            "AG_LND_EL5M_ZS" = "elevation.under.5.meters.land%", "AG_LND_FRST_K2" = "forest.area.sq.km", 
                            "AG_LND_FRST_ZS" = "forest.area.land%", "AG_LND_IRIG_AG_ZS" = "aggricultural.irrigated.aggland%", 
                            "AG_LND_PRCP_MM" = "average.precipitation.year.mm", "AG_LND_TOTL_K2" = "total.land.area.sq.km", 
                            "AG_LND_TRAC_ZS" = "agricultural.machiner.100.sq.km", "AG_PRD_CREL_MT" = "cereal.production.metric.tons", 
                            "AG_PRD_CROP_XD" = "crop.production.index", "AG_PRD_FOOD_XD" = "food.production.index", 
                            "AG_PRD_LVSK_XD" = "livestock.production.index", "AG_SRF_TOTL_K2" = "country.surface.area.sq.km", 
                            "AG_YLD_CREL_KG" = "cereal.yield", "BG_GSR_NFSV_GD_ZS" = "service.imports.and.exports.divided.by.GDP", 
                            "BM_GSR_CMCP_ZS" = "comm.computer.info.imports%", "BM_GSR_FCTY_CD" = "primary.income.payments", 
                            #"BM_GSR_GNFS_CD" = "imports.of.goods.and.services", "BM_GSR_INSF_ZS" = "insurance.financial.services.import%", 
                            "BM_GSR_MRCH_CD" = "goods.imports", "BM_GSR_NFSV_CD" = "service.imports", 
                            "BM_GSR_ROYL_CD" = "charges.for.use.of.intellectual.prop", "BM_GSR_TOTL_CD" = "goods.services.primaryincome.imports", 
                            "BM_GSR_TRAN_ZS" = "transport.services.import%", "BM_GSR_TRVL_ZS" = "travel.services.import%", 
                            "BM_KLT_DINV_GD_ZS" = "foreign.direct.investment.outflow", "BM_TRF_PRVT_CD" = "secondary.income.payments", 
                            "BM_TRF_PWKR_CD_DT" = "paid.personal.remittances", "BN_CAB_XOKA_CD" = "current.account.balance", 
                            "BN_CAB_XOKA_GD_ZS" = "current.account.balance.gdp%", "BN_FIN_TOTL_CD" = "net.financial.account", 
                            "BN_GSR_FCTY_CD" = "net.primary.income", "BM_GSR_GNFS_CD" = "net.trade.in.goods.and.services", 
                            "BN_GSR_MRCH_CD" = "net.trade.in.goods", "BN_KAC_EOMS_CD" = "net.errors.and.ommissions", 
                            "BN_KLT_DINV_CD" = "foreign.direct.investment.net", "BN_KLT_PTXL_CD" = "portfolio.investment.net", 
                            "BN_RES_INCL_CD" = "reserves.and.related.items", "BN_TRF_CURR_CD" = "net.secondary.income",
                            "BN_TRF_KOGT_CD" = "net.capital.account", "BX_GRT_EXTA_CD_WD" = "grants.exclude.technical.coop", 
                            "BX_GRT_TECH_CD_WD" = "technical.cooperation.grants", "BX_GSR_CCIS_CD" = "info.tech.comm.export", 
                            "BX_GSR_CCIS_ZS" = "info.tech.comm.export%", "BX_GSR_CMCP_ZS" = "comm.computer.etc.export%",
                            "BX_GSR_FCTY_CD" = "primary.income.receipts", "BX_GSR_GNFS_CD" = "goods.and.services.exports", 
                            "BX_GSR_INSF_ZS" = "insurance.finance.export%", "BX_GSR_MRCH_CD" = "goods.export", "BX_GSR_NFSV_CD" = "service.export", 
                            "BX_GSR_ROYL_CD" = "charges.for.use.intellectucal.prop", "BX_GSR_TOTL_CD" = "goods.services.primaryincome.export", 
                            "BX_GSR_TRAN_ZS" = "transport.export%", "BX_GSR_TRVL_ZS" = "travel.export%", 
                            "BX_KLT_DINV_CD_WD" = "foreign.direct.investment.inflow", "BX_KLT_DINV_WD_GD_ZS" = "foreign.direct.investment.inflow.GDP%",
                            "BX_KLT_DREM_CD_DT" = "primary.income.on.FDI", "BX_PEF_TOTL_CD_WD" = "portfolio.equity.inflow",
                            "BX_TRF_CURR_CD" = "secondary.income.receipts", "BX_TRF_PWKR_CD" = "personal.transfers.receipts",
                            "BX_TRF_PWKR_CD_DT" = "personal.remittances.received", "BX_TRF_PWKR_DT_GD_ZS" = "personal.remittances.received.GDP%",
                            "CM_MKT_INDX_ZG" = "S&P.global.equity.indices.%annchange", "CM_MKT_LCAP_CD" = "market.cap.domestic.companies", 
                            "CM_MKT_LCAP_GD_ZS" = "market.cap.domestic.companies.%GDP", "CM_MKT_LDOM_NO" = "total.listed.domestic.companies", 
                            "CM_MKT_TRAD_CD" = "total.value.stocks.traded", "CM_MKT_TRAD_GD_ZS" = "total.value.stocks.traded.%GDP",
                            "CM_MKT_TRNR" = "stocks.traded.turnover.ratio.domestic.shares", 
                            "DC_DAC_AUSL_CD" = "australia.net.bilateral.aid.flows.from.DAC", "DC_DAC_AUTL_CD" = "austria.net.bilateral.aid.flows.from.DAC", 
                            "DC_DAC_BELL_CD" = "belgium.net.bilateral.aid.flows.from.DAC", "DC_DAC_CANL_CD" = "canada.net.bilateral.aid.flows.from.DAC", 
                            "DC_DAC_CECL_CD" = "europeanunion.net.bilateral.aid.flows.from.DAC", "DC_DAC_CHEL_CD" = "switzerland.net.bilateral.aid.flows.from.DAC", 
                            "DC_DAC_CZEL_CD" = "czechrepublic.net.bilateral.aid.flows.from.DAC", "DC_DAC_DEUL_CD" = "germany.net.bilateral.aid.flows.from.DAC", 
                            "DC_DAC_DNKL_CD" = "denmark.net.bilateral.aid.flows.from.DAC", "DC_DAC_ESPL_CD" = "spain.net.bilateral.aid.flows.from.DAC", 
                            "DC_DAC_FINL_CD" = "finland.net.bilateral.aid.flows.from.DAC", "DC_DAC_FRAL_CD" = "france.net.bilateral.aid.flows.from.DAC",
                            "DC_DAC_GBRL_CD" = "unitedkingdom.net.bilateral.aid.flows.from.DAC", "DC_DAC_GRCL_CD" = "greece.net.bilateral.aid.flows.from.DAC", 
                            "DC_DAC_IRLL_CD" = "ireland.net.bilateral.aid.flows.from.DAC", "DC_DAC_ISLL_CD" = "iceland.net.bilateral.aid.flows.from.DAC", 
                            "DC_DAC_ITAL_CD" = "italy.net.bilateral.aid.flows.from.DAC", "DC_DAC_JPNL_CD" = "japan.net.bilateral.aid.flows.from.DAC", 
                            "DC_DAC_KORL_CD" = "korea.net.bilateral.aid.flows.from.DAC", "DC_DAC_LUXL_CD" = "luxembourg.net.bilateral.aid.flows.from.DAC",
                            "DC_DAC_NLDL_CD" = "netherlands.net.bilateral.aid.flows.from.DAC", "DC_DAC_NORL_CD" = "norway.net.bilateral.aid.flows.from.DAC", 
                            "DC_DAC_NZLL_CD" = "newzealand.net.bilateral.aid.flows.from.DAC", #wdidat$DC_DAC_ = "poland.net.bilateral.aid.flows.from.DAC", 
                            "DC_DAC_PRTL_CD" = "portugal.net.bilateral.aid.flows.from.DAC", #"DC_DAC_SVLK_CD" = "slovakia.net.bilateral.aid.flows.from.DAC", 
                            #"DC_DAC_SVLN_CD" = "slovenia.net.bilateral.aid.flows.from.DAC", 
                            "DC_DAC_SWEL_CD" = "sweden.net.bilateral.aid.flows.from.DAC",
                            "DC_DAC_TOTL_CD" = "total.net.bilateral.aid.flows.from.DAC", "DC_DAC_USAL_CD" = "unitedstates.net.bilateral.aid.flows.from.DAC",
                            "DT_AMT_DLXF_CD" = "repayments.on.ext.debt.longterm", "DT_AMT_DPNG_CD" = "repayments.on.ext.debt.private",
                            "DT_DFR_DPPG_CD" = "debt.forgiveness.or.reduction", "EN_POP_DNST" = "population.density",
                            "FP_CPI_TOTL" = "consumer.price.index", "GC_BAL_CASH_CN" = "cash.surplus.or.deficit", "NY_GDP_MKTP_CD" = "GDP.US$",
                            "SE_ADT_1524_LT_FE_ZS" = "literacy.rate.youth.female", "SE_ADT_1524_LT_ZS"= "literacy.rate.youth", "SE_ADT_1524_LT_MA_ZS" = "literacy.rate.youth.male",
                            "SE_ADT_LITR_ZS" = "literacy.rate.adult", "SE_XPD_TOTL_GD_ZS" = "government.education.expenditure", "SE_ADT_LITR_FE_ZS" = "literacy.rate.adult.female",
                            "SE_ADT_LITR_MA_ZS" = "literacy.rate.adult.male", "SE_PRM_NENR" = "primary.school.enrollment", #"SE_XPD_MTOT_ZS" = "education.staff.compensation",
                            "SE_XPD_PRIM_PC_ZS" = "primary.gov.expenditure.per.student.%GDP", #"SE_XPD_TOTL_GD_ZS" = "gov.expenditure.on.education.%GDP",
                            "SH_H2O_SAFE_ZS" = "improved.water.source.%population", "SH_XPD_PCAP" = "health.expenditure.per.capita",
                            "SI_DST_02ND_20" = "income.share.held.by.second.20%", "SI_DST_03RD_20" = "income.share.held.by.third.20%",
                            "SI_DST_04TH_20" = "income.share.held.by.fourth.20%", "SI_DST_05TH_20" = "income.share.held.by.highest.20%",
                            "SI_DST_10TH_10" = "income.share.held.by.highest.10%", "SI_DST_FRST_20" = "income.share.held.by.lowest.20%",
                            "SI_POV_GINI" = "GINI.index.of.household.equality", #"SL_EMP_1524_SP_FE_NE_ZS" = "employment.to.population.ratio.15-24.female",
                            # "SL_EMP_1524_SP_MA_NE_ZS" = "employment.to.population.ratio.15-24.male", "SL_EMP_1524_SP_NE_ZS" = "employment.to.population.ratio.15-24.total",
                            "SL_EMP_VULN_FE_ZS" = "vulnerable.employment.female.%employed", "SL_EMP_VULN_MA_ZS" = "vulnerable.employment.male.%employed",
                            "SL_EMP_VULN_ZS" = "vulnerable.employment.total.%employed", "SL_EMP_WORK_ZS" = "wage.and.salary.workers.total.%employed",
                            "SL_TLF_ACTI_ZS" = "labor.force.participation.rate.%population", #"SG_VAW_ARGU_ZS" = "women.justify.husband.beating.after.arguing.%",
                            "SL_UEM_TOTL_ZS" = "unemployment.rate.%totallaborforce", "SL_UEM_TOTL_FE_ZS" = "unemployment.rate.%femalelaborforce",
                            "SL_UEM_TOTL_MA_ZS" = "unemployment.rate.%malelaborforce", "SP_DYN_LE00_IN" = "life.expectancy.at.birth", "SP_MTR_1519_ZS" = "teen.mothers.%women15-19",
                            "SP_POP_TOTL" = "population.total", "SP_POP_TOTL_FE_ZS" = "female.population.total", #"IG_CPA_GNDR_XG" = "gender.equality.rating.cpia",
                            #"IG_CPA_IRAI_XQ" = "resource.allocation.index.ida", 
                            "MS_MIL_XPND_GD_ZS" = "military.expenditure.%GDP", "NV_AGR_TOTL_ZS" = "agriculture.value.added.%GDP",
                            "NV_IND_MANF_ZS" = "manufacturing.value.added.%GDP", "NV_IND_TOTL_ZS" = "industry.value.added.%GDP",
                            #"NY_ADJ_NNTY_PC_CD" = "adjusted.natl.income.per.capita.US$", 
                            "NY_GDP_PCAP_CD" = "GDP.per.capita.US$", "SE_PRM_CMPT_FE_ZS" = "primary.completion.rate.female%",
                            "SE_PRM_CMPT_MA_ZS" = "primary.completion.rate.male%", "SE_PRM_NENR_FE" = "primary.school.enrollment.female%", "SE_PRM_NENR_MA" = "primary.school.enrollment.male%",
                            "SE_PRM_REPT_FE_ZS" = "primary.repeat.female%enrollment", "SE_PRM_REPT_MA_ZS" = "primary.repeat.male%enrollment",
                            "SE_SEC_NENR_FE" = "secondary.enrollment.female%", "SE_SEC_NENR_MA" = "secondary.enrollment.male%", "SE_SEC_REPT_FE_ZS" = "secondary.repeat.female%enrollment",
                            "SE_SEC_REPT_MA_ZS" = "secondary.repeat.male%enrollment", "SE_XPD_SECO_PC_ZS" = "secondary.gov.expenditure.per.student.%GDP",
                            "SL_TLF_ACTI_FE_ZS" = "labor.participation.rate.female%15-64", "SL_TLF_ACTI_MA_ZS" = "labor.participation.rate.male%15-64",
                            "SL_TLF_CACT_FM_ZS" = "laborparticipation.ratio.female.to.male", "TX_VAL_AGRI_ZS_UN" = "agricultural.raw.material.export%",
                            ######## Newly added variables
                            "IC_FRM_FEMM_ZS" = "female.top.managers", "IC_FRM_FEMO_ZS" = "female.firm.owners", "IQ_CPA_GNDR_XQ" = "cpia.gender.equality.rating",
                            "IQ_CPA_MACR_XQ" = "cpia.macro.management.rating", "SG_GEN_LSOM_ZS" = "female.officials.legislators.managers",
                            "SG_GEN_PARL_ZS" = "female.national.parliament.proportion", "SH_DYN_MORT_FE" = "female.mortality.rate.ufive",
                            "SH_DYN_MORT_MA" = "male.mortality.rate.ufive", "SH_MMR_DTHS" = "maternal.deaths", "SH_MMR_RISK_ZS" = "maternal.death.risk",
                            "SL_EMP_INSV_FE_ZS" = "female.wage.employment.nonagriculture", "SL_EMP_SELF_FE_ZS" = "self.employed.female",
                            "SL_EMP_SELF_MA_ZS" = "self.employed.male", "SL_EMP_WORK_FE_ZS" = "female.wage.and.salaried.workers",
                            "SL_EMP_WORK_MA_ZS" = "male.wage.and.salaried.workers", "SL_GDP_PCAP_EM_KD" = "gdp.per.person.employed",
                            "SL_TLF_PRIM_FE_ZS" = "female.labor.force.with.primary.education", "SL_TLF_PRIM_MA_ZS" = "male.labor.force.with.primary.education",
                            "SL_TLF_SECO_FE_ZS" = "female.labor.force.with.secondary.education", "SL_TLF_SECO_MA_ZS" = "male.labor.force.with.secondary.education",
                            "SL_TLF_TOTL_FE_ZS" = "female.labor.force.proportion", "SL_UEM_LTRM_FE_ZS" = "female.long.term.unemployment", "SL_UEM_LTRM_MA_ZS" = "male.long.term.unemployment"
                            ))

econset.dac <- econset[,c(1:3, 107, 4:106, 108:1303)]
library(plotly)
aid.set <- subset(econset.dac, Total.Bilateral.Aid.Flows < 0 & year <= 2011 & country != "World")
AidPlot <- plotly::plot_ly(y = Total.Bilateral.Aid.Flows, x = year ,type = "scatter", mode = "markers + lines", color = country, opacity = 1, data = aid.set)
AidPlot
#########                            
# Correlations to research
  -Seats held by females in national parliament --> Firms with top female manager
  -Seats held by females in national parliament --> Female legislators, officials, and managerss
  -Seats held by females in national parliament --> Female mortality rate per 1000
  -Seats held by females in national parliament --> Number of maternal deaths
  -Seats held by females in national parliament --> Lifetime risk of maternal death per 1000
  -Seats held by females in national parliament --> Self employed female pct
  -Seats held by females in national parliament --> Wage and salaried workers pct females
  -Seats held by females in national parliament --> GDP per person employed 1990 values (this has underlying bias from the typed of jobs and distribution of such)
  -Seats held by females in national parliament --> Labor force with primary education pct female/male
  -Seats held by females in national parliament --> Labor force with secondary education pct female/male
  -Seats held by females in national parliament --> Long term unemployemnt pct female/male unemployed

Could look at GDP per person employed as a dependent variable with variables relating to equality as independent variables

  -Firms with top female manager --> CPIA macro management rating
  -Firms with female ownership particiation --> CPIA macro maanagement rating
######## Old Vars ######

list(factor(unique(econset$country)))
# Developing Countries:
  # Afghanistan; Angola; Bangladesh; Benin; Bhutan; Burkina Faso; Burundi
  # Cambodia; Central African Republic; Chad; Comoros; Congo, Dem. Rep.
  # Djibouti; Equatorial Guinea; Eritrea; Ethiopia; Gambia, The
  # Guinnea-Bissau; Haiti; Kiribati; Lao PDR; Lesotho; Liberia
  # Madagascar; Malawi; Mali; Mauritania; Mozambique; Myanmar
  # Nepal; Niger; Rwanda; Sao Tome and Principe; Senegal
  # Sierra Leone; Somalia; South Sudan; Sudan; Timor-Leste; Togo
  # Tuvalu; Uganda; Tanzania; Vanuatu; Yemen, Rep.; Zambia
Austria.Net.Flows <- econset2$austria.net.bilateral.aid.flows.from.DAC
Australia.Net.Flows <- econset2$australia.net.bilateral.aid.flows.from.DAC
Canada.Net.Flows <- econset2$canada.net.bilateral.aid.flows.from.DAC
Switzerland.Net.Flows <- econset2$switzerland.net.bilateral.aid.flows.from.DAC
Germany.Net.Flows <- econset2$germany.net.bilateral.aid.flows.from.DAC
France.Net.Flows <- econset2$france.net.bilateral.aid.flows.from.DAC
Total.Bilateral.Aid.Flows <- econset2$total.net.bilateral.aid.flows.from.DAC
# Summary of Bilateral Aid flows for individual countries 
country.aid.df <- data.frame(Total.Bilateral.Aid.Flows, Austria.Net.Flows, Australia.Net.Flows, Canada.Net.Flows, Switzerland.Net.Flows, Germany.Net.Flows,France.Net.Flows)
summary(country.aid.df)
View(country.aid.df)
# Find tipping point for when country becomes a donor rather than receiver of aid

econset2 <- subset(econset, select = c("country", "year", "wbcode",  "GDP.US$", "total.net.bilateral.aid.flows.from.DAC",
                                       "permanent.cropland%", "country.surface.area.sq.km", 
                                       "portfolio.investment.net", "comm.computer.etc.export%", 
                                       "goods.and.services.exports", "goods.export", "transport.export%", 
                                       "foreign.direct.investment.inflow", "market.cap.domestic.companies.%GDP", 
                                       "austria.net.bilateral.aid.flows.from.DAC", "canada.net.bilateral.aid.flows.from.DAC",
                                       "switzerland.net.bilateral.aid.flows.from.DAC", "germany.net.bilateral.aid.flows.from.DAC", 
                                       "spain.net.bilateral.aid.flows.from.DAC", "france.net.bilateral.aid.flows.from.DAC",
                                       "greece.net.bilateral.aid.flows.from.DAC", "iceland.net.bilateral.aid.flows.from.DAC", 
                                       "japan.net.bilateral.aid.flows.from.DAC", "luxembourg.net.bilateral.aid.flows.from.DAC",
                                       "norway.net.bilateral.aid.flows.from.DAC", "portugal.net.bilateral.aid.flows.from.DAC",  
                                       "belgium.net.bilateral.aid.flows.from.DAC", "europeanunion.net.bilateral.aid.flows.from.DAC", 
                                       "czechrepublic.net.bilateral.aid.flows.from.DAC", "denmark.net.bilateral.aid.flows.from.DAC", 
                                       "finland.net.bilateral.aid.flows.from.DAC", "unitedkingdom.net.bilateral.aid.flows.from.DAC", 
                                       "ireland.net.bilateral.aid.flows.from.DAC", "italy.net.bilateral.aid.flows.from.DAC",
                                       "korea.net.bilateral.aid.flows.from.DAC", "netherlands.net.bilateral.aid.flows.from.DAC", 
                                       "newzealand.net.bilateral.aid.flows.from.DAC", "sweden.net.bilateral.aid.flows.from.DAC", 
                                       "unitedstates.net.bilateral.aid.flows.from.DAC", "australia.net.bilateral.aid.flows.from.DAC", 
                                       "population.density", "cash.surplus.or.deficit",
                                       "military.expenditure.%GDP", "GDP.per.capita.US$", "literacy.rate.youth", 
                                       "literacy.rate.adult.male", "primary.completion.rate.female%",
                                       "primary.school.enrollment.female%", "primary.repeat.female%enrollment", 
                                       "secondary.enrollment.male%", "secondary.repeat.male%enrollment", 
                                       "secondary.gov.expenditure.per.student.%GDP", "comm.computer.info.imports%", 
                                       "net.trade.in.goods.and.services", "goods.imports", "transport.services.import%", 
                                       "foreign.direct.investment.outflow", "current.account.balance.gdp%", "net.trade.in.goods", 
                                       "foreign.direct.investment.net", "info.tech.comm.export%", "insurance.finance.export%", 
                                       "service.export", "travel.export%", "foreign.direct.investment.inflow.GDP%", 
                                       "market.cap.domestic.companies", "total.value.stocks.traded.%GDP", 
                                       "total.listed.domestic.companies", "consumer.price.index", 
                                       "agriculture.value.added.%GDP", "manufacturing.value.added.%GDP", "industry.value.added.%GDP",
                                       "literacy.rate.youth.female", "literacy.rate.youth.male", "literacy.rate.adult.female", 
                                       "literacy.rate.adult", "primary.completion.rate.male%", "primary.school.enrollment", 
                                       "primary.school.enrollment.male%", "primary.repeat.male%enrollment", 
                                       "secondary.enrollment.female%", "secondary.repeat.female%enrollment",
                                       "primary.gov.expenditure.per.student.%GDP", "government.education.expenditure",
                                       "improved.water.source.%population", "health.expenditure.per.capita",
                                       "income.share.held.by.second.20%", "income.share.held.by.fourth.20%",
                                       "income.share.held.by.highest.10%", "income.share.held.by.lowest.20%",
                                       "income.share.held.by.third.20%", "income.share.held.by.highest.20%", 
                                       "GINI.index.of.household.equality", "self.employed.males.%employed-males", 
                                       "vulnerable.employment.female.%employed", "vulnerable.employment.total.%employed", 
                                       "vulnerable.employment.male.%employed", "wage.and.salary.workers.total.%employed", 
                                       "labor.participation.rate.female%15-64", "labor.participation.rate.male%15-64", 
                                       "labor.force.participation.rate.%population", "laborparticipation.ratio.female.to.male", 
                                       "unemployment.rate.%femalelaborforce", "unemployment.rate.%totallaborforce", 
                                       "unemployment.rate.%malelaborforce", "life.expectancy.at.birth", "teen.mothers.%women15-19", 
                                       "population.total", "female.population.total"
                                       ))
######## Subsetting Research Variables #####
# Dependent Vars
GDP.US <- econset2$`GDP.US$`
net.bilateral.aid.flows.from.DAC <- econset2$total.net.bilateral.aid.flows.from.DAC

# Fixed Effects
country <- econset2$country
year <- econset2$year
wbcode <- econset2$wbcode

# Extraneous variables (also want to include health/medical related variables)
market.cap.domestic.companies.pctGDP <- econset2$`market.cap.domestic.companies.%GDP`
population.density <- econset2$population.density
military.expenditure.pctGDP <- econset2$`military.expenditure.%GDP`
GDP.per.capita.US <- econset2$`GDP.per.capita.US$`

# Education based variables
  # Secondary School
      # Enrollment
secondary.school.enrollment.pctMale <- econset2$`secondary.enrollment.male%`
secondary.school.enrollment.pctFemale <- econset2$`secondary.enrollment.female%`
      # Repeat
secondary.school.repeat.pctMaleEnrollment <- econset2$`secondary.repeat.male%enrollment`
secondary.school.repeat.pctFemaleEconrollment <- econset2$`secondary.repeat.female%enrollment`
  # Literacy Rates
      # Youth
literacy.rate.youth.female <- econset2$literacy.rate.youth.female
literacy.rate.youth.male <- econset2$literacy.rate.youth.male
      # Adult
literacy.rate.adult.female <- econset2$literacy.rate.adult.female
literacy.rate.adult.male <- econset2$literacy.rate.adult.male
  # Primary School
      # Completion
primary.school.completion.pctMale <- econset2$`primary.completion.rate.male%`
primary.school.completion.pctFemale <- econset2$`primary.completion.rate.female%`
      # Enrollment
primary.school.enrollment.pctFemale <- econset2$`primary.school.enrollment.female%`
primary.school.enrollment.pctMale <- econset2$`primary.school.enrollment.male%`
      # Repeat
primary.school.repeat.pctFemaleEnrollment <- econset2$`primary.repeat.female%enrollment`
primary.school.repeat.pctMaleEnrollment <- econset2$`primary.repeat.male%enrollment`

# Labor Force Variables
  # Labor Force Participation
labor.force.participation.ratio <- econset2$laborparticipation.ratio.female.to.male
labor.participation.female.15.64 <- econset2$`labor.participation.rate.female%15-64`
labor.participation.male.15.64 <- econset2$`labor.participation.rate.male%15-64`
  # Vulnerable Employment Ratio
vulnerable.employment.ratio.female <- econset2$`vulnerable.employment.female.%employed`
vulnerable.employment.ratio.male <- econset2$`vulnerable.employment.male.%employed`

  # Life expectancy at birth
life.expectancy <- econset2$life.expectancy.at.birth
econvars <- data.frame(country, year, wbcode, GDP.US, net.bilateral.aid.flows.from.DAC, labor.force.participation.ratio,
                       labor.participation.female.15.64, labor.participation.male.15.64,
                       primary.school.repeat.pctFemaleEnrollment, primary.school.repeat.pctMaleEnrollment,
                       primary.school.enrollment.pctFemale,primary.school.enrollment.pctMale, literacy.rate.adult.female,
                       literacy.rate.adult.male, literacy.rate.youth.female, literacy.rate.youth.male, 
                       secondary.school.repeat.pctMaleEnrollment, secondary.school.repeat.pctFemaleEconrollment, 
                       secondary.school.enrollment.pctMale, secondary.school.enrollment.pctFemale,vulnerable.employment.ratio.female,
                       vulnerable.employment.ratio.male, life.expectancy)

#econvars2 <- transform(econvars, Three.year.lagged.GDP.average = rowMeans(research.full[,4:6], na.rm = TRUE))


##### Creating New Variables before lag to 3-year average #####
econvars["youth.literacy.ratio.fem.mal"] <- NA
econvars$youth.literacy.ratio.fem.mal <- literacy.rate.youth.female/literacy.rate.youth.male 
youth.literacy.ratio <- econvars$youth.literacy.ratio.fem.mal

econvars["adult.literacy.ratio.fem.mal"] <- NA
econvars$adult.literacy.ratio.fem.mal <- literacy.rate.adult.female/literacy.rate.adult.male
adult.literacy.ratio <- econvars$adult.literacy.ratio.fem.mal

# Primary school completion ratio fem/mal
econvars["primary.school.completion.ratio.fem.mal"] <- NA
econvars$primary.school.completion.ratio.fem.mal <- primary.school.completion.pctFemale/primary.school.completion.pctMale
primary.school.completion.ratio <- econvars$primary.school.completion.ratio.fem.mal

# Primary school enrollment ratio fem/mal
econvars["primary.school.enrollment.ratio.fem.mal"] <- NA
econvars$primary.school.enrollment.ratio.fem.mal <- primary.school.enrollment.pctFemale/primary.school.enrollment.pctMale
primary.school.enrollment.ratio <- econvars$primary.school.completion.ratio.fem.mal

# Primary school repeat ratio fem/mal
econvars["primary.school.repeat.ratio.fem.mal"] <- NA
econvars$primary.school.repeat.ratio.fem.mal <- primary.school.repeat.pctFemaleEnrollment/primary.school.repeat.pctMaleEnrollment
primary.school.repeat.ratio <- econvars$primary.school.repeat.ratio.fem.mal

# Secondary school enrollment ratio fem/mal
econvars["secondary.school.enrollment.ratio.fem.mal"] <- NA
econvars$secondary.school.enrollment.ratio.fem.mal <- secondary.school.enrollment.pctFemale/secondary.school.enrollment.pctMale
secondary.school.enrollment.ratio <- econvars$secondary.school.enrollment.ratio.fem.mal

# Secondary school repeat ratio fem/mal
econvars["secondary.school.repeat.ratio.fem.mal"] <- NA
econvars$secondary.school.repeat.ratio.fem.mal <- secondary.school.repeat.pctFemaleEconrollment/secondary.school.repeat.pctMaleEnrollment
secondary.school.repeat.ratio <- econvars$secondary.school.repeat.ratio.fem.mal

# Vulnerable employment ratio fem/mal
econvars["vulnerable.employment.ratio.fem.mal"] <- NA
econvars$vulnerable.employment.ratio.fem.mal <- vulnerable.employment.ratio.female/vulnerable.employment.ratio.male
vulnerable.employment.ratio <- econvars$vulnerable.employment.ratio.fem.mal
# Female population to total population ratio
#econvars["population.ratio.fem.mal"] <- NA
#econvars$population.ratio.fem.mal <- popu
#dev.population.ratio <- econvars$population.ratio.fem.mal
# Labor force participation ratio
labor.participation.ratio <- econset2$laborparticipation.ratio.female.to.male

ratio.vars <- data.frame(youth.literacy.ratio, adult.literacy.ratio, primary.school.completion.ratio, primary.school.enrollment.ratio
                         , primary.school.repeat.ratio, secondary.school.enrollment.ratio, secondary.school.repeat.ratio
                         , vulnerable.employment.ratio, labor.force.participation.ratio, country, year
                         )
####### Creating lag variables #######
  # DAC Flow
econvars["previous.year.DAC.flow"] <- NA
econvars["second.previous.year.DAC.flow"] <- NA
econvars["three.year.lagged.DAC.flow.average"] <- NA
  # GDP
econvars["previous.year.GDP"] <- NA
econvars["second.previous.year.GDP"] <- NA
econvars["three.year.lagged.GDP.average"] <- NA
  # Secondary School Enrollment
econvars["previous.year.secondary.school.enrollment.ratio"] <- NA
econvars["second.previous.year.secondary.school.enrollment.ratio"] <- NA
econvars["three.year.lagged.secondary.school.enrollment.ratio"] <- NA
  # Secondary School Repeat
econvars["previous.year.secondary.school.repeat.ratio"] <- NA
econvars["second.previous.year.secondary.school.repeat.ratio"] <- NA
econvars["three.year.lagged.secondary.school.repeat.ratio"] <- NA
  # Youth Literacy Ratio
econvars["previous.year.youth.literacy.ratio"] <- NA
econvars["second.previous.year.youth.literacy.ratio"] <- NA
econvars["three.year.lagged.youth.literacy.ratio"] <- NA
  # Adult Literacy Ratio
econvars["previous.year.adult.literacy.ratio"] <- NA
econvars["second.previous.year.adult.literacy.ratio"] <- NA
econvars["three.year.lagged.adult.literacy.ratio"] <- NA
  # Primary School Completion
econvars["previous.year.primary.school.completion.ratio"] <- NA
econvars["second.previous.year.primary.school.completion.ratio"] <- NA
econvars["three.year.lagged.primary.school.completion.ratio"] <- NA
  # Primary School Enrollment
econvars["previous.year.primary.school.enrollment.ratio"] <- NA
econvars["second.previous.year.primary.school.enrollment.ratio"] <- NA
econvars["three.year.lagged.primary.school.enrollment.ratio"] <- NA
  # Primary School Repeat
econvars["previous.year.primary.school.repeat.ratio"] <- NA
econvars["second.previous.year.primary.school.repeat.ratio"] <- NA
econvars["three.year.lagged.primary.school.repeat.ratio"] <- NA
  # Labor Force Participation
econvars["previous.year.labor.force.participation.ratio"] <- NA
econvars["second.previous.year.labor.force.participation.ratio"] <- NA
econvars["three.year.lagged.labor.force.participation.ratio"] <- NA
  # Vulnerable Employment Ratio
econvars["previous.year.vulnerable.employment.ratio"] <- NA
econvars["second.previous.year.vulnerable.employment.ratio"] <- NA
econvars["three.year.lagged.vulnerable.employment.ratio"] <- NA

names(econvars)
econvars <- econvars[,c(1:3, 5, 32:34, 4, 35:37, 24, 44:46, 25, 47:49, 26, 50:52, 27, 53:55, 28, 56:58, 29, 38:40, 30, 41:43, 6, 59:61, 31, 62:64, 7:23)]

names(econvars)

###### Creating 3 year lag averages for each variable ######
  # GDP
econvars1 <- mutate(econvars, previous.year.GDP = lag(GDP.US, 1))
econvars2 <- mutate(econvars1, second.previous.year.GDP = lag(previous.year.GDP, 1))
names(econvars2)
econvars3 <- transform(econvars2, three.year.lagged.GDP.average = rowMeans(econvars2[,8:10], na.rm = TRUE))
head(econvars3)
  # DAC
econvars4 <- mutate(econvars3, previous.year.DAC.flow = lag(net.bilateral.aid.flows.from.DAC, 1))
econvars5 <- mutate(econvars4, second.previous.year.DAC.flow = lag(previous.year.DAC.flow, 1))
names(econvars5)
econvars6 <- transform(econvars5, three.year.lagged.DAC.flow.average = rowMeans(econvars5[,4:6], na.rm = TRUE))
head(econvars6)
  # Youth Literacy Ratio
econvars7 <- mutate(econvars6, previous.year.youth.literacy.ratio = lag(youth.literacy.ratio.fem.mal, 1))
econvars8 <- mutate(econvars7, second.previous.year.youth.literacy.ratio = lag(previous.year.youth.literacy.ratio, 1))
names(econvars8)
econvars9 <- transform(econvars8, three.year.lagged.youth.literacy.ratio = rowMeans(econvars8[,12:14], na.rm = TRUE))
head(econvars9)
  # Adult Literacy Ratio
econvars10 <- mutate(econvars9, previous.year.adult.literacy.ratio = lag(adult.literacy.ratio.fem.mal, 1))
econvars11 <- mutate(econvars10, second.previous.year.adult.literacy.ratio = lag(previous.year.adult.literacy.ratio, 1))
names(econvars11)
econvars12 <- transform(econvars11, three.year.lagged.adult.literacy.ratio = rowMeans(econvars11[,16:18], na.rm = TRUE))
head(econvars12)
  # Primary School Completion Ratio
econvars13 <- mutate(econvars12, previous.year.primary.school.completion.ratio = lag(primary.school.completion.ratio, 1))
econvars14 <- mutate(econvars13, second.previous.year.primary.school.completion.ratio = lag(previous.year.primary.school.completion.ratio, 1))
names(econvars14)
econvars15 <- transform(econvars14, three.year.lagged.primary.school.completion.ratio = rowMeans(econvars14[,20:22], na.rm = TRUE))
head(econvars15)
  # Primary School Repeat Ratio
econvars16 <- mutate(econvars15, previous.year.primary.school.repeat.ratio = lag(primary.school.repeat.ratio, 1))
econvars17 <- mutate(econvars16, second.previous.year.primary.school.repeat.ratio = lag(previous.year.primary.school.repeat.ratio, 1))
names(econvars17)
econvars18 <- transform(econvars17, three.year.lagged.primary.school.completion.ratio = rowMeans(econvars17[,24:26], na.rm = TRUE))
head(econvars18)
  # Primary School Enrollment Ratio
econvars19 <- mutate(econvars18, previous.year.primary.school.enrollment.ratio = lag(primary.school.enrollment.ratio, 1))
econvars20 <- mutate(econvars19, second.previous.year.primary.school.enrollment.ratio = lag(previous.year.primary.school.enrollment.ratio, 1))
names(econvars17)
econvars21 <- transform(econvars20, three.year.lagged.primary.school.enrollment.ratio = rowMeans(econvars20[,28:30], na.rm = TRUE))
head(econvars18)
  # Secondary School Enrollment Ratio
econvars22 <- mutate(econvars21, previous.year.secondary.school.enrollment.ratio = lag(secondary.school.enrollment.ratio, 1))
econvars23 <- mutate(econvars22, second.previous.year.secondary.school.enrollment.ratio = lag(previous.year.secondary.school.enrollment.ratio, 1))
names(econvars23)
econvars24 <- transform(econvars23, three.year.lagged.secondary.school.enrollment.ratio = rowMeans(econvars23[,32:34], na.rm = TRUE))
head(econvars24)
  # Secondary School Repeat Ratio
econvars25 <- mutate(econvars24, previous.year.secondary.school.repeat.ratio = lag(secondary.school.repeat.ratio, 1))
econvars26 <- mutate(econvars25, second.previous.year.secondary.school.repeat.ratio = lag(previous.year.secondary.school.repeat.ratio, 1))
names(econvars26)
econvars27 <- transform(econvars26, three.year.lagged.secondary.school.repeat.ratio = rowMeans(econvars26[,36:38], na.rm = TRUE))
head(econvars27)
  # Labor Participation Ratio
econvars28 <- mutate(econvars27, previous.year.labor.force.participation.ratio = lag(labor.force.participation.ratio, 1))
econvars29 <- mutate(econvars28, second.previous.year.labor.force.participation.ratio = lag(previous.year.labor.force.participation.ratio, 1))
names(econvars29)
econvars30 <- transform(econvars29, three.year.lagged.labor.force.participation.ratio = rowMeans(econvars29[,40:42], na.rm = TRUE))
head(econvars30)
#summary(econvars30$three.year.lagged.labor.force.participation.ratio)
  # Vulnerable Employment Ratio
econvars31 <- mutate(econvars30, previous.year.vulnerable.employment.ratio = lag(vulnerable.employment.ratio, 1))
econvars32 <- mutate(econvars31, second.previous.year.vulnerable.employment.ratio = lag(previous.year.vulnerable.employment.ratio, 1))
names(econvars32)
econvars33 <- transform(econvars32, three.year.lagged.vulnerable.employment.ratio = rowMeans(econvars32[,44:46], na.rm = TRUE))
head(econvars33)

research.set <- econvars33

##### Could Use, chose not to Creating % Change Variables #######
research.set["pct.change.DAC"] <- NA
research.set$pct.change.DAC <- research.set$three.year.lagged.DAC.flow.average/lag(research.set$three.year.lagged.DAC.flow.average, 1) - 1
research.set["pct.change.GDP"] <- NA
research.set$pct.change.GDP <- research.set$three.year.lagged.GDP.average/lag(research.set$three.year.lagged.GDP.average, 1) - 1
research.set["pct.change.youth.literacy.ratio"] <- NA
research.set$pct.change.youth.literacy.ratio <- research.set$three.year.lagged.youth.literacy.ratio/lag(research.set$three.year.lagged.youth.literacy.ratio, 1) - 1
research.set["pct.change.adult.literacy.ratio"] <- NA
research.set$pct.change.adult.literacy.ratio <- research.set$three.year.lagged.adult.literacy.ratio/lag(research.set$three.year.lagged.adult.literacy.ratio, 1) - 1
research.set["pct.change.primary.school.completion.ratio"] <- NA
research.set$pct.change.primary.school.completion.ratio <- research.set$three.year.lagged.primary.school.completion.ratio/lag(research.set$three.year.lagged.primary.school.completion.ratio, 1) - 1
research.set["pct.change.primary.school.repeat.ratio"] <- NA
research.set$pct.change.primary.school.repeat.ratio <- research.set$three.year.lagged.primary.school.repeat.ratio/lag(research.set$three.year.lagged.primary.school.repeat.ratio, 1) - 1
research.set["pct.change.primary.school.enrollment.ratio"] <- NA
research.set$pct.change.primary.school.enrollment.ratio <- research.set$three.year.lagged.primary.school.enrollment.ratio/lag(research.set$three.year.lagged.primary.school.enrollment.ratio, 1) - 1
research.set["pct.change.secondary.school.enrollment.ratio"] <- NA
research.set$pct.change.secondary.school.enrollment.ratio <- research.set$three.year.lagged.secondary.school.enrollment.ratio/lag(research.set$three.year.lagged.secondary.school.enrollment.ratio, 1) - 1
research.set["pct.change.secondary.school.repeat.ratio"] <- NA
research.set$pct.change.secondary.school.repeat.ratio <- research.set$three.year.lagged.secondary.school.repeat.ratio/lag(research.set$three.year.lagged.secondary.school.repeat.ratio, 1) - 1
research.set["pct.change.labor.participation.ratio"] <- NA
research.set$pct.change.labor.participation.ratio <- research.set$three.year.lagged.labor.participation.ratio/lag(research.set$three.year.lagged.labor.participation.ratio, 1) - 1

research.set["pct.change.vulnerable.employment.ratio"] <- NA
research.set$pct.change.vulnerable.employment.ratio <- research.set$three.year.lagged.vulnerable.employment.ratio/lag(research.set$three.year.lagged.vulnerable.employment.ratio, 1) - 1

#econ.full <- transform(econ.final, Three.year.lagged.DAC.flow.average = rowMeans(econ.final[,5:7], na.rm = TRUE))
#head(econ.full)
#View(econ.full)

#### Unused Country-Only Subset #####
econ.total <- subset(research.set, country == "Afghanistan" | country == "Albania" | country == "Algeria"| country == "American Samoa"| 
                       country == "Andorra"| country == "Angola"| country == "Antigua and Barbuda"| country == "Argentina"| country == "Armenia"| 
                       country == "Aruba"| country == "Australia"| country == "Austria"| country == "Azerbaijan"| country == "Bahamas, The"| 
                       country == "Bahrain"| country == "Bangladesh" | country ==  "Barbados" | country ==  "Belarus" | country == "Belgium" | 
                       country == "Belize" | country ==  "Benin"| country == "Bermuda"| country == "Bhutan" | country == "Bolivia" | 
                       country == "Bosnia and Herzegovina"| country == "Botswana"| country == "Brazil"| country == "Brunei Darussalam"| 
                       country == "Bulgaria"| country == "Burkina Faso"| country == "Burundi"| country == "Cambodia"| country == "Cameroon"| 
                       country == "Canada"| country == "Cape Verde"| country == "Cayman Islands"| country == "Chad"| country == "Chile"| 
                       country == "China"| country == "Colombia"| country == "Comoros"| country == "Congo, Dem. Rep."| country == "Congo, Dem. Rep."| 
                       country == "Costa Rica"| country == "Cote d' Ivoire"| country == "Croatia"| country == "Cuba"| country == "Curacao"| 
                       country == "Cyprus"| country == "Czech Republic"| country == "Denmark"| country == "Djibouti"| country == "Dominica"| 
                       country == "Dominican Republic"| country == "Ecuador"| country == "Egypt, Arab Rep."| country == "El Salvador"| 
                       country == "Equatorial Guinea"| country == "Eritrea"| country == "Estonia"| country == "Ethiopia"| country == "Faeroe Islands"| 
                       country == "Fiji"| country == "Finland"| country == "France"| country == "French Polynesia"| country == "Gabon"| 
                       country == "Gambia, The"| country == "Georgia"| country == "Germany"| country == "Ghana"| country == "Greece"| 
                       country == "Greenland"| country == "Grenada"| country == "Guam"| country == "Guatemala"| country == "Guinea"| 
                       country == "Guinea-Bissau"| country == "Guyana"| country == "Haiti"| country == "Honduras"| country == "Hungary"| 
                       country == "Iceland"| country == "India"| country == "Indonesia"| country == "Iran, Islamic Rep."| country == "Iraq"| 
                       country == "Ireland"| country == "Isle of Man"| country == "Israel"| country == "Italy"| country == "Jamaica"| country == "Japan"| 
                       country == "Jordan"| country == "Kazakhstan"| country == "Kenya"| country == "Kiribati"| country == "Korea, Dem. Rep."| 
                       country == "Korea, Rep."| country == "Kosovo"| country == "Kuwait"| country == "Kyrgyz Republic"| country == "Lao PDR"| 
                       country == "Latvia"| country == "Lebanon"| country == "Lesotho"| country == "Liberia"| country == "Libya"| country == "Liechtenstein"| 
                       country == "Lithuania"| country == "Luxembourg"| country == "Macao SAR, China"| country == "Macedonia, FYR"| country == "Madagascar"| 
                       country == "Malawi"| country == "Malaysia"| country == "Maldives"| country == "Mali"| country == "Malta"| country == "Marshall Islands"| 
                       country == "Mauritania"| country == "Mauritius"| country == "Mexico"| country == "Micronesia, Fed. Sts."| country == "Moldova"| 
                       country == "Monaco"| country == "Mongolia"| country == "Montenegro"| country == "Morocco"| country == "Mozambique"| country == "Myanmar"| 
                       country == "Namibia"| country == "Nepal"| country == "Netherlands"| country == "New Caledonia"| country == "New Zealand"| 
                       country == "Nicaragua"| country == "Niger"| country == "Nigeria"| country == "Norway"| country == "Oman"| country == "Pakistan"| 
                       country == "Palau"| country == "Panama"| country == "Papua New Guinea"| country == "Paraguay"| country == "Peru"| 
                       country == "Philippines"| country == "Poland"| country == "Portugal"| country == "Puerto Rico"| country == "Qatar"|
                       country == "Romania"| country == "Russian Federation"| country == "Rwanda"| country == "Samoa"| country == "San Marino"| 
                       country == "Sao Tome and Principe"| country == "Saudi Arabia"| country == "Senegal"| country == "Serbia"| country == "Seychelles"| 
                       country == "Sierra Leone"| country == "Singapore"| country == "Slovak Republic"| country == "Slovenia"| country == "Solomon Islands"| 
                       country == "Somalia"| country == "South Africa"| country == "Spain"| country == "Sri Lanka"| country == "St. Kitts and Nevis"| 
                       country == "St. Lucia"| country == "St. Vincent and the Grenadines"| country == "Sudan"| country == "Suriname"| country == "Swaziland"| 
                       country == "Sweden"| country == "Switzerland"| country == "Syrian Arab Republic"| country == "Tajikistan"| country == "Tanzania"| 
                       country == "Thailand"| country == "Timor-Leste"| country == "Togo"| country == "Tonga"| country == "Trinidad and Tobago"| 
                       country == "Tunisia"| country == "Turkey"| country == "Turkmenistan"| country == "Turks and Caicos Islands"| country == "Tuvalu"| 
                       country == "Uganda"| country == "Ukraine"| country == "United Kingdom"| country == "United States"| country == "Uruguay"| 
                       country == "Uzbekistan"| country == "Vanatu"| country == "Venezuela, RB"| country == "Vietnam"| country == "Yemen, Rep."| 
                       country == "Zambia"| country == "Zimbabwe")

# Creating data frame of 33 Developed Countries as stated by The World Factbook 
econset3 <- subset(econ.total, country == c("Canada", "Denmark"
                                                , "Faroe Islands", "Finland", "France", "Germany", "Greece", "Holy See", "Iceland"
                                                , "Ireland", "Israel", "Italy", "Japan", "Liechtenstein", "Luxembourg"
                                                , "Malta", "Monaco", "Netherlands", "New Zealand", "Norway", "Portugal", "San Marino"
                                                , "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"))

###### Unused Selecting random 25 country sample from econ.total dataset #########
country.research.subset <- subset(econ.total, year == "1970") 
country.subset <- country.research.subset[sample(nrow(country.research.subset), 25), ]
View(country.subset)

# Creating subset data frame of developing countries
# Developing Countries:
# Afghanistan; Angola; Bangladesh; Benin; Bhutan; Burkina Faso; Burundi
# Cambodia; Central African Republic; Chad; Comoros; Congo, Dem. Rep.
# Djibouti; Equatorial Guinea; Eritrea; Ethiopia; Gambia, The
# Guinnea-Bissau; Haiti; Kiribati; Lao PDR; Lesotho; Liberia
# Madagascar; Malawi; Mali; Mauritania; Mozambique; Myanmar
# Nepal; Niger; Rwanda; Sao Tome and Principe; Senegal
# Sierra Leone; Somalia; South Sudan; Sudan; Timor-Leste; Togo
# Tuvalu; Uganda; Tanzania; Vanuatu; Yemen, Rep.; Zambia
#list(names(country.subset$country))

###### Developed Countries Subset #####
country.subset <- subset(research.set, country == "Afghanistan" | country == "Angola" | country == "Bangladesh"| country == "Benin"
                       | country == "Bhutan" | country == "Burkina Faso"| country == "Burundi"| country == "Cambodia"
                       | country == "Central African Republic"| country == "Chad" | country == "Comoros"
                       | country == "Congo, Dem. Rep."| country == "Djibouti"| country == "Equatorial Guinea"| country == "Eritrea"
                       | country == "Ethipoia"| country == "Gambia, The"| country == "Guinnea-Bissau"| country == "Haiti"
                       | country == "Kiribati"| country == "Lao PDR"| country == "Lesotho"| country == "Liberia"
                       | country == "Madagascar" | country == "Malawi" | country == "Mali" | country == "Mauritania"
                       | country == "Mozambique" | country == "Myanmar" | country == "Nepal" | country == "Niger" 
                       | country == "Rwanda" | country == "Sao Tome and Principe" | country == "Senegal" | country == "Sierra Leone"
                       | country == "Somalia" | country == "South Sudan" | country == "Sudan" | country == "Timor-Leste"
                       | country == "Togo" | country == "Tuvalu" | country == "Uganda" | country == "Tanzania"
                       | country == "Vanuatu" | country == "Yemen, Rep." | country == "Zambia")
names(country.subset)
pdim(country.subset, index = c("country", "year"))
summary(country.subset$three.year.lagged.labor.force.participation.ratio)

####### UNUSED Create log transformations of variables #####
set.seed(50)
dev.pct.change.vulnerable.employment.ratio <- country.subset$pct.change.vulnerable.employment.ratio
    dev.logtrans.vulnerable.employment.ratio <- log(dev.pct.change.vulnerable.employment.ratio)
    dev.logtrans.vulnerable.employment.ratio <- dev.logtrans.vulnerable.employment.ratio[which(is.nan(dev.logtrans.vulnerable.employment.ratio))] <- NA
    dev.logtrans.vulnerable.employment.ratio <- dev.logtrans.vulnerable.employment.ratio[which(dev.logtrans.vulnerable.employment.ratio==Inf)] <- NA
    dev.logtrans.vulnerable.employment.ratio <- as.numeric(dev.logtrans.vulnerable.employment.ratio)
dev.pct.change.labor.participation.ratio <- country.subset$pct.change.labor.participation.ratio
    dev.logtrans.labor.participation.ratio <- log(dev.pct.change.labor.participation.ratio)
    dev.logtrans.labor.participation.ratio <- dev.logtrans.labor.participation.ratio[which(is.nan(dev.logtrans.labor.participation.ratio))] <- NA
    dev.logtrans.labor.participation.ratio <- dev.logtrans.labor.participation.ratio[which(dev.logtrans.labor.participation.ratio==Inf)] <- NA
    dev.logtrans.labor.participation.ratio <- as.numeric(dev.logtrans.labor.participation.ratio)
dev.pct.change.secondary.school.repeat.ratio <- country.subset$pct.change.secondary.school.repeat.ratio
    dev.logtrans.secondary.school.repeat.ratio <- log(dev.pct.change.secondary.school.repeat.ratio)
    dev.logtrans.secondary.school.repeat.ratio <- dev.logtrans.secondary.school.repeat.ratio[which(is.nan(dev.logtrans.secondary.school.repeat.ratio))] <- NA
    dev.logtrans.secondary.school.repeat.ratio <- dev.logtrans.secondary.school.repeat.ratio[which(dev.logtrans.secondary.school.repeat.ratio==Inf)] <- NA
    dev.logtrans.secondary.school.repeat.ratio <- as.numeric(dev.logtrans.secondary.school.repeat.ratio)
dev.pct.change.secondary.school.enrollment.ratio <- country.subset$pct.change.secondary.school.enrollment.ratio
    dev.logtrans.secondary.school.enrollment.ratio <- log(dev.pct.change.secondary.school.enrollment.ratio)
    dev.logtrans.secondary.school.enrollment.ratio <- dev.logtrans.secondary.school.enrollment.ratio[which(is.nan(dev.logtrans.secondary.school.enrollment.ratio))] <- NA
    dev.logtrans.secondary.school.enrollment.ratio <- dev.logtrans.secondary.school.enrollment.ratio[which( dev.logtrans.secondary.school.enrollment.ratio==Inf)] <- NA
    dev.logtrans.secondary.school.enrollment.ratio <- as.numeric(dev.logtrans.secondary.school.enrollment.ratio)
dev.pct.change.primary.school.enrollment.ratio <- country.subset$pct.change.primary.school.enrollment.ratio
    dev.logtrans.primary.school.enrollment.ratio <- log(dev.pct.change.primary.school.enrollment.ratio)
    dev.logtrans.primary.school.enrollment.ratio <- dev.logtrans.primary.school.enrollment.ratio[which(is.nan(dev.logtrans.primary.school.enrollment.ratio))] <- NA
    dev.logtrans.primary.school.enrollment.ratio <- dev.logtrans.primary.school.enrollment.ratio[which(dev.logtrans.primary.school.enrollment.ratio==Inf)] <- NA
    dev.logtrans.primary.school.enrollment.ratio <- as.numeric(dev.logtrans.primary.school.enrollment.ratio)
dev.pct.change.primary.school.repeat.ratio <- country.subset$pct.change.primary.school.repeat.ratio
    dev.logtrans.primary.school.repeat.ratio <- log(dev.pct.change.primary.school.repeat.ratio)
    dev.logtrans.primary.school.repeat.ratio <- dev.logtrans.primary.school.repeat.ratio[which(is.nan(dev.logtrans.primary.school.repeat.ratio))] <- NA
    dev.logtrans.primary.school.repeat.ratio <- dev.logtrans.primary.school.repeat.ratio[which(dev.logtrans.primary.school.repeat.ratio==Inf)] <- NA
    dev.logtrans.primary.school.repeat.ratio <- as.numeric(dev.logtrans.primary.school.repeat.ratio)
dev.pct.change.primary.school.completion.ratio <- country.subset$pct.change.primary.school.completion.ratio
    dev.logtrans.primary.school.completion.ratio <- log(dev.pct.change.primary.school.completion.ratio)
    dev.logtrans.primary.school.completion.ratio <- dev.logtrans.primary.school.completion.ratio[which(is.nan(dev.logtrans.primary.school.completion.ratio))] <- NA
    dev.logtrans.primary.school.completion.ratio <- dev.logtrans.primary.school.completion.ratio[which(dev.logtrans.primary.school.completion.ratio==Inf)] <- NA
    dev.logtrans.primary.school.completion.ratio <- as.numeric(dev.logtrans.primary.school.completion.ratio)
dev.pct.change.DAC <- country.subset$pct.change.DAC
    dev.logtrans.DAC <- log(dev.pct.change.DAC)
    dev.logtrans.DAC <- dev.logtrans.DAC[which(is.nan(dev.logtrans.DAC))] <- NA
    dev.logtrans.DAC <- dev.logtrans.DAC[which(dev.logtrans.DAC==Inf)] <- NA
    dev.logtrans.DAC <- as.numeric(dev.logtrans.DAC)
dev.pct.change.adult.literacy.ratio <- country.subset$pct.change.adult.literacy.ratio
    dev.logtrans.adult.literacy.ratio <- log(dev.pct.change.adult.literacy.ratio)
    dev.logtrans.adult.literacy.ratio <- dev.logtrans.adult.literacy.ratio[which(is.nan(dev.logtrans.adult.literacy.ratio))] <- NA
    dev.logtrans.adult.literacy.ratio <- dev.logtrans.adult.literacy.ratio[which(dev.logtrans.adult.literacy.ratio==Inf)] <- NA
    dev.logtrans.adult.literacy.ratio <- as.numeric(dev.logtrans.adult.literacy.ratio)
dev.pct.change.youth.literacy.ratio <- country.subset$pct.change.youth.literacy.ratio
    dev.logtrans.youth.literacy.ratio <- log(dev.pct.change.youth.literacy.ratio)
    dev.logtrans.youth.literacy.ratio <- dev.logtrans.youth.literacy.ratio[which(is.nan(dev.logtrans.youth.literacy.ratio))] <- NA
    dev.logtrans.youth.literacy.ratio <- dev.logtrans.youth.literacy.ratio[which(dev.logtrans.youth.literacy.ratio==Inf)] <- NA
    dev.logtrans.youth.literacy.ratio <- as.numeric(dev.logtrans.youth.literacy.ratio)
dev.pct.change.GDP <- country.subset$pct.change.GDP
    dev.logtrans.GDP <- log(dev.pct.change.GDP)
    dev.logtrans.GDP <- dev.logtrans.GDP[which(is.nan(dev.logtrans.GDP))] <- NA
    dev.logtrans.GDP <- dev.logtrans.GDP[which(dev.logtrans.GDP==Inf)] <- NA
    dev.logtrans.GDP <- as.numeric(dev.logtrans.GDP)
    
####### Format Variables #####
#  Fixed Effects
dev.year <- country.subset$year
dev.country <- country.subset$country
dev.wbcode <- country.subset$wbcode
      # Literacy Three Year Lag
dev.lag.youth.literacy.ratio <- country.subset$three.year.lagged.youth.literacy.ratio
#full.dev.youth.literacy.ratio <- na.spline(dev.lag.youth.literacy.ratio)
  #dev.lag.youth.literacy.ratio <- dev.lag.youth.literacy.ratio[which(is.nan(dev.lag.youth.literacy.ratio))] <- NA
  #dev.lag.youth.literacy.ratio <- dev.lag.youth.literacy.ratio[which(dev.lag.youth.literacy.ratio==Inf)] <- NA
  dev.lag.youth.literacy.ratio <- as.numeric(dev.lag.youth.literacy.ratio)
  summary(dev.lag.youth.literacy.ratio)
  
dev.lag.adult.literacy.ratio <- country.subset$three.year.lagged.adult.literacy.ratio
#full.dev.adult.literacy.ratio <- na.spline(dev.lag.adult.literacy.ratio)
  #dev.lag.adult.literacy.ratio <- dev.lag.adult.literacy.ratio[which(is.nan(dev.lag.adult.literacy.ratio))] <- NA
  #dev.lag.adult.literacy.ratio <- dev.lag.adult.literacy.ratio[which(dev.lag.adult.literacy.ratio==Inf)] <- NA
  dev.lag.adult.literacy.ratio <- as.numeric(dev.lag.adult.literacy.ratio)
  summary(dev.lag.adult.literacy.ratio)
  
      # Dependent Vars Three Year Lag
dev.lag.DAC.flow <- country.subset$three.year.lagged.DAC.flow.average
#full.dev.DAC.flow <- na.spline(dev.lag.DAC.flow)
  #dev.lag.DAC.flow <- dev.lag.DAC.flow[which(is.nan(dev.lag.DAC.flow))] <- NA
  #dev.lag.DAC.flow <- dev.lag.DAC.flow[which(dev.lag.DAC.flow==Inf)] <- NA
  dev.lag.DAC.flow <- as.numeric(dev.lag.DAC.flow)

dev.lag.GDP <- country.subset$three.year.lagged.GDP.average
#full.dev.GDP <- na.spline(dev.lag.GDP)
  #dev.lag.GDP <- dev.lag.GDP[which(is.nan(dev.lag.GDP))] <- NA
  #dev.lag.GDP <- dev.lag.GDP[which(dev.lag.GDP==Inf)] <- NA
  dev.lag.GDP <- as.numeric(dev.lag.GDP)
      # Labor Data Three Year Lag
dev.lag.labor.participation.ratio <- country.subset$three.year.lagged.labor.force.participation.ratio
#full.dev.labor.participation.ratio <- na.spline(dev.lag.labor.participation.ratio)
  #dev.lag.labor.participation.ratio <- dev.lag.labor.participation.ratio[which(is.nan(dev.lag.labor.participation.ratio))] <- NA
  #dev.lag.labor.participation.ratio <- dev.lag.DAC.flow[which(dev.lag.labor.participation.ratio==Inf)] <- NA
  dev.lag.labor.participation.ratio <- as.numeric(dev.lag.labor.participation.ratio)
  summary(dev.lag.labor.participation.ratio)
  
dev.lag.vulnerable.employment.ratio <- country.subset$three.year.lagged.vulnerable.employment.ratio
#full.dev.vulnerable.employment.ratio <- na.spline(dev.lag.vulnerable.employment.ratio)
  #dev.lag.vulnerable.employment.ratio <- dev.lag.vulnerable.employment.ratio[which(is.nan(dev.lag.vulnerable.employment.ratio))] <- NA
  #dev.lag.vulnerable.employment.ratio <- dev.lag.vulnerable.employment.ratio[which(dev.lag.vulnerable.employment.ratio==Inf)] <- NA
  dev.lag.vulnerable.employment.ratio <- as.numeric(dev.lag.vulnerable.employment.ratio)
      # Education Three Year Lag
dev.lag.primary.school.completion.ratio <- country.subset$three.year.lagged.primary.school.completion.ratio
#full.dev.primary.school.completion.ratio <- na.spline(dev.lag.primary.school.completion.ratio)
  #dev.lag.primary.school.completion.ratio <- country.subset$three.year.lagged.primary.school.completion.ratio[which(is.nan(dev.lag.primary.school.completion.ratio))] <- NA
  #dev.lag.primary.school.completion.ratio <- dev.lag.primary.school.completion.ratio[which(dev.lag.primary.school.completion.ratio==Inf)] <- NA
  dev.lag.primary.school.completion.ratio <- as.numeric(dev.lag.primary.school.completion.ratio)
  
dev.lag.primary.school.enrollment.ratio <- country.subset$three.year.lagged.primary.school.enrollment.ratio
#full.dev.primary.school.enrollment.ratio <- na.spline(dev.lag.primary.school.enrollment.ratio)
  #dev.lag.primary.school.enrollment.ratio <- dev.lag.primary.school.enrollment.ratio[which(is.nan(dev.lag.primary.school.enrollment.ratio))] <- NA
  #dev.lag.primary.school.enrollment.ratio <- dev.lag.primary.school.enrollment.ratio[which(dev.lag.primary.school.enrollment.ratio==Inf)] <- NA
  dev.lag.primary.school.enrollment.ratio <- as.numeric(dev.lag.primary.school.enrollment.ratio)
  
dev.lag.primary.school.repeat.ratio <- country.subset$three.year.lagged.primary.school.repeat.ratio
#full.dev.primary.school.repeat.ratio <- na.spline(dev.lag.primary.school.repeat.ratio)
  #dev.lag.primary.school.repeat.ratio <- dev.lag.primary.school.repeat.ratio[which(is.nan(dev.lag.primary.school.repeat.ratio))] <- NA
  #dev.lag.primary.school.repeat.ratio <- dev.lag.DAC.flow[which(dev.lag.primary.school.repeat.ratio==Inf)] <- NA
  dev.lag.primary.school.repeat.ratio <- as.numeric(dev.lag.primary.school.repeat.ratio)

dev.lag.secondary.school.enrollment.ratio <- country.subset$three.year.lagged.secondary.school.enrollment.ratio
#full.dev.secondary.school.enrollment.ratio <- na.spline(dev.lag.secondary.school.enrollment.ratio)
  #dev.lag.secondary.school.enrollment.ratio <- dev.lag.secondary.school.enrollment.ratio[which(is.nan(dev.lag.secondary.school.enrollment.ratio))] <- NA
  #dev.lag.secondary.school.enrollment.ratio <- dev.lag.secondary.school.enrollment.ratio[which(dev.lag.secondary.school.enrollment.ratio==Inf)] <- NA
  dev.lag.secondary.school.enrollment.ratio <- as.numeric(dev.lag.secondary.school.enrollment.ratio)

dev.lag.secondary.school.repeat.ratio <- country.subset$three.year.lagged.secondary.school.repeat.ratio
#full.dev.secondary.school.repeat.ratio <- na.spline(dev.lag.secondary.school.repeat.ratio)
  #dev.lag.secondary.school.repeat.ratio <- dev.lag.secondary.school.repeat.ratio[which(is.nan(dev.lag.secondary.school.repeat.ratio))] <- NA
  #dev.lag.secondary.school.repeat.ratio <- dev.lag.secondary.school.repeat.ratio[which(dev.lag.secondary.school.repeat.ratio==Inf)] <- NA
  dev.lag.secondary.school.repeat.ratio <- as.numeric(dev.lag.secondary.school.repeat.ratio)

dev.life.expectancy <- country.subset$life.expectancy

#head(full.dev.labor.participation.ratio, 55)
#summary(lm(dev.lag.GDP ~ full.dev.labor.participation.ratio))


########## Unused Code ##########
#View(research.set)
# Create new columns to lag GDP
research.set["Previous.Year.GDP"] <- NA
research.set["Second.Previous.Year.GDP"] <- NA
research.set["Three.year.lagged.GDP.average"] <- NA
research.set$GDP.US. <- as.numeric(research.set$GDP.US.)
research.set.2 <- mutate(research.set, Previous.Year.GDP = lag(GDP.US., 1))
research.full <- mutate(research.set.2, Second.Previous.Year.GDP = lag(Previous.Year.GDP, 1))
research.full <- research.full[,c(1:4, 114:116, 5, 110:113, 6:109)]
research.stack <- transform(research.full, Three.year.lagged.GDP.average = rowMeans(research.full[,4:6], na.rm = TRUE))
View(research.stack)

# Recognize that 2013 has NA data cross the board, choose not to remove the year however
#country.next <- subset(research.stack, year < 2013)
country.plus <- research.stack[,c(1:3, 9, 4:8, 10:113)]
country.plus <- country.plus[,c(1, 9, 2:8, 10:113)]
#econ.next <- mutate(econ.plus, Country_name = lag(country, 1))

# Creating new variables for % Change in Dependent Variables between 3-year averages
country.plus["percent.change.DAC"] <- NA
country.plus["percent.change.GDP"] <- NA
country.plus$percent.change.DAC <- country.plus$Three.year.lagged.DAC.flow.average/lag(country.plus$Three.year.lagged.DAC.flow.average, 1) - 1
country.plus$percent.change.GDP <- country.plus$Three.year.lagged.GDP.average/lag(country.plus$Three.year.lagged.GDP.average, 1) - 1
head(country.plus)
country.dev <- country.plus[,c(1:4, 114:115, 5:113)]
View(country.dev)


# Create new data frames to begin development model analysis
# We want to predict when a country will transition from being a DAC flow receiver to when they are a DAC contributor
##### Creating New Variables
  # Ratio of literacy rates fem/male for youth and adult
country.dev["youth.literacy.ratio.fem.mal"] <- NA
country.dev$youth.literacy.ratio.fem.mal <- dev.female.youth.literacy.rate/dev.male.youth.literacy.rate 
dev.youth.literacy.ratio <- country.dev$youth.literacy.ratio.fem.mal
research.stack <- transform(research.full, Three.year.lagged.GDP.average = rowMeans(research.full[,4:6], na.rm = TRUE))

country.dev["adult.literacy.ratio.fem.mal"] <- NA
country.dev$adult.literacy.ratio.fem.mal <- dev.female.adult.literacy.rate/dev.male.adult.literacy.rate
dev.adult.literacy.ratio <- country.dev$adult.literacy.ratio.fem.mal
  # Primary school completion ratio fem/mal
country.dev["primary.school.completion.ratio.fem.mal"] <- NA
country.dev$primary.school.completion.ratio.fem.mal <- dev.female.primary.school.completion.rate/dev.male.primary.school.completion.rate
dev.primary.school.completion.ratio <- country.dev$primary.school.completion.ratio.fem.mal

  # Primary school enrollment ratio fem/mal
country.dev["primary.school.enrollment.ratio.fem.mal"] <- NA
country.dev$primary.school.enrollment.ratio.fem.mal <- dev.female.primary.school.enrollment/dev.male.primary.school.enrollment
dev.primary.school.enrollment.ratio <- country.dev$primary.school.completion.ratio.fem.mal

  # Primary school repeat ratio fem/mal
country.dev["primary.school.repeat.ratio.fem.mal"] <- NA
country.dev$primary.school.repeat.ratio.fem.mal <- dev.female.primary.school.repeat.enrollment/dev.male.primary.school.repeat.enrollment
dev.primary.school.repeat.ratio <- country.dev$primary.school.repeat.ratio.fem.mal

  # Secondary school repeat ratio fem/mal
dev.secondary.school.enrollment.female <- country.dev$secondary.enrollment.female.
dev.secondary.school.enrollment.male <- country.dev$secondary.enrollment.male.
dev.secondary.school.repeat.female <- country.dev$secondary.repeat.female.enrollment
dev.secondary.school.repeat.male <- country.dev$secondary.repeat.male.enrollment
country.dev["secondary.school.enrollment.ratio.fem.mal"] <- NA
country.dev$secondary.school.enrollment.ratio.fem.mal <- dev.secondary.school.enrollment.female/dev.secondary.school.enrollment.male
dev.secondary.school.enrollment.ratio <- country.dev$secondary.school.enrollment.ratio.fem.mal

country.dev["secondary.school.repeat.ratio.fem.mal"] <- NA
country.dev$secondary.school.repeat.ratio.fem.mal <- dev.secondary.school.repeat.female/dev.secondary.school.repeat.male
dev.secondary.school.repeat.ratio <- country.dev$secondary.school.repeat.ratio.fem.mal

  # Vulnerable employment ratio fem/mal
country.dev["vulnerable.employment.ratio.fem.mal"] <- NA
country.dev$vulnerable.employment.ratio.fem.mal <- dev.female.vulnerable.employment.rate/dev.male.vulnerable.employment.rate
dev.vulnerable.employment.ratio <- country.dev$vulnerable.employment.ratio.fem.mal
  # Female population to total population ratio
country.dev["population.ratio.fem.mal"] <- NA
country.dev$population.ratio.fem.mal <- dev.female.population.total/dev.population.total
dev.population.ratio <- country.dev$population.ratio.fem.mal
  # Labor force participation ratio
dev.labor.participation.ratio <- country.dev$laborparticipation.ratio.female.to.male 


# Dependent Vars
#######dev.three.year.DAC.flow <- country.dev$Three.year.lagged.DAC.flow.average
dev.percent.change.three.year.average.DAC <- country.dev$percent.change.DAC
########dev.three.year.GDP.average <- country.dev$Three.year.lagged.GDP.average
dev.percent.change.three.year.average.GDP <- country.dev$percent.change.DAC

# Fixed Effets
dev.year <- country.dev$year
dev.country <- country.dev$country
dev.wbcode <- country.dev$wbcode

 
# Educational Equality
  # Literacy Rate (first two lines were created based on vars from next two)
dev.youth.literacy.ratio <- country.dev$youth.literacy.ratio.fem.mal 
dev.adult.literacy.ratio <- country.dev$adult.literacy.ratio

dev.literacy.ratio.youth <- country.dev$youth.literacy.ratio.fem.mal
dev.literacy.ratio.adult <- country.dev$adult.literacy.ratio.fem.mal
#dev.female.youth.literacy.rate <- country.dev$literacy.rate.youth.female
#dev.male.youth.literacy.rate <- country.dev$literacy.rate.youth.male
#dev.female.adult.literacy.rate <- country.dev$literacy.rate.adult.female
#dev.male.adult.literacy.rate <- country.dev$literacy.rate.adult.male

  # Primary School
dev.primary.school.enrollment.ratio <- country.dev$primary.school.completion.ratio.fem.mal
dev.primary.school.completion.ratio <- country.dev$primary.school.completion.ratio.fem.mal
dev.primary.school.repeat.ratio <- country.dev$primary.school.repeat.ratio.fem.mal
#dev.female.primary.school.repeat.percent.enrollment <- country.dev$primary.repeat.female.enrollment
#dev.male.primary.school.repeat.percent.enrollment <- country.dev$primary.repeat.male.enrollment
#dev.female.primary.school.completion.rate <- country.dev$primary.completion.rate.female.
#dev.male.primary.school.completion.rate <- country.dev$primary.completion.rate.male.
#dev.male.primary.school.enrollment <- country.dev$primary.school.enrollment.male.
#dev.female.primary.school.enrollment <- country.dev$primary.school.enrollment.female.
      # Government Expenditure
#dev.primary.expenditure.per.student.percent.GDP <- country.dev$primary.gov.expenditure.per.student..GDP

  # Secondary School
dev.secondary.school.enrollment.ratio <- country.dev$secondary.school.enrollment.ratio.fem.mal
      # Government Expenditure
dev.secondary.expenditure.per.student.percent.GDP <- country.dev$secondary.gov.expenditure.per.student..GDP

# Labor Force Equality
dev.labor.participation.ratio <- country.dev$laborparticipation.ratio.female.to.male 
dev.female.labor.participation.rate <- country.dev$labor.participation.rate.female.15.64
dev.male.labor.participation.rate <- country.dev$labor.participation.rate.male.15.64
  # Vulnerable Employment

dev.female.vulnerable.employment.rate <- country.dev$vulnerable.employment.female..employed
dev.male.vulnerable.employment.rate <- country.dev$vulnerable.employment.male..employed

# Income Equality
dev.GINI.equality.index <- country.dev$GINI.index.of.household.equality
dev.income.share.highest.10.percent <- country.dev$income.share.held.by.highest.10.
dev.income.share.highest.20.percent <- country.dev$income.share.held.by.highest.20.
dev.income.share.second.20.percent <- country.dev$income.share.held.by.second.20.
dev.income.share.third.20.percent <- country.dev$income.share.held.by.third.20.
dev.income.share.fourth.20.percent <- country.dev$income.share.held.by.fourth.20.
dev.income.share.lowest.20.percent <- country.dev$income.share.held.by.lowest.20.

############  Probably won't be included
# Country Expenditures (not included earlier) (%GDP)
country.dev$military.expenditure..GDP
country.dev$market.cap.domestic.companies..GDP
country.dev$foreign.direct.investment.inflow.GDP.
country.dev$total.value.stocks.traded..GDP

# Other
dev.female.population.total <- country.dev$female.population.total
dev.population.total <- country.dev$population.total
dev.teen.mothers.percent.women.15.19 <- country.dev$teen.mothers..women15.19


# Testing country pollution by income
emit <- wdi$EN_ATM_CO2E_KD_GD   # pollution emmitted
gini <- wdi$SI_POV_GINI # gini coefficient
income <- wdi$NY_GDP_PCAP_KD #  
incomesq <- income^2
samplerun <- data.frame(emit, gini, income, incomesq, na.rm = TRUE)
summary(samplerun)
rcorr(as.matrix(samplerun))

# Gini only
model1 <- glm(emit ~ gini)
model1

# Introduce country fixed effect
model2 <- glm (emit ~ gini + factor(wdi$country))
model2

# Gini + Income
model3 <- glm(emit ~ gini  + income)
model3

# Gini + Income + Income^2
model4 <- glm(emit ~ gini + income + incomesq)
model4

# Gini + Income + Income^2 and country fixed effect
model5 <- glm(emit ~ gini + income + incomesq + factor(wdi$country))
model5

# All variables
model6 <- glm(emit ~ gini + income + incomesq + factor(wdi$country) + factor(wdi$year))
model6

######## Begin fixed effects model research ########

############ Predicting GDP 3-Year Average
  # Descriptive statistics
dev.labor.participation.ratio.sq <- dev.lag.labor.participation.ratio^2
dev.vulnerable.employment.ratio.sq <- dev.lag.vulnerable.employment.ratio^2
dev.GDP.sq <- dev.lag.GDP^2
dev.adult.literacy.ratio.sq <- dev.lag.adult.literacy.ratio^2
dev.youth.literacy.ratio.sq <- dev.lag.youth.literacy.ratio^2
dev.primary.completion.ratio.sq <- dev.lag.primary.school.completion.ratio^2
dev.primary.enrollment.ratio.sq <- dev.lag.primary.school.enrollment.ratio^2
dev.primary.repeat.ratio.sq <- dev.lag.primary.school.repeat.ratio^2
dev.secondary.enrollment.ratio.sq <- dev.lag.secondary.school.enrollment.ratio^2
dev.secondary.repeat.ratio.sq <- dev.lag.secondary.school.repeat.ratio^2
log.dev.life.expectancy <- log(dev.life.expectancy)
dev.life.expectancy.sq <- dev.life.expectancy^2
dev.year.sq <- dev.year^2
country.subset["log.dev.GDP"] <- NA
country.subset$log.dev.GDP <- log(dev.lag.GDP)
log.dev.GDP <- country.subset$log.dev.GDP
country.subset["log.dev.labor.participation.ratio"] <- NA
country.subset$log.dev.labor.participation.ratio <- log(dev.lag.labor.participation.ratio)
log.dev.labor.participation.ratio <- country.subset$log.dev.labor.participation.ratio

country.subset["log.dev.adult.literacy.ratio"] <- NA
country.subset$log.dev.adult.literacy.ratio <- log(dev.lag.adult.literacy.ratio)
log.dev.adult.literacy.ratio <- country.subset$log.dev.adult.literacy.ratio

country.subset["log.dev.secondary.enrollment.ratio"] <- NA
country.subset$log.dev.secondary.enrollment.ratio <- log(dev.lag.secondary.school.enrollment.ratio)
log.dev.secondary.enrollment.ratio <- country.subset$log.dev.secondary.enrollment.ratio

country.subset["log.dev.vulnerable.employment.ratio"] <- NA
country.subset$log.dev.vulnerable.employment.ratio <- log(dev.lag.vulnerable.employment.ratio)
log.dev.vulnerable.employment.ratio <- country.subset$log.dev.vulnerable.employment.ratio

country.subset["log.dev.primary.school.enrollment.ratio"] <- NA
country.subset$log.dev.primary.school.enrollment.ratio <- log(dev.lag.primary.school.enrollment.ratio)
log.dev.primary.school.enrollment.ratio <- country.subset$log.dev.primary.school.enrollment.ratio


devAdultLiteracyDensity <- density(dev.lag.adult.literacy.ratio, na.rm = TRUE)
plot(devAdultLiteracyDensity, main = "Adult Literacy Ratio", xlab = "Adult Literacy Ratio")
polygon(devAdultLiteracyDensity, col = "orange", border = "orange")
kurtosis(dev.lag.adult.literacy.ratio, na.rm = TRUE)

devYouthLiteracyDensity <- density(dev.lag.youth.literacy.ratio, na.rm = TRUE)
plot(devYouthLiteracyDensity, main = "Youth Literacy Ratio", xlab = "Youth Literacy Ratio")
polygon(devYouthLiteracyDensity, col = "blue", border = "blue")
kurtosis(dev.lag.youth.literacy.ratio, na.rm = TRUE)

devLaborParticipationDensity <- density(dev.lag.labor.participation.ratio, na.rm = TRUE)
plot(devLaborParticipationDensity, main = "Labor Participation Ratio", xlab = "Labor Participation Ratio")
polygon(devLaborParticipationDensity, col = "green", border = "green")
kurtosis(dev.lag.labor.participation.ratio, na.rm = TRUE)

devVulnerableEmploymentDensity <- density(dev.lag.vulnerable.employment.ratio, na.rm = TRUE)
plot(devVulnerableEmploymentDensity, main = "Vulnerable Employment Ratio", xlab = "Vulnerable Employment Ratio")
polygon(devVulnerableEmploymentDensity, col = "red", border = "red")
kurtosis(dev.lag.vulnerable.employment.ratio, na.rm = TRUE)

devSecondaryEnrollmentDensity <- density(dev.lag.secondary.school.enrollment.ratio, na.rm = TRUE)
plot(devSecondaryEnrollmentDensity, main = "Secondary Enrollment Ratio", xlab = "Secondary Enrollment Ratio")
polygon(devSecondaryEnrollmentDensity, col = "blue", border = "blue")
kurtosis(dev.lag.secondary.school.enrollment.ratio, na.rm = TRUE)

devPrimaryEnrollmentDensity <- density(dev.lag.primary.school.enrollment.ratio, na.rm = TRUE)
plot(devPrimaryEnrollmentDensity, main = "Primary Enrollment Ratio", xlab = "Primary Enrollment Ratio")
polygon(devPrimaryEnrollmentDensity, col = "blue", border = "blue")
kurtosis(dev.lag.primary.school.enrollment.ratio, na.rm = TRUE)

devGDPLogDensity <- density(log.dev.GDP, na.rm = TRUE)
plot(devGDPLogDensity, main = "Log GDP", xlab = "Log GDP")
polygon(devGDPLogDensity, col = "red", border = "blue")
kurtosis(log.dev.GDP, na.rm = TRUE)

devGDPDensity <- density(dev.lag.GDP, na.rm = TRUE)
plot(devGDPDensity, main = "GDP", xlab = "GDP")
polygon(devGDPDensity, col = "red", border = "blue")
kurtosis(dev.lag.GDP, na.rm = TRUE)

devLifeExpectancy <- density(dev.life.expectancy, na.rm = TRUE)
plot(devLifeExpectancy, main = "Life Expectancy", xlab = "Life Expectancy")
polygon(devLifeExpectancy, col = "red", border = "blue")
kurtosis(dev.life.expectancy, na.rm = TRUE)
hist(dev.life.expectancy)

hist(labor.force.participation.ratio)
plot(density(labor.force.participation.ratio, na.rm = TRUE))
hist(dev.lag.vulnerable.employment.ratio)
plot(density(dev.lag.vulnerable.employment.ratio, na.rm = TRUE))
summary(labor.force.participation.ratio)
View(country.subset)

#describe(labor.force.participation.ratio)
describe(country.subset)
# Labor Ratio to GDP
devLaborModel1 <- lm(dev.lag.GDP ~ dev.lag.labor.participation.ratio)
summary(devLaborModel1)
# Vulnerable Employment to GDP
devLaborModel2 <- lm(dev.lag.GDP ~ dev.lag.vulnerable.employment.ratio)
summary(devLaborModel2) # note here that decrease in vulnerable employment ratio results in higher GDP (in data females have higher vulnerable employment rate than men)
# Vulnerable and Labor ratio to GDP
devLaborModel3 <- lm(dev.lag.GDP ~ dev.lag.vulnerable.employment.ratio 
                     + dev.lag.labor.participation.ratio + log.dev.labor.participation.ratio)
summary(devLaborModel3) 
# Labor ratio to Vulnerable
devLaborModel4 <- lm(dev.lag.labor.participation.ratio ~ dev.lag.vulnerable.employment.ratio)
summary(devLaborModel4)
#devLaborModel5 <- lm(dev.logtrans.GDP ~ dev.logtrans.labor.participation.ratio 
 #                    + dev.logtrans.vulnerable.employment.ratio
  #                   )
#summary(devLaborModel5)
 #Introduce square for quadtratic effect


devLaborModel6 <- lm(dev.lag.GDP ~ dev.lag.labor.participation.ratio + dev.lag.vulnerable.employment.ratio 
                     + dev.labor.participation.ratio.sq + dev.vulnerable.employment.ratio.sq
                     + log.dev.labor.participation.ratio + log.dev.vulnerable.employment.ratio
                     + dev.life.expectancy
                     )
summary(devLaborModel6)
anova(devLaborModel6)
# This is very interesting. Labor participation ratio is a significant factor in GDP up to certain point, as evidenced by positive/negative of sq and regular
# Anova shows that lag of labor participation ratio is lone significant variable in predicting GDP

# Labor ratio to adult literacy ratio
devLaborModel8 <- lm(dev.lag.labor.participation.ratio ~ dev.lag.adult.literacy.ratio)
summary(devLaborModel8)
# High collinearity between labor participation ratio and adult literacy ratio
LaborLiteracyPlot <- plotly::plot_ly(y = dev.lag.adult.literacy.ratio, x = dev.lag.labor.participation.ratio,
                                     type = "scatter", mode = "markers + lines", color = dev.country, opacity = 1)
LaborLiteracyPlot

LifeModel1 <- lm(dev.lag.labor.participation.ratio ~ dev.life.expectancy + log(dev.life.expectancy))
summary(LifeModel1)
LifeLaborEquality <- plotly::plot_ly(y = dev.lag.labor.participation.ratio, x = dev.life.expectancy,
                                     type = "scatter", mode = "markers + lines", color = dev.country, opacity = 1)
LifeLaborEquality


devModelGDPsq <- lm(dev.GDP.sq ~ dev.lag.labor.participation.ratio
                     #+ dev.lag.vulnerable.employment.ratio
                     + dev.lag.adult.literacy.ratio + log.dev.adult.literacy.ratio
                     + dev.labor.participation.ratio.sq + log.dev.labor.participation.ratio
                     #+ dev.vulnerable.employment.ratio.sq 
                     + dev.life.expectancy
                     )
summary(devModelGDPsq)
anova(devModelGDPsq)
      # Keep labor participation ratio, save vulnerable employment ratio for later use (possible interaction term)
  # Literacy ratios


devLiteracyModel1 <- lm(dev.lag.GDP ~ dev.lag.adult.literacy.ratio + dev.lag.youth.literacy.ratio)
summary(devLiteracyModel1)
devLiteracyModel2 <- lm(dev.lag.adult.literacy.ratio ~ dev.lag.youth.literacy.ratio)
summary(devLiteracyModel2)
devLiteracyModel3 <- lm(dev.lag.GDP ~ dev.lag.adult.literacy.ratio + dev.lag.youth.literacy.ratio 
                        + dev.adult.literacy.ratio.sq + dev.youth.literacy.ratio.sq
                        )
summary(devLiteracyModel3)

  # Remove youth literacy ratio, keep adult
devGDPbyLiteracyRatio <- plotly::plot_ly(y = dev.lag.GDP, x = dev.lag.adult.literacy.ratio,  
                                         type = "scatter", mode = "markers", color = dev.country, opacity = 1)
devGDPbyLiteracyRatio

AIC(devLiteracyModel3) # Akaike Information Criterion for model selection

# There is high collinearity between adult and youth literacy rates

# Schooling Equality model


#devSchoolModel1 <- lm(dev.lag.GDP ~ dev.lag.primary.school.completion.ratio)
#summary(devSchoolModel1)
devGDPbySecondaryEnrollment <- plotly::plot_ly(y = dev.lag.GDP, x = dev.lag.secondary.school.enrollment.ratio,  
                                               type = "scatter", mode = "markers + lines", color = dev.country, opacity = 1)
devGDPbySecondaryEnrollment

devSchoolModel2 <- lm(dev.lag.GDP ~ dev.lag.secondary.school.enrollment.ratio + dev.secondary.enrollment.ratio.sq)
summary(devSchoolModel2)
devLogGDPbySecondaryEnrollment <- plotly::plot_ly(y = log.dev.GDP, x = dev.secondary.enrollment.ratio.sq,  
                                                   type = "scatter", mode = "markers + lines", color = dev.country, opacity = 1)
devLOGGDPbySecondaryEnrollment

devSchoolModel3 <- lm(dev.lag.primary.school.enrollment.ratio ~ dev.lag.secondary.school.enrollment.ratio)
summary(devSchoolModel3)
# There is collinearity between primary and secondary school enrollment, I prefer to keep secondary in to see if there is underlying influence with the % of teenage pregnancies 15-19
# However, there are far more missing secondary education observations so I am inclined to use primary


# Education to Labor Equality
devEducLaborModel1 <- lm(dev.lag.labor.participation.ratio ~ dev.lag.primary.school.enrollment.ratio + log(dev.lag.primary.school.enrollment.ratio))

summary(devEducLaborModel1)

devEducLaborModel2 <- lm(dev.lag.labor.participation.ratio ~ dev.lag.secondary.school.enrollment.ratio)
summary(devEducLaborModel2)
  # Introduce logs of variables
devEducLaborModel3 <- lm(dev.labor.participation.ratio.sq ~ dev.primary.enrollment.ratio.sq + dev.secondary.enrollment.ratio.sq)
summary(devEducLaborModel3)

### Variables to be included are secondary enrollment ratio, adult literacy ratio, labor participation ratio, and vulnerable employment ratio

#OLS Regression for Predicting GDP
ols <- lm(log.dev.GDP ~ #dev.lag.adult.literacy.ratio + log.dev.adult.literacy.ratio + dev.adult.literacy.ratio.sq 
          + dev.lag.labor.participation.ratio 
          + dev.labor.participation.ratio.sq
          + log.dev.labor.participation.ratio 
          #+ dev.lag.secondary.school.enrollment.ratio + dev.secondary.enrollment.ratio.sq + log.dev.secondary.enrollment.ratio
          #+ dev.lag.vulnerable.employment.ratio + dev.vulnerable.employment.ratio.sq
          #+ log.dev.life.expectancy 
          #+ dev.life.expectancy
          + dev.life.expectancy.sq
          + dev.primary.enrollment.ratio.sq 
          + dev.lag.primary.school.enrollment.ratio
          + log(dev.lag.primary.school.enrollment.ratio)
          #, na.action = na.omit
          )

summary(ols)
anova(ols)
BIC(ols)

ols2 <- lm(dev.lag.GDP ~ dev.lag.adult.literacy.ratio + dev.adult.literacy.ratio.sq 
          + dev.lag.labor.participation.ratio + dev.labor.participation.ratio.sq
         # + dev.lag.secondary.school.enrollment.ratio + dev.secondary.enrollment.ratio.sq
        #  + dev.lag.vulnerable.employment.ratio + dev.vulnerable.employment.ratio.sq
          + dev.life.expectancy + log(dev.life.expectancy), na.action = na.omit
          )

summary(ols2)
anova(ols2)
AIC(ols2)

ols3 <- lm(log.dev.GDP ~ dev.lag.adult.literacy.ratio + log.dev.adult.literacy.ratio
           + dev.lag.labor.participation.ratio + log.dev.labor.participation.ratio
          # + dev.lag.secondary.school.enrollment.ratio + log.dev.secondary.enrollment.ratio
          # + dev.lag.vulnerable.employment.ratio + log.dev.vulnerable.employment.ratio
           + log(dev.life.expectancy), na.action = na.omit
           )

summary(ols3)
anova(ols3)
AIC(ols3)

olsplusFixedDum <- lm(log.dev.GDP ~ dev.lag.adult.literacy.ratio + dev.adult.literacy.ratio.sq 
                      + dev.lag.labor.participation.ratio + dev.labor.participation.ratio.sq
                      #    + dev.lag.secondary.school.enrollment.ratio + dev.secondary.enrollment.ratio.sq
                      #    + dev.lag.vulnerable.employment.ratio + dev.vulnerable.employment.ratio.sq
                      + log.dev.adult.literacy.ratio + log.dev.labor.participation.ratio #
                      + factor(dev.year) - 1)
summary(olsplusFixedDum)
anova(olsplusFixedDum)
AIC(olsplusFixedDum) # Model accuracy improved from introducing year fixed effect

#ols.pct <- lm(dev.GDP.sq ~ dev.adult.literacy.ratio.sq + dev.pct.change.labor.participation.ratio 
 #         + dev.pct.change.secondary.school.enrollment.ratio
  #        + dev.pct.change.vulnerable.employment.ratio
   #       )
#summary(ols.pct)
#anova(ols.pct)
#AIC(ols.pct)
length(log.dev.GDP)
length(dev.lag.labor.participation.ratio)
GDPPlot2 <- plotly::plot_ly(y = log.dev.GDP, x = dev.lag.labor.participation.ratio
                            , type = "scatter", mode = "markers + lines"
                            , color = dev.country, opacity = 1, data = country.subset)
GDPPlot2


m1 <- lm(dev.lag.labor.participation.ratio ~ dev.lag.secondary.school.enrollment.ratio)
summary(m1)
#devOLSplot <- plotly::plot_ly(data = research.set, y = dev.lag.GDP, x = ols,  type = "scatter", mode = "markers", color = econ.total$country, opacity = 1)
#devOLSplot

yhatols <- ols$fitted.values
AIC(ols)
par(mar = rep(2,4))

length(yhatols)
#	Ordinary Least Squares Regression Plot for variables in predicting GDP
plot(yhatols, pch = 20, xlab = "GDP", ylab = "ols")
#abline(lm(ols), lwd = 3, col = "red")

#	Introduce Team Based Fixed Effects **
#View(research.set)
hist(dev.lag.vulnerable.employment.ratio)
fixed.dum <- lm(log.dev.GDP ~ dev.lag.adult.literacy.ratio + log.dev.adult.literacy.ratio + dev.adult.literacy.ratio.sq 
                + dev.lag.labor.participation.ratio 
                + dev.labor.participation.ratio.sq
                + log.dev.labor.participation.ratio 
                #+ dev.lag.secondary.school.enrollment.ratio + dev.secondary.enrollment.ratio.sq
                #+ dev.lag.vulnerable.employment.ratio + dev.vulnerable.employment.ratio.sq
                #+ log.dev.life.expectancy 
                #+ dev.life.expectancy
                + dev.life.expectancy.sq 
                + dev.primary.enrollment.ratio.sq + dev.lag.primary.school.enrollment.ratio
                + log.dev.primary.school.enrollment.ratio
                + factor(dev.year) - 1 #+ factor(dev.country) -1
                )
summary(fixed.dum)
AIC(fixed.dum)
# Country f.e. reduces AIC significantly, more accurate model

fixed.dum2 <- lm(log.dev.GDP ~ dev.lag.adult.literacy.ratio + log.dev.adult.literacy.ratio + dev.adult.literacy.ratio.sq 
                + dev.lag.labor.participation.ratio 
                + dev.labor.participation.ratio.sq
                + log.dev.labor.participation.ratio 
                #+ dev.lag.secondary.school.enrollment.ratio + dev.secondary.enrollment.ratio.sq
                #+ dev.lag.vulnerable.employment.ratio + dev.vulnerable.employment.ratio.sq
                #+ log.dev.life.expectancy 
                #+ dev.life.expectancy
                + dev.life.expectancy.sq 
                #+ dev.primary.enrollment.ratio.sq + dev.lag.primary.school.enrollment.ratio
                #+ log(dev.lag.primary.school.enrollment.ratio)
                + factor(dev.country) - 1 + factor(dev.year) - 1, index = c("dev.country", "dev.year")
               #, na.action = na.omit
                )
summary(fixed.dum2)
BIC(fixed.dum2)

fixed.dum3 <- lm(log.dev.GDP ~ #dev.lag.adult.literacy.ratio + log.dev.adult.literacy.ratio + dev.adult.literacy.ratio.sq 
                 + dev.lag.labor.participation.ratio 
                 + dev.labor.participation.ratio.sq
                 + log.dev.labor.participation.ratio 
                 #+ dev.lag.secondary.school.enrollment.ratio + dev.secondary.enrollment.ratio.sq + log.dev.secondary.enrollment.ratio
                 #+ dev.lag.vulnerable.employment.ratio + dev.vulnerable.employment.ratio.sq
                 #+ log.dev.life.expectancy 
                 #+ dev.life.expectancy
                 + dev.life.expectancy.sq
                 + dev.primary.enrollment.ratio.sq 
                 + dev.lag.primary.school.enrollment.ratio
                 + log.dev.primary.school.enrollment.ratio
                 + factor(dev.country) - 1 + factor(dev.year) - 1, index = c("dev.country", "dev.year")
                 #, na.action = na.omit
                 )

summary(fixed.dum3)
AIC(fixed.dum3)
# See here that year is a predictor of GDP growth which is intuitively sensible. Country F.E. improves model the most

#	Predicting Values of xFIP- with Team Based Fixed Effect
yhatdum <- fixed.dum3$fitted.values
summary(yhatdum)

length(yhatdum)
length(yhatols)
#	Regress Line through Predicted Values Similar to OLS
scatterplot(yhatols, yhatdum, boxplots = FALSE, xlab = "yhatols", ylab = "yhatdum", smooth = FALSE, legend.plot = FALSE)
abline(lm(yhatols~yhatdum), lwd = 3, col = "red") # Presence of heteroskedasticity possible

#	Interactive Plot of Predicted Values of Log-GDP with Country and Year F.E. included.
# We see here that country's are capable of attaining better economic growth in terms of GDP by becoming more gender equitable
GDPPredictPlot <- plotly::plot_ly(data = country.subset, 
                                  y = fixed.dum3, 
                                  x = log.dev.GDP, 
                                  type = "scatter", mode = "markers", color = dev.country, opacity = 1)
GDPPredictPlot

#	Display Table Comparing 2 Models in Latex Form
YearDum.Table <- apsrtable::apsrtable(ols,fixed.dum3, model.names = c("OLS", "OLS_DUM"))
YearDum.Table

#	Determine Year Based Fixed Effects
fixedvars <- plm(log.dev.GDP ~ #dev.lag.adult.literacy.ratio + log.dev.adult.literacy.ratio + dev.adult.literacy.ratio.sq 
                   + dev.lag.labor.participation.ratio 
                 + dev.labor.participation.ratio.sq
                 + log.dev.labor.participation.ratio 
                 #+ dev.lag.secondary.school.enrollment.ratio + dev.secondary.enrollment.ratio.sq + log.dev.secondary.enrollment.ratio
                 #+ dev.lag.vulnerable.employment.ratio + dev.vulnerable.employment.ratio.sq
                 #+ log.dev.life.expectancy 
                 #+ dev.life.expectancy
                 + dev.life.expectancy.sq
                 + dev.primary.enrollment.ratio.sq 
                 + dev.lag.primary.school.enrollment.ratio
                 + log(dev.lag.primary.school.enrollment.ratio)
                 + factor(dev.country) - 1 + factor(dev.year) - 1, na.action = na.omit,
                 data = country.subset, index = c("country", "year"), model = "within")
summary(fixedvars)


fixef(fixedvars) # 	Display the fixed effects (constants for each country)
pFtest(fixedvars, ols) # Testing for fixed effects, null: OLS better than fixed
#	P-value < .05 so reject null that OLS is better than fixed
# Fixed effects model is better (including the year fixed effects)

#	Random Effects model using plm package
randomyear <- plm(log.dev.GDP ~ #dev.lag.adult.literacy.ratio + log.dev.adult.literacy.ratio + dev.adult.literacy.ratio.sq 
                  + dev.lag.labor.participation.ratio 
                  + dev.labor.participation.ratio.sq
                  + log.dev.labor.participation.ratio 
                  #+ dev.lag.secondary.school.enrollment.ratio + dev.secondary.enrollment.ratio.sq + log.dev.secondary.enrollment.ratio
                  #+ dev.lag.vulnerable.employment.ratio + dev.vulnerable.employment.ratio.sq
                  #+ log.dev.life.expectancy 
                  #+ dev.life.expectancy
                  + dev.life.expectancy.sq
                  + dev.primary.enrollment.ratio.sq 
                  + dev.lag.primary.school.enrollment.ratio
                  + log(dev.lag.primary.school.enrollment.ratio)
                  + factor(dev.country) - 1 + factor(dev.year) - 1, na.action = na.omit,
                  data = country.subset, index = c("country", "year"), model = "random")
summary(randomyear) #	Coefficient is average effect of X over Y when X changes

#	Fixed vs. Random
phtest(fixedvars, randomyear) # P-value is less than .05 so use fixed effects

# Test for Heteroskedasticity
bptest(log.dev.GDP ~ #dev.lag.adult.literacy.ratio + log.dev.adult.literacy.ratio + dev.adult.literacy.ratio.sq 
       + dev.lag.labor.participation.ratio 
       + dev.labor.participation.ratio.sq
       + log.dev.labor.participation.ratio 
       #+ dev.lag.secondary.school.enrollment.ratio + dev.secondary.enrollment.ratio.sq + log.dev.secondary.enrollment.ratio
       #+ dev.lag.vulnerable.employment.ratio + dev.vulnerable.employment.ratio.sq
       #+ log.dev.life.expectancy 
       #+ dev.life.expectancy
       + dev.life.expectancy.sq
       + dev.primary.enrollment.ratio.sq 
       + dev.lag.primary.school.enrollment.ratio
       + log(dev.lag.primary.school.enrollment.ratio)
       + factor(dev.country) - 1 + factor(dev.year) - 1,
        data = country.subset, studentize = FALSE)
# There is no heteroskedasticity present, which intuitively makes sense as world GDP rises some countries will separate themselves from the rest

LaborToLogGDPplot <- plotly::plot_ly(data = country.subset, y = log.dev.GDP, x = dev.lag.labor.participation.ratio,
                                type = "scatter", mode = "markers + lines", color = dev.country,
                                opacity = 1)
LaborToLogGDPplot
#hist(dev.lag.vulnerable.employment.ratio)

#	Fixed effects specification with Akaike Information Criterion
anova(fixed.dum3, fixedvars, test = "F")
AIC(ols)
AIC(fixed.dum3) # Fixed-effect dummy variable improved reliability of model without significant impact on dropped observations


EducationToLaborPlot <- plotly::plot_ly(data = country.subset, y = dev.lag.secondary.school.enrollment.ratio,
                                        x = dev.lag.labor.participation.ratio, type = "scatter",
                                        mode = "markers", color = dev.country, opacity = 1)
EducationToLaborPlot


LiteracyToLaborPlot <- plotly::plot_ly(data = country.subset, y = dev.lag.adult.literacy.ratio,
                                       x = dev.lag.labor.participation.ratio, type = "scatter",
                                       mode = "lines + markers", color = dev.country, opacity = 1)
LiteracyToLaborPlot


#EducationToVulnerablePlot <- plotly::plot_ly(data = country.subset, y = dev.lag.secondary.school.enrollment.ratio,
#                                       x = dev.lag.vulnerable.employment.ratio, type = "scatter",
#                                       mode = "lines + markers", color = dev.country, opacity = 1)
#EducationToVulnerablePlot

EducationToLogGDPPlot <- plotly::plot_ly(data = country.subset, y = log.dev.GDP,
                                             x = dev.lag.secondary.school.enrollment.ratio, type = "scatter",
                                             mode = "lines + markers", color = dev.country, opacity = 1)
EducationToLogGDPPlot

EducationToGDPPlot <- plotly::plot_ly(data = country.subset, y = dev.lag.GDP,
                                         x = dev.lag.secondary.school.enrollment.ratio, type = "scatter",
                                         mode = "lines + markers", color = dev.country, opacity = 1)
EducationToGDPPlot


#GDPToLaborParticipationPlot <- plotly::plot_ly(data = country.subset, y = dev.lag.labor.participation.ratio,
#                                            x = dev.lag.GDP, type = "scatter",
#                                           mode = "markers", color = dev.country, opacity = 1)
#GDPToLaborParticipationPlot


Models.Table <- stargazer(fixed.dum, fixed.dum2, fixed.dum3, type = "text",
                          add.lines = list(c("Fixed Effects?", "Year", "Country and Year", "Country and Year"),
                                           c("Dropped Variable", "None", "Primary School Enrollment", "Adult Literacy Ratio")),
                          omit = c("dev.year", "dev.country"),
                                dep.var.labels = c("log.dev.GDP"),
                                covariate.labels = c("dev.lag.adult.literacy.ratio", "log.dev.adult.literacy.ratio", "dev.adult.literacy.ratio.sq" 
                                                     ,"dev.lag.labor.participation.ratio" 
                                                     ,"dev.labor.participation.ratio.sq"
                                                     ,"log.dev.labor.participation.ratio" 
                                                     #, "dev.lag.secondary.school.enrollment.ratio", "dev.secondary.enrollment.ratio.sq"
                                                     , "log.dev.life.expectancy" 
                                                     #, "dev.life.expectancy"
                                                     , "dev.life.expectancy.sq" 
                                                     ,"dev.primary.enrollment.ratio.sq", "dev.lag.primary.school.enrollment.ratio"
                                                     ,"log.dev.primary.school.enrollment.ratio"
                                                     ),
                                out = "GenderEqualityModelTable.txt")
```{r}

```

View(country.subset)
LogGDPLogLaborPlot <- plotly::plot_ly(data = country.subset, y = log.dev.GDP,
                                      x = log.dev.labor.participation.ratio, type = "scatter",
                                      mode = "lines + markers", color = dev.country, opacity = 1)

LogGDPLogLaborPlot

describe(country.subset)


# Validated and now reprint the LogGDP (x), fixed.dum3 predicted logGDP (y)
# X-axis represents where countries actual LogGDP has been. Y-axis represents what is theoretically possible from becoming gender equal
GDPPredictPlot <- plotly::plot_ly(data = country.subset, 
                                  y = fixed.dum3, 
                                  x = log.dev.GDP, 
                                  type = "scatter", mode = "markers", color = dev.country, opacity = 1)
GDPPredictPlot
names(country.subset)
options(digits = 2)
options(scipen = 100)
descr <- stat.desc(country.subset[c(11,15,19,25,27,31,35,39,43,47,64)])
descr

DACtoEqualityModel <- lm(dev.lag.DAC.flow ~ dev.lag.labor.participation.ratio)
summary(DACtoEqualityModel)

#Data
  ## Hypothesis
########## New Vars #######
#IC_FRM_FEMM_ZS = Firms with female top manager (% of firms)
  ## Impact gender equality in employment through policies, political equality trickles down to work force
"IC_FRM_FEMM_ZS" = "female.top.managers"

#IC_FRM_FEMO_ZS = Firms with female participation in ownership (% of firms)
  ## Firms owned by females may tend to ecourage greater female labor participation and educationaal development
"IC_FRM_FEMO_ZS" = "female.firm.owners"

#IQ_CPA_GNDR_XQ = CPIA Gender Equality Rating (1 = low; 6 = high)
  ## Overall gender equality index in a nation
  ##Gender equality assesses the extent to which the country has installed institutions and programs to enforce laws and policies that promote equal access for men and women in education, health, the economy, and protection under law_
"IQ_CPA_GNDR_XQ" = "cpia.gender.equality.rating"

#IQ_CPA_MACR_XQ = CPIA Macro Management Rating (1 = low; 6 = high)
  ## Macroeconomic management assesses the monetary, exchange rate, and aggregate demand policy framework_
"IQ_CPA_MACR_XQ" = "cpia.macro.management.rating"

#SG_GEN_LSOM_ZS = Female legislators, senior officials and managers (% of total)
  ## Female legislators, senior officials and managers (% of total) refers to the share of legislators, senior officials and managers who are female_
"SG_GEN_LSOM_ZS" = "female.officials.legislators.managers"

#SG_GEN_PARL_ZS = Proportion of seats held by women in national parliaments (%)
  ## Women in parliaments are the percentage of parliamentary seats in a single or lower chamber held by women_
"SG_GEN_PARL_ZS" = "female.national.parliament.proportion"

#SH_DYN_MORT_FE = Mortality rate, under-5, female (per 1,000 live births)
  ## Under-five mortality rate, female is the probability per 1,000 that a newborn female baby will die before reaching age five, if subject to female age-specific mortality rates of the specified year_
"SH_DYN_MORT_FE" = "female.mortality.rate.ufive"

#SH_DYN_MORT_MA = Mortality rate, under-5, male (per 1,000 live births)
  ## Under-five mortality rate, male is the probability per 1,000 that a newborn male baby will die before reaching age five, if subject to male age-specific mortality rates of the specified year_
"SH_DYN_MORT_MA" = "male.mortality.rate.ufive"

#SH_MMR_DTHS = Number of maternal deaths
  ## Maternal deaths is the number of women who die during pregnancy and childbirth_
"SH_MMR_DTHS" = "maternal.deaths"

#SH_MMR_RISK_ZS = Lifetime risk of maternal death (%)
  ## Life time risk of maternal death is the probability that a 15-year-old female will die eventually from a maternal cause assuming that current levels of fertility and mortality (including maternal mortality) do not change in the future, taking into account competing causes of death_
"SH_MMR_RISK_ZS" = "maternal.death.risk"

#SL_EMP_INSV_FE_ZS = Share of women in wage employment in the nonagricultural sector (% of total nonagricultural employment)
  ## Share of women in wage employment in the nonagricultural sector is the share of female workers in wage employment in the nonagricultural sector (industry and services), expressed as a percentage of total employment in the nonagricultural sector_ Industry includes mining and quarrying (including oil production), manufacturing, construction, electricity, gas, and water, corresponding to divisions 2-5 (ISIC revision 2) or tabulation categories C-F (ISIC revision 3)_ Services include wholesale and retail trade and restaurants and hotels; transport, storage, and communications; financing, insurance, real estate, and business services; and community, social, and personal services-corresponding to divisions 6-9 (ISIC revision 2) or tabulation categories G-P (ISIC revision 3)_
"SL_EMP_INSV_FE_ZS" = "female.wage.employment.nonagriculture"

#SL_EMP_SELF_FE_ZS = Self-employed, female (% of females employed)
  ## Self-employed workers are those workers who, working on their own account or with one or a few partners or in cooperative, hold the type of jobs defined as a "self-employment jobs_"  i_e_ jobs where the remuneration is directly dependent upon the profits derived from the goods and services produced_ Self-employed workers include four sub-categories of employers, own-account workers, members of producers' cooperatives, and contributing family workers_
"SL_EMP_SELF_FE_ZS" = "self.employed.female"

#SL_EMP_SELF_MA_ZS = Self-employed, male (% of males employed)
  ## Self-employed workers are those workers who, working on their own account or with one or a few partners or in cooperative, hold the type of jobs defined as a "self-employment jobs_"  i_e_ jobs where the remuneration is directly dependent upon the profits derived from the goods and services produced_ Self-employed workers include four sub-categories of employers, own-account workers, members of producers' cooperatives, and contributing family workers_
"SL_EMP_SELF_MA_ZS" = "self.employed.male"

#SL_EMP_WORK_FE_ZS = Wage and salaried workers, female (% of females employed)
  ## Wage and salaried workers (employees) are those workers who hold the type of jobs defined as "paid employment jobs," where the incumbents hold explicit (written or oral) or implicit employment contracts that give them a basic remuneration that is not directly dependent upon the revenue of the unit for which they work_
"SL_EMP_WORK_FE_ZS" = "female.wage.and.salaried.workers"

#SL_EMP_WORK_MA_ZS = Wage and salary workers, male (% of males employed)
  ## Wage and salaried workers (employees) are those workers who hold the type of jobs defined as "paid employment jobs," where the incumbents hold explicit (written or oral) or implicit employment contracts that give them a basic remuneration that is not directly dependent upon the revenue of the unit for which they work_
"SL_EMP_WORK_MA_ZS" = "male.wage.and.salaried.workers"

#SL_GDP_PCAP_EM_KD = GDP per person employed (constant 1990 PPP $)
  ## GDP per person employed is gross domestic product (GDP) divided by total employment in the economy_ Purchasing power parity (PPP) GDP is GDP converted to 1990 constant international dollars using PPP rates_ An international dollar has the same purchasing power over GDP that a U_S_ dollar has in the United States_
"SL_GDP_PCAP_EM_KD" = "gdp.per.person.employed"

#SL_TLF_ACTI_FE_ZS = Labor force participation rate, female (% of female population ages 15-64) (modeled ILO estimate)
  ## Labor force participation rate is the proportion of the population ages 15-64 that is economically active: all people who supply labor for the production of goods and services during a specified period_


#SL_TLF_ACTI_MA_ZS = Labor force participation rate, male (% of male population ages 15-64) (modeled ILO estimate)
  ## Labor force participation rate is the proportion of the population ages 15-64 that is economically active: all people who supply labor for the production of goods and services during a specified period_

#SL_TLF_CACT_FM_NE_ZS = Ratio of female to male labor force participation rate (%) (national estimate)
  ## Labor force participation rate is the proportion of the population ages 15 and older that is economically active: all people who supply labor for the production of goods and services during a specified period_

#SL_TLF_PRIM_FE_ZS = Labor force with primary education, female (% of female labor force)
  ## Female labor force with primary education is the share of the female labor force that attained or completed primary education as the highest level of education_
"SL_TLF_PRIM_FE_ZS" = "female.labor.force.with.primary.education"

#SL_TLF_PRIM_MA_ZS = Labor force with primary education, male (% of male labor force)
  ## Male labor force with primary education is the share of the male labor force that attained or completed primary education as the highest level of education_
"SL_TLF_PRIM_MA_ZS" = "male.labor.force.with.primary.education"

#SL_TLF_SECO_FE_ZS = Labor force with secondary education, female (% of female labor force)
  ## Female labor force with primary education is the share of the female labor force that attained or completed primary education as the highest level of education_
"SL_TLF_SECO_FE_ZS" = "female.labor.force.with.secondary.education"

#SL_TLF_SECO_MA_ZS = Labor force with secondary education, male (% of male labor force)
  ## Male labor force with secondary education is the share of the male labor force that attained or completed secondary education as the highest level of education_
"SL_TLF_SECO_MA_ZS" = "male.labor.force.with.secondary.education"

#SL_TLF_TOTL_FE_ZS = Labor force, female (% of total labor force)
"SL_TLF_TOTL_FE_ZS" = "female.labor.force.proportion"

#SL_UEM_LTRM_FE_ZS = Long-term unemployment, female (% of female unemployment)
"SL_UEM_LTRM_FE_ZS" = "female.long.term.unemployment"

#SL_UEM_LTRM_MA_ZS = Long-term unemployment, male (% of male unemployment)
"SL_UEM_LTRM_MA_ZS" = "male.long.term.unemployment"






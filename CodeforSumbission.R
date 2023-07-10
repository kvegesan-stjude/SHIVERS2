if (!require(sjPlot))
  install.packages('sjPlot')
library(sjPlot)

if (!require(pROC))
  install.packages('pROC')
library(pROC)

if (!require(caret))
  install.packages('caret')
library(caret)

if (!require(data.table))
  install.packages('data.table')
library(data.table)


if (!require(gridExtra))
  install.packages('gridExtra')
library(gridExtra)



All<-read.csv('Mettelman_Minimum_dataframe.csv', encoding = "utf-8")
All<-All%>%replace(. == "#XL_EVAL_ERROR#", NA)
All<- All%>%filter(
  !Study.ID %in% c(
    'wn007300',
    'wn904813',
    'wn000616',
    'wn006622',
    'wn000061',
    'wn904633'
  )
)

All%<>%mutate_at(c('BMI', 'Age'), as.double)

#Figure 2k
HAI_H1    <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + log2(`hai_h1`),
  family = "binomial",
  data = All
)
HAI_H3    <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + log2(`hai_h3`),
  family = "binomial",
  data = All
)
HAI_bVic  <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + log2(`hai_bVic`),
  family = "binomial",
  data = All
)
HAI_bYam  <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + log2(`hai_bYam`),
  family = "binomial",
  data = All
)
NAI_N1    <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + log2(`nai_h1`),
  family = "binomial",
  data = All
)
NAI_N2    <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + log2(`nai_h3`),
  family = "binomial",
  data = All
)
NAI_bVic  <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + log2(`nai_bVic`),
  family = "binomial",
  data = All
)
NAI_bYam  <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + log2(`nai_bYam`),
  family = "binomial",
  data = All
)

plot_models(
  HAI_H1,
  HAI_H3,
  HAI_bVic,
  HAI_bYam,
  NAI_N1,
  NAI_N2,
  NAI_bVic,
  NAI_bYam,
  transform = "exp",
  title = "Influenza Infection Risk",
  colors = "bw",
  ci.lvl = 0.95,
  show.legend = FALSE,
  show.values = TRUE,
  show.p = TRUE,
  p.shape = FALSE,
  p.threshold = c(0.05, 0.01, 0.001),
  dot.size = 2.2,
  line.size = .6,
  value.size = 2,
  vline.color = "red",
  rm.terms = c("Flu_Vaccine_2018Vaccinated", "Age", "SexMale", "BMI"),
  grid.breaks = c(.05, 1, 5, 10)
) +
  labs(y = "Odds Ratio (95% CI) \n\n Protective(<1) Risk-Associated(>1)") +
  theme_sjplot2(base_size = 8) +
  theme(legend.position = "right", legend.direction = "vertical") +
  guides(col = "none")


#Figure 2l
AUC_H1    <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + I(`AUC_H1`  / 5000) ,
  family = "binomial",
  data = All
)
AUC_H3    <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + I(`AUC_H3`  / 5000) ,
  family = "binomial",
  data = All
)
AUC_VHA   <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + I(`AUC_VHA` / 5000) ,
  family = "binomial",
  data = All
)
AUC_YHA   <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + I(`AUC_YHA` / 5000) ,
  family = "binomial",
  data = All
)
AUC_N1    <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + I(`AUC_N1` / 5000),
  family = "binomial",
  data = All
)
AUC_N2    <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + I(`AUC_N2` / 5000),
  family = "binomial",
  data = All
)
AUC_VNA   <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + I(`AUC_VNA` / 5000),
  family = "binomial",
  data = All
)
AUC_YNA   <-glm(
  PCRpositive == 1 ~ Flu_Vaccine_2018 + Age + Sex + BMI + I(`AUC_YNA` / 5000),
  family = "binomial",
  data = All
)

plot_models(
  AUC_H1,
  AUC_H3,
  AUC_VHA,
  AUC_YHA,
  AUC_N1,
  AUC_N2,
  AUC_VNA,
  AUC_YNA,
  transform = "exp",
  title = "Influenza Infection Risk",
  colors = "bw",
  ci.lvl = 0.95,
  show.legend = FALSE,
  show.values = TRUE,
  show.p = TRUE,
  p.shape = FALSE,
  p.threshold = c(0.05, 0.01, 0.001),
  dot.size = 2.2,
  line.size = .6,
  value.size = 2,
  vline.color = "red",
  rm.terms = c("Flu_Vaccine_2018Vaccinated", "Age", "SexMale", "BMI"),
  grid.breaks = c(.05, 1, 5, 10)
) +
  labs(y = "Odds Ratio (95% CI) \n\n Protective(<1) Risk-Associated(>1)") +
  theme(legend.position = "right", legend.direction = "vertical") +
  guides(col = "none")


#Figure 4a
Subclinical_Symptomatic<-All%>%filter(inf_positive == 'Pos')
Age        <-glm(PCRpositive == 1 ~ Age, family = "binomial", data = Subclinical_Symptomatic)
Sex        <-glm(PCRpositive == 1 ~ Sex, family = "binomial", data = Subclinical_Symptomatic)
BMI        <-glm(PCRpositive == 1 ~ BMI, family = "binomial", data = Subclinical_Symptomatic)
FluVax2017 <-glm(PCRpositive == 1 ~ factor(fluvax2017),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
FluVax2018 <-glm(PCRpositive == 1 ~ Flu_Vaccine_2018,
                 family = "binomial",
                 data = Subclinical_Symptomatic)
HAI_H1     <-glm(PCRpositive == 1 ~ log2(`hai_h1`),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
HAI_H3     <-glm(PCRpositive == 1 ~ log2(`hai_h3`),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
HAI_bVic   <-glm(PCRpositive == 1 ~ log2(`hai_bVic`),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
HAI_bYam   <-glm(PCRpositive == 1 ~ log2(`hai_bYam`),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
NAI_N1     <-glm(PCRpositive == 1 ~ log2(`nai_h1`),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
NAI_N2     <-glm(PCRpositive == 1 ~ log2(`nai_h3`),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
NAI_bVic   <-glm(PCRpositive == 1 ~ log2(`nai_bVic`),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
NAI_bYam   <-glm(PCRpositive == 1 ~ log2(`nai_bYam`),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
AUC_H1     <-glm(PCRpositive == 1 ~ I(`AUC_H1`  / 5000) ,
                 family = "binomial",
                 data = Subclinical_Symptomatic)
AUC_H3     <-glm(PCRpositive == 1 ~ I(`AUC_H3`  / 5000) ,
                 family = "binomial",
                 data = Subclinical_Symptomatic)
AUC_VHA    <-glm(PCRpositive == 1 ~ I(`AUC_VHA` / 5000) ,
                 family = "binomial",
                 data = Subclinical_Symptomatic)
AUC_YHA    <-glm(PCRpositive == 1 ~ I(`AUC_YHA` / 5000) ,
                 family = "binomial",
                 data = Subclinical_Symptomatic)
AUC_N1     <-glm(PCRpositive == 1 ~ I(`AUC_N1` / 5000),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
AUC_N2     <-glm(PCRpositive == 1 ~ I(`AUC_N2` / 5000),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
AUC_VNA    <-glm(PCRpositive == 1 ~ I(`AUC_VNA` / 5000),
                 family = "binomial",
                 data = Subclinical_Symptomatic)
AUC_YNA    <-glm(PCRpositive == 1 ~ I(`AUC_YNA` / 5000),
                 family = "binomial",
                 data = Subclinical_Symptomatic)


plot_models(
  Age,
  Sex,
  BMI,
  FluVax2017,
  FluVax2018,
  HAI_H1,
  HAI_H3,
  HAI_bVic,
  HAI_bYam,
  NAI_N1,
  NAI_N2,
  NAI_bVic,
  NAI_bYam,
  AUC_H1,
  AUC_H3,
  AUC_VHA,
  AUC_YHA,
  AUC_N1,
  AUC_N2,
  AUC_VNA,
  AUC_YNA,
  transform = "exp",
  title = "Influenza Infection Risk",
  colors = "bw",
  ci.lvl = 0.95,
  show.legend = FALSE,
  show.values = TRUE,
  show.p = TRUE,
  p.shape = FALSE,
  p.threshold = c(0.05, 0.01, 0.001),
  dot.size = 3.2,
  line.size = .6,
  value.size = 2,
  vline.color = "red",
  grid.breaks = c(.05, 0.5, 1, 5, 50, 5000)
) +
  labs(y = "Odds Ratio (95% CI) \n\n Protective(<1) Risk-Associated(>1)") +
  theme(legend.position = "right", legend.direction = "vertical") +
  guides(col = "none")



#Figure 6d
LDFG<-All[, c('cTfh.ICOSp', 'Age', 'Sex', 'Flu_Vaccine_2018', 'PCRpositive')]

LDFG%<>%mutate_at(c('cTfh.ICOSp', 'Age'), as.double)
LDFG<-na.omit(LDFG)

# LDFG$cTfh.ICOSp<-as.numeric(LDFG$cTfh.ICOSp)
set.seed(901)
train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10)

LDFG$PCRpositive<-as.factor(LDFG$PCRpositive)

model_ctfhicos <- train(
  PCRpositive ~ cTfh.ICOSp,
  data = LDFG,
  method = "glm",
  trControl = train.control
)

model_ctfhicos_adj <- train(
  PCRpositive ~ cTfh.ICOSp + Age + Sex + Flu_Vaccine_2018,
  data = LDFG,
  method = "glm",
  trControl = train.control
)

rocobj1 <- plot.roc(
  PCRpositive ~ as.double(cTfh.ICOSp),
  data = LDFG,
  main = "cTfh.ICOS",
  percent = TRUE,
  of = "se",
  type = "shape",
  legacy.axes = TRUE,
  col = "red"
)
text(50, 50, labels = ("AUC(%)="), adj = c(0, 0))
text(
  50,
  50,
  labels = paste(round(rocobj1[["auc"]], 1)),
  adj = c(0, 1.2),
  col = "#1c61b6"
)
text(50,
     50,
     labels = ("10x CV-10 Accuracy (%) ="),
     adj = c(0, 8))
text(
  50,
  50,
  labels = paste(round((
    100 * model_ctfhicos[["results"]][["Accuracy"]]
  ), 1)),
  adj = c(0, 9.2),
  col = "#1c61b6"
)
text(
  50,
  50,
  labels = paste(round((
    100 * model_ctfhicos_adj[["results"]][["Accuracy"]]
  ), 1)),
  adj = c(0, 12.5),
  col = "#008600"
)
text(50,
     50,
     labels = ("AUC(%) Age, Sex, Vaccine Adj ="),
     adj = c(0, 11))



# Random Forest Models and Metrics ----


lymph_myeloid<-read.csv('Mettelman_Minimum_dataframe.csv', encoding = "utf-8")
lymph_myeloid<-lymph_myeloid%>%replace(. == "#XL_EVAL_ERROR#", NA)
lymph_myeloid<- lymph_myeloid%>%filter(
  !Study.ID %in% c(
    'wn007300',
    'wn904813',
    'wn000616',
    'wn006622',
    'wn000061',
    'wn904633'
  )
)

myvars<-c(
  'Age_Group',
  'Sex',
  'BMI_Cat',
  'Ethnicity',
  'Flu_Vaccine_2018',
  'HAI_A.H1N1',
  'HAI_A.H3N2',
  'HAI_B.Victoria',
  'HAI_B.Yamagata',
  'NAI_A.H1N1',
  'NAI_A.H3N2',
  'NAI_B.Victoria',
  'NAI_B.Yamagata',
  'CD4.Tcells',
  'CD4.CD107A',
  'CD4.Effector',
  'CD4.GZB',
  'CD4.IFNg',
  'CD4.IL2',
  'CD4.Memory.CCR5p',
  'CD4.Naive',
  'CD4.PD1',
  'CD4.Tcm',
  'CD4.Tem',
  'CD4.TEMRA',
  'CD4.TNFa',
  'CD4.Th17',
  'CD4.SP',
  'CD4.DP',
  'CD8.Tcells',
  'CD8.CD107A',
  'CD8.Effector',
  'CD8.GZB',
  'CD8.IFNg',
  'CD8.IL2',
  'CD8.Memory.CCR5p',
  'CD8.Naive',
  'CD8.PD1',
  'CD8.Tcm',
  'CD8.Tem',
  'CD8.TEMRA',
  'CD8.TNFa',
  'CD8.SP',
  'CD8.DP',
  'cTfh',
  'cTfh.CXCR3p',
  'cTfh.ICOSp',
  'cTfh.IL21p',
  'cTfh.PD1p',
  'Gamma.Delta',
  'NK.Cells',
  'NK.GZBp.IFNn',
  'NK.GZBn.IFNp',
  'NK.GZBp.IFNp',
  'MDSC',
  'cDC1',
  'cDC2',
  'mDC',
  'pDC',
  'NK.CK.Producing',
  'NK.Activated',
  'NK.Cytotoxic',
  'Monocytes.Classical',
  'Monocytes.Intermediate',
  'Monocytes.Nonclassical',
  'Basophils',
  'Eosinophils',
  'Neutrophils',
  'DC.CD1c',
  'Monocytes.CD80p',
  "AUC_H1",
  "AUC_H3",
  "AUC_N1",
  "AUC_N2",
  "AUC_VHA",
  "AUC_VNA",
  "AUC_YHA",
  "AUC_YNA"
)
catvars<-c(
  'Age_Group',
  'Sex',
  'BMI_Cat',
  'Ethnicity',
  'HAI_A.H1N1',
  'HAI_A.H3N2',
  'HAI_B.Victoria',
  'HAI_B.Yamagata',
  'NAI_A.H1N1',
  'NAI_A.H3N2',
  'NAI_B.Victoria',
  'NAI_B.Yamagata',
  'Flu_Vaccine_2018'
)

myvars_no_missing<-c(
  'Age_Group',
  'Sex',
  'BMI_Cat',
  'Ethnicity',
  'Flu_Vaccine_2018',
  'HAI_A.H1N1',
  'HAI_A.H3N2',
  'HAI_B.Victoria',
  'HAI_B.Yamagata',
  'NAI_A.H1N1',
  'NAI_A.H3N2',
  'NAI_B.Victoria',
  'NAI_B.Yamagata',
  'CD4.CD107A',
  'CD4.Effector',
  'CD4.GZB',
  'CD4.IFNg',
  'CD4.IL2',
  'CD4.Memory.CCR5p',
  'CD4.Naive',
  'CD4.PD1',
  'CD4.TNFa',
  'CD4.Th17',
  'CD4.SP',
  'CD4.DP',
  'CD8.CD107A',
  'CD8.Effector',
  'CD8.GZB',
  'CD8.IFNg',
  'CD8.IL2',
  'CD8.Memory.CCR5p',
  'CD8.Naive',
  'CD8.PD1',
  'CD8.TNFa',
  'CD8.SP',
  'CD8.DP',
  'cTfh.CXCR3p',
  'cTfh.ICOSp',
  'cTfh.IL21p',
  'cTfh.PD1p',
  'Gamma.Delta',
  'NK.GZBp.IFNn',
  'NK.GZBn.IFNp',
  'NK.GZBp.IFNp',
  'mDC',
  'pDC',
  'NK.CK.Producing',
  'NK.Activated',
  'NK.Cytotoxic',
  'Monocytes.Classical',
  'Monocytes.Intermediate',
  'Monocytes.Nonclassical',
  'Basophils',
  'Eosinophils',
  'Neutrophils',
  'DC.CD1c',
  "AUC_H1",
  "AUC_H3",
  "AUC_N1",
  "AUC_N2",
  "AUC_VHA",
  "AUC_VNA",
  "AUC_YHA",
  "AUC_YNA"
)


nonormal_vars<-myvars[!(myvars %in% catvars)]

lymph_myeloid%<>%mutate_at(catvars, factor)

lymph_myeloid%<>%mutate_at(nonormal_vars, as.double)
basevars = c(
  'PCRpositive',
  'Age_Group',
  'Sex',
  'BMI_Cat',
  'Ethnicity',
  'Flu_Vaccine_2018',
  "hai_h1",
  "hai_h3",
  "hai_bVic",
  "hai_bYam",
  "nai_h1",
  "nai_h3",
  "nai_bVic",
  "nai_bYam",
  
  "AUC_H1",
  "AUC_H3",
  "AUC_N1",
  "AUC_N2",
  "AUC_VHA",
  "AUC_VNA",
  "AUC_YHA",
  "AUC_YNA"
)

lymph_vars = c(
  basevars,
  c(
    'CD4.Tcells',
    'CD4.TNFa',
    'CD4.Effector',
    'CD4.IL2',
    'CD4.Naive',
    'CD4.PD1',
    'CD4.Th17',
    'CD4.DP',
    'CD8.Tcells',
    'CD8.CD107A',
    'CD8.Effector',
    'CD8.IFNg',
    'CD8.IL2',
    'CD8.Memory.CCR5p',
    'CD8.PD1',
    'CD8.TNFa',
    'CD8.SP',
    'CD8.DP',
    'cTfh.CXCR3p',
    'cTfh.ICOSp',
    'Gamma.Delta',
    'NK.GZBn.IFNp',
    'NK.GZBp.IFNp',
    'NK.GZBp.IFNn'
  )
)


myl_vars <- c(
  basevars,
  c(
    'mDC',
    'NK.CK.Producing',
    'NK.Activated',
    'NK.Cytotoxic',
    'Monocytes.Intermediate',
    'Monocytes.Nonclassical',
    'Basophils',
    'Eosinophils'
  )
)


allpredictions<-list()



#Base dataset
training<-lymph_myeloid%>%filter(training == 'training')
training$PCRpositive<-factor(
  training$PCRpositive,
  levels = c(1, 0),
  labels = c('Positive', 'Negative')
)
testing<-lymph_myeloid%>%filter(training == 'testing')
testing$PCRpositive<-factor(
  testing$PCRpositive,
  levels = c(1, 0),
  labels = c('Positive', 'Negative')
)
training_base <- na.omit(training[, basevars])
testing_base  <- na.omit(testing[, basevars])


#Lymphoid Dataset
training_lymph <- na.omit(training[, lymph_vars])
testing_lymph  <- na.omit(testing[, lymph_vars])
training_lymph_cell <- na.omit(training[, c('PCRpositive', setdiff(lymph_vars, basevars))])
testing_lymph_cell  <- na.omit(testing[, c('PCRpositive', setdiff(lymph_vars, basevars))])

#Myeloid Dataset
training_myl <- na.omit(training[, myl_vars])
testing_myl  <- na.omit(testing[, myl_vars])
training_myl_cell <- na.omit(training[, c('PCRpositive', setdiff(myl_vars, basevars))])
testing_myl_cell  <- na.omit(testing[, c('PCRpositive', setdiff(myl_vars, basevars))])

#Combined Dataset
training_all <- na.omit(training[, union(lymph_vars, myl_vars)])
testing_all  <- na.omit(testing[, union(lymph_vars, myl_vars)])
training_all_cell <- na.omit(training[, c('PCRpositive', setdiff(union(lymph_vars, myl_vars), basevars))])
testing_all_cell  <- na.omit(testing[, c('PCRpositive', setdiff(union(lymph_vars, myl_vars), basevars))])



# Base Models


fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  ## Estimate class probabilities
  classProbs = TRUE,
  ## Evaluate performance using
  ## the following function
  summaryFunction = twoClassSummary
)

set.seed(825)
rf_base <- train(
  PCRpositive ~ .,
  data = training_base,
  method = 'rf',
  metric = 'ROC',
  tuneLength  = 15,
  trControl = fitControl
)

pred<-predict(rf_base, testing_base, type = "prob")
f<-roc(testing_base$PCRpositive,
       pred$Positive,
       levels = c("Negative", "Positive"))
allpredictions<-append(allpredictions, list(rf_base = f))



# Lymphoid Models
rf_lymph <- train(
  PCRpositive ~ .,
  data = training_lymph,
  method = 'rf',
  metric = 'ROC',
  tuneLength  = 15,
  trControl = fitControl
)

pred<-predict(rf_lymph, testing_lymph, type = "prob")
f<-roc(testing_lymph$PCRpositive,
       pred$Positive,
       levels = c("Negative", "Positive"))
allpredictions<-append(allpredictions, list(rf_lymph = f))

rf_lymph_cell <- train(
  PCRpositive ~ .,
  data = training_lymph_cell,
  method = 'rf',
  metric = 'ROC',
  tuneLength  = 15,
  trControl = fitControl
)

pred<-predict(rf_lymph_cell, testing_lymph_cell, type = "prob")
f<-roc(testing_lymph_cell$PCRpositive,
       pred$Positive,
       levels = c("Negative", "Positive"))
allpredictions<-append(allpredictions, list(rf_lymph_cell = f))



# Myeloid Models

set.seed(678)
rf_myl <- train(
  PCRpositive ~ .,
  data = training_myl,
  method = 'rf',
  metric = 'ROC',
  tuneLength  = 15,
  trControl = fitControl
)


pred<-predict(rf_myl, testing_myl, type = "prob")
f<-roc(testing_myl$PCRpositive,
       pred$Positive,
       levels = c("Negative", "Positive"))
allpredictions<-append(allpredictions, list(rf_myl = f))
rf_myl_cell <- train(
  PCRpositive ~ .,
  data = training_myl_cell,
  method = 'rf',
  metric = 'ROC',
  tuneLength  = 15,
  trControl = fitControl
)


pred<-predict(rf_myl_cell, testing_myl_cell, type = "prob")
f<-roc(testing_myl_cell$PCRpositive,
       pred$Positive,
       levels = c("Negative", "Positive"))
allpredictions<-append(allpredictions, list(rf_myl_cell = f))


# Combined Model
rf_all <- train(
  PCRpositive ~ .,
  data = training_all,
  method = 'rf',
  metric = 'ROC',
  tuneLength  = 10,
  trControl = fitControl
)


pred<-predict(rf_all, testing_all, type = 'prob')
f<-roc(testing_all$PCRpositive,
       pred$Positive,
       levels = c("Negative", "Positive"))
allpredictions<-append(allpredictions, list(rf_all = f))

rf_all_cell <- train(
  PCRpositive ~ .,
  data = training_all_cell,
  method = 'rf',
  metric = 'ROC',
  tuneLength  = 10,
  trControl = fitControl
)


pred<-predict(rf_all_cell, testing_all_cell, type = 'prob')
f<-roc(testing_all_cell$PCRpositive,
       pred$Positive,
       levels = c("Negative", "Positive"))
allpredictions<-append(allpredictions, list(rf_all_cell = f))

# Print ROC curves and other Model Metrics


results<-resamples(
  list(
    'Base Model' = rf_base,
    'Base+Lymphoid Model' = rf_lymph,
    'Lymphoid Only' = rf_lymph_cell,
    'Base+Myeloid Model' = rf_myl,
    'Myeloid Only' = rf_myl_cell,
    'Base+Cell Model' = rf_all,
    'Lymphoid+Myeloid' = rf_all_cell
  )
)

results<-reshape2::melt(results$values)
results<-results %>% separate(variable, sep = '~', into = c('Model', 'Metric'))
results$Model<-factor(
  results$Model,
  levels =
    c(
      "Base Model",
      "Lymphoid Only",
      "Base+Lymphoid Model",
      "Myeloid Only",
      "Base+Myeloid Model",
      "Lymphoid+Myeloid",
      "Base+Cell Model"
    )
)


# Model Metrics
metric_roc <- ggplot(results %>% filter(Metric == 'ROC'), aes(x = Model, y =
                                                                value)) +
  geom_boxplot()+
  facet_grid(. ~ Metric)+coord_flip()+theme(axis.title.x = element_blank(), axis.title.y = element_blank())
metric_sens <- ggplot(results %>% filter(Metric == 'Sens'), aes(x = Model, y =
                                                                  value)) +
  geom_boxplot()+facet_grid(. ~ Metric)+coord_flip()+theme(axis.title.x = element_blank(), axis.title.y = element_blank())
metric_spec <- ggplot(results %>% filter(Metric == 'Spec'), aes(x = Model, y =
                                                                  value)) +
  geom_boxplot()+facet_grid(. ~ Metric)+coord_flip()+theme(axis.title.x = element_blank(), axis.title.y = element_blank())
grid.arrange(metric_roc, metric_sens, metric_spec, roc_plot, nrow = 2)

#Variable Importance
plot(varImp(rf_all))

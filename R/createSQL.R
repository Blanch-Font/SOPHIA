#' Create SQL for T1DM extraction
#'
#' @param cdm_bbdd A connection for a OMOP database via DatabaseConnector
#' @param cdm_schema A name for OMOP schema
#' @param results_sc A name for result schema
#' @param cohortTable A name of the result cohort
#'
#' @return A SQL syntax
#' @export
#'
#' @examples
#' # Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "~idiap/projects/SOPHIA_codi/data/jdbcDrivers/")
#' # dbms = Sys.getenv("DBMS")
#' # user <- if (Sys.getenv("DB_USER") == "") NULL else Sys.getenv("DB_USER")
#' # password <- if (Sys.getenv("DB_PASSWORD") == "") NULL else Sys.getenv("DB_PASSWORD")
#' # server = Sys.getenv("DB_SERVER")
#' # port = Sys.getenv("DB_PORT")
#' # connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
#' #                                                                  server = server,
#' #                                                                  user = user,
#' #                                                                  password = password,
#' #                                                                  port = port)
#' # cdm_bbdd <- DatabaseConnector::connect(connectionDetails = connectionDetails)
#' # cdm_schema <- 'omop21t2_test'
#' # results_sc <- 'sophia_test'
#' # cohortTable <- 'prova_Capr'
#' # cohortInfo <- CreateSQL_T1DM(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_T1DM <- function(cdm_bbdd,
                           cdm_schema,
                           results_sc,
                           cohortTable){
  #################################################################################################
  # Definicions del Capr (https://ohdsi.github.io/Capr/articles/complex-cohort-example.html)
  #Type 1 Diabetes Diagnosis
  T1Dx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(201254, 435216, 40484648, 40484649),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Type 1 Diabetes Diagnosis",
    includeDescendants = TRUE)
  # T1Dx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # Type 2 Diabetes segons Covid19CharacterizationCharybdis
  # <-- S'ha de posar en l'ordre que et dóna el getConceptIdDetails
  # conceptMapping <- Capr::createConceptMapping(
  #   n = 9,
  #   includeDescendants = rep(T, 9),      # <--
  #   isExcluded = c(T, T, F, T, F, F, T, T, T)) # <--
  # DMDx <- Capr::createConceptSetExpressionCustom(
  #   conceptSet = Capr::getConceptIdDetails(conceptIds = c(201820, 442793, 443238,
  #                                                         201254, 435216, 4058243, 40484648,
  #                                                         #Afegit mirant atlas-phenotype
  #                                                         195771, 761051), #diabetis secondaria
  #                                          connection = cdm_bbdd,
  #                                          vocabularyDatabaseSchema = cdm_schema),
  #   Name = "Diabetes Diagnosis",
  #   conceptMapping = conceptMapping)
  # # arreglo errors del paquet
  # DMDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()
  # Nova versio
  DM2Dx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(201826, 443732, 40482801, 40485020),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Diabetes Diagnosis",
    includeDescendants = TRUE)
  # DMDx_hist <- Capr::createConceptSetExpression(
  #   conceptSet = Capr::getConceptIdDetails(conceptIds = c(40769338, 43021173, 42539022, 46270562),
  #                                          connection = cdm_bbdd,
  #                                          vocabularyDatabaseSchema = cdm_schema),
  #   Name = "History of Diabetes Diagnosis",
  #   includeDescendants = TRUE)
  # DMDx_hist@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()
  #Secondary Diabetes Diagnosis
  SecondDMDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(195771, 761051),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Secondary Diabetes Diagnosis",
    includeDescendants = TRUE)
  # SecondDMDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # 139953
  # T1DRxNormCodes <- paste(c(139825, 274783, 314684, 352385, 400008, 51428, 5856, 86009))
  # T1Rx <- getConceptCodeDetails(conceptCode = T1DRxNormCodes,
  #                               vocabulary = "RxNorm",
  #                               connection = cdm_bbdd,
  #                               vocabularyDatabaseSchema = cdm_schema,
  #                               mapToStandard = TRUE) %>%
  T1DRxNormCodes <- paste(c(1596977, 19058398, 19078552, 19078559, 19095211, 19095212, 19112791,
                            19133793, 19135264, 21076306, 21086042, 35410536, 35412958, 40051377,
                            40052768, 42479783, 42481504, 42481541, 42899447, 42902356, 42902587,
                            42902742, 42902821, 42902945, 42903059, 44058584, 46233969, 46234047,
                            46234234, 46234237))
  T1Rx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = T1DRxNormCodes,
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Type 1 Diabetes Medications",
    includeDescendants = TRUE)
  # T1Rx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # End-stage kidney disease
  ## End stage renal disease
  vec_eskd_cod <- c(443611, 193782, 443919, 45887996, 2617395, 2617396, 44786436, 2617401, 2617405,
                    2617397, 2617404, 2617403, 2617400, 2617402, 2617399, 2617398,
                    #afegit mirant la descriptiva
                    192359)
  ## Dialysis
  vec_dial <- c(4090651, 4032243, 45889365, 4027133, 38003431)
  ## Transplatation
  vec_trans <- c(199991, 42539502, 4324887, 4309006)
  ## eGFR < 15 (més endavant)
  vec_eskd <- c(vec_eskd_cod, vec_dial, vec_trans)
  RenalDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = vec_eskd,
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "End-stage kidney disease",
    includeDescendants = TRUE)
  # RenalDx@ConceptSetExpressi<on[[1]]@id <- uuid::UUIDgenerate()
  #Abnormal Lab
  eGFR <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c("40764999", "1617023", "1619025",
                                                          "46236952"),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Abnormal eGFR",
    includeDescendants = TRUE)
  # eGFR@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # schizophrenia
  SchizophreniaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(435783,
                                                          #afegit mirant la descriptiva
                                                          433450),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Schizophrenia",
    includeDescendants = TRUE)
  # SchizophreniaDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # Epilèpsia
  SeizureDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(380378), #afegit mirant la descriptiva
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Seizure",
    includeDescendants = TRUE)
  # SeizureDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # Any malignant tumour
  MaligNeoDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(443392, 4144289,
                                                          #afegit mirant atlas-demo
                                                          439392,
                                                          #afegit mirant la descriptiva
                                                          200962, 139750, 4311499, 137809, 197500),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Malignant Neoplasm",
    includeDescendants = TRUE)
  # MaligNeoDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # Systemic steroids: H02
  # CortiRxNormCodes <- paste(c(42903427, 1555120, 19017895,
  #                             920458, 1518254, 19055344, 1506270, 19027186, 1550557, 1551099,
  #                             903963, 975125, 1507705, 19011127, 977421, 19086888, 19050907,
  #                             19009116, 19061907, 19055156, 19042801, 37499303, 985708))
  CortiRxNormCodes <- paste(c(975169, 1506426, 1506430, 1506479, 1518259, 1518292, 1551101,
                              1551122, 1551123, 1551171, 1551192, 1555142, 1592257, 19016866,
                              19018083, 19063670, 19070310, 19084229, 19101595, 19104623, 19106649,
                              19106650, 19111643, 19121383, 35606531, 35606542, 36884768, 36893086,
                              37497612, 40234819, 40241504, 40897491, 40930518, 41052849,
                              42629020))
  CortiRx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = CortiRxNormCodes,
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Systemic steroids: H02 Medications",
    includeDescendants = TRUE)
  # CortiRx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # Eating disorder
  EatingDisorderDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(439002, 436675, 438407, #F50
                                                          442165), #R63.0
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Malignant Neoplasm",
    includeDescendants = TRUE)
  # Eating disorder
  SymptomsHyperglycaemiaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(4049477,# R63.1 Polidípsia
                                                          435928,# R63.4 Abnormal weight loss
                                                          79936, 200843, 40304526 #R35 poliúria
                                                          ),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Malignant Neoplasm",
    includeDescendants = TRUE)
  #################################################################################################
  # Building Queries
  #T1Dx Condition Occurrence Query
  T1DxQuery <- Capr::createConditionOccurrence(conceptSetExpression = T1Dx)
  #T1Rx Drug Exposure Query
  T1RxQuery <- Capr::createDrugExposure(conceptSetExpression = T1Rx)
  #DMDx Condition Occurrence Query
  DM2DxQuery <- Capr::createConditionOccurrence(conceptSetExpression = DM2Dx)
  #DMDx_hist Condition Occurrence Query
  # DMDx_histQuery <- Capr::createConditionOccurrence(conceptSetExpression = DMDx_hist)
  #SecondDMDx Condition Occurrence Query
  SecondDMDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = SecondDMDx)

  #RenalDx Condition Occurrence Query
  RenalDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = RenalDx)
  #eGFR Query with value attribute
  AbeGFRQuery <- Capr::createMeasurement(
    conceptSetExpression = eGFR,
    #add attribute of eGFR < 15
    attributeList = list(Capr::createValueAsNumberAttribute(Op = "lt", Value = 15)))

  #SchizophreniaDx Condition Occurrence Query
  SchizophreniaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = SchizophreniaDx)
  #SeizureDx Condition Occurrence Query
  SeizureDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = SeizureDx)
  #MaligNeoDx Condition Occurrence Query
  MaligNeoDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = MaligNeoDx)
  #CortiRx Drug Exposure Query
  CortiRxQuery <- Capr::createDrugExposure(conceptSetExpression = CortiRx)
  #EatingDisorderDx Condition Occurrence Query
  EatingDisorderDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = EatingDisorderDx)
  #SymptomsHyperglycaemiaDx Condition Occurrence Query
  SymptomsHyperglycaemiaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = SymptomsHyperglycaemiaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  ## We defined initial entry as observed occurrence of all of the following events:
  ## + T2DM diagnosis,
  ## + prescription of a T2DM medication and
  ## + the presence of an abnormal lab
  PrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "T1DM",
    ComponentList = list(T1DxQuery,
                         T1RxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  #################################################################################################
  # Inclusion Rules
  # Inclusion with age
  AgeAtt <- Capr::createAgeAttribute(Op = "gte", Value = 18)
  Age18AndOlderGroup <- Capr::createGroup(Name = ">=18 years old",
                                          type="ALL",
                                          criteriaList = NULL,
                                          demographicCriteriaList = list(AgeAtt),
                                          Groups = NULL)

  # Time periods
  tl1 <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = "All",
                                                               StartCoeff = "Before",
                                                               EndDays = "All",
                                                               EndCoeff = "After"))
  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = "All",
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noDM2DxCount <- Capr::createCount(Query = DM2DxQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tlprev)
                                   # Timeline = tl1)
  noDM2DxGroup <- Capr::createGroup(Name = "No Diagnosis of Type 2 Diabetes",
                                    type = "ALL",
                                    criteriaList = list(noDM2DxCount))

  # No SecondDMDx at any point previous to DM in patient history
  noSecondDMDxCount <- Capr::createCount(Query = SecondDMDxQuery,
                                         Logic = "exactly",
                                         Count = 0L,
                                         Timeline = tlprev)
  noSecondDMDxGroup <- Capr::createGroup(Name = "No previous Secondary diabetes",
                                         type = "ALL",
                                         criteriaList = list(noSecondDMDxCount))

  # No RenalDx at any point previous to DM in patient history
  noRenalDxCount <- Capr::createCount(Query = RenalDxQuery,
                                      Logic = "exactly",
                                      Count = 0L,
                                      Timeline = tlprev)
  exactly0AbeGFRCount <- Capr::createCount(Query = AbeGFRQuery,
                                           Logic = "exactly",
                                           Count = 0L,
                                           Timeline = tlprev)
  noRenalDxGroup <- Capr::createGroup(Name = "No previous Renal problems",
                                      type = "ALL",
                                      criteriaList = list(noRenalDxCount,
                                                          exactly0AbeGFRCount))

  # No SchizophreniaDx at any point previous to DM in patient history
  noSchizophreniaDxCount <- Capr::createCount(Query = SchizophreniaDxQuery,
                                              Logic = "exactly",
                                              Count = 0L,
                                              Timeline = tlprev)
  noSchizophreniaDxGroup <- Capr::createGroup(Name = "No previous Schizophrenia",
                                              type = "ALL",
                                              criteriaList = list(noSchizophreniaDxCount))

  # No SeizureDx at any point previous to DM in patient history
  noSeizureDxCount <- Capr::createCount(Query = SeizureDxQuery,
                                        Logic = "exactly",
                                        Count = 0L,
                                        Timeline = tlprev)
  noSeizureDxGroup <- Capr::createGroup(Name = "No previous Seizure",
                                        type = "ALL",
                                        criteriaList = list(noSeizureDxCount))

  # No MaligNeoDx at any point previous to DM in patient history
  noMaligNeoDxCount <- Capr::createCount(Query = MaligNeoDxQuery,
                                         Logic = "exactly",
                                         Count = 0L,
                                         Timeline = tlprev)
  noMaligNeoDxGroup <- Capr::createGroup(Name = "No previous Malignant Neoplasm",
                                         type = "ALL",
                                         criteriaList = list(noMaligNeoDxCount))

  # No CortiRx at any point previous to DM in patient history
  noCortiRxCount <- Capr::createCount(Query = CortiRxQuery,
                                      Logic = "exactly",
                                      Count = 0L,
                                      Timeline = tlprev)
  noCortiRxGroup <- Capr::createGroup(Name = "No previous Corticoides",
                                      type = "ALL",
                                      criteriaList = list(noCortiRxCount))

  # No EatingDisorderDx at any point previous to DM in patient history
  noEatingDisorderDxCount <- Capr::createCount(Query = EatingDisorderDxQuery,
                                               Logic = "exactly",
                                               Count = 0L,
                                               Timeline = tlprev)
  noEatingDisorderDxGroup <- Capr::createGroup(Name = "No previous Eating disorder",
                                               type = "ALL",
                                               criteriaList = list(noEatingDisorderDxCount))

  # No SymptomsHyperglycaemiaDx at any point previous to DM in patient history
  noSymptomsHyperglycaemiaDxCount <- Capr::createCount(Query = SymptomsHyperglycaemiaDxQuery,
                                               Logic = "exactly",
                                               Count = 0L,
                                               Timeline = tlprev)
  noSymptomsHyperglycaemiaDxGroup <- Capr::createGroup(Name = "No previous Symptoms Hyperglycaemia",
                                               type = "ALL",
                                               criteriaList = list(noSymptomsHyperglycaemiaDxCount))

  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(Age18AndOlderGroup,
                                                               # noDM2DxGroup,
                                                               noSecondDMDxGroup,
                                                               noRenalDxGroup,
                                                               noSchizophreniaDxGroup,
                                                               noSeizureDxGroup,
                                                               noMaligNeoDxGroup,
                                                               noCortiRxGroup,
                                                               noEatingDisorderDxGroup,
                                                               noSymptomsHyperglycaemiaDxGroup),
                                               Limit = "First")

  #################################################################################################
  # Finalizing the Cohort Definition
  #person exits cohort if there is a diagnosis of T1DM
  CensoringCriteria <- Capr::createCensoringCriteria(Name = "Censor of Renal, Depress cases",
                                                     ComponentList = list(RenalDxQuery,
                                                                          SchizophreniaDxQuery,
                                                                          SeizureDxQuery,
                                                                          MaligNeoDxQuery))
  # La data d'entrada mínima és 2010-01-01, els anteriors són prevalents.
  # Assegurem que tenim almenys 5 anys de seguiment
  cohortEra <- Capr::createCohortEra(LeftCensorDate = "2009-12-31")
  T1DMPhenotype <- Capr::createCohortDefinition(
    Name = "T1DM",
    PrimaryCriteria = PrimaryCriteria,
    # AdditionalCriteria = AdditionalCriteria)#,
    InclusionRules = InclusionRules,
    CensoringCriteria = CensoringCriteria,
    # EndStrategy = EsCovidDiag,
    CohortEra = cohortEra)
  # JSON
  T1DMPhenotypeJson <- Capr::compileCohortDefinition(T1DMPhenotype)

  #################################################################################################
  # https://ohdsi.github.io/Capr/articles/CAPR_tutorial.html
  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 2,
                                         cdmSchema = cdm_schema,
                                         targetTable = paste(results_sc, cohortTable, sep='.'),
                                         resultSchema = results_sc,
                                         vocabularySchema = cdm_schema,
                                         generateStats = T)
  cohortInfo <- Capr::compileCohortDefinition(T1DMPhenotype, genOp)
  # Modifiquem el codi per tenir els casos anteriors a l'entrada al SIDIAP.
  cohortInfo$ohdiSQL <- gsub(pattern = 'E.start_date >=  OP.observation_period_start_date and ',
                             replacement = '',
                             x = cohortInfo$ohdiSQL,
                             fixed = T)
  cohortInfo$ohdiSQL <- gsub(pattern = 'WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE',
                             replacement = '',
                             x = cohortInfo$ohdiSQL,
                             fixed = T)
  cohortInfo$ohdiSQL <- gsub(pattern = 'AND A.START_DATE >= P.OP_START_DATE',
                             replacement = '',
                             x = cohortInfo$ohdiSQL,
                             fixed = T)
  cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_censor_stats',
                             replacement = paste0(cohortTable, '_censor_stats'),
                             x = cohortInfo$ohdiSQL)
  cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_result',
                             replacement = paste0(cohortTable, '_inclusion_result'),
                             x = cohortInfo$ohdiSQL)
  cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_stats',
                             replacement = paste0(cohortTable, '_inclusion_stats'),
                             x = cohortInfo$ohdiSQL)
  cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_summary_stats',
                             replacement = paste0(cohortTable, '_summary_stats'),
                             x = cohortInfo$ohdiSQL)
  cohortInfo$ohdiSQL <- gsub(pattern = '15,0000',
                             replacement = '15.0000',
                             x = cohortInfo$ohdiSQL)

  return(cohortInfo)
}

#' Create SQL for T2DM extraction
#'
#' @param cdm_bbdd A connection for a OMOP database via DatabaseConnector
#' @param cdm_schema A name for OMOP schema
#' @param results_sc A name for result schema
#' @param cohortTable A name of the result cohort
#'
#' @return A SQL syntax
#' @export
#'
#' @examples
#' # Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "~idiap/projects/SOPHIA_codi/data/jdbcDrivers/")
#' # dbms = Sys.getenv("DBMS")
#' # user <- if (Sys.getenv("DB_USER") == "") NULL else Sys.getenv("DB_USER")
#' # password <- if (Sys.getenv("DB_PASSWORD") == "") NULL else Sys.getenv("DB_PASSWORD")
#' # server = Sys.getenv("DB_SERVER")
#' # port = Sys.getenv("DB_PORT")
#' # connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
#' #                                                                  server = server,
#' #                                                                  user = user,
#' #                                                                  password = password,
#' #                                                                  port = port)
#' # cdm_bbdd <- DatabaseConnector::connect(connectionDetails = connectionDetails)
#' # cdm_schema <- 'omop21t2_test'
#' # results_sc <- 'sophia_test'
#' # cohortTable <- 'prova_Capr'
#' # cohortInfo <- CreateSQL_T2DM(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_T2DM <- function(cdm_bbdd,
                           cdm_schema,
                           results_sc,
                           cohortTable){
  #################################################################################################
  # Definicions del Capr (https://ohdsi.github.io/Capr/articles/complex-cohort-example.html)
  # Type 2 Diabetes segons Covid19CharacterizationCharybdis
  # <-- S'ha de posar en l'ordre que et dóna el getConceptIdDetails
  conceptMapping <- Capr::createConceptMapping(
    n = 9,
    includeDescendants = rep(T, 9),      # <--
    isExcluded = c(T, T, F, T, F, F, T, T, T)) # <--
  DMDx <- Capr::createConceptSetExpressionCustom(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(201820, 442793, 443238,
                                                          201254, 435216, 4058243, 40484648,
                                                          #Afegit mirant atlas-phenotype
                                                          195771, 761051), #diabetis secondaria
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Diabetes Diagnosis",
    conceptMapping = conceptMapping)
  # # arreglo errors del paquet
  # DMDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()
  DMDx_hist <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(40769338, 43021173, 42539022, 46270562),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "History of Diabetes Diagnosis",
    includeDescendants = TRUE)
  # DMDx_hist@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()
  #Type 1 Diabetes Diagnosis
  T1Dx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(201254, 435216, 4058243, 40484648),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Type 1 Diabetes Diagnosis",
    includeDescendants = TRUE)
  # T1Dx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()
  #Secondary Diabetes Diagnosis
  SecondDMDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(195771, 761051),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Secondary Diabetes Diagnosis",
    includeDescendants = TRUE)
  # SecondDMDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # 139953
  # T1DRxNormCodes <- paste(c(139825, 274783, 314684, 352385, 400008, 51428, 5856, 86009))
  # T1Rx <- getConceptCodeDetails(conceptCode = T1DRxNormCodes,
  #                               vocabulary = "RxNorm",
  #                               connection = cdm_bbdd,
  #                               vocabularyDatabaseSchema = cdm_schema,
  #                               mapToStandard = TRUE) %>%
  T1DRxNormCodes <- paste(c(1596977, 19058398, 19078552, 19078559, 19095211, 19095212, 19112791,
                            19133793, 19135264, 21076306, 21086042, 35410536, 35412958, 40051377,
                            40052768, 42479783, 42481504, 42481541, 42899447, 42902356, 42902587,
                            42902742, 42902821, 42902945, 42903059, 44058584, 46233969, 46234047,
                            46234234, 46234237))
  T1Rx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = T1DRxNormCodes,
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Type 1 Diabetes Medications",
    includeDescendants = TRUE)
  # T1Rx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # End-stage kidney disease
  ## End stage renal disease
  vec_eskd_cod <- c(443611, 193782, 443919, 45887996, 2617395, 2617396, 44786436, 2617401, 2617405,
                    2617397, 2617404, 2617403, 2617400, 2617402, 2617399, 2617398,
                    #afegit mirant la descriptiva
                    192359)
  ## Dialysis
  vec_dial <- c(4090651, 4032243, 45889365, 4027133, 38003431)
  ## Transplatation
  vec_trans <- c(199991, 42539502, 4324887, 4309006)
  ## eGFR < 15 (més endavant)
  vec_eskd <- c(vec_eskd_cod, vec_dial, vec_trans)
  RenalDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = vec_eskd,
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "End-stage kidney disease",
    includeDescendants = TRUE)
  # RenalDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()
  #Abnormal Lab
  eGFR <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c("40764999", "1617023", "1619025",
                                                          "46236952"),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Abnormal eGFR",
    includeDescendants = TRUE)
  # eGFR@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # Depress
  DepressDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(4098302, 433440, 440383,
                                                          #afegit mirant la descriptiva
                                                          4282096),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Depress",
    includeDescendants = TRUE)
  # DepressDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # schizophrenia
  SchizophreniaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(435783,
                                                          #afegit mirant la descriptiva
                                                          433450),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Schizophrenia",
    includeDescendants = TRUE)
  # SchizophreniaDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # Epilèpsia
  SeizureDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(380378), #afegit mirant la descriptiva
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Seizure",
    includeDescendants = TRUE)
  # SeizureDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # Any malignant tumour
  MaligNeoDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(443392, 4144289,
                                                          #afegit mirant atlas-demo
                                                          439392,
                                                          #afegit mirant la descriptiva
                                                          200962, 139750, 4311499, 137809, 197500),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Malignant Neoplasm",
    includeDescendants = TRUE)
  # MaligNeoDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # Systemic steroids: H02
  # CortiRxNormCodes <- paste(c(42903427, 1555120, 19017895,
  #                             920458, 1518254, 19055344, 1506270, 19027186, 1550557, 1551099,
  #                             903963, 975125, 1507705, 19011127, 977421, 19086888, 19050907,
  #                             19009116, 19061907, 19055156, 19042801, 37499303, 985708))
  CortiRxNormCodes <- paste(c(975169, 1506426, 1506430, 1506479, 1518259, 1518292, 1551101,
                              1551122, 1551123, 1551171, 1551192, 1555142, 1592257, 19016866,
                              19018083, 19063670, 19070310, 19084229, 19101595, 19104623, 19106649,
                              19106650, 19111643, 19121383, 35606531, 35606542, 36884768, 36893086,
                              37497612, 40234819, 40241504, 40897491, 40930518, 41052849,
                              42629020))
  CortiRx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = CortiRxNormCodes,
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Systemic steroids: H02 Medications",
    includeDescendants = TRUE)
  # CortiRx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()
  #################################################################################################
  # Building Queries
  #DMDx Condition Occurrence Query
  DMDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = DMDx)
  #DMDx_hist Condition Occurrence Query
  DMDx_histQuery <- Capr::createConditionOccurrence(conceptSetExpression = DMDx_hist)
  #T1Dx Condition Occurrence Query
  T1DxQuery <- Capr::createConditionOccurrence(conceptSetExpression = T1Dx)
  #T1Rx Drug Exposure Query
  T1RxQuery <- Capr::createDrugExposure(conceptSetExpression = T1Rx)
  #SecondDMDx Condition Occurrence Query
  SecondDMDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = SecondDMDx)

  #RenalDx Condition Occurrence Query
  RenalDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = RenalDx)
  #eGFR Query with value attribute
  AbeGFRQuery <- Capr::createMeasurement(
    conceptSetExpression = eGFR,
    #add attribute of eGFR < 15
    attributeList = list(Capr::createValueAsNumberAttribute(Op = "lt", Value = 15)))

  #DepressDx Condition Occurrence Query
  DepressDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = DepressDx)
  #SchizophreniaDx Condition Occurrence Query
  SchizophreniaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = SchizophreniaDx)
  #SeizureDx Condition Occurrence Query
  SeizureDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = SeizureDx)
  #MaligNeoDx Condition Occurrence Query
  MaligNeoDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = MaligNeoDx)
  #CortiRx Drug Exposure Query
  CortiRxQuery <- Capr::createDrugExposure(conceptSetExpression = CortiRx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  ## We defined initial entry as observed occurrence of all of the following events:
  ## + T2DM diagnosis,
  ## + prescription of a T2DM medication and
  ## + the presence of an abnormal lab
  PrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "T2DM as Covid19CharacterizationCharybdis",
    ComponentList = list(DMDxQuery,
                         DMDx_histQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  #################################################################################################
  # Inclusion Rules
  # Inclusion with age
  AgeAtt <- Capr::createAgeAttribute(Op = "gte", Value = 35)
  Age35AndOlderGroup <- Capr::createGroup(Name = ">=35 years old",
                                          type="ALL",
                                          criteriaList = NULL,
                                          demographicCriteriaList = list(AgeAtt),
                                          Groups = NULL)

  # No T1Dx at any point in patient history
  tl1 <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = "All",
                                                               StartCoeff = "Before",
                                                               EndDays = "All",
                                                               EndCoeff = "After"))
  noT1DxCount <- Capr::createCount(Query = T1DxQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tl1)
  NoT1DxGroup <- Capr::createGroup(Name = "No Diagnosis of Type 1 Diabetes",
                                   type = "ALL",
                                   criteriaList = list(noT1DxCount))

  #no exposure to T1DM medication
  tl2 <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = "All",
                                                               StartCoeff = "Before",
                                                               EndDays = 183L,
                                                               EndCoeff = "After"))
  noT1RxCount <- Capr::createCount(Query = T1RxQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tl2)
  NoT1RxGroup <- Capr::createGroup(Name = "Without Insulin [-inf, T2DM + 6m)",
                                   type = "ALL",
                                   criteriaList = list(noT1RxCount))

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = "All",
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No SecondDMDx at any point previous to DM in patient history
  noSecondDMDxCount <- Capr::createCount(Query = SecondDMDxQuery,
                                         Logic = "exactly",
                                         Count = 0L,
                                         Timeline = tlprev)
  noSecondDMDxGroup <- Capr::createGroup(Name = "No previous Secondary diabetes",
                                         type = "ALL",
                                         criteriaList = list(noSecondDMDxCount))

  # No RenalDx at any point previous to DM in patient history
  noRenalDxCount <- Capr::createCount(Query = RenalDxQuery,
                                      Logic = "exactly",
                                      Count = 0L,
                                      Timeline = tlprev)
  exactly0AbeGFRCount <- Capr::createCount(Query = AbeGFRQuery,
                                           Logic = "exactly",
                                           Count = 0L,
                                           Timeline = tlprev)
  noRenalDxGroup <- Capr::createGroup(Name = "No previous Renal problems",
                                      type = "ALL",
                                      criteriaList = list(noRenalDxCount,
                                                          exactly0AbeGFRCount))

  # No DepressDx at any point previous to DM in patient history
  noDepressDxCount <- Capr::createCount(Query = DepressDxQuery,
                                        Logic = "exactly",
                                        Count = 0L,
                                        Timeline = tlprev)
  noDepressDxGroup <- Capr::createGroup(Name = "No previous Depression",
                                        type = "ALL",
                                        criteriaList = list(noDepressDxCount))

  # No SchizophreniaDx at any point previous to DM in patient history
  noSchizophreniaDxCount <- Capr::createCount(Query = SchizophreniaDxQuery,
                                              Logic = "exactly",
                                              Count = 0L,
                                              Timeline = tlprev)
  noSchizophreniaDxGroup <- Capr::createGroup(Name = "No previous Schizophrenia",
                                              type = "ALL",
                                              criteriaList = list(noSchizophreniaDxCount))

  # No SeizureDx at any point previous to DM in patient history
  noSeizureDxCount <- Capr::createCount(Query = SeizureDxQuery,
                                        Logic = "exactly",
                                        Count = 0L,
                                        Timeline = tlprev)
  noSeizureDxGroup <- Capr::createGroup(Name = "No previous Seizure",
                                        type = "ALL",
                                        criteriaList = list(noSeizureDxCount))

  # No MaligNeoDx at any point previous to DM in patient history
  noMaligNeoDxCount <- Capr::createCount(Query = MaligNeoDxQuery,
                                         Logic = "exactly",
                                         Count = 0L,
                                         Timeline = tlprev)
  noMaligNeoDxGroup <- Capr::createGroup(Name = "No previous Malignant Neoplasm",
                                         type = "ALL",
                                         criteriaList = list(noMaligNeoDxCount))

  # No CortiRx at any point previous to DM in patient history
  noCortiRxCount <- Capr::createCount(Query = CortiRxQuery,
                                      Logic = "exactly",
                                      Count = 0L,
                                      Timeline = tlprev)
  noCortiRxGroup <- Capr::createGroup(Name = "No previous Corticoides",
                                      type = "ALL",
                                      criteriaList = list(noCortiRxCount))

  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(Age35AndOlderGroup,
                                                               NoT1DxGroup,
                                                               NoT1RxGroup,
                                                               noSecondDMDxGroup,
                                                               noRenalDxGroup,
                                                               noDepressDxGroup,
                                                               noSchizophreniaDxGroup,
                                                               noSeizureDxGroup,
                                                               noMaligNeoDxGroup,
                                                               noCortiRxGroup),
                                               Limit = "First")

  #################################################################################################
  # Finalizing the Cohort Definition
  #person exits cohort if there is a diagnosis of T1DM
  CensoringCriteria <- Capr::createCensoringCriteria(Name = "Censor of Renal, Depress cases",
                                                     ComponentList = list(RenalDxQuery,
                                                                          DepressDxQuery,
                                                                          SchizophreniaDxQuery,
                                                                          SeizureDxQuery,
                                                                          MaligNeoDxQuery))
  # La data d'entrada mínima és 2010-01-01, els anteriors són prevalents.
  # Assegurem que tenim almenys 5 anys de seguiment
  cohortEra <- Capr::createCohortEra(LeftCensorDate = "2009-12-31")
  T2DMPhenotype <- Capr::createCohortDefinition(
    Name = "T2DM as Covid19CharacterizationCharybdis",
    PrimaryCriteria = PrimaryCriteria,
    # AdditionalCriteria = AdditionalCriteria)#,
    InclusionRules = InclusionRules,
    CensoringCriteria = CensoringCriteria,
    # EndStrategy = EsCovidDiag,
    CohortEra = cohortEra)
  # JSON
  T2DMPhenotypeJson <- Capr::compileCohortDefinition(T2DMPhenotype)

  #################################################################################################
  # https://ohdsi.github.io/Capr/articles/CAPR_tutorial.html
  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 1,
                                         cdmSchema = cdm_schema,
                                         targetTable = paste(results_sc, cohortTable, sep='.'),
                                         resultSchema = results_sc,
                                         vocabularySchema = cdm_schema,
                                         generateStats = T)
  cohortInfo <- Capr::compileCohortDefinition(T2DMPhenotype, genOp)
  # Modifiquem el codi per tenir els casos anteriors a l'entrada al SIDIAP.
  cohortInfo$ohdiSQL <- gsub(pattern = 'E.start_date >=  OP.observation_period_start_date and ',
                             replacement = '',
                             x = cohortInfo$ohdiSQL,
                             fixed = T)
  cohortInfo$ohdiSQL <- gsub(pattern = 'WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE',
                             replacement = '',
                             x = cohortInfo$ohdiSQL,
                             fixed = T)
  cohortInfo$ohdiSQL <- gsub(pattern = 'AND A.START_DATE >= P.OP_START_DATE',
                             replacement = '',
                             x = cohortInfo$ohdiSQL,
                             fixed = T)
  cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_censor_stats',
                             replacement = paste0(cohortTable, '_censor_stats'),
                             x = cohortInfo$ohdiSQL)
  cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_result',
                             replacement = paste0(cohortTable, '_inclusion_result'),
                             x = cohortInfo$ohdiSQL)
  cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_stats',
                             replacement = paste0(cohortTable, '_inclusion_stats'),
                             x = cohortInfo$ohdiSQL)
  cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_summary_stats',
                             replacement = paste0(cohortTable, '_summary_stats'),
                             x = cohortInfo$ohdiSQL)
  cohortInfo$ohdiSQL <- gsub(pattern = '15,0000',
                             replacement = '15.0000',
                             x = cohortInfo$ohdiSQL)

  return(cohortInfo)
}

#' Create SQL for T2DM extraction for Outcome
#'
#' @param cdm_bbdd A connection for a OMOP database via DatabaseConnector
#' @param cdm_schema A name for OMOP schema
#' @param results_sc A name for result schema
#' @param cohortTable A name of the result cohort
#'
#' @return A SQL syntax
#' @export
#'
#' @examples
#' # Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "~idiap/projects/SOPHIA_codi/data/jdbcDrivers/")
#' # dbms = Sys.getenv("DBMS")
#' # user <- if (Sys.getenv("DB_USER") == "") NULL else Sys.getenv("DB_USER")
#' # password <- if (Sys.getenv("DB_PASSWORD") == "") NULL else Sys.getenv("DB_PASSWORD")
#' # server = Sys.getenv("DB_SERVER")
#' # port = Sys.getenv("DB_PORT")
#' # connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
#' #                                                                  server = server,
#' #                                                                  user = user,
#' #                                                                  password = password,
#' #                                                                  port = port)
#' # cdm_bbdd <- DatabaseConnector::connect(connectionDetails = connectionDetails)
#' # cdm_schema <- 'omop21t2_test'
#' # results_sc <- 'sophia_test'
#' # cohortTable <- 'prova_Capr'
#' # outcomeInfo <- CreateSQL_T2DM_outcome(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_T2DM_outcome <- function(cdm_bbdd,
                                   cdm_schema,
                                   results_sc,
                                   cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  #Angina
  AngorDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(321318),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Angor Diagnosis",
    includeDescendants = TRUE)
  # AngorDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  #AMI
  AMIDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(312327),# 4108217, 433128, 4329847),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "AMI Diagnosis",
    includeDescendants = TRUE)
  # AMIDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  #Ictus
  StrokeDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(43530727, 443454),# 255919, 43022059),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Stroke Diagnosis",
    includeDescendants = TRUE)
  # StrokeDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  #TIA
  TIADx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(373503, 381591),# 4353709, 43022059),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "TIA Diagnosis",
    includeDescendants = TRUE)
  # TIADx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  #################################################################################################
  # Building Queries
  AngorDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = AngorDx)
  AMIDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = AMIDx)
  StrokeDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = StrokeDx)
  TIADxQuery <- Capr::createConditionOccurrence(conceptSetExpression = TIADx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Angor, AMI, Stroke and TIA",
    ComponentList = list(AngorDxQuery,
                         AMIDxQuery,
                         StrokeDxQuery,
                         TIADxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 3,
                                         cdmSchema = cdm_schema,
                                         targetTable = paste(results_sc, cohortTable, sep='.'),
                                         resultSchema = results_sc,
                                         vocabularySchema = cdm_schema,
                                         generateStats = T)
  outcomeInfo <- Capr::compileCohortDefinition(OUTCOME, genOp)
  # Modifiquem el codi per tenir els casos anteriors a l'entrada al SIDIAP.
  outcomeInfo$ohdiSQL <- gsub(pattern = 'E.start_date >=  OP.observation_period_start_date and ',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'AND A.START_DATE >= P.OP_START_DATE',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_censor_stats',
                              replacement = paste0(cohortTable, '_censor_stats'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_result',
                              replacement = paste0(cohortTable, '_inclusion_result'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_stats',
                              replacement = paste0(cohortTable, '_inclusion_stats'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_summary_stats',
                              replacement = paste0(cohortTable, '_summary_stats'),
                              x = outcomeInfo$ohdiSQL)
  return(outcomeInfo)
}

#' Create SQL for AMI extraction for Outcome
#'
#' @param cdm_bbdd A connection for a OMOP database via DatabaseConnector
#' @param cdm_schema A name for OMOP schema
#' @param results_sc A name for result schema
#' @param cohortTable A name of the result cohort
#'
#' @return A SQL syntax
#' @export
#'
#' @examples
#' # Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "~idiap/projects/SOPHIA_codi/data/jdbcDrivers/")
#' # dbms = Sys.getenv("DBMS")
#' # user <- if (Sys.getenv("DB_USER") == "") NULL else Sys.getenv("DB_USER")
#' # password <- if (Sys.getenv("DB_PASSWORD") == "") NULL else Sys.getenv("DB_PASSWORD")
#' # server = Sys.getenv("DB_SERVER")
#' # port = Sys.getenv("DB_PORT")
#' # connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
#' #                                                                 server = server,
#' #                                                                 user = user,
#' #                                                                 password = password,
#' #                                                                 port = port)
#' # cdm_bbdd <- DatabaseConnector::connect(connectionDetails = connectionDetails)
#' # cdm_schema <- 'omop21t2_test'
#' # results_sc <- 'sophia_test'
#' # cohortTable <- 'prova_Capr'
#' # outcomeInfo <- CreateSQL_AMI(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_AMI <- function(cdm_bbdd,
                          cdm_schema,
                          results_sc,
                          cohortTable){
  #################################################################################################
  # Cohort OUTCOME

  #AMI
  AMIDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(#I21
      312327, 4296653, 45766075, 45766116, 4296653, 4270024, 4329847,
      #I22
      4108217, 4108677, 4108218, 45766241, 45766114, 45766114,
      #I23
      4329847, 4108678, 438172, 4119953, 4108679, 4108219, 4108220, 4108680,
      4198141),
                                             # c(312327, 4108217,
                                             #              438172, 4119953, 4108219, 4108680,
                                             #              4198141),# , 433128, 4329847),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "AMI Diagnosis",
    includeDescendants = FALSE)
  # AMIDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  #################################################################################################
  # Building Queries
  AMIDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = AMIDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Angor, AMI, Stroke and TIA",
    ComponentList = list(AMIDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 3,
                                         cdmSchema = cdm_schema,
                                         targetTable = paste(results_sc, cohortTable, sep='.'),
                                         resultSchema = results_sc,
                                         vocabularySchema = cdm_schema,
                                         generateStats = T)
  outcomeInfo <- Capr::compileCohortDefinition(OUTCOME, genOp)
  # Modifiquem el codi per tenir els casos anteriors a l'entrada al SIDIAP.
  outcomeInfo$ohdiSQL <- gsub(pattern = 'E.start_date >=  OP.observation_period_start_date and ',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'AND A.START_DATE >= P.OP_START_DATE',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_censor_stats',
                              replacement = paste0(cohortTable, '_censor_stats'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_result',
                              replacement = paste0(cohortTable, '_inclusion_result'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_stats',
                              replacement = paste0(cohortTable, '_inclusion_stats'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_summary_stats',
                              replacement = paste0(cohortTable, '_summary_stats'),
                              x = outcomeInfo$ohdiSQL)
  return(outcomeInfo)
}

#' Create SQL for angor extraction for Outcome
#'
#' @param cdm_bbdd A connection for a OMOP database via DatabaseConnector
#' @param cdm_schema A name for OMOP schema
#' @param results_sc A name for result schema
#' @param cohortTable A name of the result cohort
#'
#' @return A SQL syntax
#' @export
#'
#' @examples
#' # Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "~idiap/projects/SOPHIA_codi/data/jdbcDrivers/")
#' # dbms = Sys.getenv("DBMS")
#' # user <- if (Sys.getenv("DB_USER") == "") NULL else Sys.getenv("DB_USER")
#' # password <- if (Sys.getenv("DB_PASSWORD") == "") NULL else Sys.getenv("DB_PASSWORD")
#' # server = Sys.getenv("DB_SERVER")
#' # port = Sys.getenv("DB_PORT")
#' # connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
#' #                                                                  server = server,
#' #                                                                  user = user,
#' #                                                                  password = password,
#' #                                                                  port = port)
#' # cdm_bbdd <- DatabaseConnector::connect(connectionDetails = connectionDetails)
#' # cdm_schema <- 'omop21t2_test'
#' # results_sc <- 'sophia_test'
#' # cohortTable <- 'prova_Capr'
#' # outcomeInfo <- CreateSQL_angor(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_angor <- function(cdm_bbdd,
                            cdm_schema,
                            results_sc,
                            cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  #Angina
  AngorDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(321318, 315296, 4127089),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Angor Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  AngorDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = AngorDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Angor, AMI, Stroke and TIA",
    ComponentList = list(AngorDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 4,
                                         cdmSchema = cdm_schema,
                                         targetTable = paste(results_sc, cohortTable, sep='.'),
                                         resultSchema = results_sc,
                                         vocabularySchema = cdm_schema,
                                         generateStats = T)
  outcomeInfo <- Capr::compileCohortDefinition(OUTCOME, genOp)
  # Modifiquem el codi per tenir els casos anteriors a l'entrada al SIDIAP.
  outcomeInfo$ohdiSQL <- gsub(pattern = 'E.start_date >=  OP.observation_period_start_date and ',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'AND A.START_DATE >= P.OP_START_DATE',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_censor_stats',
                              replacement = paste0(cohortTable, '_censor_stats'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_result',
                              replacement = paste0(cohortTable, '_inclusion_result'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_stats',
                              replacement = paste0(cohortTable, '_inclusion_stats'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_summary_stats',
                              replacement = paste0(cohortTable, '_summary_stats'),
                              x = outcomeInfo$ohdiSQL)
  return(outcomeInfo)
}

#' Create SQL for Ischemic Stroke extraction for Outcome
#'
#' @param cdm_bbdd A connection for a OMOP database via DatabaseConnector
#' @param cdm_schema A name for OMOP schema
#' @param results_sc A name for result schema
#' @param cohortTable A name of the result cohort
#'
#' @return A SQL syntax
#' @export
#'
#' @examples
#' # Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "~idiap/projects/SOPHIA_codi/data/jdbcDrivers/")
#' # dbms = Sys.getenv("DBMS")
#' # user <- if (Sys.getenv("DB_USER") == "") NULL else Sys.getenv("DB_USER")
#' # password <- if (Sys.getenv("DB_PASSWORD") == "") NULL else Sys.getenv("DB_PASSWORD")
#' # server = Sys.getenv("DB_SERVER")
#' # port = Sys.getenv("DB_PORT")
#' # connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
#' #                                                                  server = server,
#' #                                                                  user = user,
#' #                                                                  password = password,
#' #                                                                  port = port)
#' # cdm_bbdd <- DatabaseConnector::connect(connectionDetails = connectionDetails)
#' # cdm_schema <- 'omop21t2_test'
#' # results_sc <- 'sophia_test'
#' # cohortTable <- 'prova_Capr'
#' # outcomeInfo <- CreateSQL_stroke_i(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_stroke_i <- function(cdm_bbdd,
                               cdm_schema,
                               results_sc,
                               cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  #Ictus
  StrokeDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(
      #I63
      443454, 4110189, 4110190, 4043731, 4110192, 4108356, 443454, 4111714,
      #I64
      #I65
      43022059, 4153380, 4159164, 443239),
      #43530727, 443454),# 255919, 43022059),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Stroke Diagnosis",
    includeDescendants = FALSE)
  # StrokeDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  #################################################################################################
  # Building Queries
  StrokeDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = StrokeDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Angor, AMI, Stroke and TIA",
    ComponentList = list(StrokeDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 5,
                                         cdmSchema = cdm_schema,
                                         targetTable = paste(results_sc, cohortTable, sep='.'),
                                         resultSchema = results_sc,
                                         vocabularySchema = cdm_schema,
                                         generateStats = T)
  outcomeInfo <- Capr::compileCohortDefinition(OUTCOME, genOp)
  # Modifiquem el codi per tenir els casos anteriors a l'entrada al SIDIAP.
  outcomeInfo$ohdiSQL <- gsub(pattern = 'E.start_date >=  OP.observation_period_start_date and ',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'AND A.START_DATE >= P.OP_START_DATE',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_censor_stats',
                              replacement = paste0(cohortTable, '_censor_stats'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_result',
                              replacement = paste0(cohortTable, '_inclusion_result'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_stats',
                              replacement = paste0(cohortTable, '_inclusion_stats'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_summary_stats',
                              replacement = paste0(cohortTable, '_summary_stats'),
                              x = outcomeInfo$ohdiSQL)
  return(outcomeInfo)
}

#' Create SQL for TIA extraction for Outcome
#'
#' @param cdm_bbdd A connection for a OMOP database via DatabaseConnector
#' @param cdm_schema A name for OMOP schema
#' @param results_sc A name for result schema
#' @param cohortTable A name of the result cohort
#'
#' @return A SQL syntax
#' @export
#'
#' @examples
#' # Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "~idiap/projects/SOPHIA_codi/data/jdbcDrivers/")
#' # dbms = Sys.getenv("DBMS")
#' # user <- if (Sys.getenv("DB_USER") == "") NULL else Sys.getenv("DB_USER")
#' # password <- if (Sys.getenv("DB_PASSWORD") == "") NULL else Sys.getenv("DB_PASSWORD")
#' # server = Sys.getenv("DB_SERVER")
#' # port = Sys.getenv("DB_PORT")
#' # connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
#' #                                                                  server = server,
#' #                                                                  user = user,
#' #                                                                  password = password,
#' #                                                                  port = port)
#' # cdm_bbdd <- DatabaseConnector::connect(connectionDetails = connectionDetails)
#' # cdm_schema <- 'omop21t2_test'
#' # results_sc <- 'sophia_test'
#' # cohortTable <- 'prova_Capr'
#' # outcomeInfo <- CreateSQL_TIA(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_TIA <- function(cdm_bbdd,
                          cdm_schema,
                          results_sc,
                          cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  #TIA
  TIADx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(#G45
      373503,
      437306, 4338523, 381036, 4112020, 4048785,
      #G46
      381591, 4110194, 4108360, 4110195, 4111710, 4111711, 4045737, 4045738, 4046360),# 4353709, 43022059),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "TIA Diagnosis",
    includeDescendants = FALSE)
  # TIADx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  #################################################################################################
  # Building Queries
  TIADxQuery <- Capr::createConditionOccurrence(conceptSetExpression = TIADx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Angor, AMI, Stroke and TIA",
    ComponentList = list(TIADxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 6,
                                         cdmSchema = cdm_schema,
                                         targetTable = paste(results_sc, cohortTable, sep='.'),
                                         resultSchema = results_sc,
                                         vocabularySchema = cdm_schema,
                                         generateStats = T)
  outcomeInfo <- Capr::compileCohortDefinition(OUTCOME, genOp)
  # Modifiquem el codi per tenir els casos anteriors a l'entrada al SIDIAP.
  outcomeInfo$ohdiSQL <- gsub(pattern = 'E.start_date >=  OP.observation_period_start_date and ',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'AND A.START_DATE >= P.OP_START_DATE',
                              replacement = '',
                              x = outcomeInfo$ohdiSQL, fixed = T)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_censor_stats',
                              replacement = paste0(cohortTable, '_censor_stats'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_result',
                              replacement = paste0(cohortTable, '_inclusion_result'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_stats',
                              replacement = paste0(cohortTable, '_inclusion_stats'),
                              x = outcomeInfo$ohdiSQL)
  outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_summary_stats',
                              replacement = paste0(cohortTable, '_summary_stats'),
                              x = outcomeInfo$ohdiSQL)
  return(outcomeInfo)
}

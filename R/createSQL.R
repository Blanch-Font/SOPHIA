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
    Name = "Type 2 Diabetes Diagnosis",
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
  # Medicació Insulina
  T1DRxNormCodes <- c(1596977, 19058398, 19078552, 19078559, 19095211, 19095212, 19112791,
                      19133793, 19135264, 21076306, 21086042, 35410536, 35412958, 40051377,
                      40052768, 42479783, 42481504, 42481541, 42899447, 42902356, 42902587,
                      42902742, 42902821, 42902945, 42903059, 44058584, 46233969, 46234047,
                      46234234, 46234237,
                      1502905, 1513843, 1513876, 1516976, 1531601, 1544838, 1550023,
                      1562586, 1567198, 1586346, 1586369, 1588986, 1590165, 1596977,
                      19013926, 19013951, 19090180, 19090187, 19090204, 19090221, 19090226,
                      19090229, 19090244, 19090247, 19090249, 19091621, 35198096, 35602717,
                      42899447, 46221581)
  T1Rx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = T1DRxNormCodes,
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Insulin Medications",
    includeDescendants = TRUE)
  # T1Rx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

  # Medicació A10B excepte Metformin
  NIADRxNormCodes <- c(793143, 793293, 1000979, 1502809, 1502826, 1502855, 1510202,
                         1515249, 1516766, 1517998, 1525215, 1529331, 1530014,
                         1547504, 1559684, 1560171, 1580747, 1583722, 1594973,
                         1597756, 19001409, 19001441, 19033498, 19033909, 19035533, 19059796,
                         19097821, 19122137, 40166035, 40170911, 40239216, 40798673,
                         40798860, 43009020, 43009032, 43009051, 43009089, 43009094, 43013884,
                         43526465, 44506754, 44785829, 44816332, 45774435, 45774751)
  NIADRx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = NIADRxNormCodes,
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "NIAD Medications",
    includeDescendants = TRUE)

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
  #NIADRx Drug Exposure Query
  NIADRxQuery <- Capr::createDrugExposure(conceptSetExpression = NIADRx)
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
  tlafte <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = "All",
                                                                  EndCoeff = "After"))
  tlafte_6m <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                     StartCoeff = "Before",
                                                                     EndDays = 183L,
                                                                     EndCoeff = "After"))
  tlafte_1y <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                     StartCoeff = "Before",
                                                                     EndDays = 365L,
                                                                     EndCoeff = "After"))

  # Insulin at any point in patient history
  T1RxCount <- Capr::createCount(Query = T1RxQuery,
                                    Logic = "at_least",
                                    Count = 1,
                                    Timeline = tlafte)
  T1RxGroup <- Capr::createGroup(Name = "Insulin after diagnosis",
                                 type = "ALL",
                                 criteriaList = list(T1RxCount))
  # No NIADRx at any point in patient history
  noNIADRxCount <- Capr::createCount(Query = NIADRxQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tl1)
  noNIADRxGroup <- Capr::createGroup(Name = "No NIAD after diagnosis",
                                    type = "ALL",
                                    criteriaList = list(noNIADRxCount))
  # No T2Dx at any point in patient history
  noDM2DxCount <- Capr::createCount(Query = DM2DxQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tlafte)
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
                                               Contents = list(#Age18AndOlderGroup,
                                                               # noDM2DxGroup,
                                                               T1RxGroup,
                                                               noNIADRxGroup,
                                                               noSecondDMDxGroup,
                                                               noRenalDxGroup,
                                                               noSchizophreniaDxGroup,
                                                               noSeizureDxGroup,
                                                               noMaligNeoDxGroup,
                                                               noCortiRxGroup,
                                                               noEatingDisorderDxGroup),
                                                               # noSymptomsHyperglycaemiaDxGroup),
                                               Limit = "First")

  #################################################################################################
  # Finalizing the Cohort Definition
  #person exits cohort if there is a diagnosis of T1DM
  CensoringCriteria <- Capr::createCensoringCriteria(Name = "Censor of Renal, Depress cases",
                                                     ComponentList = list(RenalDxQuery,
                                                                          SchizophreniaDxQuery,
                                                                          SeizureDxQuery,
                                                                          MaligNeoDxQuery,
                                                                          CortiRxQuery,
                                                                          EatingDisorderDxQuery))#,
                                                                          # SymptomsHyperglycaemiaDxQuery))
  # La data d'entrada mínima és 2010-01-01, els anteriors són prevalents.
  # Assegurem que tenim almenys 5 anys de seguiment
  cohortEra <- Capr::createCohortEra(LeftCensorDate = "2009-12-31")
  T1DMPhenotype <- Capr::createCohortDefinition(
    Name = "T1DM",
    PrimaryCriteria = PrimaryCriteria,
    # AdditionalCriteria = AdditionalCriteria)#,
    InclusionRules = InclusionRules,
    # CensoringCriteria = CensoringCriteria,
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
  conceptMapping <- Capr::createConceptMapping(n = 6,
                                               includeDescendants = rep(T, 6),
                                               isExcluded = c(T, F, F, F, T, T))
  DMDx <- Capr::createConceptSetExpressionCustom(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(195771, 201820, 442793, 443238, 761051, 4058243),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Diabetes Diagnosis",
    conceptMapping = conceptMapping)
  DMDx_hist <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(40769338, 43021173, 42539022, 46270562),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "History of DM Diagnosis",
    includeDescendants = TRUE)

  #Type 1 Diabetes Diagnosis
  T1Dx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(201254, 435216, 40484648, 40484649),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Type 1 Diabetes Diagnosis",
    includeDescendants = TRUE)

  #Type 2 Diabetes Diagnosis
  T2Dx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(201826, 443732, 40482801, 40485020),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Type 2 Diabetes Diagnosis",
    includeDescendants = TRUE)
  conceptMapping <- Capr::createConceptMapping(n = 14,
                                               includeDescendants = rep(T, 14),
                                               isExcluded = c(T, T, F, T, T, F,
                                                              F, T, T, T, T,
                                                              T, T, T))
  UDMDx <- Capr::createConceptSetExpressionCustom(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(195771, 201254, 201820, 201826, 435216, 442793,
                                                          443238, 443732, 761051, 4058243, 40482801,
                                                          40484648, 40484649, 40485020),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Undefinded Diabetes Diagnosis",
    conceptMapping = conceptMapping)

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

  # Medicació A10B
  NIADRxNormCodes <- c(793143, 793293, 1000979, 1502809, 1502826, 1502855, 1510202,
                       1515249, 1516766, 1517998, 1525215, 1529331, 1530014,
                       1547504, 1559684, 1560171, 1580747, 1583722, 1594973,
                       1597756, 19001409, 19001441, 19033498, 19033909, 19035533, 19059796,
                       19097821, 19122137, 40166035, 40170911, 40239216, 40798673,
                       40798860, 43009020, 43009032, 43009051, 43009089, 43009094, 43013884,
                       43526465, 44506754, 44785829, 44816332, 45774435, 45774751)
  NIADRx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = NIADRxNormCodes,
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "NIAD Medications",
    includeDescendants = TRUE)

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
  #T2Dx Condition Occurrence Query
  T2DxQuery <- Capr::createConditionOccurrence(conceptSetExpression = T2Dx)
  #UDMDx Condition Occurrence Query
  UDMDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = UDMDx)
  #T1Rx Drug Exposure Query
  T1RxQuery <- Capr::createDrugExposure(conceptSetExpression = T1Rx)
  #NIADRx Drug Exposure Query
  NIADRxQuery <- Capr::createDrugExposure(conceptSetExpression = NIADRx)
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
  # Building Count
  # Time Lines
  # Qualsevol moment previ o posterior
  tl1 <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = "All",
                                                               StartCoeff = "Before",
                                                               EndDays = "All",
                                                               EndCoeff = "After"))
  # Primers 6 mesos
  tl2 <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = "All",
                                                               StartCoeff = "Before",
                                                               EndDays = 183L,
                                                               EndCoeff = "After"))
  # Qualsevol moment previ
  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = "All",
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # Qualsevol moment posterior
  tlafte <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = "All",
                                                                  EndCoeff = "After"))


  #at least 1 occurrence of DMDx
  atLeast1DMDxCount <- Capr::createCount(Query = DMDxQuery,
                                   Logic = "at_least",
                                   Count = 1L,
                                   Timeline = tlprev)
  atLeast1DMDx_histCount <- Capr::createCount(Query = DMDx_histQuery,
                                   Logic = "at_least",
                                   Count = 1L,
                                   Timeline = tlprev)
  #at least 1 occurrence of T1DM
  atLeast1T1DMCount <- Capr::createCount(Query = T1DxQuery,
                                   Logic = "at_least",
                                   Count = 1L,
                                   Timeline = tl1)
  #at least 1 occurrence of T2DM
  atLeast1T2DMCount <- Capr::createCount(Query = T2DxQuery,
                                         Logic = "at_least",
                                         Count = 1L,
                                         Timeline = tl1)
  #at least 1 occurrence of UDM
  atLeast1UDMCount <- Capr::createCount(Query = UDMDxQuery,
                                        Logic = "at_least",
                                        Count = 1L,
                                        Timeline = tl1)
  # No T1Dx at any point in patient history
  noT1DxCount <- Capr::createCount(Query = T1DxQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tl1)
  # No T2Dx at any point in patient history
  noT2DxCount <- Capr::createCount(Query = T2DxQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tl1)
  # No UDMDx at any point in patient history
  noUDMDxCount <- Capr::createCount(Query = UDMDxQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tl1)
  # No DMDx at any point in patient history
  noDMDxCount <- Capr::createCount(Query = DMDxQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tl1)
  # No DMDx_hist at any point in patient history
  noDMDx_histCount <- Capr::createCount(Query = DMDx_histQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tl1)
  #no exposure to T1DM medication (6 mesos després del 1r diagnòstic)
  noT1RxCount <- Capr::createCount(Query = T1RxQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tl1)
  #no exposure to T1DM medication (6 mesos després del 1r diagnòstic)
  noT1Rx_6mCount <- Capr::createCount(Query = T1RxQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tl2)
  # No NIADRx at any point in patient history
  atLeastNIADRxCount <- Capr::createCount(Query = NIADRxQuery,
                                          Logic = "at_least",
                                          Count = 1L,
                                          Timeline = tlafte)
  # No SecondDMDx at any point previous to DM in patient history
  noSecondDMDxCount <- Capr::createCount(Query = SecondDMDxQuery,
                                         Logic = "exactly",
                                         Count = 0L,
                                         Timeline = tlprev)
  # No RenalDx at any point previous to DM in patient history
  noRenalDxCount <- Capr::createCount(Query = RenalDxQuery,
                                      Logic = "exactly",
                                      Count = 0L,
                                      Timeline = tlprev)
  exactly0AbeGFRCount <- Capr::createCount(Query = AbeGFRQuery,
                                           Logic = "exactly",
                                           Count = 0L,
                                           Timeline = tlprev)
  # No DepressDx at any point previous to DM in patient history
  noDepressDxCount <- Capr::createCount(Query = DepressDxQuery,
                                        Logic = "exactly",
                                        Count = 0L,
                                        Timeline = tlprev)
  # No SchizophreniaDx at any point previous to DM in patient history
  noSchizophreniaDxCount <- Capr::createCount(Query = SchizophreniaDxQuery,
                                              Logic = "exactly",
                                              Count = 0L,
                                              Timeline = tlprev)
  # No SeizureDx at any point previous to DM in patient history
  noSeizureDxCount <- Capr::createCount(Query = SeizureDxQuery,
                                        Logic = "exactly",
                                        Count = 0L,
                                        Timeline = tlprev)
  # No MaligNeoDx at any point previous to DM in patient history
  noMaligNeoDxCount <- Capr::createCount(Query = MaligNeoDxQuery,
                                         Logic = "exactly",
                                         Count = 0L,
                                         Timeline = tlprev)
  # No CortiRx at any point previous to DM in patient history
  noCortiRxCount <- Capr::createCount(Query = CortiRxQuery,
                                      Logic = "exactly",
                                      Count = 0L,
                                      Timeline = tlprev)

  #################################################################################################
  # Create Group
  NoT1DxGroup <- Capr::createGroup(Name = "No Diagnosis of Type 1 Diabetes",
                                   type = "ALL",
                                   criteriaList = list(noT1DxCount))
  DMDxGroup <- Capr::createGroup(Name = "DMDX Diagnosis",
                                   type = "ANY",
                                   criteriaList = list(atLeast1DMDxCount,
                                                       atLeast1DMDx_histCount))
  #Path 1: T2DM and no T1DM
  Pathway_1_Group <- Capr::createGroup(
    Name = "Pathway1",
    Description = "only T2DMD",
    type = "ALL",
    criteriaList = list(atLeast1T2DMCount, noUDMDxCount, noT1DxCount))
  #Path 2: T2DM, DMD, DMDX_hist and no T1DM
  Pathway_2_Group <- Capr::createGroup(
    Name = "Pathway2",
    Description = "T2DMD, UDM and No T1DM",
    type = "ALL",
    criteriaList = list(atLeast1T2DMCount, atLeast1UDMCount, noT1DxCount))
  Pathway_3_Group <- Capr::createGroup(
    Name = "Pathway3",
    Description = "UDM, no T2DM, No T1DM and any NIAD other than metformin",
    type = "ALL",
    criteriaList = list(atLeast1UDMCount, noT2DxCount, noT1DxCount, atLeastNIADRxCount))
  Pathway_4_Group <- Capr::createGroup(
    Name = "Pathway4",
    Description = "T1DM and no Insuline",
    type = "ALL",
    criteriaList = list(atLeast1T1DMCount, noT1RxCount))
  Pathway_5_Group <- Capr::createGroup(
    Name = "Pathway5",
    Description = "T1DM and any NIAD other than metformin",
    type = "ALL",
    criteriaList = list(atLeast1T1DMCount, atLeastNIADRxCount))
  Pathway_6_Group <- Capr::createGroup(
    Name = "Pathway6",
    Description = "any NIAD other than metformin, no (DMDX or DMDX_hist)",
    type = "ALL",
    criteriaList = list(atLeastNIADRxCount, noDMDxCount, noDMDx_histCount))
  Pathway_Group <- Capr::createGroup(
    Name = "Case for T2DM using algorithm",
    type = "ANY",
    Groups = list(Pathway_1_Group, Pathway_2_Group, Pathway_3_Group, Pathway_4_Group,
                  Pathway_5_Group, Pathway_6_Group)
  )

  #################################################################################################
  # Creating the Initial Cohort Entry
  ## We defined initial entry as observed occurrence of all of the following events:
  ## + T2DM diagnosis,
  ## + prescription of a T2DM medication and
  ## + the presence of an abnormal lab
  PrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "T2DM as algorithm",
    ComponentList = list(DMDxQuery,
                         NIADRxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  #################################################################################################
  # Additional Rules
  # Inclusion with age
  AgeAtt <- Capr::createAgeAttribute(Op = "gte", Value = 35)
  Age35AndOlderGroup <- Capr::createGroup(Name = ">=35 years old",
                                          type="ALL",
                                          criteriaList = NULL,
                                          demographicCriteriaList = list(AgeAtt),
                                          Groups = NULL)


  AdditionalCriteria <- Capr::createAdditionalCriteria(
    Name = "Pathway",
    Contents = Pathway_Group,
    Limit = "First"
  )

  #################################################################################################
  # Inclusion Rules

  #no exposure to T1DM medication
  noT1Rx_6mGroup <- Capr::createGroup(Name = "Without Insulin [-inf, T2DM + 6m)",
                                   type = "ALL",
                                   criteriaList = list(noT1Rx_6mCount))

  # No SecondDMDx at any point previous to DM in patient history
  noSecondDMDxGroup <- Capr::createGroup(Name = "No previous Secondary diabetes",
                                         type = "ALL",
                                         criteriaList = list(noSecondDMDxCount))

  # No RenalDx at any point previous to DM in patient history
  noRenalDxGroup <- Capr::createGroup(Name = "No previous Renal problems",
                                      type = "ALL",
                                      criteriaList = list(noRenalDxCount,
                                                          exactly0AbeGFRCount))

  # No DepressDx at any point previous to DM in patient history
  noDepressDxGroup <- Capr::createGroup(Name = "No previous Depression",
                                        type = "ALL",
                                        criteriaList = list(noDepressDxCount))

  # No SchizophreniaDx at any point previous to DM in patient history
  noSchizophreniaDxGroup <- Capr::createGroup(Name = "No previous Schizophrenia",
                                              type = "ALL",
                                              criteriaList = list(noSchizophreniaDxCount))

  # No SeizureDx at any point previous to DM in patient history
  noSeizureDxGroup <- Capr::createGroup(Name = "No previous Seizure",
                                        type = "ALL",
                                        criteriaList = list(noSeizureDxCount))

  # No MaligNeoDx at any point previous to DM in patient history
  noMaligNeoDxGroup <- Capr::createGroup(Name = "No previous Malignant Neoplasm",
                                         type = "ALL",
                                         criteriaList = list(noMaligNeoDxCount))

  # No CortiRx at any point previous to DM in patient history
  noCortiRxGroup <- Capr::createGroup(Name = "No previous Corticoides",
                                      type = "ALL",
                                      criteriaList = list(noCortiRxCount))

  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(Age35AndOlderGroup,
                                                               # NoT1DxGroup,
                                                               Pathway_Group,
                                                               noT1Rx_6mGroup,
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
    # AdditionalCriteria = AdditionalCriteria,
    InclusionRules = InclusionRules,
    # CensoringCriteria = CensoringCriteria,
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
      312327, #Acute myocardial infarction
      4296653, #Acute ST segment elevation myocardial infarction
      45766075, #Acute anterior ST segment elevation myocardial infarction
      45766116, #Acute ST segment elevation myocardial infarction of inferior wall
      4296653, #Acute ST segment elevation myocardial infarction
      4270024, #Acute non-ST segment elevation myocardial infarction
      4329847, #Myocardial infarction
      #I22
      4108217, #Subsequent myocardial infarction
      4108677, #Subsequent myocardial infarction of anterior wall
      4108218, #Subsequent myocardial infarction of inferior wall
      45766241, #Subsequent non-ST segment elevation myocardial infarction
      45766114, #Subsequent ST segment elevation myocardial infarction
      #I23
      4108678, #Hemopericardium due to and following acute myocardial infarction
      438172, #Atrial septal defect due to and following acute myocardial infarction
      4119953, #Post-infarction ventricular septal defect
      4108679, #Rupture of cardiac wall without hemopericardium as current complication following acute myocardial infarction
      4108219, #Rupture of chordae tendinae due to and following acute myocardial infarction
      4108220, #Rupture of papillary muscle as current complication following acute myocardial infarction
      4108680, #Thrombosis of atrium, auricular appendage, and ventricle due to and following acute myocardial infarction
      4198141, #Post infarct angina
      # Altres
      434376, #Acute myocardial infarction of anterior wall
      438170, #	Acute myocardial infarction of inferior wall
      4176969, #Sequelae of cardiovascular disorders
      37309626, #Myocardial infarction due to demand ischemia
      43020460, #Acute ST segment elevation myocardial infarction involving left anterior descending coronary artery
      46270162, #	Acute ST segment elevation myocardial infarction due to left coronary artery occlusion
      46270163 #Acute ST segment elevation myocardial infarction due to right coronary artery occlusion
    ),
                                             # c(312327, 4108217,
                                             #              438172, 4119953, 4108219, 4108680,
                                             #              4198141),# , 433128, 4329847),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "AMI Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  AMIDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = AMIDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: AMI",
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
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(#I20
      321318, #Angina pectoris
      315296, #Preinfarction syndrome
      4127089), #Coronary artery spasm
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Angor Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(36712983, 40481132, 40482638, 43021857),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  AngorDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = AngorDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Angor",
    ComponentList = list(AngorDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)

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
      443454, #Cerebral infarction
      4110189, #Cerebral infarct due to thrombosis of precerebral arteries
      4110190, #Cerebral infarction due to embolism of precerebral arteries
      4043731, #Infarction - precerebral
      4110192, #Cerebral infarction due to thrombosis of cerebral arteries
      4108356, #Cerebral infarction due to embolism of cerebral arteries
      4111714, #Cerebral infarction due to cerebral venous thrombosis, non-pyogenic
      #I64
      #I65
      43022059, #Disease of non-coronary systemic artery
      4153380, #Disorder of carotid artery
      4159164, #Disorder of basilar artery
      443239, #Precerebral arterial occlusion
      #Altres
      255919, #Finding of head and neck region
      313226, #Carotid artery occlusion
      321887, #Disorder of artery
      381316, #Cerebrovascular accident
      381591, #Cerebrovascular disease
      4006294, #Basilar artery embolism
      4028073, #Disorder of artery of neck
      4213731, #Carotid artery embolism
      4273526, #Vertebral artery thrombosis
      4274969, #Vertebral artery embolism
      4288310, #Carotid artery obstruction
      4311124, #Carotid artery thrombosis
      4338227, #Basilar artery thrombosis
      45767658, #Cerebral infarction due to thrombosis of middle cerebral artery
      45772786, #Cerebral infarction due to embolism of middle cerebral artery
      46270031, #Cerebral infarction due to occlusion of precerebral artery
      46273649 #Cerebral infarction due to occlusion of basilar artery
    ),
      #43530727, 443454),# 255919, 43022059),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Stroke Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(4180169, 442752, 44782470),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  StrokeDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = StrokeDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Stroke",
    ComponentList = list(StrokeDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)
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
      373503, #Transient cerebral ischemia
      437306, #Transient global amnesia
      4338523, #Amaurosis fugax
      381036, #	Multiple AND bilateral precerebral artery stenosis
      4112020, #Carotid artery syndrome hemispheric
      4048785, #Vertebrobasilar territory transient ischemic attack
      #G46
      381591, #Cerebrovascular disease
      4110194, #Middle cerebral artery syndrome
      4108360, #Anterior cerebral artery syndrome
      4110195, #Posterior cerebral artery syndrome
      4111710, #Brainstem stroke syndrome
      4111711, #Cerebellar stroke syndrome
      4045737, #Pure motor lacunar infarction
      4045738, #Pure sensory lacunar infarction
      4046360, #Lacunar infarction
      # Altres: I65 i I66
      # 255919, #Finding of head and neck region
      43022059, #Disease of non-coronary systemic artery
      4153380, #Disorder of carotid artery
      4353709, #Intracerebral vascular finding
      4112023, #Occlusion and stenosis of middle cerebral artery
      4111716, #Occlusion and stenosis of anterior cerebral artery
      4111717, #Occlusion and stenosis of posterior cerebral artery
      4159164, #Disorder of basilar artery
      443239, #Precerebral arterial occlusion
      4112024, #Occlusion and stenosis of cerebellar arteries
      372924, #Cerebral artery occlusion
      4028073, #Disorder of artery of neck
      4288310 #Carotid artery obstruction
    ),# 4353709, 43022059),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "TIA Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  TIADxQuery <- Capr::createConditionOccurrence(conceptSetExpression = TIADx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: TIA",
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

#' Create SQL for Nephropathy from DM extraction for Outcome
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
#' # outcomeInfo <- CreateSQL_nephro(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_nephro <- function(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  # Nephorpathy due to DM
  NephroDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(192279, #Disorder of kidney due to diabetes mellitus
                                                          200687, #Renal disorder due to type 1 diabetes mellitus
                                                          443731, #Renal disorder due to type 2 diabetes mellitus
                                                          43531578 #Chronic kidney disease due to type 2 diabetes mellitus
                                                          ),
      connection = cdm_bbdd,
      vocabularyDatabaseSchema = cdm_schema),
    Name = "Nephropathy due to DM Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(4202383, #Drug-induced diabetes mellitus
                                                          195771 #Secondary diabetes mellitus
                                                          ),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  NephroDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = NephroDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Nephropathy due to DM",
    ComponentList = list(NephroDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 7,
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

#' Create SQL for Retinopathy from DM extraction for Outcome
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
#' # outcomeInfo <- CreateSQL_retino(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_retino <- function(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  # Retinopathy due to DM
  RetinoDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(376114, #Severe nonproliferative retinopathy due to diabetes mellitus
                                                          376979, #Cataract due to diabetes mellitus
                                                          377552, #Moderate nonproliferative retinopathy due to diabetes mellitus
                                                          378743, #Mild nonproliferative retinopathy due to diabetes mellitus
                                                          380096, #	Proliferative retinopathy due to diabetes mellitus
                                                          380097, #	Macular edema due to diabetes mellitus
                                                          443733, #Disorder of eye due to type 2 diabetes mellitus
                                                          443767, #Disorder of eye due to diabetes mellitus
                                                          4174977, #Retinopathy due to diabetes mellitus
                                                          4221495, #Cataract due to diabetes mellitus type 2
                                                          4225656, #Cataract due to diabetes mellitus type 1
                                                          4227210, #Retinopathy due to type 1 diabetes mellitus
                                                          4266637, #Severe nonproliferative retinopathy without macular edema due to diabetes mellitus
                                                          4290822, #Severe nonproliferative retinopathy with clinically significant macular edema due to diabetes mellitus
                                                          4338896, #Traction retinal detachment involving macula
                                                          4338897, #Combined traction and rhegmatogenous retinal detachment
                                                          4338901, #Traction detachment of retina due to diabetes mellitus
                                                          37016179, #Mild nonproliferative retinopathy due to type 1 diabetes mellitus
                                                          37016180, #Moderate nonproliferative retinopathy due to type 1 diabetes mellitus
                                                          42538169, #Disorder of eye due to type 1 diabetes mellitus
                                                          43530656, #Nonproliferative retinopathy due to type 2 diabetes mellitus
                                                          43530685, #Proliferative retinopathy due to type 2 diabetes mellitus
                                                          45757435, #	Mild nonproliferative retinopathy due to type 2 diabetes mellitus
                                                          45763583, #Nonproliferative diabetic retinopathy due to type 1 diabetes mellitus
                                                          45763584, #Proliferative retinopathy due to type 1 diabetes mellitus
                                                          45769873, #Traction detachment of retina due to type 1 diabetes mellitus
                                                          45770830, #Macular edema and retinopathy due to type 2 diabetes mellitus
                                                          45770881, #Moderate nonproliferative retinopathy due to type 2 diabetes mellitus
                                                          45773064, #Traction detachment of retina due to type 2 diabetes mellitus
                                                          35626043, #Severe nonproliferative retinopathy of right eye due to diabetes mellitus
                                                          35626044, #Severe nonproliferative retinopathy of left eye due to diabetes mellitus
                                                          4252356 #O/E - left eye proliferative diabetic retinopathy
    ),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Retinopathy due to DM Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(37016358, #Moderate nonproliferative retinopathy due to secondary diabetes mellitus
                                                          195771 #Secondary diabetes mellitus
    ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  RetinoDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = RetinoDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Retinopathy due to DM",
    ComponentList = list(RetinoDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 8,
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

#' Create SQL for Neuropathy from DM extraction for Outcome
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
#' # outcomeInfo <- CreateSQL_neuro(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_neuro <- function(cdm_bbdd,
                            cdm_schema,
                            results_sc,
                            cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  # Neuropathy due to DM
  NeuroDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(376065, #Disorder of nervous system due to type 2 diabetes mellitus
                                                          376112, #Polyneuropathy due to diabetes mellitus
                                                          377821, #Disorder of nervous system due to type 1 diabetes mellitus
                                                          443730, #Disorder of nervous system due to diabetes mellitus
                                                          4044391, #Neuropathy due to diabetes mellitus
                                                          4048028, #Diabetic mononeuropathy
                                                          4140466, #Lumbosacral radiculoplexus neuropathy due to type 2 diabetes mellitus
                                                          4143857, #Lumbosacral radiculoplexus neuropathy due to type 1 diabetes mellitus
                                                          4175440, #Autonomic neuropathy due to diabetes mellitus
                                                          4191611, #Lumbosacral radiculoplexus neuropathy due to diabetes mellitus
                                                          4222415, #Mononeuropathy due to type 2 diabetes mellitus
                                                          4225055, #Mononeuropathy due to type 1 diabetes mellitus
                                                          37016767, #Autonomic neuropathy due to type 1 diabetes mellitus
                                                          37016768, #Autonomic neuropathy due to type 2 diabetes mellitus
                                                          37017431, #Polyneuropathy due to type 1 diabetes mellitus
                                                          37017432 #Polyneuropathy due to type 2 diabetes mellitus
                                                          ),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Neuropathy due to DM Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(195771 #Secondary diabetes mellitus
    ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  NeuroDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = NeuroDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Neuropathy due to DM",
    ComponentList = list(NeuroDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 9,
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

#' Create SQL for PAD from DM extraction for Outcome
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
#' # outcomeInfo <- CreateSQL_PAD(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_PAD <- function(cdm_bbdd,
                            cdm_schema,
                            results_sc,
                            cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  # Neuropathy due to DM
  PADDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(
      74719, #Ulcer of foot
      134380, #Erythromelalgia
      # 138525, #Pain in limb
      195834, #Atherosclerosis of renal artery
      197304, #Ulcer of lower extremity
      312934, #Atherosclerosis of aorta
      314965, #Embolism and thrombosis of an arm or leg artery
      315558, #Atherosclerosis of arteries of the extremities
      317577, #Arteriosclerotic gangrene
      318443, #Arteriosclerotic vascular disease
      321052, #Peripheral vascular disease
      321882, #Generalized atherosclerosis
      442287, #Rest pain
      442774, #Intermittent claudication
      443358, #Ulcer of heel
      443593, #Ulcer of thigh
      4171556, #Ankle ulcer
      4177703, #Ulcer
      4316222, #Venous intermittent claudication
      4325344, #Pain at rest due to peripheral vascular disease
      35611566, #Bilateral lower limb atherosclerosis pain at rest co-occurrent and due to atherosclerosis
      35615028, #Bilateral atherosclerosis of lower limbs with gangrene
      36712805, #Pain at rest of left lower limb co-occurrent and due to atherosclerosis
      36712806, #Intermittent claudication of right lower limb co-occurrent and due to atherosclerosis
      36712807, #Pain at rest of right lower limb co-occurrent and due to atherosclerosis
      36712963, #Gangrene of left lower limb due to atherosclerosis
      36717006, #Intermittent claudication of bilateral lower limbs co-occurrent and due to atherosclerosis
      36717279, #Gangrene of right lower limb due to atherosclerosis
      36717286, #Intermittent claudication of left lower limb co-occurrent and due to atherosclerosis
      37110250, #Atherosclerosis of artery of lower limb
      37312519, #Ulcer of calf due to atherosclerosis of artery of lower limb
      37312520, #Ischemic foot ulcer due to atherosclerosis of artery of lower limb
      37312524, #Ulcer of ankle due to atherosclerosis of artery of lower limb
      37312529, #Intermittent claudication due to atherosclerosis of artery of limb
      37312531, #Gangrene of limb due to atherosclerosis of artery of limb
      40479625, #Atherosclerosis of artery
      40483538, #Atherosclerosis of bypass graft of limb
      40484541, #Atherosclerosis of autologous vein bypass graft of limb
      40484551, #Atherosclerosis of nonautologous biological bypass graft of limb
      819, #Chronic occlusion of artery of extremity
      46271459 #Atherosclerosis of bypass graft of lower limb
      ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Neuropathy due to DM Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(4070679, #Postthrombotic syndrome
                                                          44782715 #Chronic peripheral venous hypertension with lower extremity complication
    ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  PADDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = PADDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Neuropathy due to DM",
    ComponentList = list(PADDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 10,
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

#' Create SQL for unstable angor extraction for Outcome
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
CreateSQL_angor_unstable <- function(cdm_bbdd,
                            cdm_schema,
                            results_sc,
                            cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  #Angina
  AngorDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(#I20.0
      315296 #Preinfarction syndrome
      ),
      connection = cdm_bbdd,
      vocabularyDatabaseSchema = cdm_schema),
    Name = "Angor Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(36712983, 40481132, 40482638, 43021857),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  AngorDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = AngorDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Angor",
    ComponentList = list(AngorDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)

  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 11,
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

#' Create SQL for AMI extraction for Outcome for WP4-T1DM
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
CreateSQL_AMI_WP4 <- function(cdm_bbdd,
                              cdm_schema,
                              results_sc,
                              cohortTable){
  #################################################################################################
  # Cohort OUTCOME

  #AMI
  AMIDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(#I21
      312327, #Acute myocardial infarction
      4296653, #Acute ST segment elevation myocardial infarction
      45766075, #Acute anterior ST segment elevation myocardial infarction
      45766116, #Acute ST segment elevation myocardial infarction of inferior wall
      4296653, #Acute ST segment elevation myocardial infarction
      4270024, #Acute non-ST segment elevation myocardial infarction
      4329847, #Myocardial infarction
      #I22
      4108217, #Subsequent myocardial infarction
      4108677, #Subsequent myocardial infarction of anterior wall
      4108218, #Subsequent myocardial infarction of inferior wall
      45766241, #Subsequent non-ST segment elevation myocardial infarction
      45766114, #Subsequent ST segment elevation myocardial infarction
      #I23
      4108678, #Hemopericardium due to and following acute myocardial infarction
      438172, #Atrial septal defect due to and following acute myocardial infarction
      4119953, #Post-infarction ventricular septal defect
      4108679, #Rupture of cardiac wall without hemopericardium as current complication following acute myocardial infarction
      4108219, #Rupture of chordae tendinae due to and following acute myocardial infarction
      4108220, #Rupture of papillary muscle as current complication following acute myocardial infarction
      4108680, #Thrombosis of atrium, auricular appendage, and ventricle due to and following acute myocardial infarction
      4198141, #Post infarct angina
      # Altres
      434376, #Acute myocardial infarction of anterior wall
      438170, #	Acute myocardial infarction of inferior wall
      4176969, #Sequelae of cardiovascular disorders
      37309626, #Myocardial infarction due to demand ischemia
      43020460, #Acute ST segment elevation myocardial infarction involving left anterior descending coronary artery
      46270162, #	Acute ST segment elevation myocardial infarction due to left coronary artery occlusion
      46270163, #Acute ST segment elevation myocardial infarction due to right coronary artery occlusion
      #I25
      # 36712983, #Angina co-occurrent and due to coronary arteriosclerosis
      314666, #	Old myocardial infarction
      315286, #Chronic ischemic heart disease
      315296, #Preinfarction syndrome <--------------
      316427, #Aneurysm of coronary vessels
      317576, #Coronary arteriosclerosis
      321318, #Angina pectoris <---------------------
      438168, #	Aneurysm of heart
      443563, #Arteriosclerosis of coronary artery bypass graft
      764123, #Atherosclerosis of coronary artery without angina pectoris
      4110961, #Generalized ischemic myocardial dysfunction
      4124683, #Silent myocardial ischemia
      4127089, #Coronary artery spasm <--------------
      36712779, #Chronic total occlusion of coronary artery
      36712982, #Unstable angina co-occurrent and due to coronary arteriosclerosis
      36712983, #Angina co-occurrent and due to coronary arteriosclerosis
      36714444, #Non-obstructive atherosclerosis of coronary artery
      37115756, #Dissection of coronary artery
      37312532, #Coronary arteriosclerosis in artery of transplanted heart
      40481132, #Arteriosclerosis of coronary artery bypass graft of transplanted heart
      40481919, #Coronary atherosclerosis
      40482638, #Arteriosclerosis of autologous vein coronary artery bypass graft
      40482655, #Arteriosclerosis of nonautologous coronary artery bypass graft
      43020480, #Acquired coronary artery fistula
      43021857, #Arteriosclerosis of autologous arterial coronary artery bypass graft
      43021858 #Arteriosclerosis of autologous coronary artery bypass graft
    ),
    # c(312327, 4108217,
    #              438172, 4119953, 4108219, 4108680,
    #              4198141),# , 433128, 4329847),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "AMI Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  AMIDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = AMIDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: AMI",
    ComponentList = list(AMIDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 12,
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

#' Create SQL for Stroke extraction for Outcome
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
CreateSQL_strokeWP4 <- function(cdm_bbdd,
                                cdm_schema,
                                results_sc,
                                cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  #Ictus
  StrokeDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(
      # I60
      432923, #Subarachnoid hemorrhage
      4108952, #Subarachnoid hemorrhage from carotid siphon and bifurcation
      4111708, #Subarachnoid hemorrhage from vertebral artery
      4148906, #Spontaneous subarachnoid hemorrhage
      # I61
      376713, #Cerebral hemorrhage
      4049659, #Subcortical hemorrhage
      4110185, #Intracerebral hemorrhage, intraventricular
      4110186, #Intracerebral hemorrhage, multiple localized
      4144154, #Non-traumatic intracerebral ventricular hemorrhage
      4176892, #Cortical hemorrhage
      4319328, #Brain stem hemorrhage
      42535424, #Spontaneous hemorrhage of cortical intracerebral hemisphere
      42535425, #Spontaneous hemorrhage of cerebral hemisphere
      42539269, #Spontaneous hemorrhage of brain stem
      43530674, #Spontaneous cerebellar hemorrhage
      43530727, #Spontaneous cerebral hemorrhage
      # I62
      436430, #Nontraumatic extradural hemorrhage
      4111709, #Non-traumatic subdural hemorrhage
      42535426, #Acute nontraumatic subdural hemorrhage
      42538062, #Spontaneous intracranial hemorrhage
      43530727, #Spontaneous cerebral hemorrhage
      43530851, #Chronic non-traumatic intracranial subdural hemorrhage
      #I63
      313226, #Carotid artery occlusion
      321887, #Disorder of artery
      443239, #Precerebral arterial occlusion
      443454, #Cerebral infarction
      4006294, #Basilar artery embolism
      4043731, #Infarction - precerebral
      4108356, #Cerebral infarction due to embolism of cerebral arteries
      4110189, #Cerebral infarct due to thrombosis of precerebral arteries
      4110190, #Cerebral infarction due to embolism of precerebral arteries
      4110192, #Cerebral infarction due to thrombosis of cerebral arteries
      4111714, #Cerebral infarction due to cerebral venous thrombosis, non-pyogenic
      4213731, #Carotid artery embolism
      4273526, #Vertebral artery thrombosis
      4274969, #Vertebral artery embolism
      4288310, #Carotid artery obstruction
      4311124, #Carotid artery thrombosis
      4338227, #Basilar artery thrombosis
      45767658, #Cerebral infarction due to thrombosis of middle cerebral artery
      45772786, #Cerebral infarction due to embolism of middle cerebral artery
      46270031, #Cerebral infarction due to occlusion of precerebral artery
      46273649, #Cerebral infarction due to occlusion of basilar artery
      #G45
      373503, #Transient cerebral ischemia
      381036, #Multiple AND bilateral precerebral artery stenosis
      437306, #Transient global amnesia
      4048785, #Vertebrobasilar territory transient ischemic attack
      4112020, #Carotid artery syndrome hemispheric
      4338523 #Amaurosis fugax
      ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Stroke Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(4180169, 442752, 44782470),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  StrokeDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = StrokeDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Stroke",
    ComponentList = list(StrokeDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 13,
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

#' Create SQL for Stroke extraction for Outcome
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
CreateSQL_neuroWP4 <- function(cdm_bbdd,
                               cdm_schema,
                               results_sc,
                               cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  # Neuro
  NeuroDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(
      # E10.4
      377821, # Disorder of nervous system due to type 1 diabetes mellitus
      4143857, # Lumbosacral radiculoplexus neuropathy due to type 1 diabetes mellitus
      4225055, # Mononeuropathy due to type 1 diabetes mellitus
      37016767, # Autonomic neuropathy due to type 1 diabetes mellitus
      37017431 # Polyneuropathy due to type 1 diabetes mellitus
    ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Stroke Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(4180169, 442752, 44782470),
                                           connection = cdm_bbdd,
                                           vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  NeuroDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = NeuroDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Neuro WP4",
    ComponentList = list(NeuroDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 14,
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

#' Create SQL for Nephropathy from DM1 for WP4 extraction for Outcome
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
#' # outcomeInfo <- CreateSQL_nephro(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_nephroWP4 <- function(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  # Nephorpathy due to DM
  NephroDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(200687 #Renal disorder due to type 1 diabetes mellitus
    ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Nephropathy due to DM Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(4202383, #Drug-induced diabetes mellitus
                                                          195771 #Secondary diabetes mellitus
    ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  NephroDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = NephroDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Nephropathy due to DM",
    ComponentList = list(NephroDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 15,
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

#' Create SQL for Retinopathy from DM1 for WP4 extraction for Outcome
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
#' # outcomeInfo <- CreateSQL_retino(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_retinoWP4 <- function(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  # Retinopathy due to DM
  RetinoDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(
      376114, #Severe nonproliferative retinopathy due to diabetes mellitus
      380097, #	Macular edema due to diabetes mellitus
      4225656, #Cataract due to diabetes mellitus type 1
      4227210, #Retinopathy due to type 1 diabetes mellitus
      4338896, #Traction retinal detachment involving macula
      4338897, #Combined traction and rhegmatogenous retinal detachment
      37016179, #Mild nonproliferative retinopathy due to type 1 diabetes mellitus
      37016180, #Moderate nonproliferative retinopathy due to type 1 diabetes mellitus
      42538169, #Disorder of eye due to type 1 diabetes mellitus
      45763583, #Nonproliferative diabetic retinopathy due to type 1 diabetes mellitus
      45763584, #Proliferative retinopathy due to type 1 diabetes mellitus
      45769873 #Traction detachment of retina due to type 1 diabetes mellitus
      ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Retinopathy due to DM Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(37016358, #Moderate nonproliferative retinopathy due to secondary diabetes mellitus
                                                          195771 #Secondary diabetes mellitus
    ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  RetinoDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = RetinoDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Retinopathy due to DM",
    ComponentList = list(RetinoDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 16,
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

#' Create SQL for Diabetic foot from DM1 for WP4 extraction for Outcome
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
#' # outcomeInfo <- CreateSQL_retino(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_footWP4 <- function(cdm_bbdd,
                                cdm_schema,
                                results_sc,
                                cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  # Retinopathy due to DM
  FootDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(
      45770902 # Ulcer of lower limb due to type 1 diabetes mellitus
    ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Retinopathy due to DM Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(37016358, #Moderate nonproliferative retinopathy due to secondary diabetes mellitus
                                                          195771 #Secondary diabetes mellitus
    ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  FootDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = FootDx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: Diabetic Foot due to DM",
    ComponentList = list(FootDxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 17,
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

#' Create SQL for Diabetic ketoacidosis from DM1 for WP4 extraction for Outcome
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
#' # outcomeInfo <- CreateSQL_retino(cdm_bbdd, cdm_schema, results_sc, cohortTable)
CreateSQL_DKAWP4 <- function(cdm_bbdd,
                              cdm_schema,
                              results_sc,
                              cohortTable){
  #################################################################################################
  # Cohort OUTCOME
  # Retinopathy due to DM
  DKADx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(
      439770, #Ketoacidosis due to type 1 diabetes mellitus
      4009303, #Diabetic ketoacidosis without coma
      4224254 #Ketoacidotic coma due to type 1 diabetes mellitus
    ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Retinopathy due to DM Diagnosis",
    includeDescendants = FALSE)
  CodisForaDx <- Capr::createConceptSetExpression(
    conceptSet = Capr::getConceptIdDetails(conceptIds = c(37016358, #Moderate nonproliferative retinopathy due to secondary diabetes mellitus
                                                          195771 #Secondary diabetes mellitus
    ),
    connection = cdm_bbdd,
    vocabularyDatabaseSchema = cdm_schema),
    Name = "Codis Fora Diagnosis",
    includeDescendants = FALSE)

  #################################################################################################
  # Building Queries
  DKADxQuery <- Capr::createConditionOccurrence(conceptSetExpression = DKADx)
  CodisForaDxQuery <- Capr::createConditionOccurrence(conceptSetExpression = CodisForaDx)

  #################################################################################################
  # Creating the Initial Cohort Entry
  OutcomePrimaryCriteria <- Capr::createPrimaryCriteria(
    Name = "Outcome: DKA due to DM",
    ComponentList = list(DKADxQuery),
    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L,
                                                      PostDays = 0L),
    Limit = "All")

  tlprev <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 0L,
                                                                  StartCoeff = "Before",
                                                                  EndDays = 0L,
                                                                  EndCoeff = "After"))
  # No T2Dx at any point in patient history
  noCodisForaDxCount <- Capr::createCount(Query = CodisForaDxQuery,
                                          Logic = "exactly",
                                          Count = 0L,
                                          Timeline = tlprev)
  noCodisForaDxGroup <- Capr::createGroup(Name = "No altres codis",
                                          type = "ALL",
                                          criteriaList = list(noCodisForaDxCount))
  InclusionRules <- Capr::createInclusionRules(Name = "Inclusion Rules",
                                               Contents = list(noCodisForaDxGroup),
                                               Limit = "First")

  OUTCOME <- Capr::createCohortDefinition(Name = "OUTCOME",
                                          PrimaryCriteria = OutcomePrimaryCriteria,
                                          InclusionRules = InclusionRules)
  # JSON
  OUTCOMEJson <- Capr::compileCohortDefinition(OUTCOME)

  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                         cohortId = 18,
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

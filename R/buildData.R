#' Build a Covariate Data for cohort
#'
#' @param cdm_bbdd A connection for a OMOP database via DatabaseConnector
#' @param cdm_schema A name for OMOP schema
#' @param results_sc A name for result schema
#' @param cohortTable A name of the result cohort
#' @param acohortId A Cohort number
#'
#' @return A covariateData object
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Not yet
buildData <- function(cdm_bbdd,
                      cdm_schema,
                      results_sc,
                      cohortTable,
                      acohortId = 1){
  covDemo <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAge = TRUE)

  BMI_conceptId <- c(3038553, 40762638) #LOINC
  height_conceptId <- c(3036277, 3015514) #LOINC
  weight_conceptId <- c(3025315) #LOINC
  covMeasValueAny <- FeatureExtraction::createCovariateSettings(
    useMeasurementValueAnyTimePrior = TRUE,
    includedCovariateConceptIds = c(BMI_conceptId,
                                    height_conceptId,
                                    weight_conceptId),
    addDescendantsToInclude = TRUE)

  SBP_conceptId <- c(3004249, #LOINC
                     4152194) #SNOMED
  DBP_conceptId <- c(3012888, #LOINC
                     4154790) #SNOMED
  cT_conceptId <- c(3019900, 3027114) #LOINC
  cHDL_conceptId <- c(3011884, 3007070, 3023602) #LOINC
  cLDL_conceptId <- c(3028437, 3001308) #LOINC
  cVLDL_conceptId <- c(3022487) #LOINC
  Tg_conceptId <- c(3022192, 42868692)  #LOINC
  glu_conceptId <- c(3004501,
                     46235168, 40757523, 3005834, 40757527, 3016567, 40757528, 40757529,
                     40757627, 40757628,
                     3018251, 46236948,
                     3015024, 3036895, 3001022, 3016701, 3018582, 3008799, 3045700,
                     3020491) #LOINC
  alt_conceptId <- c(3006923, 46235106)
  CRP_conceptId <- c(3020460) #LOINC
  ferritin_conceptId <- c(3001122) #LOINC
  WBC_conceptId <- c(3010813) #LOINC
  neutrophils_conceptId <- c(3017732, 3046321, 43055364) #LOINC
  basophils_conceptId <- c(3006315, 43055368) #LOINC
  eosinophils_conceptId <- c(3013115, 43055367) #LOINC
  monocytes_conceptId <- c(3001604, 43055365) #LOINC
  lymphocytes_conceptId <- c(3019198, 43055366) #LOINC
  HbA1c_conceptId <- c(3034639, 3004410) #LOINC
  creatinine_conceptId <- c(3016723)
  covMeasValueLong <- FeatureExtraction::createCovariateSettings(
    useMeasurementValueLongTerm = TRUE,
    longTermStartDays = 2*(-365.25),
    endDays = 0,
    includedCovariateConceptIds = c(
      SBP_conceptId,
      DBP_conceptId,
      cT_conceptId,
      cHDL_conceptId,
      cLDL_conceptId,
      cVLDL_conceptId,
      Tg_conceptId,
      glu_conceptId,
      alt_conceptId,
      CRP_conceptId,
      ferritin_conceptId,
      WBC_conceptId,
      neutrophils_conceptId,
      basophils_conceptId,
      eosinophils_conceptId,
      monocytes_conceptId,
      lymphocytes_conceptId,
      HbA1c_conceptId,
      creatinine_conceptId),
    addDescendantsToInclude = TRUE)

  T2DM_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 111,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 111,
                      analysisName = "T2DM",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(201820, 442793, 443238),
    addDescendantsToInclude = TRUE,
    excludedCovariateConceptIds = c(201254, 435216, 4058243, 40484648,195771, 761051),
    addDescendantsToExclude = TRUE)

  obesity_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 112,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 112,
                      analysisName = "Obesity",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(433736),
    addDescendantsToInclude = TRUE)

  angor_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 113,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 113,
                      analysisName = "Angina pectoris",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(321318, 315296, 4127089),
    addDescendantsToInclude = FALSE)

  ami_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 114,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 114,
                      analysisName = "AMI",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(#I21
      312327, 4296653, 45766075, 45766116, 4296653, 4270024, 4329847,
      #I22
      4108217, 4108677, 4108218, 45766241, 45766114, 45766114,
      #I23
      4329847, 4108678, 438172, 4119953, 4108679, 4108219, 4108220, 4108680,
      4198141),
    addDescendantsToInclude = FALSE)

  stroke_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 115,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 115,
                      analysisName = "Stroke",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(
      #I63
      443454, 4110189, 4110190, 4043731, 4110192, 4108356, 443454, 4111714,
      #I64
      #I65
      43022059, 4153380, 4159164, 443239),
    addDescendantsToInclude = FALSE)

  TIA_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 116,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 116,
                      analysisName = "TIA",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(#G45
      373503,
      437306, 4338523, 381036, 4112020, 4048785,
      #G46
      381591, 4110194, 4108360, 4110195, 4111710, 4111711, 4045737, 4045738, 4046360),
    addDescendantsToInclude = FALSE)

  COPD_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 117,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 117,
                      analysisName = "COPD",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(255841, 261325, 255573),
    addDescendantsToInclude = TRUE)

  CKD_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 118,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 118,
                      analysisName = "CKD",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(46271022, 192359),
    addDescendantsToInclude = TRUE)

  cancer_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 119,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 119,
                      analysisName = "Cancer",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(443392, 4144289, 439392, 200962, 139750, 4311499, 137809, 197500),
    addDescendantsToInclude = TRUE)

  depress_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 120,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 120,
                      analysisName = "Depress",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(4282096, 4282316, 433440),
    addDescendantsToInclude = TRUE)

  htn_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 121,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 121,
                      analysisName = "HTN",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(320128, 442604, 444101, 319034, 443919, 44782429, 44784621,
                                    439696, 319826, 317895, 443771, 4110948, 319826),
    addDescendantsToInclude = TRUE)

  hf_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 122,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 122,
                      analysisName = "HF",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(316139, 319835, 439846, 443580, 443587, 4229440, 4273632, 40479192,
                                    40479576, 40480602, 40480603, 40481042, 40481043, 40482727, 44782718,
                                    44782733),
    addDescendantsToInclude = TRUE)

  liver_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 123,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 123,
                      analysisName = "LiverFailure",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(4098652, 197795, 4211974, 4012113,
                                    192240, 192242, 193693, 196625, 197490, 197494, 198683,
                                    198964, 439673, 439674, 439675,
                                    4245975, 200763, 4267417, 194990, 194984,
                                    192675, 192680, 196455, 199867, 200762,
                                    201901, 377604, 4026125, 4046123, 4058696, 4059290, 4064161,
                                    4135822, 4238978, 4240725, 4313846, 4340390,
                                    4340394, 4340941, 4340948, 40484532, 46269836),
    addDescendantsToInclude = TRUE)

  ra_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 124,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 124,
                      analysisName = "RA",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(36684997, 80809), #M05, M06
    addDescendantsToInclude = TRUE)

  sleep_apnea_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 125,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 125,
                      analysisName = "SleepApnea",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(313459), #G47.3
    addDescendantsToInclude = TRUE)

  pcos_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 126,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 126,
                      analysisName = "PCOS",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(40443308), #E28.2
    addDescendantsToInclude = TRUE)

  T1DM_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 127,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 127,
                      analysisName = "T1DM",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(201254, 435216, 40484648, 40484649),
    addDescendantsToInclude = TRUE)

  A10_conceptId <- c(21600712,
                     782681, 793321, 1502829, 1502830, 1503327, 1525221, 1529352,
                     1547554, 1596977, 1597761, 1597772, 1597773, 1597781, 1597792,
                     19006931, 19021312, 19023424, 19023425, 19023426, 19029030, 19029061,
                     19058398, 19059800, 19077638, 19077682, 19078552, 19078559, 19079293,
                     19079465, 19095211, 19095212, 19099055, 19101729, 19112791, 19125041,
                     19125045, 19125049, 19129179, 19133793, 19135264, 21022404, 21036596,
                     21061594, 21061613, 21076306, 21081251, 21086042, 21100924, 21114195,
                     21133671, 21169719, 35408233, 35410536, 35412102, 35412890, 35412958,
                     36403507, 36403509, 36884964, 40044221, 40051377, 40052768, 40054707,
                     40139098, 40164885, 40164888, 40164891, 40164897, 40164913, 40164916,
                     40164942, 40164943, 40164946, 40166037, 40166041, 40239218, 42479624,
                     42479783, 42481504, 42481541, 42482012, 42482588, 42656231, 42656236,
                     42656240, 42708086, 42708090, 42899447, 42902356, 42902587, 42902742,
                     42902821, 42902945, 42902992, 42903059, 42903341, 43013885, 43013905,
                     43013911, 43013918, 43013924, 43013928, 43526467, 43526471, 44032735,
                     44058584, 44123708, 44785831, 45774709, 45774754, 45774893, 46233969,
                     46234047, 46234234, 46234237, 46287408, 46287689)

  sel_med_conceptId <- c(21600712, #DRUGS USED IN DIABETES
                         #Aquestes insulines no les troba
                         21076306, 44058584, 21086042, 21036596,
                         21601238, #C01
                         21600381, #C02
                         21601461, #C03
                         21601664, #C07
                         21601744, #C08
                         21601782, #C09
                         21601853, #C10
                         21603933 #M01A
  )
  covDrug <- FeatureExtraction::createCovariateSettings(
    useDrugGroupEraMediumTerm = TRUE,
    mediumTermStartDays = -365.25,
    endDays = 0,
    includedCovariateConceptIds = sel_med_conceptId,
    addDescendantsToInclude = TRUE)

  # smoking_vars <- FeatureExtraction::createAnalysisDetails(
  #   analysisId = 810,
  #   sqlFileName = "DomainConcept.sql",
  #   parameters = list(analysisId = 810,
  #                     analysisName = "Tobacco",
  #                     startDay = "anyTimePrior",
  #                     endDay = 1,
  #                     subType = "last",
  #                     domainId = "Observation",
  #                     domainTable = "observation",
  #                     domainConceptId = "VALUE_AS_CONCEPT_ID",
  #                     domainStartDate = "observation_date",
  #                     domainEndDate = ""),
  #   includedCovariateConceptIds = c(45879404, 45884037, 45883458),
  #   addDescendantsToInclude = TRUE)

  SmokingCovSet <- createSmokingCovariateSettings(useSmoking = TRUE)

  T2DM_TimeCovSet <- createT2DM_TimeCovariateSettings(useT2DM_Time = TRUE)
  T1DM_TimeCovSet <- createT1DM_TimeCovariateSettings(useT1DM_Time = TRUE)

  covariateSettings <- list(covDemo,
                            covMeasValueAny,
                            covMeasValueLong,
                            FeatureExtraction::createDetailedCovariateSettings(
                              list(T2DM_vars,
                                   obesity_vars,
                                   angor_vars,
                                   ami_vars,
                                   stroke_vars,
                                   TIA_vars,
                                   COPD_vars,
                                   CKD_vars,
                                   cancer_vars,
                                   depress_vars,
                                   htn_vars,
                                   hf_vars,
                                   liver_vars,
                                   ra_vars,
                                   sleep_apnea_vars,
                                   pcos_vars,
                                   T1DM_vars)),
                            covDrug,
                            SmokingCovSet,
                            T2DM_TimeCovSet,
                            T1DM_TimeCovSet)

  covariateData <- FeatureExtraction::getDbCovariateData(
    connection = cdm_bbdd,
    cdmDatabaseSchema = cdm_schema,
    cohortDatabaseSchema = results_sc,
    cohortTable = cohortTable,
    cohortId = acohortId,
    rowIdField = "subject_id",
    covariateSettings = covariateSettings)

  T2DM_conceptId <- c(201530111, 201826111, 376065111, 443729111, 443731111, 443733111,
                      4193704111, 4196141111, 4221495111, 36714116111, 37016349111,
                      37017432111, 43530685111, 43530690111, 43531563111, 43531578111,
                      45770830111,
                      # Afegits executant el SIDIAP
                      4099651111, 45757363111, 4140466111, 37016768111, 4222876111, 43531616111,
                      45770881111)
  DM_conceptId <- c(442793111, 321822111, 443730111, 192279111, 4048028111, 4226798111,
                    201820111, 4008576111, 443767111,
                    # Afegits executant el SIDIAP
                    4044391111, 4009303111, 376112111, 4159742111, 4114427111, 4131908111)
  obesity_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 112),
    .data$covariateId)
  angina_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 113),
    .data$covariateId)
  ami_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 114),
    .data$covariateId)
  stroke_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 115),
    .data$covariateId)
  tia_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 116),
    .data$covariateId)
  COPD_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 117),
    .data$covariateId)
  CKD_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 118),
    .data$covariateId)
  cancer_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 119),
    .data$covariateId)
  depress_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 120),
    .data$covariateId)
  htn_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 121),
    .data$covariateId)
  hf_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 122),
    .data$covariateId)
  liver_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 123),
    .data$covariateId)
  ra_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 124),
    .data$covariateId)
  sleep_apnea_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 125),
    .data$covariateId)
  pcos_conceptId <- dplyr::pull(
    dplyr::filter(covariateData$covariateRef,
                  .data$analysisId == 126),
    .data$covariateId)

  # covariateData$covariates <- dplyr::mutate(
  #   .data = covariateData$covariates,
  #   covariateId = dplyr::if_else(.data$covariateId %in% T2DM_conceptId, 201826111, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% DM_conceptId, 201820111, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% obesity_conceptId, 433736112, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% angina_conceptId, 321318113, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% ami_conceptId, 312327114, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% stroke_conceptId, 443454115, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% tia_conceptId, 373503116, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% COPD_conceptId, 255841117, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% CKD_conceptId, 46271022118, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% cancer_conceptId, 443392119, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% depress_conceptId, 4282096120, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% htn_conceptId, 320128121, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% hf_conceptId, 316139122, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% liver_conceptId, 194984123, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% ra_conceptId, 80809124, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% sleep_apnea_conceptId, 313459125, .data$covariateId),
  #   covariateId = dplyr::if_else(.data$covariateId %in% pcos_conceptId, 40443308126, .data$covariateId))
  # covariateData$covariates <- dplyr::distinct(covariateData$covariates)
  return(covariateData)
}

#' Transform covariateData object into FlatTable
#'
#' Si volem agafar l'últim valor podem canviar-ho
#'
#' @param covariateData A covariateDate object
#'
#' @return A data.table with the covariate data
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' #Not yet
transformToFlat <- function(covariateData){
  bbdd_covar <- dplyr::collect(covariateData$covariates)
  bbdd_covar <-  dplyr::mutate(
    .data = bbdd_covar,
    variable = as.character(NA),
    variable = dplyr::if_else(.data$covariateId == 1002, 'age', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 8507001, 'sex_male', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 8532001, 'sex_female', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 111, 'T2DM', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 112, 'obesity', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 113, 'angor', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 114, 'ami', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 115, 'stroke', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 116, 'tia', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 117, 'COPD', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 118, 'CKD', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 119, 'cancer', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 120, 'depress', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 121, 'htn', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 122, 'hf', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 123, 'liver', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 124, 'ra', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 125, 'sleep_apnea', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 126, 'pcos', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 127, 'T1DM', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21600712411, 'A10', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21600713411, 'A10A', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21600744411, 'A10B', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21601238411, 'C01', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21600381411, 'C02', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21601461411, 'C03', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21601664411, 'C07', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21601744411, 'C08', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21601782411, 'C09', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21601853411, 'C10', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21603933411, 'M01A', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 45879404, 'Never', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 45884037, 'Current', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 45883458, 'Former', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3038553531705, 'BMI', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3036277582705, 'height', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3025315529705, 'weight', .data$variable),
    variable = dplyr::if_else(.data$covariateId %in% c(3004249323706, 4152194876706), 'SBP', .data$variable),
    variable = dplyr::if_else(.data$covariateId %in% c(3012888323706, 4154790876706), 'DBP', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3010813848706, 'Leukocytes', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3001604554706, 'Monocytes', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3034639554706, 'HbA1c', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3027114840706, 'cT', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3011884840706, 'cHDL', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3028437840706, 'cLDL', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3022192840706, 'Tg', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3004501840706, 'Glucose', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3006923645706, 'ALT', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3020460751706, 'CRP', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3001122748706, 'Ferritin', .data$variable),
    variable = dplyr::if_else(substr(.data$covariateId, 1, 7) == 3016723, 'Creatinine', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 201820211, 'TimeT2DM', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 201254212, 'TimeT1DM', .data$variable))
  bbdd_covar <- dplyr::group_by(.data = bbdd_covar, .data$rowId, .data$variable)
  bbdd_covar <- dplyr::summarise(
    .data = bbdd_covar,
    covariateValue = mean(.data$covariateValue),
    .groups = 'keep')
  bbdd_covar <- dplyr::ungroup(x = bbdd_covar)
  bbdd_covar <- tidyr::pivot_wider(
    data = bbdd_covar,
    id_cols = 'rowId',
    names_from = 'variable',
    values_from = 'covariateValue')
  bbdd_covar <- dplyr::mutate(
    .data = bbdd_covar,
    dplyr::across(dplyr::any_of(c('sex_female', 'sex_male', 'T2DM', 'obesity', 'angor', 'tia', 'stroke',
                                  'ami', 'Current', 'Former', 'Never',
                                  'A10', 'A10A', 'A10B','C01', 'C02', 'C03', 'C07', 'C08', 'C09', 'C10',
                                  'M01A',
                                  'COPD', 'CKD', 'cancer', 'depress', 'htn', 'hf', 'liver', 'ra',
                                  'sleep_apnea', 'pcos')),
                  ~ tidyr::replace_na(.x, 0)))
    # sex_female = dplyr::if_else(is.na(sex_female), 0, sex_female),
    # sex_male = dplyr::if_else(is.na(sex_male), 0, sex_male),
    # T2DM = dplyr::if_else(is.na(T2DM), 0, T2DM),
    # obesity = dplyr::if_else(is.na(obesity), 0, obesity),
    # angor = dplyr::if_else(is.na(angor), 0, angor),
    # tia = dplyr::if_else(is.na(tia), 0, tia),
    # stroke = dplyr::if_else(is.na(stroke), 0, stroke),
    # ami = dplyr::if_else(is.na(ami), 0, ami),
    # Current = dplyr::if_else(is.na(Current), 0, Current),
    # Former = dplyr::if_else(is.na(Former), 0, Former))
  return(bbdd_covar)
}

#' Auxiliar function to create Smoking status
#'
#' @param useSmoking Logical valor
#'
#' @return covariateSettings object
#' @export
#'
#' @examples
#' #Not yet
createSmokingCovariateSettings <- function(useSmoking = TRUE){
  covariateSettings <- list(useSmoking = useSmoking)
  attr(covariateSettings, "fun") <- "getDbSmokingCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

#' Auxiliar function to create Smokins status SQL implementation
#'
#' @param connection A connection for a OMOP database via DatabaseConnector
#' @param oracleTempSchema Only for Oracle Database
#' @param cdmDatabaseSchema A name for OMOP schema
#' @param cohortTable A name of the result cohort
#' @param cohortId A Cohort number
#' @param cdmVersion CDM version
#' @param rowIdField Column with the subject identification
#' @param covariateSettings covariateSettings object generetad via FeatureExtraction
#' @param aggregated Logical value
#'
#' @return Function to use with FeatureExtraction to build covariateData
#' @export
#'
#' @examples
#' #Not yet
getDbSmokingCovariateData <- function(connection,
                                      oracleTempSchema = NULL,
                                      cdmDatabaseSchema,
                                      cohortTable = "#cohort_person",
                                      cohortId = -1,
                                      cdmVersion = "5",
                                      rowIdField = "subject_id",
                                      covariateSettings,
                                      aggregated = FALSE){
  writeLines("Constructing Smoking covariates")
  if (covariateSettings$useSmoking == FALSE) {
    return(NULL)
  }

  # Some SQL to construct the covariate:
  sql <- "SELECT DISTINCT ON (obs2.person_id)
                 obs2.person_id AS row_id,
                 obs2.value_as_concept_id AS covariate_id,
                 1 AS covariate_value
          FROM (SELECT *
                FROM @cdm_database_schema.OBSERVATION obs
                INNER JOIN @cohort_table cohort
                      ON cohort.subject_id = obs.person_id
                WHERE obs.observation_concept_id = 43054909 AND
                      obs.observation_date <= cohort.cohort_start_date AND
                      cohort.cohort_definition_id IN (@cohort_definition_id)) obs2
          ORDER BY obs2.person_id, obs2.observation_date desc"
  sql <- SqlRender::render(sql,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_table = cohortTable, # ha de ser results_sc.cohortTable
                           cohort_definition_id = cohortId)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = c(45879404, 45884037, 45883458),
                             covariateName = c('Never smoker', 'Current some day smoker',
                                               'Former smoker'),
                             analysisId = 500,
                             conceptId = rep(43054909, 3))
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 500,
                            analysisName = "Smoking status",
                            domainId = "Observation",
                            startDay = NA,
                            endDay = 0,
                            isBinary = "Y",
                            missingMeansZero = "Y")
  # Construct analysis reference:
  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"
  return(result)
}

#' Auxiliar function to create Time form T2DM diagnosis
#'
#' @param useT2DM_Time Logical valor
#'
#' @return covariateSettings object
#' @export
#'
#' @examples
#' #Not yet
createT2DM_TimeCovariateSettings <- function(useT2DM_Time = TRUE){
  covariateSettings <- list(useT2DM_Time = useT2DM_Time)
  attr(covariateSettings, "fun") <- "getDbuseT2DM_TimeCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

#' Auxiliar function to create Time from T2DM diagnosis status SQL implementation
#'
#' @param connection A connection for a OMOP database via DatabaseConnector
#' @param oracleTempSchema Only for Oracle Database
#' @param cdmDatabaseSchema A name for OMOP schema
#' @param cohortTable A name of the result cohort
#' @param cohortId A Cohort number
#' @param cdmVersion CDM version
#' @param rowIdField Column with the subject identification
#' @param covariateSettings covariateSettings object generetad via FeatureExtraction
#' @param aggregated Logical value
#'
#' @return Function to use with FeatureExtraction to build covariateData
#' @export
#'
#' @examples
#' #Not yet
getDbuseT2DM_TimeCovariateData <- function(connection,
                                           oracleTempSchema = NULL,
                                           cdmDatabaseSchema,
                                           cohortTable = "#cohort_person",
                                           cohortId = -1,
                                           cdmVersion = "5",
                                           rowIdField = "subject_id",
                                           covariateSettings,
                                           aggregated = FALSE){
  writeLines("Constructing T2DM_Time covariates")
  if (covariateSettings$useT2DM_Time == FALSE) {
    return(NULL)
  }

  included_sql <- SqlRender::render(sql = "SELECT *
                                           FROM @schema_cdm.CONCEPT_ANCESTOR
                                           WHERE ancestor_concept_id IN (@diab_id)",
                                    schema_cdm = cdmDatabaseSchema,
                                    diab_id = c(201820, 442793, 443238))
  included_id <- DatabaseConnector::querySql(connection,
                                             sql = included_sql)
  excluded_sql <- SqlRender::render(sql = "SELECT *
                                           FROM @schema_cdm.CONCEPT_ANCESTOR
                                           WHERE ancestor_concept_id IN (@diab_id)",
                                    schema_cdm = cdmDatabaseSchema,
                                    diab_id = c(201254, 435216, 4058243, 40484648,195771, 761051))
  excluded_id <- DatabaseConnector::querySql(connection,
                                             sql = excluded_sql)
  # Some SQL to construct the covariate:
  sql <- "SELECT DISTINCT ON (cond2.person_id)
                 cond2.person_id AS row_id,
                 201820211  AS covariate_id,
                 DATEDIFF(DAY, cond2.condition_start_date, cond2.cohort_start_date) AS covariate_value
          FROM (SELECT *
                FROM @cdm_database_schema.CONDITION_OCCURRENCE cond
                INNER JOIN @cohort_table cohort
                      ON cohort.subject_id = cond.person_id
                WHERE cond.condition_start_date <= DATEADD(DAY, 0, cohort.cohort_start_date)
                  AND cond.condition_concept_id != 0
                  AND cond.condition_concept_id NOT IN (@excluded_concept_table)
                  AND cond.condition_concept_id IN (@included_concept_table)
                  AND cohort.cohort_definition_id IN (@cohort_definition_id)) cond2
          ORDER BY cond2.person_id, cond2.condition_start_date"
  sql <- SqlRender::render(sql,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_table = cohortTable, # ha de ser results_sc.cohortTable
                           cohort_definition_id = cohortId,
                           excluded_concept_table = excluded_id$DESCENDANT_CONCEPT_ID,
                           included_concept_table = included_id$DESCENDANT_CONCEPT_ID)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = c(201820211),
                             covariateName = c('Time from T2DM'),
                             analysisId = 211,
                             conceptId = 201820)
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 211,
                            analysisName = "Time from T2DM",
                            domainId = "Condition",
                            startDay = NA,
                            endDay = 0,
                            isBinary = "N",
                            missingMeansZero = "N")
  # Construct analysis reference:
  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"
  return(result)
}

#' Auxiliar function to create Time form T1DM diagnosis
#'
#' @param useT1DM_Time Logical valor
#'
#' @return covariateSettings object
#' @export
#'
#' @examples
#' #Not yet
createT1DM_TimeCovariateSettings <- function(useT1DM_Time = TRUE){
  covariateSettings <- list(useT1DM_Time = useT1DM_Time)
  attr(covariateSettings, "fun") <- "getDbuseT1DM_TimeCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

#' Auxiliar function to create Time from T1DM diagnosis status SQL implementation
#'
#' @param connection A connection for a OMOP database via DatabaseConnector
#' @param oracleTempSchema Only for Oracle Database
#' @param cdmDatabaseSchema A name for OMOP schema
#' @param cohortTable A name of the result cohort
#' @param cohortId A Cohort number
#' @param cdmVersion CDM version
#' @param rowIdField Column with the subject identification
#' @param covariateSettings covariateSettings object generetad via FeatureExtraction
#' @param aggregated Logical value
#'
#' @return Function to use with FeatureExtraction to build covariateData
#' @export
#'
#' @examples
#' #Not yet
getDbuseT1DM_TimeCovariateData <- function(connection,
                                           oracleTempSchema = NULL,
                                           cdmDatabaseSchema,
                                           cohortTable = "#cohort_person",
                                           cohortId = -1,
                                           cdmVersion = "5",
                                           rowIdField = "subject_id",
                                           covariateSettings,
                                           aggregated = FALSE){
  writeLines("Constructing T1DM_Time covariates")
  if (covariateSettings$useT1DM_Time == FALSE) {
    return(NULL)
  }

  included_sql <- SqlRender::render(sql = "SELECT *
                                           FROM @schema_cdm.CONCEPT_ANCESTOR
                                           WHERE ancestor_concept_id IN (@diab_id)",
                                    schema_cdm = cdmDatabaseSchema,
                                    diab_id = c(201254, 435216, 40484648, 40484649))
  included_id <- DatabaseConnector::querySql(connection,
                                             sql = included_sql)
  # Some SQL to construct the covariate:
  sql <- "SELECT DISTINCT ON (cond2.person_id)
                 cond2.person_id AS row_id,
                 201254212  AS covariate_id,
                 DATEDIFF(DAY, cond2.condition_start_date, cond2.cohort_start_date) AS covariate_value
          FROM (SELECT *
                FROM @cdm_database_schema.CONDITION_OCCURRENCE cond
                INNER JOIN @cohort_table cohort
                      ON cohort.subject_id = cond.person_id
                WHERE cond.condition_start_date <= DATEADD(DAY, 0, cohort.cohort_start_date)
                  AND cond.condition_concept_id != 0
                  AND cond.condition_concept_id IN (@included_concept_table)
                  AND cohort.cohort_definition_id IN (@cohort_definition_id)) cond2
          ORDER BY cond2.person_id, cond2.condition_start_date"
  sql <- SqlRender::render(sql,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_table = cohortTable, # ha de ser results_sc.cohortTable
                           cohort_definition_id = cohortId,
                           included_concept_table = included_id$DESCENDANT_CONCEPT_ID)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = c(201254212),
                             covariateName = c('Time from T1DM'),
                             analysisId = 212,
                             conceptId = 201254)
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 212,
                            analysisName = "Time from T1DM",
                            domainId = "Condition",
                            startDay = NA,
                            endDay = 0,
                            isBinary = "N",
                            missingMeansZero = "N")
  # Construct analysis reference:
  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"
  return(result)
}

#' Build a Follow-up Data for cohort
#'
#' @param cdm_bbdd A connection for a OMOP database via DatabaseConnector
#' @param cdm_schema A name for OMOP schema
#' @param results_sc A name for result schema
#' @param cohortTable A name of the result cohort
#' @param acohortId A Cohort number
#' @param bbdd_covar A data.table create by transformToFlat function
#'
#' @return A data.frame object
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Not yet
buildFollowUp <- function(cdm_bbdd,
                          cdm_schema,
                          results_sc,
                          cohortTable,
                          acohortId = 1,
                          bbdd_covar){
  obs_per_sql <- "SELECT * FROM @omopSc.OBSERVATION_PERIOD
                  WHERE person_id IN (SELECT subject_id FROM @resultSc.@cohortTable)"
  obs_per <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                         sql = SqlRender::render(sql = obs_per_sql,
                                                                 omopSc = cdm_schema,
                                                                 resultSc = results_sc,
                                                                 cohortTable = cohortTable))
  # death_sql <- "SELECT * FROM @omopSc.DEATH
  #               WHERE person_id IN (SELECT subject_id FROM @resultSc.@cohortTable)"
  # death <- DatabaseConnector::querySql(connection = cdm_bbdd,
  #                                      sql = SqlRender::render(sql = death_sql,
  #                                                              omopSc = cdm_schema,
  #                                                              resultSc = results_sc,
  #                                                              cohortTable = cohortTable))

  cohort <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = SqlRender::render(sql = "SELECT * FROM @resultSc.@cohortTable",
                                                                resultSc = results_sc,
                                                                cohortTable = cohortTable))

  cohort_event <- cohort[cohort$COHORT_DEFINITION_ID %in% c(acohortId, 3:6),]
  cohort_event$event <- as.character(NA)
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == acohortId] <- 'dintro'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 3] <- 'AMI'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 4] <- 'Angor'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 5] <- 'StrokeI'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 6] <- 'TIA'

  cohort_event_w <- tidyr::pivot_wider(data = cohort_event,
                                       id_cols = "SUBJECT_ID",
                                       names_from = 'event',
                                       names_prefix = 'ep_',
                                       values_from = "COHORT_START_DATE")
  names(cohort_event_w)[2] <- 'dintro'
  cohort_event_w <- merge(cohort_event_w,
                          obs_per[, c("PERSON_ID", "OBSERVATION_PERIOD_END_DATE")],
                          by.x = 'SUBJECT_ID',
                          by.y = 'PERSON_ID',
                          all.x = TRUE)

  bbdd_covar <- merge(bbdd_covar,
                      cohort_event_w,
                      by.x = 'rowId',
                      by.y = "SUBJECT_ID",
                      all.x = TRUE)
  bbdd_covar$i.ep_AMI <- 0
  bbdd_covar$i.ep_AMI[bbdd_covar$dintro < bbdd_covar$ep_AMI &
                        bbdd_covar$ep_AMI <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_AMI <- as.numeric(pmin(bbdd_covar$ep_AMI, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)
  bbdd_covar$i.ep_Angor <- 0
  bbdd_covar$i.ep_Angor[bbdd_covar$dintro < bbdd_covar$ep_Angor &
                          bbdd_covar$ep_Angor <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_Angor <- as.numeric(pmin(bbdd_covar$ep_Angor, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)
  bbdd_covar$i.ep_StrokeI <- 0
  bbdd_covar$i.ep_StrokeI[bbdd_covar$dintro < bbdd_covar$ep_StrokeI &
                            bbdd_covar$ep_StrokeI <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_StrokeI <- as.numeric(pmin(bbdd_covar$ep_StrokeI, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)
  bbdd_covar$i.ep_TIA <- 0
  bbdd_covar$i.ep_TIA[bbdd_covar$dintro < bbdd_covar$ep_TIA &
                        bbdd_covar$ep_TIA <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_TIA <- as.numeric(pmin(bbdd_covar$ep_TIA, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)

  return(bbdd_covar)
}

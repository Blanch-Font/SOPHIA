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
  albumin_ser_conceptId <- c(3024561)
  CKDEPI_conceptId <- c(40764999)
  GOT_conceptId <- c(3010587)
  GGT_conceptId <- c(3026910)
  vitD_conceptId <- c(43055034)
  PEPTIDCs_conceptId <- c(3010084)
  covMeasValueLong <- FeatureExtraction::createCovariateSettings(
    useMeasurementValueLongTerm = TRUE,
    longTermStartDays = (-2)*365.25,
    endDays = 0*365.25,
    includedCovariateConceptIds = c(
      SBP_conceptId,
      DBP_conceptId,
      # cT_conceptId,
      # cHDL_conceptId,
      # cLDL_conceptId,
      # cVLDL_conceptId,
      # Tg_conceptId,
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
      creatinine_conceptId,
      albumin_ser_conceptId,
      CKDEPI_conceptId,
      GOT_conceptId,
      GGT_conceptId,
      vitD_conceptId,
      PEPTIDCs_conceptId),
    addDescendantsToInclude = TRUE)
  covMeasValue_lipid <- FeatureExtraction::createTemporalCovariateSettings(
    useMeasurementValue = TRUE,
    temporalStartDays = c(-2, 1)*365.25,
    temporalEndDays = c(0, 2)*365.25,
    includedCovariateConceptIds = c(
      cT_conceptId,
      cHDL_conceptId,
      cLDL_conceptId,
      cVLDL_conceptId,
      Tg_conceptId),
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
    includedCovariateConceptIds = c(201826, 443732, 40482801, 40485020),
    addDescendantsToInclude = TRUE)

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
    includedCovariateConceptIds = c(#I20
      321318, #Angina pectoris
      315296, #Preinfarction syndrome
      4127089), #Coronary artery spasm,
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
    includedCovariateConceptIds = c(312327, #Acute myocardial infarction
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
                                    46270163),
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
      4046360 #Lacunar infarction
    ),
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

  nephro_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 128,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 128,
                      analysisName = "Nephropathy due to DM",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(192279, #Disorder of kidney due to diabetes mellitus
                                    200687, #Renal disorder due to type 1 diabetes mellitus
                                    443731, #Renal disorder due to type 2 diabetes mellitus
                                    43531578 #Chronic kidney disease due to type 2 diabetes mellitus
    ),
    addDescendantsToInclude = FALSE)

  retino_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 129,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 129,
                      analysisName = "Retinopathy due to DM",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(376114, #Severe nonproliferative retinopathy due to diabetes mellitus
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
                                    45773064 #Traction detachment of retina due to type 2 diabetes mellitus
    ),
    addDescendantsToInclude = FALSE)

  neuro_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 130,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 130,
                      analysisName = "Neuropathy due to DM",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(376065, #Disorder of nervous system due to type 2 diabetes mellitus
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
    addDescendantsToInclude = FALSE)

  PAD_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 131,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 131,
                      analysisName = "PAD",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(
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
      44782819, #Chronic occlusion of artery of extremity
      46271459 #Atherosclerosis of bypass graft of lower limb
    ),
    addDescendantsToInclude = FALSE)

  dka_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 132,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 130,
                      analysisName = "Neuropathy due to DM",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(439770, #Ketoacidosis due to type 1 diabetes mellitus
                                    4009303, #Diabetic ketoacidosis without coma
                                    4224254 #Ketoacidotic coma due to type 1 diabetes mellitus
    ),
    addDescendantsToInclude = FALSE)

  hypoglyc_vars <- FeatureExtraction::createAnalysisDetails(
    analysisId = 133,
    sqlFileName = "DomainConcept.sql",
    parameters = list(analysisId = 130,
                      analysisName = "Neuropathy due to DM",
                      startDay = "anyTimePrior",
                      endDay = 0,
                      subType = "all",
                      domainId = "Condition",
                      domainTable = "condition_occurrence",
                      domainConceptId = "condition_concept_id",
                      domainStartDate = "condition_start_date",
                      domainEndDate = "condition_start_date"),
    includedCovariateConceptIds = c(45769876, #Hypoglycemia due to type 1 diabetes mellitus
                                    4228112 #Hypoglycemic coma due to type 1 diabetes mellitus
                                    ),
    addDescendantsToInclude = FALSE)

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
  T1DM_AgeCovSet <- createT1DM_AgeCovariateSettings(useT1DM_Age = TRUE)
  T1Rx_TimeCovSet <- createT1Rx_TimeCovariateSettings(useT1Rx_Time = TRUE)
  T1Rx_AgeCovSet <- createT1Rx_AgeCovariateSettings(useT1Rx_Age = TRUE)
  C10_TimeCovSet <- createC10_TimeCovariateSettings(useC10_Time = TRUE)
  HTNRx_TimeCovSet <- createHTNRx_TimeCovariateSettings(useHTNRx_Time = TRUE)

  covariateSettings <- list(covDemo,
                            covMeasValueAny,
                            covMeasValueLong,
                            # covMeasValue_lipid,
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
                                   T1DM_vars,
                                   nephro_vars,
                                   retino_vars,
                                   neuro_vars,
                                   PAD_vars,
                                   dka_vars,
                                   hypoglyc_vars)),
                            covDrug,
                            SmokingCovSet,
                            T2DM_TimeCovSet,
                            T1DM_TimeCovSet,
                            T1Rx_TimeCovSet,
                            T1DM_AgeCovSet,
                            T1Rx_AgeCovSet,
                            C10_TimeCovSet,
                            HTNRx_TimeCovSet)

  covariateData <- FeatureExtraction::getDbCovariateData(
    connection = cdm_bbdd,
    cdmDatabaseSchema = cdm_schema,
    cohortDatabaseSchema = results_sc,
    cohortTable = cohortTable,
    cohortId = acohortId,
    rowIdField = "subject_id",
    covariateSettings = covariateSettings)

  covariateData_temp <- FeatureExtraction::getDbCovariateData(
    connection = cdm_bbdd,
    cdmDatabaseSchema = cdm_schema,
    cohortDatabaseSchema = results_sc,
    cohortTable = cohortTable,
    cohortId = acohortId,
    rowIdField = "subject_id",
    covariateSettings = covMeasValue_lipid)

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
  return(list(covariateData = covariateData, covariateData_temp = covariateData_temp))
}

#' Transform covariateData object into FlatTable
#'
#' Si volem agafar l'últim valor podem canviar-ho
#'
#' @param covariateData A covariateDate object
#' @param covariateData_temp A Temporal covariateDate object
#'
#' @return A data.table with the covariate data
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' #Not yet
transformToFlat <- function(covariateData,
                            covariateData_temp){
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
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 128, 'nephro', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 129, 'retino', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 130, 'neuro', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 131, 'PAD', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 132, 'DKA', .data$variable),
    variable = dplyr::if_else(stringr::str_sub(.data$covariateId, start = -3L) == 133, 'Hypoglyc', .data$variable),
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
    variable = dplyr::if_else(substr(.data$covariateId, 1, 7) == 3024561, 'Albumin', .data$variable),
    variable = dplyr::if_else(substr(.data$covariateId, 1, 8) == 40764999, 'CKDEPI', .data$variable),
    variable = dplyr::if_else(substr(.data$covariateId, 1, 7) == 3010587, 'GOT', .data$variable),
    variable = dplyr::if_else(substr(.data$covariateId, 1, 7) == 3026910, 'GGT', .data$variable),
    variable = dplyr::if_else(substr(.data$covariateId, 1, 8) == 43055034, 'vitD', .data$variable),
    variable = dplyr::if_else(substr(.data$covariateId, 1, 7) == 3010084, 'PEPTIDCs', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 201820211, 'TimeT2DM', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 201254212, 'TimeT1DM', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 201254216, 'TimeT1Rx', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 201254213, 'AgeT1DM', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 201254217, 'AgeT1Rx', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21601853214, 'TimeC10', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 21600381215, 'TimeHTNRx', .data$variable))
  bbdd_covar <- dplyr::group_by(.data = bbdd_covar, .data$rowId, .data$variable)
  bbdd_covar <- dplyr::summarise(
    .data = bbdd_covar,
    covariateValue = mean(.data$covariateValue),
    .groups = 'keep')
  bbdd_covar <- dplyr::ungroup(x = bbdd_covar)
  bbdd_covar_temp <- dplyr::collect(covariateData_temp$covariates)
  bbdd_covar_temp <-  dplyr::mutate(
    .data = bbdd_covar_temp,
    variable = as.character(NA),
    variable = dplyr::if_else(.data$covariateId == 3027114840702 & .data$timeId == 1, 'cT', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3011884840702 & .data$timeId == 1, 'cHDL', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3028437840702 & .data$timeId == 1, 'cLDL', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3022192840702 & .data$timeId == 1, 'Tg', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3027114840702 & .data$timeId == 2, 'cT_1y', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3011884840702 & .data$timeId == 2, 'cHDL_1y', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3028437840702 & .data$timeId == 2, 'cLDL_1y', .data$variable),
    variable = dplyr::if_else(.data$covariateId == 3022192840702 & .data$timeId == 2, 'Tg_1y', .data$variable))
  bbdd_covar_temp <- dplyr::group_by(.data = bbdd_covar_temp, .data$rowId, .data$variable)
  bbdd_covar_temp <- dplyr::summarise(
    .data = bbdd_covar_temp,
    covariateValue = mean(.data$covariateValue),
    .groups = 'keep')
  bbdd_covar_temp <- dplyr::ungroup(x = bbdd_covar_temp)
  bbdd_covar <- tidyr::pivot_wider(
    data = rbind(bbdd_covar, bbdd_covar_temp),
    id_cols = 'rowId',
    names_from = 'variable',
    values_from = 'covariateValue')
  bbdd_covar <- dplyr::mutate(
    .data = bbdd_covar,
    dplyr::across(dplyr::any_of(c('sex_female', 'sex_male', 'T2DM', 'obesity', 'angor', 'tia',
                                  'stroke', 'ami', 'Current', 'Former', 'Never',
                                  'A10', 'A10A', 'A10B','C01', 'C02', 'C03', 'C07', 'C08', 'C09',
                                  'C10', 'M01A',
                                  'COPD', 'CKD', 'cancer', 'depress', 'htn', 'hf', 'liver', 'ra',
                                  'sleep_apnea', 'pcos',
                                  "neuro", "nephro", "retino", "PAD")),
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
  # bbdd_covar <- dplyr::mutate(
  #   .data = bbdd_covar,
  #   TimeT1DM_diag = TimeT1DM,
  #   TimeT1DM = pmax(TimeT1DM, TimeT1Rx, na.rm = T))
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

#' Auxiliar function to create Age for first T1DM diagnosis
#'
#' @param useT1DM_Age Logical valor
#'
#' @return covariateSettings object
#' @export
#'
#' @examples
#' #Not yet
createT1DM_AgeCovariateSettings <- function(useT1DM_Age = TRUE){
  covariateSettings <- list(useT1DM_Age = useT1DM_Age)
  attr(covariateSettings, "fun") <- "getDbuseT1DM_AgeCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

#' Auxiliar function to create Age for T1DM diagnosis status SQL implementation
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
getDbuseT1DM_AgeCovariateData <- function(connection,
                                           oracleTempSchema = NULL,
                                           cdmDatabaseSchema,
                                           cohortTable = "#cohort_person",
                                           cohortId = -1,
                                           cdmVersion = "5",
                                           rowIdField = "subject_id",
                                           covariateSettings,
                                           aggregated = FALSE){
  writeLines("Constructing T1DM_Age covariates")
  if (covariateSettings$useT1DM_Age == FALSE) {
    return(NULL)
  }

  included_sql <- SqlRender::render(sql = "SELECT *
                                           FROM @schema_cdm.CONCEPT_ANCESTOR
                                           WHERE ancestor_concept_id IN (@diab_id)",
                                    schema_cdm = cdmDatabaseSchema,
                                    diab_id = c(201254, 435216, 40484648, 40484649))
  included_id <- DatabaseConnector::querySql(connection,
                                             sql = included_sql)
  # Falta afegir la data de naixement a la cohort. S'ha de fer un INNER JOIN
  # Some SQL to construct the covariate:
  sql <- "SELECT DISTINCT ON (cond2.person_id)
               cond2.person_id AS row_id,
               201254213 AS covariate_id,
               DATEDIFF(DAY, DATEFROMPARTS(cond2.year_of_birth, cond2.month_of_birth, cond2.day_of_birth),
                        cond2.condition_start_date)/365.25 AS covariate_value
        FROM (SELECT cond.person_id,
                       cond.condition_start_date,
                       person_table.year_of_birth,
                       person_table.month_of_birth,
                       person_table.day_of_birth
              FROM @cdm_database_schema.CONDITION_OCCURRENCE cond
              INNER JOIN @cohort_table cohort
                    ON cohort.subject_id = cond.person_id
                    INNER JOIN @cdm_database_schema.PERSON person_table
                          ON person_table.person_id = cohort.subject_id
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
  covariateRef <- data.frame(covariateId = c(201254213),
                             covariateName = c('Age from T1DM'),
                             analysisId = 213,
                             conceptId = 201254)
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 213,
                            analysisName = "Age from T1DM",
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
#' @param useT1Rx_Time Logical valor
#'
#' @return covariateSettings object
#' @export
#'
#' @examples
#' #Not yet
createT1Rx_TimeCovariateSettings <- function(useT1Rx_Time = TRUE){
  covariateSettings <- list(useT1Rx_Time = useT1Rx_Time)
  attr(covariateSettings, "fun") <- "getDbuseT1Rx_TimeCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

#' Auxiliar function to create Time from T1Rx diagnosis status SQL implementation
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
getDbuseT1Rx_TimeCovariateData <- function(connection,
                                           oracleTempSchema = NULL,
                                           cdmDatabaseSchema,
                                           cohortTable = "#cohort_person",
                                           cohortId = -1,
                                           cdmVersion = "5",
                                           rowIdField = "subject_id",
                                           covariateSettings,
                                           aggregated = FALSE){
  writeLines("Constructing T1Rx_Time covariates")
  if (covariateSettings$useT1Rx_Time == FALSE) {
    return(NULL)
  }

  vec_insulin <- c(1596977, 19058398, 19078552, 19078559, 19095211, 19095212, 19112791, 19133793,
                   19135264, 21076306, 21086042, 35410536, 35412958, 40051377, 40052768, 42479783,
                   42481504, 42481541, 42899447, 42902356, 42902587, 42902742, 42902821, 42902945,
                   42903059, 44058584, 46233969, 46234047, 46234234, 46234237, 1502905, 1513843,
                   1513876, 1516976, 1531601, 1544838, 1550023, 1562586, 1567198, 1586346, 1586369,
                   1588986, 1590165, 1596977, 19013926, 19013951, 19090180, 19090187, 19090204,
                   19090221, 19090226, 19090229, 19090244, 19090247, 19090249, 19091621, 35198096,
                   35602717, 42899447, 46221581)
  included_sql <- SqlRender::render(sql = "SELECT *
                                           FROM @schema_cdm.CONCEPT_ANCESTOR
                                           WHERE ancestor_concept_id IN (@diab_id)",
                                    schema_cdm = cdmDatabaseSchema,
                                    diab_id = vec_insulin)
  included_id <- DatabaseConnector::querySql(connection,
                                             sql = included_sql)
  # Some SQL to construct the covariate:
  sql <- "SELECT DISTINCT ON (cond2.person_id)
                 cond2.person_id AS row_id,
                 201254216  AS covariate_id,
                 DATEDIFF(DAY, cond2.DRUG_EXPOSURE_START_DATE, cond2.cohort_start_date) AS covariate_value
          FROM (SELECT *
                FROM @cdm_database_schema.DRUG_EXPOSURE cond
                INNER JOIN @cohort_table cohort
                      ON cohort.subject_id = cond.person_id
                WHERE cond.DRUG_EXPOSURE_START_DATE <= DATEADD(DAY, 0, cohort.cohort_start_date)
                  AND cond.DRUG_CONCEPT_ID != 0
                  AND cond.DRUG_CONCEPT_ID IN (@included_concept_table)
                  AND cohort.cohort_definition_id IN (@cohort_definition_id)) cond2
          ORDER BY cond2.person_id, cond2.DRUG_EXPOSURE_START_DATE"
  sql <- SqlRender::render(sql,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_table = cohortTable, # ha de ser results_sc.cohortTable
                           cohort_definition_id = cohortId,
                           included_concept_table = included_id$DESCENDANT_CONCEPT_ID)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = c(201254216),
                             covariateName = c('Time from T1Rx'),
                             analysisId = 216,
                             conceptId = 201254)
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 216,
                            analysisName = "Time from T1Rx",
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

#' Auxiliar function to create Age for first T1Rx drug
#'
#' @param useT1Rx_Age Logical valor
#'
#' @return covariateSettings object
#' @export
#'
#' @examples
#' #Not yet
createT1Rx_AgeCovariateSettings <- function(useT1Rx_Age = TRUE){
  covariateSettings <- list(useT1Rx_Age = useT1Rx_Age)
  attr(covariateSettings, "fun") <- "getDbuseT1Rx_AgeCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

#' Auxiliar function to create Age for T1Rx diagnosis status SQL implementation
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
getDbuseT1Rx_AgeCovariateData <- function(connection,
                                          oracleTempSchema = NULL,
                                          cdmDatabaseSchema,
                                          cohortTable = "#cohort_person",
                                          cohortId = -1,
                                          cdmVersion = "5",
                                          rowIdField = "subject_id",
                                          covariateSettings,
                                          aggregated = FALSE){
  writeLines("Constructing T1Rx_Age covariates")
  if (covariateSettings$useT1Rx_Age == FALSE) {
    return(NULL)
  }

  vec_insulin <- c(1596977, 19058398, 19078552, 19078559, 19095211, 19095212, 19112791, 19133793,
                   19135264, 21076306, 21086042, 35410536, 35412958, 40051377, 40052768, 42479783,
                   42481504, 42481541, 42899447, 42902356, 42902587, 42902742, 42902821, 42902945,
                   42903059, 44058584, 46233969, 46234047, 46234234, 46234237, 1502905, 1513843,
                   1513876, 1516976, 1531601, 1544838, 1550023, 1562586, 1567198, 1586346, 1586369,
                   1588986, 1590165, 1596977, 19013926, 19013951, 19090180, 19090187, 19090204,
                   19090221, 19090226, 19090229, 19090244, 19090247, 19090249, 19091621, 35198096,
                   35602717, 42899447, 46221581)
  included_sql <- SqlRender::render(sql = "SELECT *
                                           FROM @schema_cdm.CONCEPT_ANCESTOR
                                           WHERE ancestor_concept_id IN (@diab_id)",
                                    schema_cdm = cdmDatabaseSchema,
                                    diab_id = vec_insulin)
  included_id <- DatabaseConnector::querySql(connection,
                                             sql = included_sql)
  # Falta afegir la data de naixement a la cohort. S'ha de fer un INNER JOIN
  # Some SQL to construct the covariate:
  sql <- "SELECT DISTINCT ON (cond2.person_id)
               cond2.person_id AS row_id,
               201254217 AS covariate_id,
               DATEDIFF(DAY, DATEFROMPARTS(cond2.year_of_birth, cond2.month_of_birth, cond2.day_of_birth),
                        cond2.DRUG_EXPOSURE_START_DATE)/365.25 AS covariate_value
        FROM (SELECT cond.person_id,
                       cond.DRUG_EXPOSURE_START_DATE,
                       person_table.year_of_birth,
                       person_table.month_of_birth,
                       person_table.day_of_birth
              FROM @cdm_database_schema.DRUG_EXPOSURE cond
              INNER JOIN @cohort_table cohort
                    ON cohort.subject_id = cond.person_id
                    INNER JOIN @cdm_database_schema.PERSON person_table
                          ON person_table.person_id = cohort.subject_id
              WHERE cond.DRUG_EXPOSURE_START_DATE <= DATEADD(DAY, 0, cohort.cohort_start_date)
                  AND cond.DRUG_CONCEPT_ID != 0
                  AND cond.DRUG_CONCEPT_ID IN (@included_concept_table)
                  AND cohort.cohort_definition_id IN (@cohort_definition_id)) cond2
          ORDER BY cond2.person_id, cond2.DRUG_EXPOSURE_START_DATE"
  sql <- SqlRender::render(sql,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_table = cohortTable, # ha de ser results_sc.cohortTable
                           cohort_definition_id = cohortId,
                           included_concept_table = included_id$DESCENDANT_CONCEPT_ID)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = c(201254217),
                             covariateName = c('Age from T1Rx'),
                             analysisId = 217,
                             conceptId = 201254)
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 217,
                            analysisName = "Age from T1Rx",
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
  death_sql <- "SELECT * FROM @omopSc.DEATH
                WHERE person_id IN (SELECT subject_id FROM @resultSc.@cohortTable)"
  death <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                       sql = SqlRender::render(sql = death_sql,
                                                               omopSc = cdm_schema,
                                                               resultSc = results_sc,
                                                               cohortTable = cohortTable))

  cohort <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = SqlRender::render(sql = "SELECT * FROM @resultSc.@cohortTable",
                                                                resultSc = results_sc,
                                                                cohortTable = cohortTable))

  cohort_event <- cohort[cohort$COHORT_DEFINITION_ID %in% c(acohortId, 3:18),]
  cohort_event$event <- as.character(NA)
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == acohortId] <- 'dintro'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 3] <- 'AMI'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 4] <- 'Angor'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 5] <- 'StrokeI'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 6] <- 'TIA'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 7] <- 'Nephropathy'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 8] <- 'Retinopathy'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 9] <- 'Neuropathy'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 10] <- 'PAD'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 11] <- 'Angor_unstable'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 12] <- 'AMI_WP4'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 13] <- 'stroke_WP4'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 14] <- 'neuroWP4'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 15] <- 'nephroWP4'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 16] <- 'retinoWP4'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 17] <- 'footWP4'
  cohort_event$event[cohort_event$COHORT_DEFINITION_ID == 18] <- 'DKAWP4'

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
  bbdd_covar <- merge(bbdd_covar,
                      death,
                      by.x = 'rowId',
                      by.y = "PERSON_ID",
                      all.x = TRUE)
  bbdd_covar$i.ep_AMI <- 0
  bbdd_covar$i.ep_AMI[bbdd_covar$dintro < bbdd_covar$ep_AMI &
                        bbdd_covar$ep_AMI <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_AMI <- as.numeric(pmin(bbdd_covar$ep_AMI, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_Angor <- 0
  bbdd_covar$i.ep_Angor[bbdd_covar$dintro < bbdd_covar$ep_Angor &
                          bbdd_covar$ep_Angor <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_Angor <- as.numeric(pmin(bbdd_covar$ep_Angor, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_StrokeI <- 0
  bbdd_covar$i.ep_StrokeI[bbdd_covar$dintro < bbdd_covar$ep_StrokeI &
                            bbdd_covar$ep_StrokeI <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_StrokeI <- as.numeric(pmin(bbdd_covar$ep_StrokeI, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_TIA <- 0
  bbdd_covar$i.ep_TIA[bbdd_covar$dintro < bbdd_covar$ep_TIA &
                        bbdd_covar$ep_TIA <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_TIA <- as.numeric(pmin(bbdd_covar$ep_TIA, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_Nephropathy <- 0
  bbdd_covar$i.ep_Nephropathy[bbdd_covar$dintro < bbdd_covar$ep_Nephropathy &
                                bbdd_covar$ep_Nephropathy <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_Nephropathy <- as.numeric(pmin(bbdd_covar$ep_Nephropathy, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_Retinopathy <- 0
  bbdd_covar$i.ep_Retinopathy[bbdd_covar$dintro < bbdd_covar$ep_Retinopathy &
                                bbdd_covar$ep_Retinopathy <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_Retinopathy <- as.numeric(pmin(bbdd_covar$ep_Retinopathy, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_Neuropathy <- 0
  bbdd_covar$i.ep_Neuropathy[bbdd_covar$dintro < bbdd_covar$ep_Neuropathy &
                                bbdd_covar$ep_Neuropathy <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_Neuropathy <- as.numeric(pmin(bbdd_covar$ep_Neuropathy, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_PAD <- 0
  bbdd_covar$i.ep_PAD[bbdd_covar$dintro < bbdd_covar$ep_PAD &
                               bbdd_covar$ep_PAD <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_PAD <- as.numeric(pmin(bbdd_covar$ep_PAD, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_Angor_unstable <- 0
  bbdd_covar$i.ep_Angor_unstable[bbdd_covar$dintro < bbdd_covar$ep_Angor_unstable &
                                   bbdd_covar$ep_Angor_unstable <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_Angor_unstable <- as.numeric(pmin(bbdd_covar$ep_Angor_unstable, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_AMI_WP4 <- 0
  bbdd_covar$i.ep_AMI_WP4[bbdd_covar$dintro < bbdd_covar$ep_AMI_WP4 &
                            bbdd_covar$ep_AMI_WP4 <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_AMI_WP4 <- as.numeric(pmin(bbdd_covar$ep_AMI_WP4, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_stroke_WP4 <- 0
  bbdd_covar$i.ep_stroke_WP4[bbdd_covar$dintro < bbdd_covar$ep_stroke_WP4 &
                            bbdd_covar$ep_stroke_WP4 <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_stroke_WP4 <- as.numeric(pmin(bbdd_covar$ep_stroke_WP4, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_neuroWP4 <- 0
  bbdd_covar$i.ep_neuroWP4[bbdd_covar$dintro < bbdd_covar$ep_neuroWP4 &
                               bbdd_covar$ep_neuroWP4 <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_neuroWP4 <- as.numeric(pmin(bbdd_covar$ep_neuroWP4, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_nephroWP4 <- 0
  bbdd_covar$i.ep_nephroWP4[bbdd_covar$dintro < bbdd_covar$ep_nephroWP4 &
                             bbdd_covar$ep_nephroWP4 <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_nephroWP4 <- as.numeric(pmin(bbdd_covar$ep_nephroWP4, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_retinoWP4 <- 0
  bbdd_covar$i.ep_retinoWP4[bbdd_covar$dintro < bbdd_covar$ep_retinoWP4 &
                              bbdd_covar$ep_retinoWP4 <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_retinoWP4 <- as.numeric(pmin(bbdd_covar$ep_retinoWP4, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_footWP4 <- 0
  bbdd_covar$i.ep_footWP4[bbdd_covar$dintro < bbdd_covar$ep_footWP4 &
                              bbdd_covar$ep_footWP4 <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_footWP4 <- as.numeric(pmin(bbdd_covar$ep_footWP4, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25
  bbdd_covar$i.ep_DKAWP4 <- 0
  bbdd_covar$i.ep_DKAWP4[bbdd_covar$dintro < bbdd_covar$ep_DKAWP4 &
                            bbdd_covar$ep_DKAWP4 <= bbdd_covar$OBSERVATION_PERIOD_END_DATE] <- 1
  bbdd_covar$t.ep_DKAWP4 <- as.numeric(pmin(bbdd_covar$ep_DKAWP4, bbdd_covar$OBSERVATION_PERIOD_END_DATE, na.rm = T) - bbdd_covar$dintro)/365.25

  return(bbdd_covar)
}

#' Auxiliar function to create Time form C10 diagnosis
#'
#' @param useC10_Time Logical valor
#'
#' @return covariateSettings object
#' @export
#'
#' @examples
#' #Not yet
createC10_TimeCovariateSettings <- function(useC10_Time = TRUE){
  covariateSettings <- list(useC10_Time = useC10_Time)
  attr(covariateSettings, "fun") <- "getDbuseC10_TimeCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

#' Auxiliar function to create Time from C10 diagnosis status SQL implementation
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
getDbuseC10_TimeCovariateData <- function(connection,
                                          oracleTempSchema = NULL,
                                          cdmDatabaseSchema,
                                          cohortTable = "#cohort_person",
                                          cohortId = -1,
                                          cdmVersion = "5",
                                          rowIdField = "subject_id",
                                          covariateSettings,
                                          aggregated = FALSE){
  writeLines("Constructing C10_Time covariates")
  if (covariateSettings$useC10_Time == FALSE) {
    return(NULL)
  }

  included_sql <- SqlRender::render(sql = "SELECT *
                                           FROM @schema_cdm.CONCEPT_ANCESTOR
                                           WHERE ancestor_concept_id IN (@diab_id)",
                                    schema_cdm = cdmDatabaseSchema,
                                    diab_id = c(21601853))
  included_id <- DatabaseConnector::querySql(connection,
                                             sql = included_sql)
  # Some SQL to construct the covariate:
  sql <- "SELECT DISTINCT ON (cond2.person_id)
                 cond2.person_id AS row_id,
                 21601853214  AS covariate_id,
                 DATEDIFF(DAY, cond2.DRUG_EXPOSURE_START_DATE, cond2.cohort_start_date) AS covariate_value
          FROM (SELECT *
                FROM @cdm_database_schema.DRUG_EXPOSURE cond
                INNER JOIN @cohort_table cohort
                      ON cohort.subject_id = cond.person_id
                WHERE cond.DRUG_EXPOSURE_START_DATE <= DATEADD(DAY, 0, cohort.cohort_start_date)
                  AND cond.DRUG_CONCEPT_ID != 0
                  AND cond.DRUG_CONCEPT_ID IN (@included_concept_table)
                  AND cohort.cohort_definition_id IN (@cohort_definition_id)) cond2
          ORDER BY cond2.person_id, cond2.DRUG_EXPOSURE_START_DATE"
  sql <- SqlRender::render(sql,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_table = cohortTable, # ha de ser results_sc.cohortTable
                           cohort_definition_id = cohortId,
                           included_concept_table = included_id$DESCENDANT_CONCEPT_ID)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = c(21601853214),
                             covariateName = c('Time from C10'),
                             analysisId = 214,
                             conceptId = 21601853)
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 214,
                            analysisName = "Time from C10",
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

#' Auxiliar function to create Time form HTN medication diagnosis
#'
#' @param useHTNRx_Time Logical valor
#'
#' @return covariateSettings object
#' @export
#'
#' @examples
#' #Not yet
createHTNRx_TimeCovariateSettings <- function(useHTNRx_Time = TRUE){
  covariateSettings <- list(useHTNRx_Time = useHTNRx_Time)
  attr(covariateSettings, "fun") <- "getDbuseHTNRx_TimeCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

#' Auxiliar function to create Time from HTN Rx diagnosis status SQL implementation
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
getDbuseHTNRx_TimeCovariateData <- function(connection,
                                            oracleTempSchema = NULL,
                                            cdmDatabaseSchema,
                                            cohortTable = "#cohort_person",
                                            cohortId = -1,
                                            cdmVersion = "5",
                                            rowIdField = "subject_id",
                                            covariateSettings,
                                            aggregated = FALSE){
  writeLines("Constructing HTNRx_Time covariates")
  if (covariateSettings$useHTNRx_Time == FALSE) {
    return(NULL)
  }

  included_sql <- SqlRender::render(sql = "SELECT *
                                           FROM @schema_cdm.CONCEPT_ANCESTOR
                                           WHERE ancestor_concept_id IN (@diab_id)",
                                    schema_cdm = cdmDatabaseSchema,
                                    diab_id = c(21600381, #C02
                                                21601461, #C03
                                                21601664, #C07
                                                21601744, #C08
                                                21601782)) #C09
  included_id <- DatabaseConnector::querySql(connection,
                                             sql = included_sql)
  # Some SQL to construct the covariate:
  sql <- "SELECT DISTINCT ON (cond2.person_id)
                 cond2.person_id AS row_id,
                 21600381215  AS covariate_id,
                 DATEDIFF(DAY, cond2.DRUG_EXPOSURE_START_DATE, cond2.cohort_start_date) AS covariate_value
          FROM (SELECT *
                FROM @cdm_database_schema.DRUG_EXPOSURE cond
                INNER JOIN @cohort_table cohort
                      ON cohort.subject_id = cond.person_id
                WHERE cond.DRUG_EXPOSURE_START_DATE <= DATEADD(DAY, 0, cohort.cohort_start_date)
                  AND cond.DRUG_CONCEPT_ID != 0
                  AND cond.DRUG_CONCEPT_ID IN (@included_concept_table)
                  AND cohort.cohort_definition_id IN (@cohort_definition_id)) cond2
          ORDER BY cond2.person_id, cond2.DRUG_EXPOSURE_START_DATE"
  sql <- SqlRender::render(sql,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_table = cohortTable, # ha de ser results_sc.cohortTable
                           cohort_definition_id = cohortId,
                           included_concept_table = included_id$DESCENDANT_CONCEPT_ID)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = c(21600381215),
                             covariateName = c('Time from HTNRx'),
                             analysisId = 215,
                             conceptId = 21600381)
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 215,
                            analysisName = "Time from HTNRX",
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

#' Funcion para pasar del servidor a una tabla plana
#' Necesita totes les coses del servidor més quina cohort volem contruir.
#'
#' @param cdm_bbdd A connection for a OMOP database via DatabaseConnector
#' @param cdm_schema A name for OMOP schema
#' @param results_sc A name for result schema
#' @param cohortTable A name of the result cohort
#' @param acohortId A Cohort number
#'
#' @return List with three objects: descriptive for numeric and category and database ready to analysis.
#' @export
#'
#' @examples
#' #Not yet
FunCovar <- function(cdm_bbdd,
                     cdm_schema,
                     results_sc,
                     cohortTable,
                     acohortId){
  covariateData_aux <- buildData(cdm_bbdd = cdm_bbdd,
                                 cdm_schema = cdm_schema,
                                 results_sc = results_sc,
                                 cohortTable = cohortTable,
                                 acohortId = acohortId)
  covariateData_aux_temp <- covariateData_aux$covariateData_temp
  covariateData_aux <- covariateData_aux$covariateData
  # #Calculem el MPR pel T1DM i els eliminem si MPR inferior a 90%
  # if (acohortId == 2){
  #   sql_cohort <- "SELECT * FROM sophia_test.cohorttable WHERE cohort_definition_id = 2"
  #   cohort <- DatabaseConnector::querySql(connection = cdm_bbdd,
  #                                         sql = sql_cohort)
  #   cohort$time_obs <- with(cohort, as.numeric(COHORT_END_DATE - COHORT_START_DATE + 1))
  #   vec_insulin <- c(1596977, 19058398, 19078552, 19078559, 19095211, 19095212, 19112791, 19133793,
  #                    19135264, 21076306, 21086042, 35410536, 35412958, 40051377, 40052768, 42479783,
  #                    42481504, 42481541, 42899447, 42902356, 42902587, 42902742, 42902821, 42902945,
  #                    42903059, 44058584, 46233969, 46234047, 46234234, 46234237, 1502905, 1513843,
  #                    1513876, 1516976, 1531601, 1544838, 1550023, 1562586, 1567198, 1586346, 1586369,
  #                    1588986, 1590165, 1596977, 19013926, 19013951, 19090180, 19090187, 19090204,
  #                    19090221, 19090226, 19090229, 19090244, 19090247, 19090249, 19091621, 35198096,
  #                    35602717, 42899447, 46221581)
  #   sql_insulin <- "SELECT * FROM omop21t2_test.DRUG_ERA WHERE person_id IN (@id) AND DRUG_CONCEPT_ID IN (@insulin)"
  #   insulin <- DatabaseConnector::querySql(connection = cdm_bbdd,
  #                                          sql = SqlRender::render(sql_insulin,
  #                                                                  id = cohort$SUBJECT_ID,
  #                                                                  insulin = vec_insulin))
  #   insulin_treat <- dplyr::arrange(.data = insulin,
  #                                   PERSON_ID, DRUG_ERA_START_DATE)
  #   n_prev <- pull(count(insulin_treat), n)
  #   cond <- TRUE
  #   while(cond){
  #     insulin_treat <- group_by(insulin_treat, PERSON_ID)
  #     insulin_treat <- mutate(insulin_treat,
  #                             canvi = lag(DRUG_ERA_END_DATE) < DRUG_ERA_START_DATE,
  #                             canvi = dplyr::if_else(is.na(canvi), FALSE, canvi),
  #                             treat = cumsum(canvi))
  #     insulin_treat <- group_by(insulin_treat, PERSON_ID, treat)
  #     insulin_treat <- summarise(insulin_treat,
  #                                DRUG_ERA_START_DATE = min(DRUG_ERA_START_DATE),
  #                                DRUG_ERA_END_DATE = max(DRUG_ERA_END_DATE),
  #                                .groups = 'drop')
  #     n_act <- pull(count(insulin_treat), n)
  #     # cat('Prev: ', n_prev, ' Act: ', n_act, '\n')
  #     cond <- n_prev > n_act
  #     n_prev <- n_act
  #   }
  #   insulin_treat <- mutate(insulin_treat,
  #                           time = as.numeric(DRUG_ERA_END_DATE - DRUG_ERA_START_DATE + 1))
  #   cohort <- dplyr::left_join(cohort,
  #                              insulin_treat,
  #                              by = c('SUBJECT_ID' = 'PERSON_ID'))
  #   cohort$MPR <- with(cohort, time/time_obs)
  #   cohort <- dplyr::filter(cohort, 0.75 < MPR)
  #   covariateData_aux_temp <- dplyr::filter(covariateData_aux_temp,
  #                                           rowId %in% cohort$SUBJECT_ID)
  #   covariateData_aux <- dplyr::filter(covariateData_aux,
  #                                      rowId %in% cohort$SUBJECT_ID)
  # }
  covariateData2_aux <- FeatureExtraction::aggregateCovariates(covariateData_aux)
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
  cov_cate_resum_aux <- dplyr::filter(
    .data = covariateData2_aux$covariateRef,
    .data$analysisId %in% c(411, 413) & .data$conceptId %in% sel_med_conceptId |
      !(.data$analysisId %in% c(411, 413)))
  cov_cate_resum_aux <- dplyr::inner_join(
    x = cov_cate_resum_aux,
    y = covariateData2_aux$covariates)
  cov_cate_resum_aux <- dplyr::mutate(
    .data = cov_cate_resum_aux,
    covariateId = as.character(floor(.data$covariateId)),
    analysisId = as.integer(.data$analysisId),
    conceptId = as.integer(.data$conceptId),
    sumValue = as.integer(.data$sumValue),
    averageValue = .data$averageValue*100)
  cov_cate_resum_aux <-dplyr::collect(x = cov_cate_resum_aux)
  cov_num_resum_aux <- dplyr::inner_join(
    x = covariateData2_aux$covariateRef,
    y = covariateData2_aux$covariatesContinuous)
  cov_num_resum_aux <- dplyr::mutate(
    .data = cov_num_resum_aux,
    covariateId = as.character(floor(.data$covariateId)),
    analysisId = as.integer(.data$analysisId),
    conceptId = as.integer(.data$conceptId))
  cov_num_resum_aux <- dplyr::select(
    .data = cov_num_resum_aux,
    -.data$covariateId, -.data$analysisId, -.data$conceptId)
  cov_num_resum_aux <- dplyr::collect(x = cov_num_resum_aux)
  bbdd_covar_aux <- transformToFlat(covariateData_aux,
                                    covariateData_aux_temp)
  bbdd_covar_aux <- buildFollowUp(cdm_bbdd = cdm_bbdd,
                                  cdm_schema = cdm_schema,
                                  results_sc = results_sc,
                                  cohortTable = cohortTable,
                                  acohortId = acohortId,
                                  bbdd_covar = bbdd_covar_aux)
  return(list(cov_cate_resum = cov_cate_resum_aux,
              cov_num_resum = cov_num_resum_aux,
              bbdd_covar = bbdd_covar_aux))
}

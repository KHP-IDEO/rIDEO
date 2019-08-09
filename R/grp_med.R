## Medications classification function/s

grp_med <- function(x, depth = 'full') { # x is character variable of medicines
  # check input
  if(!is.character(x)) {
    stop("Medicines input must be a character variable", call. = FALSE)
  }
  # check output detail
  else if(depth == 'full') {
    # transform x to lower case prior to string searching
    x_lower <- stringr::str_to_lower(x)
    # vectors of medications by group
    insulins <- c("insulin", "abasaglar", "actrapid", "apidra", "degludec", "glarine", "humalog", "human mixtard", "humulin", "hypurin", "insulatard", "insuman", "lantus", "levemir", "mixtard", "novomix", "novorapid", "toujeo", "tresiba", "velosulin")
    metformin <- c("metformin", "diabex", "diaformin", "dianben", "fortamet", "glucophage", "glumetza", "obimet", "riomet")
    sulphonylureas <- c("amaryl", "daonil", "deamelin-s", "diamicron", "glibenclamide", "glibenese", "gliclazide", "glimepiride", "glipizide", "gliquidone", "glucotrol", "glurenorm", "glyburide", "glyclpramide", "minodiab", "tolbutamide")
    meglitinides <- c("nateglinide", "prandin", "repaglinide", "starlix")
    thiazolidinedione_glitazones <- c("actos", "avandia", "pioglitazone", "rosiglitazone")
    DPP_4 <- c("eucreas", "galvus", "janumet", "januvia", "linagliptin", "onglyza", "saxagliptin", "sitagliptin", "tradjenta", "vildagliptin")
    GLP_1 <- c("bydureon", "byetta", "dulaglutide", "exenatide", "liraglutide", "lixisenatide", "lyxumia", "ozempic", "semaglutide", "trulicity", "victoza")
    SGLT2 <- c("canagliflozin", "dapagliflozin", "empagliflozin", "farxiga", "forxiga", "invokana", "jardiance")
    alpha_glucosidase <- c("glucobay", "acarbose")
    lucentis <- c("lucentis", "ranibizumab")
    statins <- c("statin", "atorvastatin", "crestor", "lescol", "lipitor", "lipostat","pravastatin", "rosuvastatin", "simvastatin", "zocor")
    ACE_inhibitors <- c( "accupro", "accuretic", "acepril", "acezide", "capoten", "capozide", "captopril", "carace", "cilazapril", "coversyl", "enalapril", "fosinopril",  "gopten", "imidapril", "innovace", "innozide", "lisinopril", "lopace", "moexipril",
                         "perdix", "perindopril", "quinapril", "ramipril", "staril", "tanatril", "tarka", "trandolapril", "triapin", "tritace", "vascase", "zestoretic", "zestril")
    ARB <- c("amias", "aprovel", "candesartan", "cozaar", "diovan", "eprosartan", "exforge", "irbesartan",  "losartan", "olmesartan", "olmetec", "telmisartan", "teveten", "valsartan")
    CCB <- c("adalat", "adizem", "amlodipine", "amlostine", "angitil", "cardene", "coracten", "dilcardia", "dilzem", "diltiazem", "felodipine", "felotens", "istin", "lacidipine", "lercanidipine", "motens", "nicardipine", "nifedipine", "nisoldipine",
             "plendil", "prescal", "securon", "slozem", "tildiem", "vascalpha", "verapamil", "verapress", "viazem", "zanidip", "zemtard")
    diuretics <- c("amilamont", "amiloride", "aprinox", "bendroflumethiazide", "bumetanide", "burinex", "chlortalidone", "co-amilofruse", "co-amilozide", "co-flumactone", "co-tenidone", "co-triamterzide", "coversyl",  "cyclopenthiazide", "diurexan",
                   "dyazide", "dytide",	"eplerenone", "frumil", "frusene", "furosemide", "hygroton", "idactone", "indapamide", "inspra", "kalspare", "lasilactone", "lasix", "metenix",  "metolazone", "moduret", "natrilix", "navidrex", "prestim",
                   "spironolactone", "tenoret", "torasemide", "torem", "triamterene", "xipamide")

    # main function
    output <- factor(dplyr::if_else(stringr::str_detect(x_lower, paste(insulins, collapse = "|")), "Insulin",
                          dplyr::if_else(stringr::str_detect(x_lower, paste(metformin, collapse = "|")), "Metformin",
                                dplyr::if_else(stringr::str_detect(x_lower, paste(sulphonylureas, collapse = "|")), "Sulphonylureas",
                                               dplyr::if_else(stringr::str_detect(x_lower, paste(meglitinides, collapse = "|")), "Meglitinides",
                                                              dplyr::if_else(stringr::str_detect(x_lower, paste(thiazolidinedione_glitazones, collapse = "|")), "Thiazolidinedione/Glitazones",
                                                                             dplyr::if_else(stringr::str_detect(x_lower, paste(DPP_4, collapse = "|")), "DPP-4",
                                                                                            dplyr::if_else(stringr::str_detect(x_lower, paste(GLP_1, collapse = "|")), "GLP-1",
                                                                                                           dplyr::if_else(stringr::str_detect(x_lower, paste(SGLT2, collapse = "|")), "SGLT2",
                                                                                                                          dplyr::if_else(stringr::str_detect(x_lower, paste(alpha_glucosidase, collapse = "|")), "Alpha-glucosidase",
                                                                                                                                         dplyr::if_else(stringr::str_detect(x_lower, paste(lucentis, collapse = "|")), "Lucentis",
                                                                                                                                                                     dplyr::if_else(stringr::str_detect(x_lower, paste(statins, collapse = "|")), "Statins",
                                                                                                                                                                                    dplyr::if_else(stringr::str_detect(x_lower, paste(ACE_inhibitors, collapse = "|")), "ACE inhibitors",
                                                                                                                                                                                                   dplyr::if_else(stringr::str_detect(x_lower, paste(ARB, collapse = "|")), "ARBs",
                                                                                                                                                                                                                  dplyr::if_else(stringr::str_detect(x_lower, paste(CCB, collapse = "|")), "CCBs",
                                                                                                                                                                                                                                 dplyr::if_else(stringr::str_detect(x_lower, paste(diuretics, collapse = "|")), "Diuretics", "Other")))))))))))))))
    )
  }

    else if(depth == 'part') {
      # transform x to lower case prior to string searching
      x_lower <- stringr::str_to_lower(x)
      # vectors of medications by group
      insulins <- c("insulin", "abasaglar", "actrapid", "apidra", "degludec", "glarine", "humalog", "human mixtard", "humulin", "hypurin", "insulatard", "insuman", "lantus", "levemir", "mixtard", "novomix", "novorapid", "toujeo", "tresiba", "velosulin")
      metformin <- c("metformin", "diabex", "diaformin", "dianben", "fortamet", "glucophage", "glumetza", "obimet", "riomet")
      other_OAD <- c("amaryl", "daonil", "deamelin-s", "diamicron", "glibenclamide", "glibenese", "gliclazide", "glimepiride", "glipizide", "gliquidone", "glucotrol", "glurenorm", "glyburide", "glyclpramide", "minodiab", "tolbutamide",
                           "nateglinide", "prandin", "repaglinide", "starlix", "actos", "avandia", "pioglitazone", "rosiglitazone", "eucreas", "galvus", "janumet", "januvia", "linagliptin", "onglyza", "saxagliptin", "sitagliptin", "tradjenta", "vildagliptin",
                           "bydureon", "byetta", "dulaglutide", "exenatide", "liraglutide", "lixisenatide", "lyxumia", "ozempic", "semaglutide", "trulicity", "victoza", "canagliflozin", "dapagliflozin", "empagliflozin", "farxiga", "forxiga", "invokana", "jardiance", "glucobay", "acarbose")
      lucentis <- c("lucentis", "ranibizumab")
      statins <- c("statin", "atorvastatin", "crestor", "lescol", "lipitor", "lipostat","pravastatin", "rosuvastatin", "simvastatin", "zocor")
      anti_hypertensives <- c( "accupro", "accuretic", "acepril", "acezide", "capoten", "capozide", "captopril", "carace", "cilazapril", "coversyl", "enalapril", "fosinopril",  "gopten", "imidapril", "innovace", "innozide", "lisinopril", "lopace", "moexipril", "perdix", "perindopril",
                               "quinapril", "ramipril", "staril", "tanatril", "tarka", "trandolapril", "triapin", "tritace", "vascase", "zestoretic", "zestril", "amias", "aprovel", "candesartan", "cozaar", "diovan", "eprosartan", "exforge", "irbesartan",  "losartan", "olmesartan",
                               "olmetec", "telmisartan", "teveten", "valsartan", "adalat", "adizem", "amlodipine", "amlostine", "angitil", "cardene", "coracten", "dilcardia", "dilzem", "diltiazem", "felodipine", "felotens", "istin", "lacidipine", "lercanidipine", "motens", "nicardipine",
                               "nifedipine", "nisoldipine", "plendil", "prescal", "securon", "slozem", "tildiem", "vascalpha", "verapamil", "verapress", "viazem", "zanidip", "zemtard", "amilamont", "amiloride", "aprinox", "bendroflumethiazide", "bumetanide", "burinex", "chlortalidone",
                               "co-amilofruse", "co-amilozide", "co-flumactone", "co-tenidone", "co-triamterzide", "coversyl",  "cyclopenthiazide", "diurexan", "dyazide", "dytide",	"eplerenone", "frumil", "frusene", "furosemide", "hygroton", "idactone", "indapamide", "inspra", "kalspare",
                               "lasilactone", "lasix", "metenix",  "metolazone", "moduret", "natrilix", "navidrex", "prestim", "spironolactone", "tenoret", "torasemide", "torem", "triamterene", "xipamide")

      # main function
      output <- factor(dplyr::if_else(stringr::str_detect(x_lower, paste(insulins, collapse = "|")), "Insulin",
                                      dplyr::if_else(stringr::str_detect(x_lower, paste(metformin, collapse = "|")), "Metformin",
                                                     dplyr::if_else(stringr::str_detect(x_lower, paste(oral_diab_other, collapse = "|")), "Other OAD",
                                                                    dplyr::if_else(stringr::str_detect(x_lower, paste(lucentis, collapse = "|")), "Lucentis",
                                                                                   dplyr::if_else(stringr::str_detect(x_lower, paste(statins, collapse = "|")), "Statins",
                                                                                                  dplyr::if_else(stringr::str_detect(x_lower, paste(anti_hypertensives, collapse = "|")), "Anti-hypertensives", "Other"))))))
      )
  }

    return(output)


}

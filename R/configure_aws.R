library(sparklyr)

conf <- spark_config()

conf$sparklyr.defaultPackages <- "org.apache.hadoop:hadoop-aws:3.3.6"

sc <- spark_connect(master = "local", config = conf)

path <- "s3a://cmspartb/Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2021.csv"


ptb <- spark_read_csv(sc, "ptb_spark",
                      path =  path,
                      memory = TRUE,
                      columns = list(
                        Rndrng_NPI = "character",
                        Rndrng_Prvdr_Last_Org_Name = "character",
                        Rndrng_Prvdr_First_Name = "character",
                        Rndrng_Prvdr_MI = "character",
                        Rndrng_Prvdr_Crdntls = "character",
                        Rndrng_Prvdr_Gndr = "character",
                        Rndrng_Prvdr_Ent_Cd = "character",
                        Rndrng_Prvdr_St1 = "character",
                        Rndrng_Prvdr_St2 = "character",
                        Rndrng_Prvdr_City = "character",
                        Rndrng_Prvdr_State_Abrvtn = "character",
                        Rndrng_Prvdr_State_FIPS = "character",
                        Rndrng_Prvdr_Zip5 = "character",
                        Rndrng_Prvdr_RUCA = "character",
                        Rndrng_Prvdr_RUCA_Desc = "character",
                        Rndrng_Prvdr_Cntry = "character",
                        Rndrng_Prvdr_Type = "character",
                        Rndrng_Prvdr_Mdcr_Prtcptg_Ind = "character",
                        HCPCS_Cd = "character",
                        HCPCS_Desc = "character",
                        HCPCS_Drug_Ind = "character",
                        Place_Of_Srvc = "character",
                        Tot_Benes = "character",
                        Tot_Srvcs = "character",
                        Tot_Bene_Day_Srvcs = "character",
                        Avg_Sbmtd_Chrg = "character",
                        Avg_Mdcr_Alowd_Amt = "character",
                        Avg_Mdcr_Pymt_Amt = "character",
                        Avg_Mdcr_Stdzd_Amt = "character"
                        ),
                      infer_schema = FALSE
)

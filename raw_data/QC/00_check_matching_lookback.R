
# Matching checks ---------------------------------------------------------
# Checking that mean lookback is similar between cohorts and that it
# never exceeds 60 days.
# Findings:  
# MmeanDiff MmedDiff MmaxDiff MminDiff
# -28.64178      -29       -1      -59
# Check how many negative patients were matchd to each positive
Neg_PA %>% group_by(test_patient) %>% summarise(count=n()) %>% group_by(count) %>% summarise(n =n())

# Get list of positive patients in negative cohort
pos_match2 <- Neg_PA %>% select(test_patient) %>% unique %>% rename(PATIENT_ID=test_patient)

# Get list of positive patients and their lookback lengths
pos_lookback = merge(pos_match2, Pos_PA, by = "PATIENT_ID") %>%
  select(PATIENT_ID, lookback_days) %>%
  rename(PosLookback=lookback_days,
         PosPatientID=PATIENT_ID)

# Get list of negative patients and their lookback lengths		 
x <- Neg_PA %>% select(patient_id, LOOKBACK_DAYS, test_patient) %>%
  rename(NegLookback = LOOKBACK_DAYS,
         NegPatientID=patient_id,
         PosPatientID=test_patient)

# Merge the two together and calculate difference in lookback		 
y <- merge(x, pos_lookback, by="PosPatientID" )
y <- y %>% mutate(DiffLookback = PosLookback - NegLookback )

# Summarise the differences per postive patient
diff <- y %>% group_by(PosPatientID) %>% summarise(meanDiff = mean(DiffLookback),
                                                   medDiff = median(DiffLookback),
                                                   maxDiff = max(DiffLookback),
                                                   minDiff= min(DiffLookback)
)

# Aggregate the differences across all positve patients
diff %>% summarise(MmeanDiff = mean(meanDiff),
                   MmedDiff = median(medDiff),
                   MmaxDiff = max(maxDiff),
                   MminDiff = min(minDiff)
)


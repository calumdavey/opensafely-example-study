"""
Dataset definition for the example study.

Extracts: date of birth, sex, and whether each patient has ever had
a recorded diagnosis of diabetes, hypertension, or asthma.
"""

from ehrql import Dataset
from ehrql.tables.core import clinical_events, patients

dataset = Dataset()

# All registered patients
dataset.define_population(patients.exists_for_patient())

# Demographics
dataset.date_of_birth = patients.date_of_birth
dataset.sex = patients.sex

# Conditions — SNOMED CT codes present in the dummy data
diabetes_codes     = ["73211009", "44054006"]
hypertension_codes = ["38341003"]
asthma_codes       = ["195967001"]

dataset.has_diabetes = clinical_events.where(
    clinical_events.snomedct_code.is_in(diabetes_codes)
).exists_for_patient()

dataset.has_hypertension = clinical_events.where(
    clinical_events.snomedct_code.is_in(hypertension_codes)
).exists_for_patient()

dataset.has_asthma = clinical_events.where(
    clinical_events.snomedct_code.is_in(asthma_codes)
).exists_for_patient()

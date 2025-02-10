# UKHSA E. coli and MSSA Analysis Project

## Project Overview
Analysis of E. coli and MSSA bloodstream infection rates across Integrated Care Board (ICB) regions, incorporating healthcare interactions, deprivation indices, and regional infection patterns.

## Data Sources

### Primary Datasets
- E. coli and MSSA bloodstream infection rates by ICB
- GP appointments (2018-present)
- Hospital bed occupancy (2010-2024, Q1-Q2)
- Index of Multiple Deprivation (IMD) 2019
- Antibiotic prescribing patterns (April 2015-December 2023)

### Dataset Limitations

#### GDPR Constraints
- Demographic data (age/sex distributions) unavailable due to GDPR Article 9(1)
- Limited ability to join multiple datasets using ICB region as common variable

#### GP Appointments
- Limited temporal scope (2018-present)
- Data structure variations between CCGs and ICBs
- Reduced granularity due to CCG-ICB conversion process

#### Hospital Bed Occupancy
- Lacks ICB identifiers
- Quarterly structure limits temporal analysis precision
- Missing geographical identifiers

#### IMD Data
- Based on 2019 census (pre-COVID)
- Reduced granularity from LSOA to ICB conversion
- Limited to composite measures
- Four-year update interval (2015, 2019)

#### Antibiotic Prescribing
- No pre-2015 data
- Data suppression for patient counts <5
- Broad age groupings limit precise analysis

## Future Improvements
1. Establish protocols for accessing demographic stratifications while maintaining GDPR compliance
2. Implement advanced forecasting techniques and machine learning models
3. Develop custom IMD weightings
4. Enhance data granularity preservation during geographical conversions

## Technical Notes
- Data conversion required between CCG and ICB systems
- Manual intervention needed for some CCG-ICB mappings
- Monthly and annual data points available for temporal analysis
- Integration possible between GP appointments and infection rates datasets

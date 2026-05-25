# Data on Inmates

## Information on Datasets

### 1. Segregation – Detailed Dataset 

-   **Dataset Name**: `b01df`
-   **Description**: This dataset details the use of segregation for all individuals in Ontario’s adult correctional system. Each row of data represents a placement in segregation conditions.
-   **Variables**:
    -   `_id`: (Row ID) - Auto-generated variable from the Ontario Data Catalogue. Cannot be used to link datasets.
    -   `EndFiscalYear`: (End Fiscal Year) - The end year of the fiscal year period (e.g., 2023 for fiscal year 2022/23).
    -   `UniqueIndividual_ID`: (Unique Identifier) - A random number assigned to an individual (format: YYYY-XXXXX-SG).
    -   `Gender`: Preferred gender identity of the inmate (Female, Male).
    -   `Region_AtTimeOfPlacement`: Region on file at the time of placement (Central, Eastern, Northern, Toronto, Western).
    -   `Region_MostRecentPlacement`: Region on file for the most recent placement during the reporting period.
    -   `Age_Category`: Age category of the inmate as of April 1st (18 to 24, 25 to 49, 50+).
    -   `NumberConsecutiveDays_Segregation`: Duration of the segregation placement in days.
    -   `SegReason_SecurityOfInstitution_SafetyOfOthers`: (Boolean) Reason for placement was to protect security of institution or safety of others.
    -   `SegReason_InmateNeedsProtection`: (Boolean) Reason for placement was that the inmate was in need of protection.
    -   `SegReason_InmateNeedsProtection_Medical`: (Boolean) Reason for placement was for medical protection.
    -   `SegReason_SecurityOfInstitution_SafetyOfOthers_Medical`: (Boolean) Reason for placement was for medical security/safety reasons.
    -   `SegReason_Disciplinary_Segregation`: (Boolean) Reason for placement was disciplinary segregation.
    -   `SegReason_InmateRefuseSearch_Scan`: (Boolean) Reason for placement was due to inmate refusing a search or scan.
    -   `SegReason_Other`: (Boolean) Non-disciplinary segregation placements identified through internal review (only applicable to 2024+ data).
    -   `MentalHealth_Alert`: (Boolean) Presence of a verified or unverified mental health alert.
    -   `SuicideRisk_Alert`: (Boolean) Presence of a suicide risk alert.
    -   `SuicideWatch_Alert`: (Boolean) Presence of a suicide watch alert (a type of suicide risk alert).
    -   `Number_Of_Placements`: Total number of placements with the same characteristics.

------------------------------------------------------------------------

### 2. Segregation – Detailed Total Days 

-   **Dataset Name**: `b02df`
-   **Description**: This dataset details the aggregate number of days unique individuals spent in segregation conditions.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `UniqueIndividual_ID`: (Unique Identifier)
    -   `Gender`: (Gender)
    -   `Region_MostRecentPlacement`: (Region Based On Most Recent Placement)
    -   `Age_Category`: (Age Category)
    -   `TotalAggregatedDays_Segregation`: Total number of days a unique individual spent in segregation during the fiscal year.

------------------------------------------------------------------------

### 3. Segregation Placements - Alerts and Hold Flags by Institution

-   **Dataset Name**: `b03df`
-   **Description**: This dataset details the number of segregation placements, by alerts and hold flags, for each institution.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Region_AtTimeOfPlacement`: (Region At Time Of Placement)
    -   `Institution_AtTimeOfPlacement`: Institution on file at the time of placement.
    -   `Alert_Type`: Type of alert on file (Immigration Hold Flag, Mental Health Alert, Serious Mental Illness Alert, Suicide Risk Alert, Suicide Watch Alert, Transgender Alert).
    -   `Alert_Presence`: (Yes/No) Presence of the indicated alert.
    -   `Number_SegregationPlacements`: Total number of segregation placements.

------------------------------------------------------------------------

### 4. Segregation Placements - Maximum, Median and Mode Consecutive Durations by Region

-   **Dataset Name**: `b04df`
-   **Description**: This dataset details the maximum, median, and mode of consecutive days of segregation placements by region and gender.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Region_AtTimeOfPlacement`: (Region At Time Of Placement)
    -   `Gender`: (Gender) - Includes "All" category.
    -   `Measure`: The type of measure calculated (Maximum, Median, Mode).
    -   `NumberConsecutiveDays_Segregation`: The value of the calculated measure.

------------------------------------------------------------------------

### 5. Segregation Placements - Distribution of Placements by Duration

-   **Dataset Name**: `b05df`
-   **Description**: This dataset details the distribution of placements based on the number of consecutive days in segregation.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Consecutive_Duration`: Binned duration of the segregation placement (e.g., 1 day, 2 days, 6 to 10 days).
    -   `Number_SegregationPlacements`: Total number of segregation placements within that duration bin.

------------------------------------------------------------------------

### 6. Segregation Placements - Reason for Placement by Institution

-   **Dataset Name**: `b06df`
-   **Description**: This dataset details the reasons for segregation placements, broken down by institution.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Region_AtTimeOfPlacement`: (Region At Time Of Placement)
    -   `Institution_AtTimeOfPlacement`: (Institution At Time Of Placement)
    -   `Gender`: (Gender)
    -   `Reason`: Identified reason for placement (e.g., Close Confinement, Inmate Needs Protection).
    -   `Number_SegregationPlacements`: Total number of segregation placements for that reason.

------------------------------------------------------------------------

### 7. Segregation Placements - Alerts and Hold Flags by Gender

-   **Dataset Name**: `b07df`
-   **Description**: This dataset details the total number of segregation placements with and without specific alerts, by gender.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Alert_Type`: (Alert Type)
    -   `Gender`: (Gender)
    -   `Number_Segregation_Placements_Without_Alert`: Total placements without the indicated alert.
    -   `Number_Segregation_Placements_With_Alert`: Total placements with the indicated alert.

------------------------------------------------------------------------

### 8. Segregation Placements – Median and Mode Consecutive Durations by Institution 

-   **Dataset Name**: `b08df`
-   **Description**: This dataset details the median and mode of consecutive days of segregation placements by institution and gender.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Region_AtTimeOfPlacement`: (Region At Time Of Placement)
    -   `Institution_AtTimeOfPlacement`: (Institution At Time Of Placement)
    -   `Gender`: (Gender) - Includes "All" category.
    -   `Measure`: The type of measure calculated (Median, Mode).
    -   `NumberConsecutiveDays_Segregation`: The value of the calculated measure.

------------------------------------------------------------------------

### 9. Individuals in Segregation - Number of Placements in Segregation

-   **Dataset Name**: `b09df`
-   **Description**: This dataset details the number of times individuals were placed in segregation, by gender.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `NumberPlacements_Segregation`: The total number of placements an individual had (e.g., 1 placement, 2 placements).
    -   `Gender`: (Gender)
    -   `NumberIndividuals_Segregation`: The total number of unique individuals for that placement count.

------------------------------------------------------------------------

### 10. Individuals in Segregation and Restrictive Confinement - Total Individuals

-   **Dataset Name**: `c01df`
-   **Description**: This dataset details the total number of unique individuals in custody, restrictive confinement, and segregation, by gender.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Gender`: (Gender)
    -   `NumberIndividuals_InCustody`: Total unique individuals in custody.
    -   `NumberIndividuals_RestrictiveConfinement`: Total unique individuals in restrictive confinement.
    -   `NumberIndividuals_Segregation`: Total unique individuals in segregation.

------------------------------------------------------------------------

### 11. Individuals in Segregation and Restrictive Confinement by Institution 

-   **Dataset Name**: `c02df`
-   **Description**: This dataset details the total number of unique individuals in restrictive confinement and segregation by institution, region, and gender.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Region_MostRecentPlacement`: (Region Based On Most Recent Placement)
    -   `Institution_MostRecentPlacement`: (Institution Based On Most Recent Placement)
    -   `Gender`: (Gender)
    -   `NumberIndividuals_RestrictiveConfinement`: Total unique individuals in restrictive confinement.
    -   `NumberIndividuals_Segregation`: Total unique individuals in segregation.

------------------------------------------------------------------------

### 12. Individuals in Segregation and Restrictive Confinement by Race and Gender 

-   **Dataset Name**: `c03df`
-   **Description**: This dataset details the total number of unique individuals in custody, restrictive confinement, and segregation by race and gender.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Race`: Race of the inmate.
    -   `Gender`: (Gender)
    -   `NumberIndividuals_InCustody`: (Total Number of Individuals in Custody)
    -   `NumberIndividuals_RestrictiveConfinement`: (Total Number of Individuals in Restrictive Confinement)
    -   `NumberIndividuals_Segregation`: (Total Number of Individuals in Segregation Conditions)

------------------------------------------------------------------------

### 13. Individuals in Segregation and Restrictive Confinement by Race and Region 

-   **Dataset Name**: `c04df`
-   **Description**: This dataset details the total number of individuals in restrictive confinement and segregation by race and region.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Region_MostRecentPlacement`: (Region Based On Most Recent Placement)
    -   `Race`: (Race)
    -   `NumberIndividuals_RestrictiveConfinement`: (Total Number of Individuals in Restrictive Confinement)
    -   `NumberIndividuals_Segregation`: (Total Number of Individuals in Segregation Conditions)

------------------------------------------------------------------------

### 14. Individuals in Segregation and Restrictive Confinement by Religion and Region 

-   **Dataset Name**: `c05df`
-   **Description**: This dataset details the total number of individuals in custody, restrictive confinement, and segregation by religion/spiritual affiliation and region.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Region_MostRecentPlacement`: (Region Based On Most Recent Placement)
    -   `Religion`: The religious or spiritual affiliation of the inmate.
    -   `NumberIndividuals_RestrictiveConfinement`: (Total Number of Individuals in Restrictive Confinement)
    -   `NumberIndividuals_Segregation`: (Total Number of Individuals in Segregation Conditions)

------------------------------------------------------------------------

### 15. Individuals in Segregation and Restrictive Confinement by Age Category and Region 

-   **Dataset Name**: `c06df`
-   **Description**: This dataset details the total number of individuals in restrictive confinement and segregation by age category and region.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Region_MostRecentPlacement`: (Region Based On Most Recent Placement)
    -   `Age_Category`: (Age Category)
    -   `NumberIndividuals_RestrictiveConfinement`: (Total Number of Individuals in Restrictive Confinement)
    -   `NumberIndividuals_Segregation`: (Total Number of Individuals in Segregation Conditions)

------------------------------------------------------------------------

### 16. Individuals in Segregation and Restrictive Confinement - Alerts and Hold Flags 

-   **Dataset Name**: `c07df`
-   **Description**: This dataset details the total number of individuals in custody, restrictive confinement, and segregation by type of alert on file.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Alert_Type`: (Alert Type)
    -   `Gender`: (Gender)
    -   `NumberIndividuals_InCustody`: (Total Number of Individuals in Custody)
    -   `NumberIndividuals_RestrictiveConfinement`: (Total Number of Individuals in Restrictive Confinement)
    -   `NumberIndividuals_Segregation`: (Total Number of Individuals in Segregation Conditions)

------------------------------------------------------------------------

### 17. Individuals in Segregation and Restrictive Confinement by Religion and Gender 

-   **Dataset Name**: `c08df`
-   **Description**: This dataset details the total number of individuals in custody, restrictive confinement, and segregation by religion/spiritual affiliation and gender.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Religion`: (Religion)
    -   `Gender`: (Gender)
    -   `NumberIndividuals_InCustody`: (Total Number of Individuals in Custody)
    -   `NumberIndividuals_RestrictiveConfinement`: (Total Number of Individuals in Restrictive Confinement)
    -   `NumberIndividuals_Segregation`: (Total Number of Individuals in Segregation Conditions)

------------------------------------------------------------------------

### 18. Individuals in Segregation and Restrictive Confinement by Age Category and Gender 

-   **Dataset Name**: `c09df`
-   **Description**: This dataset details the total number of individuals in custody, restrictive confinement, and segregation by age category and gender.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Age_Category`: (Age Category)
    -   `Gender`: (Gender)
    -   `NumberIndividuals_InCustody`: (Total Number of Individuals in Custody)
    -   `NumberIndividuals_RestrictiveConfinement`: (Total Number of Individuals in Restrictive Confinement)
    -   `NumberIndividuals_Segregation`: (Total Number of Individuals in Segregation Conditions)

------------------------------------------------------------------------

### 19. Segregation and Restrictive Confinement - Maximum, Median and Mode Aggregate Durations by Institution 

-   **Dataset Name**: `c10df`
-   **Description**: This dataset details the maximum, median, and mode of aggregate days an individual spent in segregation and restrictive confinement by institution.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Region_MostRecentPlacement`: (Region Based On Most Recent Placement)
    -   `Institution_MostRecentPlacement`: (Institution Based On Most Recent Placement)
    -   `Gender`: (Gender) - Includes "All" category.
    -   `Measure`: (Maximum, Median, Mode)
    -   `TotalAggregatedDays_RestrictiveConfinement`: The value of the calculated measure for restrictive confinement.
    -   `TotalAggregatedDays_Segregation`: The value of the calculated measure for segregation.

------------------------------------------------------------------------

### 20. Individuals in Segregation and Restrictive Confinement by Aggregate Length of Time 

-   **Dataset Name**: `c11df`
-   **Description**: This dataset details the distribution of individuals by the total number of days spent in restrictive confinement or segregation during the fiscal year.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Aggregate_Duration`: Binned aggregate duration of days (e.g., 1 day, 2 days, 6 to 10 days).
    -   `NumberIndividuals_RestrictiveConfinement`: Total unique individuals in restrictive confinement for that duration bin.
    -   `NumberIndividuals_Segregation`: Total unique individuals in segregation for that duration bin.

------------------------------------------------------------------------

### 21. Segregation and Restrictive Confinement - Maximum, Median and Mode Aggregate Durations by Region 

-   **Dataset Name**: `c12df`
-   **Description**: This dataset details the maximum, median, and mode of aggregate days an individual spent in segregation and restrictive confinement by region and gender.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `EndFiscalYear`: (End Fiscal Year)
    -   `Region_MostRecentPlacement`: (Region Based On Most Recent Placement)
    -   `Gender`: (Gender) - Includes "All" category.
    -   `Measure`: (Maximum, Median, Mode)
    -   `TotalAggregatedDays_RestrictiveConfinement`: The value of the calculated measure for restrictive confinement.
    -   `TotalAggregatedDays_Segregation`: The value of the calculated measure for segregation.

------------------------------------------------------------------------

### 22. Deaths in Custody – Detailed Dataset 

-   **Dataset Name**: `d01df`
-   **Description**: This dataset details all custodial-related deaths that occurred within a calendar year. Each row reflects a custodial-related death.
-   **Variables**:
    -   `_id`: (Row ID) - Auto-generated variable from the Ontario Data Catalogue. Cannot be used to link datasets.
    -   `Year`: (Calendar Year) - Calendar year reporting period.
    -   `UniqueIndividual_ID`: (Unique Identifier) - A random number assigned to an individual (format: YYYY-XXXXX-DC).
    -   `Region_AtTimeOfDeath`: (Region At Time Of Death) - Region on file at the time of the individual's death (Central, Eastern, Northern, Toronto, Western).
    -   `HousingUnit_Type`: (Housing Unit Type) - The location or conditions of confinement of the individual prior to death (General Population, Segregation, Specialized Care, Protective Custody Outside of a Facility).
    -   `MedicalCauseOfDeath`: (Medical Cause of Death) - The medical cause of death as determined by the Office of the Chief Coroner (Drug Toxicity, Natural Causes, Other, Unknown / To Be Determined).
    -   `MeansOfDeath`: (Means of Death) - The means of death as determined by the Office of the Chief Coroner (Accident, Homicide, Natural Causes, Suicide, Undetermined, To Be Determined).

------------------------------------------------------------------------

### 23. Deaths in Custody - Gender

-   **Dataset Name**: `d02df`
-   **Description**: This dataset details all custodial-related deaths by gender.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `Year`: (Calendar Year)
    -   `Gender`: (Gender) - Preferred gender identity of the individual (Female, Male).
    -   `Number_CustodialDeaths`: (Number of Custodial Deaths) - Number of deaths where an individual was under the ministry’s supervision.

------------------------------------------------------------------------

### 24. Deaths in Custody - Race 

-   **Dataset Name**: `d03df`
-   **Description**: This dataset details all custodial-related deaths by race.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `Year`: (Calendar Year)
    -   `Race`: (Race) - The race of the individual (Another Race Category, Black, East Asian, Indigenous, Latino, Middle Eastern, More Than One Reported Race Category, South Asian, Unknown Or Not Reported, White).
    -   `Number_CustodialDeaths`: (Number of Custodial Deaths)

------------------------------------------------------------------------

### 25. Deaths in Custody - Religion

-   **Dataset Name**: `d04df`
-   **Description**: This dataset details all custodial-related deaths by religion or spiritual affiliation.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `Year`: (Calendar Year)
    -   `Religion`: (Religion) - The religious or spiritual affiliation of the individual (Another Religion/Spiritual Affiliation, Buddhist, Christian, Hindu, Indigenous Spirituality, Jewish, Muslim, More Than One Reported Religion or Spiritual Affiliation, No Religion, Sikh, Unknown Or Not Reported).
    -   `Number_CustodialDeaths`: (Number of Custodial Deaths)

------------------------------------------------------------------------

### 26. Deaths in Custody - Age Category 

-   **Dataset Name**: `d05df`
-   **Description**: This dataset details all custodial-related deaths by age category.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `Year`: (Calendar Year)
    -   `Age_Category`: (Age Category) - Age category of the individual at the time of death (18 to 24, 25 to 49, 50+).
    -   `Number_CustodialDeaths`: (Number Custodial Deaths)

------------------------------------------------------------------------

### 27. Deaths in Custody - Alerts and Medical Cause of Death 

-   **Dataset Name**: `d06df`
-   **Description**: This dataset details all custodial-related deaths by alert and medical cause of death.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `Year`: (Calendar Year)
    -   `Alert_Type`: (Alert Type) - The type of alert present on an inmate's file (Mental Health Alert, Suicide Risk Alert, Suicide Watch Alert).
    -   `MedicalCauseOfDeath`: (Medical Cause of Death)
    -   `Number_CustodialDeaths`: (Number Custodial Deaths)

------------------------------------------------------------------------

### 28. Deaths in Custody - Alerts and Housing Unit Type

-   **Dataset Name**: `d07df`
-   **Description**: This dataset details all custodial-related deaths by alert and housing unit.
-   **Variables**:
    -   `_id`: (Row ID)
    -   `Year`: (Calendar Year)
    -   `Alert_Type`: (Alert Type)
    -   `HousingUnit_Type`: (Housing Unit Type) - The location or conditions of confinement of the individual prior to death (General Population / Protective Custody Unit, Outside of a Facility, Segregation Conditions, Specialized Care Unit).
    -   `Number_CustodialDeaths`: (Number Custodial Deaths)

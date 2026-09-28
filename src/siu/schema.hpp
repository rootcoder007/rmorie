// SPDX-License-Identifier: AGPL-3.0-or-later
//
// schema.hpp -- the panel-reviewed SIU field schema. Every field is handled
// uniformly; `is_count` flags fields that must be COUNTED (0 is a real answer,
// e.g. witness-officer-only cases), and `desc` is the per-field instruction
// used when reading the report ONE COLUMN AT A TIME (the per-field granularity
// that stops a model skimming the report once and hallucinating 60 answers).
// This is the single source of truth so no column is special-cased.
#pragma once
#include <string>
#include <vector>

namespace siu {

struct Field {
    std::string name;
    bool is_count;      // count distinct entities; 0 is valid
    std::string desc;   // what to look for, for a focused per-field read
};

// The canonical reviewed columns. `number_of_subject_officers` is the ONE
// spelling written back (officers/officials/officils variants normalise to it).
inline const std::vector<Field>& schema() {
    static const std::vector<Field> kFields = {
        {"police_service", false,
         "the police service(s) whose officials the SIU investigated"},
        {"date_of_incident_iso", false,
         "the date the incident occurred (ISO YYYY-MM-DD)"},
        {"date_siu_notified_iso", false,
         "the date the SIU was notified/invoked (ISO YYYY-MM-DD)"},
        {"date_of_director_decision_iso", false,
         "the date of the Director's decision (ISO YYYY-MM-DD)"},
        {"siu_investigators", true,
         "the number of SIU investigators assigned"},
        {"siu_forensics_investigators", true,
         "the number of SIU forensic investigators assigned"},
        {"number_of_witness_officials", true,
         "the count of distinct WITNESS officers/officials (WO)"},
        {"number_of_civilian_witnesses", true,
         "the count of distinct civilian witnesses (CW)"},
        {"number_of_subject_officers", true,
         "the count of distinct SUBJECT officers/officials (SO); a "
         "witness-officer-only investigation is 0"},
        {"age_affected", false,
         "the age (or age range) of the affected person/complainant"},
        {"sex_gender_affected", false,
         "the sex/gender of the affected person/complainant"},
        {"charges_recommended", false,
         "whether charges were recommended/laid, and against whom"},
        {"directors_name", false, "the name of the SIU Director who decided"},
        {"location_of_call", false,
         "the location/address where the incident occurred"},
        {"specific_injuries", false,
         "the specific injuries the affected person sustained"},
        {"relevant_legislation", false,
         "the legislation/Criminal Code sections the Director analysed"},
    };
    return kFields;
}

}  // namespace siu

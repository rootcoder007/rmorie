// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Parser test on a synthetic report shaped like a real SIU director's report
// (section headers, split "SO\n#1" tags, signature block). Asserts every
// schema field. No framework: non-zero exit on failure.
#include <cstdio>
#include <cstdlib>
#include <string>

#include "siu/parse.hpp"

static const char* kReport = R"(SIU Director's Report - Case # 23-OFD-001

The Investigation
Notification of the SIU
On January 6, 2023, the Barrie Police Service contacted the SIU with the
following information. It reported that on January 5, 2023, in the City of
Barrie, a 34-year-old man was seriously injured during an arrest.

The Team
Number of SIU Investigators assigned: 3
Number of SIU Forensic Investigators assigned: 1

Civilian Witnesses
CW
#1 Interviewed
CW
#2 Interviewed

Subject Officials
SO
#1 Declined interview
SO
#2 Interviewed

Witness Officials
WO
#1 Interviewed
WO
#2 Interviewed
WO
#3 Interviewed

Incident Narrative
On January 5, 2023, officers of the Barrie Police Service responded to a
disturbance. The Complainant suffered a fractured left arm during the arrest.

Nature of Injuries / Treatment
The Complainant was diagnosed with a fractured left arm and treated in hospital.

Relevant Legislation
Section 25, Criminal Code - Protection of persons acting under authority
Section 34, Criminal Code - Defence of person

Analysis and Director's Decision
There are no reasonable grounds to believe that either subject official
committed a criminal offence. No charges shall issue.

Date: April 28, 2023

Joseph Martino
Director
Special Investigations Unit
)";

static int fails = 0;
static void expect(const char* field, const std::string& got,
                   const std::string& want) {
    if (got != want) {
        std::printf("FAIL %-32s got=[%s] want=[%s]\n", field, got.c_str(),
                    want.c_str());
        ++fails;
    } else {
        std::printf("ok   %-32s [%s]\n", field, got.c_str());
    }
}

int main() {
    const auto f = siu::parse_report_text(kReport);
    auto g = [&](const char* k) {
        auto it = f.find(k);
        return it == f.end() ? std::string("<absent>") : it->second;
    };

    expect("_language", g("_language"), "en");
    expect("police_service", g("police_service"), "Barrie Police Service");
    expect("date_of_incident_iso", g("date_of_incident_iso"), "2023-01-05");
    expect("date_siu_notified_iso", g("date_siu_notified_iso"), "2023-01-06");
    expect("date_of_director_decision_iso", g("date_of_director_decision_iso"),
           "2023-04-28");
    expect("siu_investigators", g("siu_investigators"), "3");
    expect("siu_forensics_investigators", g("siu_forensics_investigators"), "1");
    expect("number_of_subject_officers", g("number_of_subject_officers"), "2");
    expect("number_of_witness_officials", g("number_of_witness_officials"), "3");
    expect("number_of_civilian_witnesses", g("number_of_civilian_witnesses"), "2");
    expect("age_affected", g("age_affected"), "34");
    expect("sex_gender_affected", g("sex_gender_affected"), "man");
    expect("charges_recommended", g("charges_recommended"), "FALSE");
    expect("directors_name", g("directors_name"), "Joseph Martino");
    expect("location_of_call", g("location_of_call"), "City of Barrie");
    // injuries + legislation: assert substrings (free text)
    if (g("specific_injuries").find("fractured left arm") == std::string::npos) {
        std::printf("FAIL specific_injuries got=[%s]\n",
                    g("specific_injuries").c_str());
        ++fails;
    } else {
        std::printf("ok   specific_injuries               [%s]\n",
                    g("specific_injuries").c_str());
    }
    if (g("relevant_legislation").find("Criminal Code") == std::string::npos) {
        std::printf("FAIL relevant_legislation got=[%s]\n",
                    g("relevant_legislation").c_str());
        ++fails;
    } else {
        std::printf("ok   relevant_legislation            [%s]\n",
                    g("relevant_legislation").c_str());
    }

    // date helper edge cases
    if (siu::to_iso_date("March 3 2021") != "2021-03-03") ++fails;
    if (siu::to_iso_date("2020-12-31") != "2020-12-31") ++fails;
    if (!siu::to_iso_date("garbage").empty()) ++fails;

    if (fails) {
        std::printf("%d parse test(s) FAILED\n", fails);
        return 1;
    }
    std::printf("all parse tests passed\n");
    return 0;
}

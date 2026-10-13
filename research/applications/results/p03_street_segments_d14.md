## P3 interference on the street-segment network: Toronto Police Service Division 14

Data: City of Toronto Centreline (road segments: Local, Collector, Minor Arterial, Major Arterial), joined where two segments share an intersection, inside the extent of Division 14's 2024 break-and-enters; the largest connected piece has 3,264 segments and 5,650 adjacencies (degree 1 to 7, mean 3.46). Of 513 located incidents, 506 lie within 150 m of a segment midpoint and are counted on the nearest.

The hot set S is the repeat segments, those with two or more incidents in the year: 120 of the 3,264 segments (3.7%), holding 325 of the 506 snapped incidents (64%); 181 segments had exactly one.

| quantity | value |
|---|---|
| edges cut | 418 |
| volume of S | 430 |
| volume of the complement | 10,870 |
| conductance h(S) | 0.9721 |
| Rayleigh quotient of the test vector | 1.011 |
| lambda_2 of the normalised Laplacian | 0.0004195 |
| lambda_2 <= Rayleigh <= 2 h(S) holds | TRUE |

Reading: the conductance 0.972 is the share of the hot segments' adjacencies that lead off the hot set; on average 97% of a hot segment's neighbouring segments are not hot. A patrol or crackdown on the hot segments therefore puts most of any displacement or diffusion on streets that were not treated, and a comparison of treated hot segments with their neighbours compares treated streets with spillover-exposed ones. lambda_2 = 0.0004195 is the spectral gap of the division's street graph (small: long chains of local streets), checked against the proved easy direction lambda_2 <= 2 h(S).

### Exposure probabilities, 120 treated segments allocated at random (2,000 draws)

| class | smallest_probability | mean_probability | largest_probability |
|---|---|---|---|
| untreated, no treated neighbour | 0.7315 | 0.8467 | 0.942 |
| untreated, a treated neighbour | 0.0245 | 0.1165 | 0.2305 |
| treated | 0.022 | 0.03676 | 0.052 |

Every segment needs a positive probability of every class for the Horvitz-Thompson spillover estimator to exist; 0 segments drew no 'untreated with a treated neighbour' allocation in 2,000 draws (the smallest probabilities set the variance). Snapping to the nearest midpoint is an approximation for long segments; the hot set was not randomised, so no effect is estimated here.

Theorems: Research.P3.testVec_orth, Research.P3.testVec_dnorm, Research.P3.testVec_dirichlet, Research.P3.rayleigh_testVec, Research.P3.rayleigh_le_two_conductance, Research.P3.lambda2_le_rayleigh_testVec, Research.P3.cheeger_easy.

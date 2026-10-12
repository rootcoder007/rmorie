# P3: the Toronto neighbourhood network as the interference design

Source: City of Toronto Neighbourhoods (158 polygons, WGS84) and Neighbourhood Improvement Areas; queen contiguity (475 edges, degrees 3 to 11, mean 6).
The ledger names street segments; the public files carry neighbourhoods, so this is the neighbourhood-level
version of the design computation and is labelled as such.

## The NIA cut (33 of 158 neighbourhoods)

| quantity | value |
|---|---|
| edges cut | 98 |
| volume of S | 190 |
| volume of the complement | 760 |
| conductance h(S) | 0.515789473684211 |
| Rayleigh quotient of the test vector | 0.644736842105263 |
| lambda_2 of the normalised Laplacian | 0.0157451440809582 |
| lambda_2 <= Rayleigh <= 2 h(S) holds | TRUE |

The conductance is the share of the NIAs' edge volume that leaves the set: 0.516. That is a scatter, not a cluster: about half of every NIA's neighbours are outside the set, so a treatment confined to the NIAs puts most of its contiguity spillover on neighbourhoods that are not treated, and a comparison of NIAs with their neighbours is a comparison of treated places with the places that received the spillover. lambda_2 = 0.01575 is the spectral gap of the whole city graph; the proved chain lambda_2 <= E(f)/D(f) <= 2 h(S) is checked above, and the gap between 0.01575 and 1.03 is the room the Cheeger hard direction (lambda_2 >= h^2/2, still open in Lean) would close.

## Exposure probabilities under a completely randomised allocation of 33 treated neighbourhoods (5,000 draws)

| class | smallest_probability | largest_probability |
|---|---|---|
| untreated, no treated neighbour | 0.0558 | 0.3956 |
| untreated, a treated neighbour | 0.389 | 0.7394 |
| treated | 0.1948 | 0.2242 |

The ten neighbourhoods with the rarest exposure class (the ones that set the Horvitz-Thompson variance):

| neighbourhood | degree | nia | p_untreated_no_treated_neighbour | p_untreated_treated_neighbour | p_treated |
|---|---|---|---|---|---|
| 098 | 11 | FALSE | 0.0558 | 0.7394 | 0.2048 |
| 052 | 9 | FALSE | 0.0858 | 0.705 | 0.2092 |
| 128 | 9 | FALSE | 0.0868 | 0.7078 | 0.2054 |
| 039 | 9 | FALSE | 0.088 | 0.694 | 0.218 |
| 120 | 9 | FALSE | 0.0882 | 0.7048 | 0.207 |
| 038 | 9 | FALSE | 0.0898 | 0.7044 | 0.2058 |
| 040 | 9 | FALSE | 0.0898 | 0.6972 | 0.213 |
| 071 | 9 | FALSE | 0.0912 | 0.7 | 0.2088 |
| 095 | 9 | FALSE | 0.0918 | 0.7 | 0.2082 |
| 056 | 9 | FALSE | 0.094 | 0.6934 | 0.2126 |

Every neighbourhood must have a positive probability of each exposure class for the Horvitz-Thompson estimator of the spillover effect to exist (Research.P3's design condition); the table shows the classes and the smallest probabilities, which set the variance. The outcomes (break and enter by neighbourhood) are not used here: the NIAs were not randomised, so an HT estimate would not be one.


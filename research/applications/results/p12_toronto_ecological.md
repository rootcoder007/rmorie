## P12 ecological inference: Toronto's neighbourhoods, 2021 Census

Data: City of Toronto Neighbourhood Profiles, 2021 Census, 158-neighbourhood model (25% sample, population in private households, low income by the after-tax low-income measure, LIM-AT).

Knowing only each neighbourhood's group share p and low-income share q, a group's low-income rate there lies in [max(0, (p + q - 1)/p), min(1, q/p)], both ends attainable (Research.P12.dd_bounds, Cells.ends_attained), and the city-wide rate's bounds are the population-weighted means (dd_aggregate_bounds).

### Checked against the answer the census publishes

The profiles also publish low income by age, so the bounds computed from the age share and the overall share alone can be checked against the true rate in every neighbourhood:

| group | group_share | mean_width | inside | neighbourhoods | city_lower | city_upper | city_truth |
|---|---|---|---|---|---|---|---|
| children 0-17 | 0.167 | 0.716 | 158 | 158 | 0 | 0.697 | 0.145 |
| seniors 65+ | 0.165 | 0.727 | 158 | 158 | 0 | 0.702 | 0.169 |
| adults 18-64 | 0.667 | 0.19 | 158 | 158 | 0.00476 | 0.198 | 0.119 |

The true rate lies inside the bounds in 474 of 474 neighbourhood-group checks (allowing the census's rounding of counts to multiples of five), as the theorem requires, and the city-wide truths (0.145, 0.169, 0.119) lie inside the aggregate intervals. The intervals are wide where the group is small: children are 17% of residents, so their bounds average 0.72 wide.

### Applied where the census publishes no cross-tabulation: visible minority

Visible minority residents are 56% of the city's private-household population. Their city-wide low-income rate lies in [0.007, 0.237] from neighbourhood shares alone; the neighbourhood-level correlation of visible-minority share and low-income share is 0.52, which by itself says nothing about whether visible-minority residents are more often low income than others (Robinson 1950; Research.P12.ecological_ge: the ecological correlation overstates whenever the within-neighbourhood part is orthogonal). The narrowest neighbourhood intervals are where the group is nearly everyone:

| neighbourhood | visible_minority_share | low_income_share | lower | upper | width |
|---|---|---|---|---|---|
| Morningside Heights | 0.964 | 0.0571 | 0.0222 | 0.0593 | 0.037 |
| Milliken | 0.95 | 0.146 | 0.102 | 0.154 | 0.0522 |
| Highland Creek | 0.785 | 0.0642 | 0 | 0.0817 | 0.0817 |
| Agincourt North | 0.922 | 0.123 | 0.0493 | 0.134 | 0.0847 |
| Malvern West | 0.919 | 0.097 | 0.0176 | 0.106 | 0.0879 |
| Centennial Scarborough | 0.486 | 0.0441 | 0 | 0.0908 | 0.0908 |
| West Rouge | 0.671 | 0.0619 | 0 | 0.0923 | 0.0923 |
| Steeles | 0.915 | 0.148 | 0.0689 | 0.162 | 0.0927 |

Theorems: Research.P12.pq_ge, Research.P12.pq_le_p, Research.P12.pq_le_q, Research.P12.dd_bounds, Research.P12.Cells.ends_attained, Research.P12.dd_aggregate_bounds, Research.P12.ecological_ge.

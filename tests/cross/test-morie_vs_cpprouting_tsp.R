skip_if_not_installed("cppRouting")
skip_if_not_installed("TSP")

test_that("ShortestPath and TrafficAssignment match cppRouting", {
  E <- rbind(c(1, 2, 4), c(1, 3, 1), c(3, 2, 2), c(2, 4, 1), c(3, 4, 5), c(4, 5, 3), c(2, 5, 6), c(3, 5, 9))
  g <- cppRouting::makegraph(data.frame(from = E[, 1], to = E[, 2], cost = E[, 3]), directed = TRUE)
  ref <- cppRouting::get_distance_matrix(g, from = 1, to = 1:5)
  expect_equal(ShortestPath(5, E, 1)$distance, unname(as.vector(ref)))
  links <- rbind(c(1, 2, 10, 100), c(2, 4, 12, 60), c(1, 3, 15, 120), c(3, 4, 8, 90), c(2, 3, 3, 40), c(4, 5, 5, 200))
  od <- rbind(c(1, 4, 180), c(1, 5, 60), c(2, 5, 50))
  mine <- TrafficAssignment(5, links, od, alpha = 0.15, beta = 4, gap = 1e-12)
  gr <- cppRouting::makegraph(data.frame(from = links[, 1], to = links[, 2], cost = links[, 3]), directed = TRUE,
                              capacity = links[, 4], alpha = 0.15, beta = 4)
  ref <- cppRouting::assign_traffic(gr, from = od[, 1], to = od[, 2], demand = od[, 3], algorithm = "fw",
                                    max_gap = 1e-9, verbose = FALSE)
  key <- paste(ref$data$from, ref$data$to)
  flow_ref <- ref$data$flow[match(paste(links[, 1], links[, 2]), key)]
  expect_equal(mine$flow, flow_ref, tolerance = 1e-5)
})

test_that("TspTour exact optimum is no longer than TSP's best heuristic tour", {
  pts <- rbind(c(0, 0), c(3, 1), c(6, 0), c(7, 4), c(4, 6), c(1, 5), c(2, 2.5), c(5, 3), c(3.5, 4))
  D <- as.matrix(stats::dist(pts))
  ex <- TspTour(D)
  set.seed(1)  # two_opt improves a random initial tour
  ref <- TSP::solve_TSP(TSP::TSP(D), method = "two_opt")
  expect_lte(ex$length, TSP::tour_length(ref) + 1e-12)
  expect_equal(ex$length, TSP::tour_length(TSP::TOUR(ex$tour, tsp = TSP::TSP(D))))
})

// SPDX-License-Identifier: AGPL-3.0-or-later
//
// morie_spatial.cpp -- RcppArmadillo hot loops for spatial-voting models.
// Conventions match src/morie/_spatial_voting.py exactly (unweighted A-M,
// linear-projection bootstrap, omega-only Wordfish standardisation).

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;

namespace {

inline double pnorm_std(double x) {
    return 0.5 * std::erfc(-x / std::sqrt(2.0));
}

inline double clip01(double p) {
    if (p < 1e-10) return 1e-10;
    if (p > 1.0 - 1e-10) return 1.0 - 1e-10;
    return p;
}

inline double clip(double x, double lo, double hi) {
    return std::min(std::max(x, lo), hi);
}

}  // namespace

// [[Rcpp::export]]
Rcpp::List morie_spatial_nominate_iterate_cpp(arma::mat votes,
                                              arma::mat X,
                                              arma::vec w,
                                              arma::mat nv,
                                              arma::mat mid,
                                              double beta,
                                              int max_iter) {
    const arma::uword n_leg = votes.n_rows;
    const arma::uword n_votes = votes.n_cols;
    const arma::uword n_dims = X.n_cols;

    arma::umat mask = arma::umat(n_leg, n_votes, arma::fill::zeros);
    for (arma::uword i = 0; i < n_leg; ++i) {
        for (arma::uword j = 0; j < n_votes; ++j) {
            mask(i, j) = std::isnan(votes(i, j)) ? 0u : 1u;
        }
    }

    for (int iter = 0; iter < max_iter; ++iter) {
        for (arma::uword j = 0; j < n_votes; ++j) {
            arma::uvec valid_idx;
            valid_idx.set_size(n_leg);
            arma::uword nv_count = 0;
            for (arma::uword i = 0; i < n_leg; ++i) {
                if (mask(i, j)) valid_idx(nv_count++) = i;
            }
            if (nv_count < 2) continue;
            valid_idx.resize(nv_count);

            arma::rowvec yea_center = mid.row(j);
            arma::rowvec nay_center = mid.row(j);
            arma::uword n_yea = 0, n_nay = 0;
            arma::rowvec sum_yea(n_dims, arma::fill::zeros);
            arma::rowvec sum_nay(n_dims, arma::fill::zeros);
            for (arma::uword k = 0; k < nv_count; ++k) {
                const arma::uword i = valid_idx(k);
                if (votes(i, j) == 1.0) {
                    sum_yea += X.row(i);
                    ++n_yea;
                } else {
                    sum_nay += X.row(i);
                    ++n_nay;
                }
            }
            if (n_yea > 0) yea_center = sum_yea / static_cast<double>(n_yea);
            if (n_nay > 0) nay_center = sum_nay / static_cast<double>(n_nay);

            arma::rowvec direction = yea_center - nay_center;
            const double dn = std::sqrt(arma::dot(direction, direction));
            if (dn > 1e-10) nv.row(j) = direction / dn;
            mid.row(j) = 0.5 * (yea_center + nay_center);
        }

        for (arma::uword i = 0; i < n_leg; ++i) {
            arma::uword n_valid = 0;
            arma::rowvec acc(n_dims, arma::fill::zeros);
            for (arma::uword j = 0; j < n_votes; ++j) {
                if (!mask(i, j)) continue;
                arma::rowvec target;
                if (votes(i, j) == 1.0) {
                    target = mid.row(j) + 0.5 * nv.row(j);
                } else {
                    target = mid.row(j) - 0.5 * nv.row(j);
                }
                acc += target;
                ++n_valid;
            }
            if (n_valid >= 2) X.row(i) = acc / static_cast<double>(n_valid);
        }

        double max_norm = 0.0;
        for (arma::uword i = 0; i < n_leg; ++i) {
            const double rn = std::sqrt(arma::dot(X.row(i), X.row(i)));
            if (rn > max_norm) max_norm = rn;
        }
        if (max_norm > 1.0) X /= max_norm;
    }

    double ll_final = 0.0;
    long long total = 0, correct = 0;
    arma::vec cp(n_votes, arma::fill::zeros);
    for (arma::uword j = 0; j < n_votes; ++j) {
        cp(j) = arma::dot(nv.row(j), mid.row(j));
        for (arma::uword i = 0; i < n_leg; ++i) {
            if (!mask(i, j)) continue;
            arma::rowvec dy = X.row(i) - (mid.row(j) + 0.5 * nv.row(j));
            arma::rowvec dn = X.row(i) - (mid.row(j) - 0.5 * nv.row(j));
            double d_yea = 0.0, d_nay = 0.0;
            for (arma::uword k = 0; k < n_dims; ++k) {
                d_yea += w(k) * dy(k) * dy(k);
                d_nay += w(k) * dn(k) * dn(k);
            }
            const double u_diff =
                beta * (std::exp(-0.5 * d_yea) - std::exp(-0.5 * d_nay));
            const double p = clip01(pnorm_std(u_diff));
            const double y = votes(i, j);
            ll_final += y * std::log(p) + (1.0 - y) * std::log(1.0 - p);
            const bool pred_yea = p > 0.5;
            if ((pred_yea && y == 1.0) || (!pred_yea && y == 0.0)) ++correct;
            ++total;
        }
    }
    const double gmp = total > 0 ? static_cast<double>(correct) / total : 0.0;

    return List::create(_["ideal_points"] = X,
                        _["normal_vectors"] = nv,
                        _["midpoints"] = mid,
                        _["dim_weights"] = w,
                        _["cutpoints"] = cp,
                        _["log_lik"] = ll_final,
                        _["gmp"] = gmp,
                        _["n_dims"] = static_cast<int>(n_dims));
}

// [[Rcpp::export]]
arma::mat morie_spatial_emirt_theta_update_cpp(arma::mat theta,
                                               const arma::mat& a,
                                               const arma::vec& d,
                                               const arma::mat& votes) {
    const arma::uword n_leg = votes.n_rows;
    const arma::uword n_votes = votes.n_cols;
    const arma::uword n_dims = theta.n_cols;
    const arma::mat Ip = arma::eye<arma::mat>(n_dims, n_dims);

    for (arma::uword i = 0; i < n_leg; ++i) {
        std::vector<arma::uword> valid;
        valid.reserve(n_votes);
        for (arma::uword j = 0; j < n_votes; ++j) {
            if (!std::isnan(votes(i, j))) valid.push_back(j);
        }
        if (valid.empty()) continue;

        const arma::uword m = valid.size();
        arma::mat a_i(m, n_dims);
        arma::vec y_i(m), d_i(m);
        for (arma::uword k = 0; k < m; ++k) {
            const arma::uword j = valid[k];
            a_i.row(k) = a.row(j);
            y_i(k) = votes(i, j);
            d_i(k) = d(j);
        }

        arma::vec eta = a_i * theta.row(i).t() + d_i;
        for (arma::uword k = 0; k < m; ++k) eta(k) = clip(eta(k), -20.0, 20.0);
        arma::vec p = 1.0 / (1.0 + arma::exp(-eta));
        arma::vec resid = y_i - p;
        arma::vec wv = p % (1.0 - p) + 1e-10;

        arma::mat H = a_i.t() * (a_i.each_col() % wv) + Ip;
        arma::vec g = a_i.t() * resid;
        theta.row(i) += arma::solve(H, g, arma::solve_opts::fast).t();
    }
    return theta;
}

// [[Rcpp::export]]
Rcpp::List morie_spatial_smacof_step_cpp(arma::mat X, const arma::mat& D,
                                         arma::mat W) {
    const arma::uword n = X.n_rows;
    W.diag().zeros();

    arma::vec v_diag = arma::sum(W, 1);
    arma::mat V = arma::diagmat(v_diag);
    arma::mat V_inv = arma::pinv(V);

    arma::mat dX(n, n, arma::fill::zeros);
    for (arma::uword i = 0; i < n; ++i) {
        for (arma::uword j = i + 1; j < n; ++j) {
            arma::rowvec diff = X.row(i) - X.row(j);
            const double dij = std::sqrt(arma::dot(diff, diff));
            dX(i, j) = dij;
            dX(j, i) = dij;
        }
    }

    arma::mat B(n, n, arma::fill::zeros);
    for (arma::uword i = 0; i < n; ++i) {
        double row_sum = 0.0;
        for (arma::uword j = 0; j < n; ++j) {
            if (i == j) continue;
            if (dX(i, j) > 1e-12) {
                B(i, j) = -W(i, j) * D(i, j) / dX(i, j);
                row_sum += B(i, j);
            }
        }
        B(i, i) = -row_sum;
    }

    arma::mat X_new = V_inv * B * X;

    for (arma::uword i = 0; i < n; ++i) {
        for (arma::uword j = i + 1; j < n; ++j) {
            arma::rowvec diff = X_new.row(i) - X_new.row(j);
            const double dij = std::sqrt(arma::dot(diff, diff));
            dX(i, j) = dij;
            dX(j, i) = dij;
        }
    }
    double stress = 0.0;
    for (arma::uword i = 0; i < n; ++i) {
        for (arma::uword j = 0; j < n; ++j) {
            const double r = D(i, j) - dX(i, j);
            stress += W(i, j) * r * r;
        }
    }
    stress *= 0.5;

    return List::create(_["coordinates"] = X_new, _["stress"] = stress,
                        _["distances"] = dX);
}

// [[Rcpp::export]]
Rcpp::List morie_spatial_classical_mds_cpp(const arma::mat& D, int n_dims) {
    const arma::uword n = D.n_rows;
    arma::mat A = D % D;
    arma::mat H = arma::eye<arma::mat>(n, n) - arma::ones<arma::mat>(n, n) / static_cast<double>(n);
    arma::mat B = -0.5 * H * A * H;

    arma::vec evals;
    arma::mat evecs;
    arma::eig_sym(evals, evecs, B);
    evals = arma::reverse(evals);
    evecs = arma::fliplr(evecs);

    arma::vec pos = evals.head(n_dims);
    pos.transform([](double v) { return v < 0.0 ? 0.0 : v; });
    arma::mat coords = evecs.cols(0, n_dims - 1) * arma::diagmat(arma::sqrt(pos));

    arma::mat d_model(n, n, arma::fill::zeros);
    for (arma::uword i = 0; i < n; ++i) {
        for (arma::uword j = i + 1; j < n; ++j) {
            arma::rowvec diff = coords.row(i) - coords.row(j);
            const double dij = std::sqrt(arma::dot(diff, diff));
            d_model(i, j) = dij;
            d_model(j, i) = dij;
        }
    }
    // v0.9.5.6+: Kruskal stress-1 normalises by sum(D^2), not sum(d_model^2).
    double num = 0.0, den = 0.0;
    for (arma::uword i = 0; i < n; ++i) {
        for (arma::uword j = 0; j < n; ++j) {
            if (D(i, j) > 0.0) {
                const double r = d_model(i, j) - D(i, j);
                num += r * r;
                den += D(i, j) * D(i, j);
            }
        }
    }
    const double stress = den > 0.0 ? std::sqrt(num / den) : 0.0;

    const double total_abs = arma::accu(arma::abs(evals));
    const double fit = total_abs > 0.0 ? arma::accu(pos) / total_abs : 0.0;

    return List::create(_["coordinates"] = coords,
                        _["eigenvalues"] = evals.head(n_dims),
                        _["stress"] = stress, _["fit"] = fit,
                        _["B_matrix"] = B);
}

// [[Rcpp::export]]
arma::vec morie_spatial_wordfish_omega_update_cpp(const arma::mat& dtm,
                                                  const arma::vec& psi,
                                                  const arma::vec& alpha,
                                                  const arma::vec& beta,
                                                  arma::vec omega) {
    const arma::uword n_docs = dtm.n_rows;
    const arma::uword n_words = dtm.n_cols;

    for (arma::uword i = 0; i < n_docs; ++i) {
        double g = 0.0, h = -1.0;
        const double psi_i = psi(i);
        const double om_i = omega(i);
        for (arma::uword w = 0; w < n_words; ++w) {
            double eta = psi_i + alpha(w) + beta(w) * om_i;
            if (eta > 20.0) eta = 20.0;
            else if (eta < -20.0) eta = -20.0;
            const double mu = std::exp(eta);
            const double bw = beta(w);
            g += bw * (dtm(i, w) - mu);
            h -= bw * bw * mu;
        }
        omega(i) = om_i - g / h;
    }

    const double m = arma::mean(omega);
    const double s = arma::stddev(omega);
    omega = (omega - m) / (s + 1e-12);
    return omega;
}

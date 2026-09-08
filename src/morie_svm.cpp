// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Native SMO solver for support vector machines.
//
// Replaces the delegation to e1071, which is a wrapper around LIBSVM, so the
// formulation implemented here is LIBSVM's own, from Chang & Lin, "LIBSVM: A
// Library for Support Vector Machines" (last updated 2022-08-23):
//
//   C-SVC dual, Eq. (2):
//       min_alpha  0.5 alpha^T Q alpha - e^T alpha
//       s.t.       y^T alpha = 0,  0 <= alpha_i <= C
//       with       Q_ij = y_i y_j K(x_i, x_j)
//
//   eps-SVR dual, Eq. (11): the same solver over 2l variables with
//       y' = [+1 ... +1, -1 ... -1]
//       p  = [eps - z_1 ... eps - z_l, eps + z_1 ... eps + z_l]
//   so no separate solver is needed; the fitted coefficient is alpha_i - alpha*_i.
//
//   Optimality, Eq. (17)-(18). With
//       I_up(alpha)  = {t | alpha_t < C, y_t = 1  or  alpha_t > 0, y_t = -1}
//       I_low(alpha) = {t | alpha_t < C, y_t = -1 or  alpha_t > 0, y_t = 1}
//       m(alpha) = max_{t in I_up}  -y_t grad_t f,
//       M(alpha) = min_{t in I_low} -y_t grad_t f
//   a feasible alpha is stationary iff m(alpha) <= M(alpha); we stop when
//       m(alpha) - M(alpha) <= tol.                                  Eq. (19)
//
//   Working set selection WSS 1, Eq. (20)-(21), from Fan, Chen & Lin (2005):
//       a_ts = K_tt + K_ss - 2 K_ts,   b_ts = -y_t grad_t f + y_s grad_s f > 0
//       i = argmax_t { -y_t grad_t f | t in I_up }
//       j = argmin_t { -b_it^2 / abar_it | t in I_low, -y_t grad_t f < -y_i grad_i f }
//   which picks the pair that approximately minimises the objective, rather
//   than the maximal-violating pair alone.
//
// Kernels are LIBSVM's four (README, -t):
//       linear      u'v
//       polynomial  (gamma u'v + coef0)^degree
//       RBF         exp(-gamma |u - v|^2)
//       sigmoid     tanh(gamma u'v + coef0)
//
// The gradient is maintained incrementally (Eq. 23) so each iteration costs
// two kernel columns rather than a full pass over Q.
//
// References
//   Boser, Guyon & Vapnik (1992), COLT '92, 144-152.
//   Cortes & Vapnik (1995), Machine Learning 20(3), 273-297.
//   Platt (1998), Sequential Minimal Optimization, MSR-TR-98-14.
//   Fan, Chen & Lin (2005), JMLR 6, 1889-1918.

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

using namespace Rcpp;

namespace {

const double TAU = 1e-12;

struct Kernel {
  const double* X;      // column-major, l x n
  int l;
  int n;
  int type;             // 0 linear, 1 poly, 2 rbf, 3 sigmoid
  double gamma;
  double coef0;
  double degree;
  std::vector<double> sq;   // squared norms, for the RBF expansion
  // Columns are cached on first use, not precomputed: the working set
  // concentrates on the support vectors, so only a fraction of the l columns
  // is ever touched. Precomputing the whole l x l matrix costs O(l^2 n)
  // up front and loses badly at larger l -- which is why LIBSVM caches
  // columns rather than the matrix. Capped at ~256 MB; past that, columns
  // beyond the cap are recomputed instead of stored.
  mutable std::vector<std::vector<double> > col;
  mutable std::vector<char> have;
  mutable size_t cached_bytes = 0;
  static const size_t kCacheBudget = 256u * 1024u * 1024u;

  void init() {
    if (type == 2) {
      sq.assign(l, 0.0);
      for (int i = 0; i < l; ++i) {
        double s = 0.0;
        for (int k = 0; k < n; ++k) {
          const double v = X[i + static_cast<size_t>(k) * l];
          s += v * v;
        }
        sq[i] = s;
      }
    }
    col.assign(l, std::vector<double>());
    have.assign(l, 0);
  }

  // Full kernel column for i, cached when there is room.
  const double* column(int i) const {
    if (have[i]) return col[i].data();
    const size_t bytes = static_cast<size_t>(l) * sizeof(double);
    std::vector<double> c(l);
    for (int j = 0; j < l; ++j) c[j] = raw(i, j);
    if (cached_bytes + bytes > kCacheBudget) {
      scratch = c;                 // no room: hand back a transient copy
      return scratch.data();
    }
    cached_bytes += bytes;
    col[i] = c;
    have[i] = 1;
    return col[i].data();
  }
  mutable std::vector<double> scratch;

  inline double dot(int i, int j) const {
    double s = 0.0;
    for (int k = 0; k < n; ++k)
      s += X[i + static_cast<size_t>(k) * l] * X[j + static_cast<size_t>(k) * l];
    return s;
  }

  inline double raw(int i, int j) const {
    switch (type) {
      case 0: return dot(i, j);
      case 1: return std::pow(gamma * dot(i, j) + coef0, degree);
      case 2: return std::exp(-gamma * (sq[i] + sq[j] - 2.0 * dot(i, j)));
      default: return std::tanh(gamma * dot(i, j) + coef0);
    }
  }

  inline double eval(int i, int j) const {
    if (have[i]) return col[i][j];
    if (have[j]) return col[j][i];
    return raw(i, j);
  }
};

// Solves the dual over `nv` variables. For SVR nv = 2l and variable v maps to
// sample v % l; for classification nv = l and the map is the identity.
struct Solver {
  const Kernel* K;
  int nv;
  int l;
  std::vector<double> y;     // +-1 per variable
  std::vector<double> p;     // linear term
  double C;
  double tol;
  int max_iter;

  std::vector<double> alpha;
  std::vector<double> G;     // gradient of f
  int iters = 0;

  inline int smp(int v) const { return v < l ? v : v - l; }

  // Q_vw = y_v y_w K(smp(v), smp(w))
  inline double Q(int v, int w) const {
    return y[v] * y[w] * K->eval(smp(v), smp(w));
  }

  bool select(int& out_i, int& out_j) const {
    double Gmax = -std::numeric_limits<double>::infinity();
    int i = -1;
    for (int t = 0; t < nv; ++t) {
      const bool up = (y[t] > 0 && alpha[t] < C - 1e-12) ||
                      (y[t] < 0 && alpha[t] > 1e-12);
      if (!up) continue;
      const double v = -y[t] * G[t];
      if (v > Gmax) { Gmax = v; i = t; }
    }
    if (i < 0) return false;

    double Gmin = std::numeric_limits<double>::infinity();
    double best = std::numeric_limits<double>::infinity();
    int j = -1;
    const double Kii = K->eval(smp(i), smp(i));
    for (int t = 0; t < nv; ++t) {
      const bool low = (y[t] > 0 && alpha[t] > 1e-12) ||
                       (y[t] < 0 && alpha[t] < C - 1e-12);
      if (!low) continue;
      const double gt = -y[t] * G[t];
      if (gt < Gmin) Gmin = gt;
      if (gt >= Gmax) continue;                 // b_it must be > 0
      const double b = Gmax - gt;                       // b_it, Eq. (20)
      double a = Kii + K->eval(smp(t), smp(t)) -
                 2.0 * K->eval(smp(i), smp(t));          // a_it, Eq. (20)
      if (a <= 0) a = TAU;                                // abar_it
      const double obj = -(b * b) / a;
      if (obj < best) { best = obj; j = t; }
    }
    if (j < 0) return false;
    if (Gmax - Gmin < tol) return false;        // Eq. (19)
    out_i = i; out_j = j;
    return true;
  }

  void solve() {
    alpha.assign(nv, 0.0);
    G.assign(p.begin(), p.end());               // alpha = 0  =>  grad = p
    int i, j;
    for (iters = 0; iters < max_iter; ++iters) {
      if (!select(i, j)) break;

      const double Kii = K->eval(smp(i), smp(i));
      const double Kjj = K->eval(smp(j), smp(j));
      const double Kij = K->eval(smp(i), smp(j));
      const double old_i = alpha[i], old_j = alpha[j];

      // LIBSVM's quad_coef is QD[i] + QD[j] -/+ 2 Q_ij with the *y-signed*
      // Q_ij = y_i y_j K_ij, so both branches reduce to K_ii + K_jj - 2 K_ij:
      // the squared distance between the two points in feature space, which
      // is non-negative. Using +2 K_ij here inflates the step denominator
      // under an all-positive kernel (RBF still converges, just slower) but
      // is outright wrong when K_ij < 0, as a linear kernel allows -- the
      // solver then cycles until max_iter.
      const double quad_dist = Kii + Kjj - 2.0 * Kij;
      if (y[i] != y[j]) {
        double quad = quad_dist;
        if (quad <= 0) quad = TAU;
        const double delta = (-G[i] - G[j]) / quad;
        const double diff = alpha[i] - alpha[j];
        alpha[i] += delta;
        alpha[j] += delta;
        if (diff > 0) {
          if (alpha[j] < 0) { alpha[j] = 0; alpha[i] = diff; }
        } else {
          if (alpha[i] < 0) { alpha[i] = 0; alpha[j] = -diff; }
        }
        if (diff > 0) {
          if (alpha[i] > C) { alpha[i] = C; alpha[j] = C - diff; }
        } else {
          if (alpha[j] > C) { alpha[j] = C; alpha[i] = C + diff; }
        }
      } else {
        double quad = quad_dist;
        if (quad <= 0) quad = TAU;
        const double delta = (G[i] - G[j]) / quad;
        const double sum = alpha[i] + alpha[j];
        alpha[i] -= delta;
        alpha[j] += delta;
        if (sum > C) {
          if (alpha[i] > C) { alpha[i] = C; alpha[j] = sum - C; }
        } else {
          if (alpha[j] < 0) { alpha[j] = 0; alpha[i] = sum; }
        }
        if (sum > C) {
          if (alpha[j] > C) { alpha[j] = C; alpha[i] = sum - C; }
        } else {
          if (alpha[i] < 0) { alpha[i] = 0; alpha[j] = sum; }
        }
      }

      // Incremental gradient update, Eq. (23). Both kernel columns are
      // fetched once and streamed, so this is a memory pass rather than
      // 2 * nv dot products.
      const double di = alpha[i] - old_i, dj = alpha[j] - old_j;
      const double* Ki = K->column(smp(i));
      std::vector<double> Kj_copy;
      {
        const double* kj = K->column(smp(j));
        Kj_copy.assign(kj, kj + l);          // column(i) may reuse scratch
      }
      const double yi = y[i], yj = y[j];
      for (int t = 0; t < nv; ++t) {
        const double yt = y[t];
        const int st = smp(t);
        G[t] += yt * yi * Ki[st] * di + yt * yj * Kj_copy[st] * dj;
      }
    }
  }

  // rho, following LIBSVM's calculate_rho: average over the free variables,
  // else the midpoint of the bracketing bounds.
  double rho() const {
    double ub = std::numeric_limits<double>::infinity();
    double lb = -std::numeric_limits<double>::infinity();
    double sum_free = 0.0;
    int nfree = 0;
    for (int t = 0; t < nv; ++t) {
      const double yG = y[t] * G[t];
      if (alpha[t] >= C - 1e-12) {
        if (y[t] < 0) ub = std::min(ub, yG); else lb = std::max(lb, yG);
      } else if (alpha[t] <= 1e-12) {
        if (y[t] > 0) ub = std::min(ub, yG); else lb = std::max(lb, yG);
      } else {
        ++nfree;
        sum_free += yG;
      }
    }
    if (nfree > 0) return sum_free / nfree;
    if (!std::isfinite(ub) || !std::isfinite(lb)) return 0.0;
    return (ub + lb) / 2.0;
  }
};

Kernel make_kernel(const NumericMatrix& X, int type, double gamma,
                   double coef0, double degree) {
  Kernel k;
  k.X = &X[0];
  k.l = X.nrow();
  k.n = X.ncol();
  k.type = type;
  k.gamma = gamma;
  k.coef0 = coef0;
  k.degree = degree;
  k.init();
  return k;
}

}  // namespace

//' Binary C-SVC via SMO (compiled)
//' @noRd
// [[Rcpp::export]]
List morie_svc_train_cpp(NumericMatrix X, NumericVector y, double C,
                         int kernel_type, double gamma, double coef0,
                         double degree, double tol, int max_iter) {
  Kernel k = make_kernel(X, kernel_type, gamma, coef0, degree);
  const int l = X.nrow();
  Solver s;
  s.K = &k; s.nv = l; s.l = l;
  s.y.assign(y.begin(), y.end());
  s.p.assign(l, -1.0);                 // p = -e for C-SVC
  s.C = C; s.tol = tol; s.max_iter = max_iter;
  s.solve();
  NumericVector coef(l);
  for (int i = 0; i < l; ++i) coef[i] = s.alpha[i] * s.y[i];
  return List::create(_["alpha"] = NumericVector(s.alpha.begin(), s.alpha.end()),
                      _["coef"] = coef,
                      _["rho"] = s.rho(),
                      _["iterations"] = s.iters);
}

//' eps-SVR via SMO (compiled)
//' @noRd
// [[Rcpp::export]]
List morie_svr_train_cpp(NumericMatrix X, NumericVector z, double C,
                         double epsilon, int kernel_type, double gamma,
                         double coef0, double degree, double tol,
                         int max_iter) {
  Kernel k = make_kernel(X, kernel_type, gamma, coef0, degree);
  const int l = X.nrow();
  Solver s;
  s.K = &k; s.nv = 2 * l; s.l = l;
  s.y.assign(2 * l, 1.0);
  s.p.assign(2 * l, 0.0);
  for (int i = 0; i < l; ++i) {
    s.y[i] = 1.0;          s.p[i] = epsilon - z[i];
    s.y[i + l] = -1.0;     s.p[i + l] = epsilon + z[i];
  }
  s.C = C; s.tol = tol; s.max_iter = max_iter;
  s.solve();
  NumericVector coef(l);
  for (int i = 0; i < l; ++i) coef[i] = s.alpha[i] - s.alpha[i + l];
  return List::create(_["coef"] = coef,
                      _["rho"] = s.rho(),
                      _["iterations"] = s.iters);
}

//' Decision values for new data given fitted SVM coefficients (compiled)
//' @noRd
// [[Rcpp::export]]
NumericVector morie_svm_decision_cpp(NumericMatrix SV, NumericVector coef,
                                     double rho, NumericMatrix Xnew,
                                     int kernel_type, double gamma,
                                     double coef0, double degree) {
  const int ls = SV.nrow(), nn = Xnew.nrow(), n = SV.ncol();
  std::vector<double> sq_sv(ls, 0.0), sq_new(nn, 0.0);
  if (kernel_type == 2) {
    for (int i = 0; i < ls; ++i)
      for (int k = 0; k < n; ++k) sq_sv[i] += SV(i, k) * SV(i, k);
    for (int i = 0; i < nn; ++i)
      for (int k = 0; k < n; ++k) sq_new[i] += Xnew(i, k) * Xnew(i, k);
  }
  NumericVector out(nn);
  for (int a = 0; a < nn; ++a) {
    double acc = 0.0;
    for (int b = 0; b < ls; ++b) {
      double d = 0.0;
      for (int k = 0; k < n; ++k) d += Xnew(a, k) * SV(b, k);
      double kv;
      switch (kernel_type) {
        case 0: kv = d; break;
        case 1: kv = std::pow(gamma * d + coef0, degree); break;
        case 2: kv = std::exp(-gamma * (sq_new[a] + sq_sv[b] - 2.0 * d)); break;
        default: kv = std::tanh(gamma * d + coef0); break;
      }
      acc += coef[b] * kv;
    }
    out[a] = acc - rho;
  }
  return out;
}

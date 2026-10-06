// SPDX-License-Identifier: AGPL-3.0-or-later
//
// libmorie -- the morie C++ numeric core (binding-agnostic).
//
// This header is THE single numeric source of truth for morie. It is
// header-only and depends on nothing but the C++ standard library --
// no nanobind, no Rcpp, no numpy. Every function takes plain raw
// pointers + sizes so it can be bound, unchanged, by:
//
//   * nanobind  -> Python  (libmorie/kernels.cpp, libmorie/hawkes.cpp)
//   * Rcpp      -> R       (r-package/morie/src/rcpp_morie.cpp)
//
// Because both languages call into the SAME compiled arithmetic, the
// Python<->R parity bug class (e.g. row-major vs column-major, subtly
// divergent reimplementations) is eliminated by construction.
//
// CANONICAL COPY: libmorie/morie_core.hpp. The R package vendors a
// copy at r-package/morie/src/morie_core.h -- keep the two in sync.

#pragma once

#include <algorithm>
#include <climits>
#include <cmath>
#include <complex>
#include <limits>
#include <cstddef>
#include <cstdint>
#include <random>
#include <vector>

// ---------------------------------------------------------------------------
// Toolchain integrity guards
//
// morie's numeric core assumes the host compiler uses two's complement for
// signed integers (so signed int has no negative zero, INT_MIN is one less
// than -INT_MAX, and signed bit patterns match the C++20 standard model).
// C++20 makes two's complement mandatory; we're on CXX_STD = CXX17 so the
// standard doesn't formally enforce it. Every R-supported compiler (gcc,
// clang, MSVC) on every R-supported platform (x86_64, aarch64, RISC-V) has
// used two's complement for decades, so these static_asserts exist purely
// to fail compile-time on a hypothetical sign-magnitude or
// ones'-complement toolchain rather than silently miscomputing at runtime.
//
// Test: in two's complement, ~0u (bitwise NOT of an unsigned zero) has
// every bit set; reinterpreting that as signed int yields -1 exactly.
// In sign-magnitude or ones'-complement, the bit pattern of -1 differs.
static_assert(static_cast<int>(~0u) == -1,
              "morie requires a two's-complement signed-integer "
              "representation. C++20 mandates this; pre-C++20 compilers "
              "that diverge cannot be used with this package.");
// Bytes must be 8 bits for our raw-pointer / cstddef-based interfaces.
static_assert(CHAR_BIT == 8,
              "morie assumes 8-bit bytes (CHAR_BIT == 8).");

namespace morie::core {

// An optional interrupt hook for the quadratic passes (the Hawkes kernels
// over n events are O(n * window), and a window that covers everything --
// a million identical timestamps -- is O(n^2)). The host installs a
// function that checks for a pending interrupt and throws; with none
// installed (the Python binding) the kernels run exactly as before.
inline void (*&interrupt_hook())() {
    static void (*hook)() = nullptr;
    return hook;
}
inline void poll_interrupt(std::size_t i) {
    if ((i & 1023) == 0) {
        void (*h)() = interrupt_hook();
        if (h) h();
    }
}

inline const double kPi = 3.14159265358979323846;
inline const double kInvSqrt2Pi = 1.0 / std::sqrt(2.0 * kPi);
inline const double kLogSqrt2Pi = 0.5 * std::log(2.0 * kPi);

// Sentinel for an infeasible parameter vector (Hawkes likelihood).
inline const double kBig = 1e12;

// --- summary statistics ------------------------------------------------------

// Base R's algorithm: an extended-precision sum, then one corrective
// pass over the residuals, so the result agrees with mean() to the last
// bit on ordinary data. If the sum overflows although every input is
// finite (rep(1e308, 3) on a platform whose long double is 64-bit), a
// running mean, which cannot overflow, is used instead.
// Running mean: cannot overflow on finite input. The update is written
// as a/k - m/k rather than (a - m)/k because a - m itself overflows for
// inputs of opposite sign near the largest double; each term here is
// bounded by the largest |a|. mean() falls back to it when the
// extended-precision sum overflows, which only happens where long double
// is 64-bit, so it is also reachable directly (through
// rmbl_mean_running()) and tested on every platform.
inline double mean_running(const double *a, std::size_t n) {
    if (n == 0) return std::nan("");
    double m = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        const double k = static_cast<double>(i + 1);
        m += a[i] / k - m / k;
    }
    return m;
}

inline double mean(const double *a, std::size_t n) {
    if (n == 0) return std::nan("");
    long double s = 0.0L;
    for (std::size_t i = 0; i < n; ++i) s += a[i];
    s /= static_cast<long double>(n);
    if (std::isfinite(static_cast<double>(s))) {
        long double t = 0.0L;
        for (std::size_t i = 0; i < n; ++i) t += (a[i] - s);
        s += t / static_cast<long double>(n);
        return static_cast<double>(s);
    }
    for (std::size_t i = 0; i < n; ++i) {
        if (!std::isfinite(a[i])) return static_cast<double>(s);
    }
    return mean_running(a, n);
}

inline double variance(const double *a, std::size_t n, int ddof) {
    if (static_cast<long long>(n) - ddof <= 0) return std::nan("");
    const double m = mean(a, n);
    double sq = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        const double d = a[i] - m;
        sq += d * d;
    }
    return sq / (static_cast<double>(n) - static_cast<double>(ddof));
}

inline double stddev(const double *a, std::size_t n, int ddof) {
    return std::sqrt(variance(a, n, ddof));
}

// Centred two-pass form. The one-pass n*sxy - sx*sy expansion cancels
// catastrophically once the spread is small relative to the mean (wrong at
// the 2nd decimal for CV 1e-7, NaN by 1e-8, |r| > 1 by 1e-15); the centred
// sums are exact to rounding, and the result is clamped to [-1, 1] so that
// rounding can never report a correlation outside the definition.
inline double cor_pearson(const double *x, const double *y, std::size_t n) {
    if (n < 2) return std::nan("");
    const double mx = mean(x, n), my = mean(y, n);
    if (!std::isfinite(mx) || !std::isfinite(my)) return std::nan("");
    long double sxy = 0.0L, sxx = 0.0L, syy = 0.0L;
    for (std::size_t i = 0; i < n; ++i) {
        const long double a = x[i] - mx, b = y[i] - my;
        sxy += a * b;
        sxx += a * a;
        syy += b * b;
    }
    if (sxx <= 0.0L || syy <= 0.0L) return std::nan("");
    long double r = sxy / std::sqrt(sxx * syy);
    if (r > 1.0L) r = 1.0L;
    if (r < -1.0L) r = -1.0L;
    return static_cast<double>(r);
}

inline double euclid_dist(const double *a, const double *b, std::size_t n) {
    double s = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        const double d = a[i] - b[i];
        s += d * d;
    }
    return std::sqrt(s);
}

// --- array-valued kernels (write into a caller-provided buffer) --------------

inline void normal_pdf(const double *x, std::size_t n, double mean_,
                       double sd, double *out) {
    const double inv = 1.0 / sd;
    for (std::size_t i = 0; i < n; ++i) {
        const double z = (x[i] - mean_) * inv;
        out[i] = inv * kInvSqrt2Pi * std::exp(-0.5 * z * z);
    }
}

inline void normal_logpdf(const double *x, std::size_t n, double mean_,
                          double sd, double *out) {
    const double inv = 1.0 / sd;
    const double base = -std::log(sd) - kLogSqrt2Pi;
    for (std::size_t i = 0; i < n; ++i) {
        const double z = (x[i] - mean_) * inv;
        out[i] = base - 0.5 * z * z;
    }
}

inline void trimmed_ipw_weights(const double *treat, const double *propensity,
                                std::size_t n, double trim_lo,
                                double trim_hi, double *out) {
    for (std::size_t i = 0; i < n; ++i) {
        double e = propensity[i];
        if (e < trim_lo) {
            e = trim_lo;
        } else if (e > trim_hi) {
            e = trim_hi;
        }
        out[i] = (treat[i] == 1.0) ? (1.0 / e) : (1.0 / (1.0 - e));
    }
}

// Bootstrap-replicate means: B resamples of size n drawn with
// replacement from `a`, each replicate's mean written to out[b].
// Uses std::mt19937_64 seeded with `seed` -- fully reproducible for a
// given seed. (There is intentionally no pure-numpy equivalent: a
// different RNG would silently change the replicates.)
inline void bootstrap_mean(const double *a, std::size_t n, std::size_t B,
                           unsigned long long seed, double *out) {
    if (n == 0) {
        for (std::size_t b = 0; b < B; ++b) out[b] = std::nan("");
        return;
    }
    std::mt19937_64 rng(seed);
    std::uniform_int_distribution<std::size_t> idx(0, n - 1);
    for (std::size_t b = 0; b < B; ++b) {
        long double s = 0.0L;
        for (std::size_t i = 0; i < n; ++i) {
            s += a[idx(rng)];
        }
        out[b] = static_cast<double>(s / static_cast<long double>(n));
    }
}

// --- Hawkes-process likelihood ----------------------------------------------

// Negative log-likelihood: exponential triggering kernel, constant
// baseline. The O(n) recursion A_i = exp(-beta*dt)*(A_{i-1}+beta) is
// genuinely sequential. Returns kBig for an infeasible parameter set.
inline double hawkes_ll_exp_const(const double *t, std::size_t n, double T,
                                  double a0, double eta, double beta) {
    if (!(1e-6 < eta && eta < 0.999)) return kBig;
    if (!(0.05 < beta && beta < 30.0)) return kBig;
    if (!(-20.0 < a0 && a0 < 20.0)) return kBig;

    const double nu = std::exp(a0);
    if (!std::isfinite(nu) || nu <= 0.0) return kBig;

    double log_sum = 0.0;
    double A = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        double lam_i;
        if (i == 0) {
            lam_i = nu;
        } else {
            const double dt = t[i] - t[i - 1];
            A = std::exp(-beta * dt) * (A + beta);
            lam_i = nu + eta * A;
        }
        if (!std::isfinite(lam_i) || lam_i <= 0.0) return kBig;
        log_sum += std::log(lam_i);
    }

    double integral = nu * T;
    for (std::size_t i = 0; i < n; ++i) {
        integral += eta * (1.0 - std::exp(-beta * (T - t[i])));
    }
    if (!std::isfinite(log_sum) || !std::isfinite(integral)) return kBig;

    return -(log_sum - integral);
}

// Negative log-likelihood: Weibull triggering kernel, constant
// baseline. The Weibull kernel is not memoryless, so there is no O(n)
// recursion -- each event sums over all prior events (O(n^2)).
// Returns kBig for an infeasible parameter set.
inline double hawkes_ll_weibull_const(const double *t, std::size_t n, double T,
                                      double a0, double eta, double alpha,
                                      double lam) {
    if (!(1e-6 < eta && eta < 0.999)) return kBig;
    if (!(0.05 < alpha && alpha < 20.0)) return kBig;
    if (!(1e-3 < lam && lam < 1e3)) return kBig;
    if (!(-20.0 < a0 && a0 < 20.0)) return kBig;

    const double nu = std::exp(a0);

    double log_sum = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        double s = 0.0;
        for (std::size_t j = 0; j < i; ++j) {
            const double x = (t[i] - t[j]) / lam;
            if (x > 1e-12) {
                const double z = std::pow(x, alpha);
                if (z < 700.0) {
                    s += (alpha / lam) * std::pow(x, alpha - 1.0) *
                         std::exp(-z);
                }
            }
        }
        const double lam_at = nu + eta * s;
        if (lam_at <= 0.0) return kBig;
        log_sum += std::log(lam_at);
    }

    double integral = nu * T;
    for (std::size_t i = 0; i < n; ++i) {
        const double u = T - t[i];
        if (u > 0.0) {
            const double x = u / lam;
            integral += eta * (1.0 - std::exp(-std::pow(x, alpha)));
        }
    }
    return -(log_sum - integral);
}

// Negative log-likelihood: Lomax (Omori-type power-law) triggering
// kernel, constant baseline. Like the Weibull kernel this is not
// memoryless -- exact O(n^2). The caller is responsible for the
// parameter bounds (alpha > 1 so log(alpha-1) is finite, c > 0).
inline double hawkes_ll_lomax_const(const double *t, std::size_t n, double T,
                                    double a0, double eta, double alpha,
                                    double c) {
    const double nu = std::exp(a0);
    const double log_const =
        std::log(alpha - 1.0) + (alpha - 1.0) * std::log(c);

    double log_sum = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        double s = 0.0;
        for (std::size_t j = 0; j < i; ++j) {
            const double u = t[i] - t[j];
            const double log_d = log_const - alpha * std::log(u + c);
            s += std::exp(log_d);
        }
        const double lam_i = nu + eta * s;
        if (lam_i <= 0.0) return kBig;
        log_sum += std::log(lam_i);
    }

    double integral = nu * T;
    for (std::size_t i = 0; i < n; ++i) {
        const double u = T - t[i];
        if (u > 0.0) {
            integral += eta * (1.0 - std::pow(c / (u + c), alpha - 1.0));
        }
    }
    return -(log_sum - integral);
}

// Regularized lower incomplete gamma P(a, x): series for x < a+1,
// continued fraction (Lentz) otherwise (Numerical Recipes 6.2). Used
// by the gamma-kernel Hawkes compensator integral.
inline double gamma_cdf_regularized(double a, double x) {
    if (x <= 0.0) return 0.0;
    const double gln = std::lgamma(a);
    if (x < a + 1.0) {
        double ap = a;
        double sum = 1.0 / a;
        double delta = sum;
        for (int i = 0; i < 200; ++i) {
            ap += 1.0;
            delta *= x / ap;
            sum += delta;
            if (std::fabs(delta) < std::fabs(sum) * 1e-12) break;
        }
        return sum * std::exp(-x + a * std::log(x) - gln);
    }
    double b = x + 1.0 - a;
    double c = 1.0 / 1e-30;
    double d = 1.0 / b;
    double h = d;
    for (int i = 1; i <= 200; ++i) {
        const double an = -static_cast<double>(i) * (static_cast<double>(i) - a);
        b += 2.0;
        d = an * d + b;
        if (std::fabs(d) < 1e-30) d = 1e-30;
        c = b + an / c;
        if (std::fabs(c) < 1e-30) c = 1e-30;
        d = 1.0 / d;
        const double delta = d * c;
        h *= delta;
        if (std::fabs(delta - 1.0) < 1e-12) break;
    }
    return 1.0 - std::exp(-x + a * std::log(x) - gln) * h;
}

// Negative log-likelihood: gamma triggering kernel, constant baseline.
// Non-Markovian -- exact O(n^2). The compensator integral uses the
// regularized incomplete gamma above. Returns kBig when infeasible.
inline double hawkes_ll_gamma_const(const double *t, std::size_t n, double T,
                                    double a0, double eta, double alpha,
                                    double beta) {
    if (!(1e-6 < eta && eta < 0.999)) return kBig;
    if (!(0.05 < alpha && alpha < 20.0)) return kBig;
    if (!(0.05 < beta && beta < 30.0)) return kBig;
    if (!(-20.0 < a0 && a0 < 20.0)) return kBig;

    const double nu = std::exp(a0);
    const double log_const = alpha * std::log(beta) - std::lgamma(alpha);

    double log_sum = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        double s = 0.0;
        for (std::size_t j = 0; j < i; ++j) {
            const double u = t[i] - t[j];
            if (u > 1e-300) {
                const double log_d =
                    log_const + (alpha - 1.0) * std::log(u) - beta * u;
                s += std::exp(log_d);
            }
        }
        const double lam_i = nu + eta * s;
        if (lam_i <= 0.0) return kBig;
        log_sum += std::log(lam_i);
    }

    double integral = nu * T;
    for (std::size_t i = 0; i < n; ++i) {
        integral += eta * gamma_cdf_regularized(alpha, beta * (T - t[i]));
    }
    return -(log_sum - integral);
}

// Negative log-likelihood: exponential triggering kernel, sinusoidal
// (time-varying) baseline. The baseline integral is supplied as a
// pre-built grid (grid / grid_vals, length n_grid) and integrated by
// the trapezoidal rule; the caller builds the grid. The O(n) recursion
// on the exponential kernel still applies. Returns kBig if infeasible.
inline double hawkes_ll_exp_sin(const double *t, std::size_t n, double T,
                                double a0, double a1, double a2, double a3,
                                double eta, double beta, const double *grid,
                                const double *grid_vals, std::size_t n_grid) {
    if (!(1e-6 < eta && eta < 0.999)) return kBig;
    if (!(0.05 < beta && beta < 30.0)) return kBig;
    if (!(-20.0 < a0 && a0 < 20.0)) return kBig;

    const double two_pi_y = 2.0 * kPi / 365.25;
    const double T_safe = (T > 1.0) ? T : 1.0;

    double log_sum = 0.0;
    double A = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        const double nu_i = std::exp(a0 + a1 * (t[i] / T_safe) +
                                     a2 * std::sin(two_pi_y * t[i]) +
                                     a3 * std::cos(two_pi_y * t[i]));
        double lam_i;
        if (i == 0) {
            lam_i = nu_i;
        } else {
            const double dt = t[i] - t[i - 1];
            A = std::exp(-beta * dt) * (A + beta);
            lam_i = nu_i + eta * A;
        }
        if (lam_i <= 0.0) return kBig;
        log_sum += std::log(lam_i);
    }

    // baseline integral: trapezoidal rule over the supplied grid
    double g_int = 0.0;
    for (std::size_t k = 0; k + 1 < n_grid; ++k) {
        g_int += 0.5 * (grid_vals[k] + grid_vals[k + 1]) *
                 (grid[k + 1] - grid[k]);
    }
    double integral = g_int;
    for (std::size_t i = 0; i < n; ++i) {
        integral += eta * (1.0 - std::exp(-beta * (T - t[i])));
    }
    return -(log_sum - integral);
}

// Negative log-likelihood: Weibull triggering kernel, sinusoidal
// baseline. Combines the O(n^2) Weibull inner sum with the trapezoid
// baseline integral over the caller-supplied grid. Returns kBig when
// infeasible.
inline double hawkes_ll_weibull_sin(const double *t, std::size_t n, double T,
                                    double a0, double a1, double a2, double a3,
                                    double eta, double alpha, double lam,
                                    const double *grid,
                                    const double *grid_vals,
                                    std::size_t n_grid) {
    if (!(1e-6 < eta && eta < 0.999)) return kBig;
    if (!(0.05 < alpha && alpha < 20.0)) return kBig;
    if (!(1e-3 < lam && lam < 1e3)) return kBig;
    if (!(-20.0 < a0 && a0 < 20.0)) return kBig;

    const double two_pi_y = 2.0 * kPi / 365.25;
    const double T_safe = (T > 1.0) ? T : 1.0;

    double log_sum = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        const double nu_i = std::exp(a0 + a1 * (t[i] / T_safe) +
                                     a2 * std::sin(two_pi_y * t[i]) +
                                     a3 * std::cos(two_pi_y * t[i]));
        double s = 0.0;
        for (std::size_t j = 0; j < i; ++j) {
            const double x = (t[i] - t[j]) / lam;
            if (x > 1e-12) {
                const double z = std::pow(x, alpha);
                if (z < 700.0) {
                    s += (alpha / lam) * std::pow(x, alpha - 1.0) *
                         std::exp(-z);
                }
            }
        }
        const double lam_i = nu_i + eta * s;
        if (!std::isfinite(lam_i) || lam_i <= 0.0) return kBig;
        log_sum += std::log(lam_i);
    }

    double g_int = 0.0;
    for (std::size_t k = 0; k + 1 < n_grid; ++k) {
        g_int += 0.5 * (grid_vals[k] + grid_vals[k + 1]) *
                 (grid[k + 1] - grid[k]);
    }
    double integral = g_int;
    for (std::size_t i = 0; i < n; ++i) {
        const double u = T - t[i];
        if (u > 0.0) {
            const double x = u / lam;
            integral += eta * (1.0 - std::exp(-std::pow(x, alpha)));
        }
    }
    return -(log_sum - integral);
}

// Negative log-likelihood: Lomax (Omori-type power-law) triggering
// kernel, sinusoidal baseline. O(n^2) inner sum + the trapezoid
// baseline integral. The kernel enforces its own parameter bounds
// (alpha in (1.05, 30), c in (1e-4, 100)). Returns kBig if infeasible.
inline double hawkes_ll_lomax_sin(const double *t, std::size_t n, double T,
                                  double a0, double a1, double a2, double a3,
                                  double eta, double alpha, double c,
                                  const double *grid, const double *grid_vals,
                                  std::size_t n_grid) {
    if (!(1e-6 < eta && eta < 0.999)) return kBig;
    if (!(1.05 < alpha && alpha < 30.0)) return kBig;
    if (!(1e-4 < c && c < 100.0)) return kBig;
    if (!(-20.0 < a0 && a0 < 20.0)) return kBig;

    const double two_pi_y = 2.0 * kPi / 365.25;
    const double T_safe = (T > 1.0) ? T : 1.0;
    const double log_const =
        std::log(alpha - 1.0) + (alpha - 1.0) * std::log(c);

    double log_sum = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        const double nu_i = std::exp(a0 + a1 * (t[i] / T_safe) +
                                     a2 * std::sin(two_pi_y * t[i]) +
                                     a3 * std::cos(two_pi_y * t[i]));
        double s = 0.0;
        for (std::size_t j = 0; j < i; ++j) {
            const double u = t[i] - t[j];
            if (u > 0.0) {
                const double log_d = log_const - alpha * std::log(u + c);
                s += std::exp(log_d);
            }
        }
        const double lam_i = nu_i + eta * s;
        if (!std::isfinite(lam_i) || lam_i <= 0.0) return kBig;
        log_sum += std::log(lam_i);
    }

    double g_int = 0.0;
    for (std::size_t k = 0; k + 1 < n_grid; ++k) {
        g_int += 0.5 * (grid_vals[k] + grid_vals[k + 1]) *
                 (grid[k + 1] - grid[k]);
    }
    double integral = g_int;
    for (std::size_t i = 0; i < n; ++i) {
        const double u = T - t[i];
        if (u > 0.0) {
            integral += eta * (1.0 - std::pow(c / (u + c), alpha - 1.0));
        }
    }
    return -(log_sum - integral);
}

// --- user-callback bridge ----------------------------------------------------
//
// A C-ABI function pointer double(double). The numba @cfunc bridge JITs
// a user's Python kernel into exactly this, and the C++ loop below
// calls it natively -- no Python interpreter, no GIL -- inside the
// O(n^2) hot loop.
using HawkesKernelFn = double (*)(double);

// Generic Hawkes negative log-likelihood with a USER-supplied
// triggering kernel: g(dt) is the kernel and G(u) = integral_0^u g.
// Both are plain function pointers, so the O(n^2) excitation sum and
// the compensator call user code at native speed. Returns kBig when
// infeasible.
inline double hawkes_ll_custom(const double *t, std::size_t n, double T,
                               double nu, double eta, HawkesKernelFn g,
                               HawkesKernelFn G) {
    if (!(nu > 0.0) || !std::isfinite(nu)) return kBig;

    double log_sum = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        double s = 0.0;
        for (std::size_t j = 0; j < i; ++j) {
            s += g(t[i] - t[j]);
        }
        const double lam_i = nu + eta * s;
        if (!std::isfinite(lam_i) || lam_i <= 0.0) return kBig;
        log_sum += std::log(lam_i);
    }

    double integral = nu * T;
    for (std::size_t i = 0; i < n; ++i) {
        integral += eta * G(T - t[i]);
    }
    if (!std::isfinite(log_sum) || !std::isfinite(integral)) return kBig;
    return -(log_sum - integral);
}

// --- sub-quadratic Weibull (task #72) ---------------------------------------
//
// Truncated / sliding-window form of hawkes_ll_weibull_const. Beyond
// u = lam * 700^(1/alpha) the kernel's exp(-(u/lam)^alpha) underflows
// to exactly 0 -- those terms contribute nothing, and the exact O(n^2)
// version already skips them via its `z < 700` guard. So cutting the
// inner loop there is EXACT, bit-for-bit identical to the O(n^2)
// result, not an approximation -- it cannot bias the MLE.
//
// Event times are sorted, so the lower bound advances monotonically:
// a two-pointer window gives O(n*w), w = events within the cutoff.
// For a slowly-decaying kernel (small alpha) w -> n and it degrades
// gracefully to O(n^2), still exact.
inline double hawkes_ll_weibull_const_trunc(const double *t, std::size_t n,
                                            double T, double a0, double eta,
                                            double alpha, double lam) {
    if (!(1e-6 < eta && eta < 0.999)) return kBig;
    if (!(0.05 < alpha && alpha < 20.0)) return kBig;
    if (!(1e-3 < lam && lam < 1e3)) return kBig;
    if (!(-20.0 < a0 && a0 < 20.0)) return kBig;

    const double nu = std::exp(a0);
    const double cutoff = lam * std::pow(700.0, 1.0 / alpha);

    double log_sum = 0.0;
    std::size_t lo = 0;
    for (std::size_t i = 0; i < n; ++i) {
        while (lo < i && (t[i] - t[lo]) > cutoff) ++lo;
        double s = 0.0;
        for (std::size_t j = lo; j < i; ++j) {
            const double x = (t[i] - t[j]) / lam;
            if (x > 1e-12) {
                const double z = std::pow(x, alpha);
                if (z < 700.0) {
                    s += (alpha / lam) * std::pow(x, alpha - 1.0) *
                         std::exp(-z);
                }
            }
        }
        const double lam_at = nu + eta * s;
        if (lam_at <= 0.0) return kBig;
        log_sum += std::log(lam_at);
    }

    double integral = nu * T;
    for (std::size_t i = 0; i < n; ++i) {
        const double u = T - t[i];
        if (u > 0.0) {
            const double x = u / lam;
            integral += eta * (1.0 - std::exp(-std::pow(x, alpha)));
        }
    }
    return -(log_sum - integral);
}

// Truncated / sliding-window form of hawkes_ll_gamma_const. The gamma
// kernel's log-density  log_d = log_const + (alpha-1)*log(u) - beta*u
// falls monotonically past the peak; beyond the cutoff below it is
// < -745, so exp(log_d) underflows to exactly 0. The exact O(n^2)
// version already adds those zeros, so cutting the inner loop there is
// bit-for-bit identical -- not an approximation, cannot bias the MLE.
//
// No closed form solves log_d = -745 (the mixed log(u)-beta*u term),
// so the cutoff uses a guaranteed upper bound on (alpha-1)*log(u):
// for alpha > 1, (alpha-1)log(u) <= (beta/2)u + K with K the bound's
// maximum; for alpha <= 1 the term is <= 0 once u >= 1. The cutoff is
// thus a little wider than the true underflow point -- conservative
// but always correct. For a slowly decaying kernel the window -> n and
// it degrades gracefully to exact O(n^2).
inline double hawkes_ll_gamma_const_trunc(const double *t, std::size_t n,
                                          double T, double a0, double eta,
                                          double alpha, double beta) {
    if (!(1e-6 < eta && eta < 0.999)) return kBig;
    if (!(0.05 < alpha && alpha < 20.0)) return kBig;
    if (!(0.05 < beta && beta < 30.0)) return kBig;
    if (!(-20.0 < a0 && a0 < 20.0)) return kBig;

    const double nu = std::exp(a0);
    const double log_const = alpha * std::log(beta) - std::lgamma(alpha);

    double cutoff;
    if (alpha > 1.0 + 1e-12) {
        const double K = (alpha - 1.0) *
                             std::log(2.0 * (alpha - 1.0) / beta) -
                         (alpha - 1.0);
        cutoff = 2.0 * (log_const + K + 745.2) / beta;
    } else {
        cutoff = (log_const + 745.2) / beta;
        if (cutoff < 1.0) cutoff = 1.0;
    }

    double log_sum = 0.0;
    std::size_t lo = 0;
    for (std::size_t i = 0; i < n; ++i) {
        while (lo < i && (t[i] - t[lo]) > cutoff) ++lo;
        double s = 0.0;
        for (std::size_t j = lo; j < i; ++j) {
            const double u = t[i] - t[j];
            if (u > 1e-300) {
                const double log_d =
                    log_const + (alpha - 1.0) * std::log(u) - beta * u;
                s += std::exp(log_d);
            }
        }
        const double lam_i = nu + eta * s;
        if (lam_i <= 0.0) return kBig;
        log_sum += std::log(lam_i);
    }

    double integral = nu * T;
    for (std::size_t i = 0; i < n; ++i) {
        integral += eta * gamma_cdf_regularized(alpha, beta * (T - t[i]));
    }
    return -(log_sum - integral);
}

// --- sum-of-exponentials (SoE) Hawkes likelihood (task #73) ------------------
//
// Hawkes negative log-likelihood with a sum-of-exponentials triggering
// kernel:  g(u) = sum_m  w[m] * exp(-beta[m] * u).
//
// Each exponential component is memoryless, so it carries its own O(n)
// recursion  A_m,i = exp(-beta_m * dt) * (1 + A_m,{i-1}).  Running M
// such recursions in parallel evaluates the likelihood in O(M*n) --
// the sub-quadratic engine for any kernel (Weibull / gamma / Lomax)
// once it has been fitted to an SoE form.
//
// With M = 1 and w = beta = {b} this reduces exactly (to rounding) to
// the exponential-kernel likelihood hawkes_ll_exp_const.
inline double hawkes_ll_soe(const double *t, std::size_t n, double T,
                            double nu, double eta, const double *w,
                            const double *beta, std::size_t M) {
    if (!(nu > 0.0) || !std::isfinite(nu)) return kBig;

    std::vector<double> A(M, 0.0);  // one recursion state per component
    double log_sum = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        double excite = 0.0;
        if (i > 0) {
            const double dt = t[i] - t[i - 1];
            for (std::size_t m = 0; m < M; ++m) {
                A[m] = std::exp(-beta[m] * dt) * (1.0 + A[m]);
                excite += w[m] * A[m];
            }
        }
        const double lam_i = nu + eta * excite;
        if (!std::isfinite(lam_i) || lam_i <= 0.0) return kBig;
        log_sum += std::log(lam_i);
    }

    // compensator: integral of g over [0, T-t_i] is, per component,
    // (w_m / beta_m) * (1 - exp(-beta_m * (T - t_i))).
    double integral = nu * T;
    for (std::size_t i = 0; i < n; ++i) {
        const double u = T - t[i];
        for (std::size_t m = 0; m < M; ++m) {
            integral += eta * (w[m] / beta[m]) *
                        (1.0 - std::exp(-beta[m] * u));
        }
    }
    if (!std::isfinite(log_sum) || !std::isfinite(integral)) return kBig;
    return -(log_sum - integral);
}

// Complex-pole SoE Hawkes likelihood (task #73, gamma hybrid).
//
// Same O(M*n) recursion as hawkes_ll_soe, but the decay rates beta and
// weights w are complex. The matrix-pencil fit of a gamma tail returns
// real poles plus complex-conjugate pairs; a conjugate pair is the
// real damped oscillation  2*Re(w*exp(-beta*u)), so the excitation and
// the compensator are accumulated in complex arithmetic and the real
// part is taken. With purely real (w, beta) this is identical to
// hawkes_ll_soe.
//
// The caller must pass conjugate poles in matching pairs, so the
// imaginary parts cancel and lambda is real; Re() then only discards
// rounding noise. Re(beta) > 0 keeps |exp(-beta*dt)| < 1, so the
// recursion is stable -- the fitter guarantees this.
inline double hawkes_ll_soe_cplx(const double *t, std::size_t n, double T,
                                 double nu, double eta,
                                 const std::complex<double> *w,
                                 const std::complex<double> *beta,
                                 std::size_t M) {
    if (!(nu > 0.0) || !std::isfinite(nu)) return kBig;

    std::vector<std::complex<double>> A(M, std::complex<double>(0.0, 0.0));
    double log_sum = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        double excite = 0.0;
        if (i > 0) {
            const double dt = t[i] - t[i - 1];
            std::complex<double> acc(0.0, 0.0);
            for (std::size_t m = 0; m < M; ++m) {
                A[m] = std::exp(-beta[m] * dt) * (1.0 + A[m]);
                acc += w[m] * A[m];
            }
            excite = acc.real();
        }
        const double lam_i = nu + eta * excite;
        if (!std::isfinite(lam_i) || lam_i <= 0.0) return kBig;
        log_sum += std::log(lam_i);
    }

    double integral = nu * T;
    for (std::size_t i = 0; i < n; ++i) {
        const double u = T - t[i];
        std::complex<double> acc(0.0, 0.0);
        for (std::size_t m = 0; m < M; ++m) {
            acc += (w[m] / beta[m]) *
                   (1.0 - std::exp(-beta[m] * u));
        }
        integral += eta * acc.real();
    }
    if (!std::isfinite(log_sum) || !std::isfinite(integral)) return kBig;
    return -(log_sum - integral);
}

// Hybrid gamma-kernel Hawkes likelihood (task #73).
//
// The gamma kernel (shape alpha > 1) is not completely monotone, so it
// has no global sum-of-exponentials form. This splits the lag axis at
// u_split: lags in [0, u_split] (the rising/peak region) use the EXACT
// kernel via a sliding window; lags beyond u_split use the SoE fitted
// by soe_fit_gamma_tail (w_soe, beta_soe -- complex, shifted so the
// modes describe g(u_split + s)).
//
// Events sorted => two monotone pointers. j_grad counts events whose
// lag has crossed u_split; as it advances, each graduating event is
// folded into the SoE state S_m (a "graduation" recursion). Events in
// [j_grad, i) are still inside the exact window. Cost O(n*w + M*n),
// w = events within u_split. The compensator splits the same way: the
// exact part is the regularized incomplete gamma, the tail part the
// closed-form SoE integral.
inline double hawkes_ll_gamma_hybrid(
    const double *t, std::size_t n, double T, double a0, double eta,
    double alpha, double beta, double u_split,
    const std::complex<double> *w_soe,
    const std::complex<double> *beta_soe, std::size_t M) {
    if (!(1e-6 < eta && eta < 0.999)) return kBig;
    if (!(0.05 < alpha && alpha < 20.0)) return kBig;
    if (!(0.05 < beta && beta < 30.0)) return kBig;
    if (!(-20.0 < a0 && a0 < 20.0)) return kBig;
    if (!(u_split > 0.0)) return kBig;

    const double nu = std::exp(a0);
    const double log_const = alpha * std::log(beta) - std::lgamma(alpha);

    std::vector<std::complex<double>> S(M, std::complex<double>(0.0, 0.0));
    double log_sum = 0.0;
    std::size_t j_grad = 0;
    for (std::size_t i = 0; i < n; ++i) {
        if (i > 0) {
            const double dt = t[i] - t[i - 1];
            for (std::size_t m = 0; m < M; ++m)
                S[m] *= std::exp(-beta_soe[m] * dt);
        }
        // graduate events whose lag has just crossed u_split
        while (j_grad < i && (t[i] - t[j_grad]) > u_split) {
            const double s = t[i] - t[j_grad] - u_split;
            for (std::size_t m = 0; m < M; ++m)
                S[m] += std::exp(-beta_soe[m] * s);
            ++j_grad;
        }
        // tail excitation (SoE) ...
        std::complex<double> tail(0.0, 0.0);
        for (std::size_t m = 0; m < M; ++m) tail += w_soe[m] * S[m];
        double excite = tail.real();
        // ... plus window excitation (exact kernel)
        for (std::size_t j = j_grad; j < i; ++j) {
            const double u = t[i] - t[j];
            if (u > 1e-300) {
                const double log_d =
                    log_const + (alpha - 1.0) * std::log(u) - beta * u;
                excite += std::exp(log_d);
            }
        }
        const double lam_i = nu + eta * excite;
        if (!std::isfinite(lam_i) || lam_i <= 0.0) return kBig;
        log_sum += std::log(lam_i);
    }

    double integral = nu * T;
    for (std::size_t i = 0; i < n; ++i) {
        const double X = T - t[i];
        if (X <= 0.0) continue;
        if (X <= u_split) {
            integral += eta * gamma_cdf_regularized(alpha, beta * X);
        } else {
            integral += eta * gamma_cdf_regularized(alpha, beta * u_split);
            const double x_tail = X - u_split;
            std::complex<double> csum(0.0, 0.0);
            for (std::size_t m = 0; m < M; ++m)
                csum += (w_soe[m] / beta_soe[m]) *
                        (1.0 - std::exp(-beta_soe[m] * x_tail));
            integral += eta * csum.real();
        }
    }
    if (!std::isfinite(log_sum) || !std::isfinite(integral)) return kBig;
    return -(log_sum - integral);
}
// --- general Hawkes likelihood with analytic gradient (any baseline) --------
//
// lambda(t) = nu(t) + eta * sum_{t_j < t} g(t - t_j), g a normalised kernel:
//   kernel 0  exponential  g = b e^{-bu}                          psi = (b)
//   kernel 1  Weibull      g = (k/l)(u/l)^{k-1} e^{-(u/l)^k}       psi = (k, l)
//   kernel 2  gamma        g = b^a u^{a-1} e^{-bu} / Gamma(a)     psi = (a, b)
//   kernel 3  Lomax        g = a c^a (u+c)^{-(a+1)}               psi = (a, c)
// The baseline enters only through nu_i = nu(t_i) (the caller adds its own
// integral and gradient, using inv_lam), so one routine serves every baseline.
//
// Returns  L = sum_i log lambda_i - eta * sum_i G(T - t_i)  (G the kernel CDF),
// and, when grad is non-null, grad[0] = dL/deta, grad[1..npsi] = dL/dpsi, and
// inv_lam[i] = 1/lambda_i (so the caller gets dL/d(baseline params) as
// sum_i dnu_i * inv_lam[i]). A non-finite or non-positive intensity returns -kBig.
//
// method 0  exact: O(n) recursion for the exponential kernel (Ozaki 1979, with
//           a second state for d/db); for Weibull and gamma the double sum stops
//           where the kernel underflows to exactly 0 (bit-identical to the full
//           O(n^2) sum); Lomax is the full O(n^2) sum.
// method 1  sum of exponentials (Lomax; gamma with a < 1): the completely
//           monotone part r^{-beta} = (1/Gamma(beta)) Int exp(-r e^s + beta s) ds
//           by the trapezoid rule with the step of Beylkin & Monzon (2010,
//           ACHA 28:131, Theorem 3), relative error <= eps per term on
//           r in [delta, 1]; O(n K) with one Ozaki state per node (two for the
//           gamma rate). The gradient is that of the approximation.
// method 2  truncated: each event excites only lags with kernel tail mass
//           above eps, u <= G^{-1}(1 - eps); O(n w). An approximation (the
//           omitted excitation is non-negative), for light-tailed kernels.

inline double digamma_fn(double x) {
    double r = 0.0;
    while (x < 6.0) {
        r -= 1.0 / x;
        x += 1.0;
    }
    const double f = 1.0 / (x * x);
    return r + std::log(x) - 0.5 / x -
           f * (1.0 / 12 - f * (1.0 / 120 - f * (1.0 / 252 - f * (1.0 / 240 - f * (1.0 / 132)))));
}

struct HawkesKernel {
    int kind;
    double p0, p1;
    // per-parameter constants, computed once (not per pair): log-normaliser and digamma (gamma),
    // log(c) (Lomax), log(l) (Weibull)
    double lconst = 0.0, dig = 0.0, lp1 = 0.0;
    HawkesKernel(int k, double a, double b) : kind(k), p0(a), p1(b) {
        if (kind == 2) {
            lp1 = std::log(p1);
            lconst = p0 * lp1 - std::lgamma(p0);
            dig = digamma_fn(p0);
        } else if (kind == 3 || kind == 1) {
            lp1 = std::log(p1);
        }
    }
    // density and d(density)/d(psi); false when the term is exactly 0, and outside the
    // support (u <= 0): an event does not excite one at the same instant
    bool dens(double u, double &g, double &d0, double &d1) const {
        if (!(u > 0.0)) return false;
        switch (kind) {
        case 0: {
            const double e = std::exp(-p0 * u);
            g = p0 * e;
            d0 = e * (1.0 - p0 * u);
            d1 = 0.0;
            return true;
        }
        case 1: {
            const double x = u / p1;
            if (!(x > 1e-12)) return false;
            const double lx = std::log(x), z = std::exp(p0 * lx);
            if (!(z < 700.0)) return false;
            g = (p0 / p1) * std::exp((p0 - 1.0) * lx - z);
            d0 = g * (1.0 / p0 + lx - z * lx);
            d1 = g * p0 * (z - 1.0) / p1;
            return true;
        }
        case 2: {
            if (!(u > 1e-300)) return false;
            const double lu = std::log(u);
            const double lg = lconst + (p0 - 1.0) * lu - p1 * u;
            if (lg < -745.0) return false;
            g = std::exp(lg);
            d0 = g * (lp1 - dig + lu);
            d1 = g * (p0 / p1 - u);
            return true;
        }
        default: {
            const double luc = std::log(u + p1);
            g = std::exp(std::log(p0) + p0 * lp1 - (p0 + 1.0) * luc);
            d0 = g * (1.0 / p0 + lp1 - luc);
            d1 = g * (p0 / p1 - (p0 + 1.0) / (u + p1));
            return true;
        }
        }
    }
    // log-density and its psi-derivatives, finite where the density itself underflows to 0
    // (EM's M-step weighs log g at new parameters on pairs the old ones barely reached);
    // false only outside the support (u <= 0)
    bool logdens(double u, double &lg, double &d0, double &d1) const {
        if (!(u > 0.0)) return false;
        switch (kind) {
        case 0:
            lg = std::log(p0) - p0 * u;
            d0 = 1.0 / p0 - u;
            d1 = 0.0;
            return true;
        case 1: {
            const double lx = std::log(u / p1), z = std::exp(p0 * lx);
            lg = std::log(p0) - lp1 + (p0 - 1.0) * lx - z;
            d0 = 1.0 / p0 + lx - z * lx;
            d1 = p0 * (z - 1.0) / p1;
            return true;
        }
        case 2: {
            const double lu = std::log(u);
            lg = lconst + (p0 - 1.0) * lu - p1 * u;
            d0 = lp1 - dig + lu;
            d1 = p0 / p1 - u;
            return true;
        }
        default: {
            const double luc = std::log(u + p1);
            lg = std::log(p0) + p0 * lp1 - (p0 + 1.0) * luc;
            d0 = 1.0 / p0 + lp1 - luc;
            d1 = p0 / p1 - (p0 + 1.0) / (u + p1);
            return true;
        }
        }
    }
    void cdf(double u, double &G, double &d0, double &d1) const {
        d0 = d1 = 0.0;
        if (!(u > 0.0)) {
            G = 0.0;
            return;
        }
        switch (kind) {
        case 0: {
            const double e = std::exp(-p0 * u);
            G = 1.0 - e;
            d0 = u * e;
            return;
        }
        case 1: {
            const double x = u / p1, z = std::pow(x, p0), e = std::exp(-z);
            G = 1.0 - e;
            if (x > 0.0) d0 = e * z * std::log(x);
            d1 = -e * z * p0 / p1;
            return;
        }
        case 2: {
            G = gamma_cdf_regularized(p0, p1 * u);
            const double lg = lconst + (p0 - 1.0) * std::log(u) - p1 * u;
            d1 = std::exp(lg) * u / p1;
            const double h = 1e-5 * std::max(1.0, p0);
            d0 = (gamma_cdf_regularized(p0 + h, p1 * u) - gamma_cdf_regularized(p0 - h, p1 * u)) / (2.0 * h);
            return;
        }
        default: {
            const double q = p1 / (u + p1), qa = std::pow(q, p0);
            G = 1.0 - qa;
            d0 = -qa * std::log(q);
            d1 = -p0 * std::pow(q, p0 - 1.0) * u / ((u + p1) * (u + p1));
            return;
        }
        }
    }
    // the lag beyond which a term is exactly 0 in double precision (exact method)
    double underflow_lag() const {
        switch (kind) {
        case 0: return 745.2 / p0 + 1.0;
        case 1: return p1 * std::pow(700.0, 1.0 / p0);
        case 2: {
            const double lc = p0 * std::log(p1) - std::lgamma(p0);
            if (p0 > 1.0 + 1e-12) {
                const double K = (p0 - 1.0) * std::log(2.0 * (p0 - 1.0) / p1) - (p0 - 1.0);
                return 2.0 * (lc + K + 745.2) / p1;
            }
            return std::max(1.0, (lc + 745.2) / p1);
        }
        default: return std::numeric_limits<double>::infinity();
        }
    }
    // the lag with tail mass eps: G(u) = 1 - eps
    double tail_lag(double eps) const {
        switch (kind) {
        case 0: return -std::log(eps) / p0;
        case 1: return p1 * std::pow(-std::log(eps), 1.0 / p0);
        case 3: return p1 * (std::pow(eps, -1.0 / p0) - 1.0);
        default: {
            double lo = 0.0, hi = std::max(1.0, p0 / p1);
            while (gamma_cdf_regularized(p0, p1 * hi) < 1.0 - eps && hi < 1e300) hi *= 2.0;
            for (int it = 0; it < 200 && hi - lo > 1e-12 * hi; ++it) {
                const double mid = 0.5 * (lo + hi);
                (gamma_cdf_regularized(p0, p1 * mid) < 1.0 - eps ? lo : hi) = mid;
            }
            return hi;
        }
        }
    }
};

// nodes s_k and step h for r^{-beta} on r in [delta, 1], relative error eps
inline void soe_nodes(double beta, double delta, double eps, std::vector<double> &s, double &h) {
    h = 6.283185307179586 / (std::log(3.0) + beta * std::log(1.0 / std::cos(1.0)) + std::log(1.0 / eps));
    const double s_lo = (std::log(eps) + std::lgamma(beta) + std::log(beta)) / beta - 2.0 * h;
    const double s_hi = std::log((std::log(1.0 / eps) + beta * std::max(0.0, std::log(1.0 / delta)) + 40.0) / delta);
    s.clear();
    for (double x = s_lo; x <= s_hi + h; x += h) s.push_back(x);
}

inline double hawkes_eval(const double *t, std::size_t n, double T, const double *nu, double eta,
                          int kind, const double *psi, int method, double eps, double soe_R, double soe_delta,
                          double *grad, double *inv_lam) {
    HawkesKernel k{kind, psi[0], kind == 0 ? 0.0 : psi[1]};
    const int np = kind == 0 ? 1 : 2;
    double L = 0.0, gE = 0.0, g0 = 0.0, g1 = 0.0;
    std::vector<double> S(n, 0.0), D0(n, 0.0), D1(n, 0.0);  // excitation and its psi-derivatives

    bool soe = method == 1 && (kind == 3 || (kind == 2 && psi[0] < 1.0));
    std::vector<double> soe_s;
    double soe_h = 0.0;
    if (soe) {
        // K nodes cost O(n K); the exact pair loop at most O(n^2 / 2): with fewer than 2K events
        // (small data, or gamma near shape 1 where K grows like log(eps)/(1 - shape)) exact is cheaper
        soe_nodes(kind == 3 ? psi[0] + 1.0 : 1.0 - psi[0], soe_delta, eps, soe_s, soe_h);
        if (2 * soe_s.size() > n) soe = false;
    }
    if (kind == 0 && method != 2) {
        // Ozaki: A_i = sum e^{-b d}, B_i = sum d e^{-b d}, over the events strictly before t_i
        // (an event does not excite one at the same instant): `tie` counts the events at
        // t_{i-1}, which join the sum only once time moves on
        double A = 0.0, B = 0.0, tie = 1.0;
        for (std::size_t i = 1; i < n; ++i) {
            const double dt = t[i] - t[i - 1];
            if (dt > 0.0) {
                const double e = std::exp(-psi[0] * dt);
                B = e * (B + dt * (A + tie));
                A = e * (A + tie);
                tie = 1.0;
            } else {
                tie += 1.0;
            }
            S[i] = psi[0] * A;
            D0[i] = A - psi[0] * B;
        }
    } else if (soe) {
        const double beta = kind == 3 ? psi[0] + 1.0 : 1.0 - psi[0];
        const std::vector<double> &s = soe_s;
        const double h = soe_h;
        const std::size_t K = s.size();
        std::vector<double> rate(K), w(K), dw0(K), dw1(K);
        for (std::size_t m = 0; m < K; ++m) {
            const double rho = std::exp(s[m]) / soe_R;
            if (kind == 3) {  // g = a c^a R^{-(a+1)} (h/Gamma(a+1)) sum e^{(a+1)s} e^{-rho c} e^{-rho u}
                const double a = psi[0], c = psi[1];
                const double lw = std::log(a) + a * std::log(c) - (a + 1.0) * std::log(soe_R) + std::log(h) -
                                  std::lgamma(a + 1.0) + (a + 1.0) * s[m] - rho * c;
                rate[m] = rho;
                w[m] = std::exp(lw);
                dw0[m] = w[m] * (1.0 / a + std::log(c) - std::log(soe_R) - digamma_fn(a + 1.0) + s[m]);
                dw1[m] = w[m] * (a / c - rho);
            } else {  // gamma a<1: g = b^a R^{-beta} h e^{beta s} / (Gamma(a) Gamma(beta)) e^{-(rho+b) u}
                const double a = psi[0], b = psi[1];
                const double lw = a * std::log(b) - beta * std::log(soe_R) + std::log(h) + beta * s[m] -
                                  std::lgamma(a) - std::lgamma(beta);
                rate[m] = rho + b;
                w[m] = std::exp(lw);
                dw0[m] = w[m] * (std::log(b) + std::log(soe_R) - s[m] - digamma_fn(a) + digamma_fn(beta));
                dw1[m] = w[m] * (a / b);
            }
        }
        std::vector<double> A(K, 0.0), B(K, 0.0);
        double tie = 1.0;  // events at t_{i-1}: strictly earlier events only, as the Ozaki loop
        for (std::size_t i = 1; i < n; ++i) {
            const double dt = t[i] - t[i - 1];
            const bool moved = dt > 0.0;
            double si = 0.0, d0 = 0.0, d1 = 0.0;
            for (std::size_t m = 0; m < K; ++m) {
                if (moved) {
                    const double e = std::exp(-rate[m] * dt);
                    if (kind == 2) B[m] = e * (B[m] + dt * (A[m] + tie));
                    A[m] = e * (A[m] + tie);
                }
                si += w[m] * A[m];
                d0 += dw0[m] * A[m];
                d1 += dw1[m] * A[m] - (kind == 2 ? w[m] * B[m] : 0.0);
            }
            S[i] = si;
            D0[i] = d0;
            D1[i] = d1;
            tie = moved ? 1.0 : tie + 1.0;
        }
    } else {
        // soe on a kernel it cannot represent (gamma with shape >= 1, or fewer than 2K events) truncates
        const double cut = method != 0 ? std::min(k.tail_lag(eps), k.underflow_lag()) : k.underflow_lag();
        std::size_t lo = 0;
        for (std::size_t i = 1; i < n; ++i) {
            while (lo < i && t[i] - t[lo] > cut) ++lo;
            poll_interrupt(i);
            double si = 0.0, d0 = 0.0, d1 = 0.0, g, a0, a1;
            for (std::size_t j = lo; j < i; ++j) {
                if (k.dens(t[i] - t[j], g, a0, a1)) {
                    si += g;
                    d0 += a0;
                    d1 += a1;
                }
            }
            S[i] = si;
            D0[i] = d0;
            D1[i] = d1;
        }
    }
    for (std::size_t i = 0; i < n; ++i) {
        const double lam = nu[i] + eta * S[i];
        if (!(lam > 0.0) || !std::isfinite(lam)) return -kBig;
        L += std::log(lam);
        const double il = 1.0 / lam;
        if (inv_lam) inv_lam[i] = il;
        gE += S[i] * il;
        g0 += eta * D0[i] * il;
        g1 += eta * D1[i] * il;
    }
    for (std::size_t i = 0; i < n; ++i) {
        double G, c0, c1;
        k.cdf(T - t[i], G, c0, c1);
        L -= eta * G;
        gE -= G;
        g0 -= eta * c0;
        g1 -= eta * c1;
    }
    if (grad) {
        grad[0] = gE;
        grad[1] = g0;
        if (np > 1) grad[2] = g1;
    }
    return std::isfinite(L) ? L : -kBig;
}


// The baselines of the TPS Hawkes models (log link):
//   kind 0  nu(t) = exp(a0)
//   kind 1  nu(t) = exp(a0 + a1 t/T + a2 sin(2 pi t/365.25) + a3 cos(2 pi t/365.25))
// with the integral by the trapezoid rule on max(64, floor(T)+1) points, as
// morie's and rmorie's own code (the sinusoid has no closed-form integral).
inline int hawkes_baseline_n(int bkind) { return bkind == 0 ? 1 : 4; }

inline void hawkes_baseline_terms(double t, double T, int bkind, double *f) {
    f[0] = 1.0;
    if (bkind == 1) {
        f[1] = t / std::max(T, 1.0);
        f[2] = std::sin(6.283185307179586 * t / 365.25);
        f[3] = std::cos(6.283185307179586 * t / 365.25);
    }
}

// negative log-likelihood and (when grad is non-null) its gradient over
// theta = (a[0..nb), eta, psi[0..np)): the routine the optimisers call.
inline double hawkes_nll_grad(const double *t, std::size_t n, double T, int bkind, const double *a, double eta,
                              int kind, const double *psi, int method, double eps, double soe_R, double soe_delta,
                              double *grad) {
    const int nb = hawkes_baseline_n(bkind);
    std::vector<double> nu(n), f(nb);
    for (std::size_t i = 0; i < n; ++i) {
        hawkes_baseline_terms(t[i], T, bkind, f.data());
        double lin = 0.0;
        for (int m = 0; m < nb; ++m) lin += a[m] * f[m];
        nu[i] = std::exp(lin);
    }
    std::vector<double> il(grad ? n : 0);
    double g[3] = {0.0, 0.0, 0.0};
    const double L = hawkes_eval(t, n, T, nu.data(), eta, kind, psi, method, eps, soe_R, soe_delta,
                                 grad ? g : nullptr, grad ? il.data() : nullptr);
    if (!(L > -kBig)) return kBig;
    // baseline integral and its gradient
    double I = 0.0;
    std::vector<double> dI(nb, 0.0);
    if (bkind == 0) {
        I = std::exp(a[0]) * T;
        dI[0] = I;
    } else {
        const std::size_t m = std::max<std::size_t>(64, static_cast<std::size_t>(T) + 1);
        const double step = T / static_cast<double>(m - 1);
        for (std::size_t q = 0; q < m; ++q) {
            const double tq = step * static_cast<double>(q);
            hawkes_baseline_terms(tq, T, bkind, f.data());
            double lin = 0.0;
            for (int r = 0; r < nb; ++r) lin += a[r] * f[r];
            const double w = (q == 0 || q == m - 1 ? 0.5 : 1.0) * step * std::exp(lin);
            I += w;
            for (int r = 0; r < nb; ++r) dI[r] += w * f[r];
        }
    }
    if (grad) {
        for (int r = 0; r < nb; ++r) {
            double s = 0.0;
            for (std::size_t i = 0; i < n; ++i) {
                hawkes_baseline_terms(t[i], T, bkind, f.data());
                s += nu[i] * f[r] * il[i];
            }
            grad[r] = -(s - dI[r]);
        }
        const int np = kind == 0 ? 1 : 2;
        for (int q = 0; q <= np; ++q) grad[nb + q] = -g[q];
    }
    const double nll = -(L - I);
    return std::isfinite(nll) ? nll : kBig;
}
// Time-rescaling residuals U_i = 1 - exp(-(Lambda(t_i) - Lambda(t_{i-1}))) of a
// fitted Hawkes process (Brown et al. 2002, Neural Comput. 14:325), with
// Lambda(t) = int_0^t nu + eta sum_{t_j < t} G(t - t_j). Writing the kernel
// part through the survival Sbar = 1 - G, sum_{j<i} G(t_i - t_j) = i - Q_i with
// Q_i = sum_{j<i} Sbar(t_i - t_j), so each increment is eta (1 - Q_i + Q_{i-1})
// plus the baseline's: Q by Ozaki's recursion (exponential), a window that
// ends where Sbar < 1e-15 (Weibull, gamma), or a sum of exponentials of
// Sbar = c^a (u+c)^{-a} (Lomax, relative error 1e-12). The baseline integral
// is the trapezoid rule on max(256, floor(T)+1) points, interpolated, as
// morie's Python code. Replaces an O(n^2) Python loop.
inline void hawkes_rescaled(const double *t, std::size_t n, double T, int bkind, const double *a, double eta,
                            int kind, const double *psi, double *U) {
    HawkesKernel k{kind, psi[0], kind == 0 ? 0.0 : psi[1]};
    const int nb = hawkes_baseline_n(bkind);
    std::vector<double> Q(n, 0.0), f(nb);
    if (kind == 0) {
        double A = 0.0;
        for (std::size_t i = 1; i < n; ++i) {
            A = std::exp(-psi[0] * (t[i] - t[i - 1])) * (A + 1.0);
            Q[i] = A;
        }
    } else if (kind == 3) {
        const double av = psi[0], c = psi[1], R = T + c, eps = 1e-12;
        std::vector<double> s;
        double h;
        soe_nodes(av, c / R, eps, s, h);
        const std::size_t K = s.size();
        std::vector<double> rate(K), w(K), A(K, 0.0);
        for (std::size_t m = 0; m < K; ++m) {
            rate[m] = std::exp(s[m]) / R;
            w[m] = std::exp(av * std::log(c) - av * std::log(R) + std::log(h) - std::lgamma(av) + av * s[m] - rate[m] * c);
        }
        for (std::size_t i = 1; i < n; ++i) {
            const double dt = t[i] - t[i - 1];
            double q = 0.0;
            for (std::size_t m = 0; m < K; ++m) {
                A[m] = std::exp(-rate[m] * dt) * (A[m] + 1.0);
                q += w[m] * A[m];
            }
            Q[i] = q;
        }
    } else {
        const double cut = std::max(k.underflow_lag(), k.tail_lag(1e-15));
        std::size_t lo = 0;
        for (std::size_t i = 1; i < n; ++i) {
            while (lo < i && t[i] - t[lo] > cut) ++lo;
            poll_interrupt(i);
            double q = 0.0, G, d0, d1;
            for (std::size_t j = lo; j < i; ++j) {
                k.cdf(t[i] - t[j], G, d0, d1);
                q += 1.0 - G;
            }
            Q[i] = q;
        }
    }
    // cumulative baseline on the grid, then linear interpolation at the events
    const std::size_t m = std::max<std::size_t>(256, static_cast<std::size_t>(T) + 1);
    const double step = T / static_cast<double>(m - 1);
    std::vector<double> cum(m, 0.0), val(m);
    for (std::size_t q = 0; q < m; ++q) {
        hawkes_baseline_terms(step * static_cast<double>(q), T, bkind, f.data());
        double lin = 0.0;
        for (int r = 0; r < nb; ++r) lin += a[r] * f[r];
        val[q] = std::exp(lin);
        if (q) cum[q] = cum[q - 1] + 0.5 * (val[q] + val[q - 1]) * step;
    }
    auto cumB = [&](double x) {
        if (x <= 0.0) return 0.0;
        if (x >= T) return cum[m - 1];
        const double pos = x / step;
        const std::size_t q = std::min<std::size_t>(static_cast<std::size_t>(pos), m - 2);
        return cum[q] + (pos - static_cast<double>(q)) * (cum[q + 1] - cum[q]);
    };
    double prevB = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        const double B = cumB(t[i]);
        double inc = B - prevB + (i ? eta * (1.0 - Q[i] + Q[i - 1]) : 0.0);
        prevB = B;
        if (!(inc > 1e-12)) inc = 1e-12;
        U[i] = 1.0 - std::exp(-inc);
    }
}
// --- EM for the Hawkes process (Veen & Schoenberg 2008, JASA 103:614) --------
//
// With the branching structure as missing data, the E-step gives each event i
// the probabilities p_i0 = nu_i / lambda_i (immigrant) and
// p_ij = eta g(t_i - t_j) / lambda_i (offspring of j); the M-step maximises
//   Q = sum_i p_i0 log nu_i - int nu
//     + sum_{i,j} p_ij log(eta g(t_i - t_j; psi)) - eta sum_j G(T - t_j; psi).
// hawkes_intensity gives the lambda_i of the E-step; hawkes_em_pass the kernel
// part of Q for a new psi with the p_ij of the old parameters recomputed on the
// fly (no n x w matrix is stored): P = sum p_ij, Qk = sum p_ij log g(u; psi_new)
// and dQk/dpsi_new. The pair loop is the exact method's (it stops where the
// old kernel underflows to 0, so the sum is the full one).
inline void hawkes_intensity(const double *t, std::size_t n, double T, int bkind, const double *a, double eta,
                             int kind, const double *psi, double *lam) {
    const int nb = hawkes_baseline_n(bkind);
    std::vector<double> nu(n), f(nb), il(n);
    for (std::size_t i = 0; i < n; ++i) {
        hawkes_baseline_terms(t[i], T, bkind, f.data());
        double lin = 0.0;
        for (int m = 0; m < nb; ++m) lin += a[m] * f[m];
        nu[i] = std::exp(lin);
    }
    double g[3];
    hawkes_eval(t, n, T, nu.data(), eta, kind, psi, 0, 1e-12, 0.0, 0.0, g, il.data());
    for (std::size_t i = 0; i < n; ++i) lam[i] = il[i] > 0.0 ? 1.0 / il[i] : 0.0;
}

inline double hawkes_em_pass(const double *t, std::size_t n, const double *lam_old, double eta_old, int kind,
                             const double *psi_old, const double *psi_new, double *P, double *dQ) {
    HawkesKernel ko{kind, psi_old[0], kind == 0 ? 0.0 : psi_old[1]};
    HawkesKernel kn{kind, psi_new[0], kind == 0 ? 0.0 : psi_new[1]};
    const double cut = ko.underflow_lag();
    double Q = 0.0, sumP = 0.0, d0 = 0.0, d1 = 0.0;
    std::size_t lo = 0;
    for (std::size_t i = 1; i < n; ++i) {
        while (lo < i && t[i] - t[lo] > cut) ++lo;
        poll_interrupt(i);
        const double inv = 1.0 / lam_old[i];
        for (std::size_t j = lo; j < i; ++j) {
            const double u = t[i] - t[j];
            double go, a0, a1, lgn, b0, b1;
            if (!ko.dens(u, go, a0, a1)) continue;
            const double p = eta_old * go * inv;
            if (!(p > 0.0) || !kn.logdens(u, lgn, b0, b1)) continue;
            sumP += p;
            Q += p * lgn;
            d0 += p * b0;
            d1 += p * b1;
        }
    }
    *P = sumP;
    dQ[0] = d0;
    dQ[1] = d1;
    return Q;
}
// sum_j G(T - t_j; psi) and its psi-gradient: the expected number of offspring
// per unit branching ratio (the compensator's kernel part; EM's M-step).
inline double hawkes_cdf_sum(const double *t, std::size_t n, double T, int kind, const double *psi, double *grad) {
    HawkesKernel k{kind, psi[0], kind == 0 ? 0.0 : psi[1]};
    double S = 0.0, g0 = 0.0, g1 = 0.0, G, d0, d1;
    for (std::size_t i = 0; i < n; ++i) {
        k.cdf(T - t[i], G, d0, d1);
        S += G;
        g0 += d0;
        g1 += d1;
    }
    grad[0] = g0;
    grad[1] = g1;
    return S;
}
// --- exact one-sample Kolmogorov-Smirnov distribution -----------------------
//
// P(D_n < d) by Marsaglia, Tsang & Wang (2003, J. Stat. Softw. 8(18)): with
// k = floor(n d) + 1, m = 2k - 1 and h = k - n d, the m x m matrix H of the
// paper raised to the n-th power gives P = n!/n^n (H^n)_{kk}. The power is
// taken by repeated squaring, the entries rescaled by 1e140 when they grow, the
// exponent carried separately. O(m^3 log n).
inline void ks_mat_mult(const std::vector<double> &A, const std::vector<double> &B, std::vector<double> &C, int m) {
    std::vector<double> R(static_cast<std::size_t>(m) * m, 0.0);
    for (int i = 0; i < m; ++i)
        for (int l = 0; l < m; ++l) {
            const double a = A[static_cast<std::size_t>(i) * m + l];
            if (a == 0.0) continue;
            for (int j = 0; j < m; ++j) R[static_cast<std::size_t>(i) * m + j] += a * B[static_cast<std::size_t>(l) * m + j];
        }
    C.swap(R);
}

inline void ks_mat_power(const std::vector<double> &A, int eA, std::vector<double> &V, int &eV, int m, int n) {
    if (n == 1) {
        V = A;
        eV = eA;
        return;
    }
    ks_mat_power(A, eA, V, eV, m, n / 2);
    std::vector<double> B;
    ks_mat_mult(V, V, B, m);
    int eB = 2 * eV;
    if (n % 2 == 0) {
        V.swap(B);
        eV = eB;
    } else {
        ks_mat_mult(A, B, V, m);
        eV = eA + eB;
    }
    if (V[static_cast<std::size_t>(m / 2) * m + m / 2] > 1e140) {
        for (double &v : V) v *= 1e-140;
        eV += 140;
    }
}

inline double ks_pkolmogorov_exact(int n, double d) {
    if (!(d > 0.0)) return 0.0;
    if (d >= 1.0) return 1.0;
    const int k = static_cast<int>(n * d) + 1, m = 2 * k - 1;
    const double h = k - n * d;
    std::vector<double> H(static_cast<std::size_t>(m) * m);
    for (int i = 0; i < m; ++i)
        for (int j = 0; j < m; ++j) H[static_cast<std::size_t>(i) * m + j] = (i - j + 1 < 0) ? 0.0 : 1.0;
    for (int i = 0; i < m; ++i) {
        H[static_cast<std::size_t>(i) * m] -= std::pow(h, i + 1);
        H[static_cast<std::size_t>(m - 1) * m + i] -= std::pow(h, m - i);
    }
    H[static_cast<std::size_t>(m - 1) * m] += (2 * h - 1 > 0 ? std::pow(2 * h - 1, m) : 0.0);
    for (int i = 0; i < m; ++i)
        for (int j = 0; j < m; ++j)
            if (i - j + 1 > 0)
                for (int g = 1; g <= i - j + 1; ++g) H[static_cast<std::size_t>(i) * m + j] /= g;
    std::vector<double> Q;
    int eQ = 0;
    ks_mat_power(H, 0, Q, eQ, m, n);
    double s = Q[static_cast<std::size_t>(k - 1) * m + k - 1];
    for (int i = 1; i <= n; ++i) {
        s = s * i / n;
        if (s < 1e-140) {
            s *= 1e140;
            eQ -= 140;
        }
    }
    return s * std::pow(10.0, eQ);
}
// --- the whole fit in C++: projected BFGS on hawkes_nll_grad -----------------
//
// Bertsekas's projected quasi-Newton (the scheme L-BFGS-B builds on): variables
// held at a bound by a gradient pointing out of the box are fixed for the step,
// the BFGS direction acts on the rest, the Armijo search runs along the
// projected path, H0 = (s'y / y'y) I before the first update, and a stalled
// direction restarts once from steepest descent. One routine for morie's
// Python and R arms, so the two reach the same optimum from the same start.
inline double hawkes_fit_pbfgs(const double *t, std::size_t n, double T, int bkind, int kind, int method,
                               double eps, double soe_R, double soe_delta, const double *lo, const double *hi,
                               double *x, int maxiter, double gtol, int *iters) {
    const int nb = hawkes_baseline_n(bkind), np = kind == 0 ? 1 : 2, d = nb + 1 + np;
    auto fgrad = [&](const std::vector<double> &v, std::vector<double> &g) {
        double f = hawkes_nll_grad(t, n, T, bkind, v.data(), v[nb], kind, v.data() + nb + 1, method, eps, soe_R,
                                   soe_delta, g.data());
        if (!std::isfinite(f) || f >= 1e11) {
            std::fill(g.begin(), g.end(), 0.0);
            return kBig;
        }
        return f;
    };
    auto proj = [&](std::vector<double> &v) {
        for (int i = 0; i < d; ++i) v[i] = std::min(std::max(v[i], lo[i]), hi[i]);
    };
    std::vector<double> xv(x, x + d), g(d), xn(d), gn(d), dir(d), s(d), y(d), hy(d);
    proj(xv);
    double f = fgrad(xv, g);
    std::vector<double> H(static_cast<std::size_t>(d) * d, 0.0);
    auto reset = [&]() {
        std::fill(H.begin(), H.end(), 0.0);
        for (int i = 0; i < d; ++i) H[static_cast<std::size_t>(i) * d + i] = 1.0;
    };
    reset();
    bool first = true, scale_h0 = true, just_reset = false;
    std::vector<char> fr_prev;
    int it = 0, slow = 0;
    for (it = 1; it <= maxiter; ++it) {
        double pg = 0.0;
        for (int i = 0; i < d; ++i) pg = std::max(pg, std::fabs(xv[i] - std::min(std::max(xv[i] - g[i], lo[i]), hi[i])));
        if (pg < gtol) break;
        std::vector<char> fr(d);
        for (int i = 0; i < d; ++i) fr[i] = !((xv[i] <= lo[i] && g[i] > 0) || (xv[i] >= hi[i] && g[i] < 0));
        // a variable joining or leaving its bound changes the subspace the BFGS matrix models:
        // start it afresh there (curvature learned with the variable free mis-scales the rest; a
        // Lomax fit drifting to the bound on alpha crawled through 1,164 iterations along the ridge)
        if (fr != fr_prev) {
            if (!fr_prev.empty()) {
                reset();
                first = scale_h0 = true;
            }
            fr_prev = fr;
        }
        double slope = 0.0;
        for (int i = 0; i < d; ++i) {
            double acc = 0.0;
            if (fr[i])
                for (int j = 0; j < d; ++j)
                    if (fr[j]) acc += H[static_cast<std::size_t>(i) * d + j] * g[j];
            dir[i] = fr[i] ? -acc : 0.0;
            slope += g[i] * dir[i];
        }
        if (slope >= 0.0) {
            reset();
            for (int i = 0; i < d; ++i) dir[i] = fr[i] ? -g[i] : 0.0;
            first = scale_h0 = true;
        }
        double step = 1.0;
        if (first) {
            double dm = 0.0;
            for (int i = 0; i < d; ++i) dm = std::max(dm, std::fabs(dir[i]));
            step = std::min(1.0, 1.0 / std::max(dm, 1e-300));
        }
        bool accepted = false;
        double fn = kBig;
        for (int ls = 0; ls < 60; ++ls) {
            for (int i = 0; i < d; ++i) xn[i] = xv[i] + step * dir[i];
            proj(xn);
            fn = fgrad(xn, gn);
            double dec = 0.0;
            for (int i = 0; i < d; ++i) dec += g[i] * (xn[i] - xv[i]);
            if (std::isfinite(fn) && fn <= f + 1e-4 * dec) {
                accepted = true;
                break;
            }
            step *= 0.5;
        }
        if (!accepted) {
            if (!just_reset) {
                reset();
                first = scale_h0 = just_reset = true;
                continue;
            }
            break;
        }
        first = false;
        double sy = 0.0, ss = 0.0, yy = 0.0, smax = 0.0, xmax = 1.0;
        for (int i = 0; i < d; ++i) {
            s[i] = xn[i] - xv[i];
            y[i] = gn[i] - g[i];
            sy += s[i] * y[i];
            ss += s[i] * s[i];
            yy += y[i] * y[i];
            smax = std::max(smax, std::fabs(s[i]));
            xmax = std::max(xmax, std::fabs(xv[i]));
        }
        const bool done = std::fabs(f - fn) <= 1e-13 * std::max(1.0, std::fabs(f)) && smax <= 1e-12 * xmax;
        if (sy > 1e-12 * std::sqrt(ss * yy)) {
            const double rho = 1.0 / sy;
            if (scale_h0) {
                const double gam = sy / yy;
                std::fill(H.begin(), H.end(), 0.0);
                for (int i = 0; i < d; ++i) H[static_cast<std::size_t>(i) * d + i] = gam;
                scale_h0 = false;
            }
            double yhy = 0.0;
            for (int i = 0; i < d; ++i) {
                double acc = 0.0;
                for (int j = 0; j < d; ++j) acc += H[static_cast<std::size_t>(i) * d + j] * y[j];
                hy[i] = acc;
                yhy += y[i] * acc;
            }
            for (int i = 0; i < d; ++i)
                for (int j = 0; j < d; ++j)
                    H[static_cast<std::size_t>(i) * d + j] +=
                        -rho * (hy[i] * s[j] + s[i] * hy[j]) + (rho * rho * yhy + rho) * s[i] * s[j];
        }
        // relative decrease below 1e-12 three times running: the remaining iterations only polish
        // digits that cannot matter (scipy's L-BFGS-B stops the same way, by factr)
        slow = (f - fn) <= 1e-12 * std::max(1.0, std::fabs(f)) ? slow + 1 : 0;
        xv = xn;
        f = fn;
        g = gn;
        if (slow >= 3) break;
        if (done) {
            if (!just_reset) {
                reset();
                first = scale_h0 = just_reset = true;
                continue;
            }
            break;
        }
        just_reset = false;
    }
    for (int i = 0; i < d; ++i) x[i] = xv[i];
    if (iters) *iters = it;
    return f;
}
// --- splitmix64 uniforms -------------------------------------------------------
//
// The generator of Steele, Lea & Flood (2014, OOPSLA): one 64-bit state stepped
// by the golden-ratio increment and mixed twice; u = (z >> 11) * 2^-53. A
// stream both arms can reproduce bit for bit (morie's Python computes the same
// numbers in pure Python), used where the R and Python arms must draw the
// same "random" numbers, e.g. the within-day jitter of tied event dates.
inline void splitmix64_uniforms(std::uint64_t seed, std::size_t n, double *out) {
    std::uint64_t x = seed;
    for (std::size_t i = 0; i < n; ++i) {
        x += 0x9E3779B97F4A7C15ULL;
        std::uint64_t z = x;
        z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
        z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
        z ^= z >> 31;
        out[i] = static_cast<double>(z >> 11) * (1.0 / 9007199254740992.0);
    }
}

}  // namespace morie::core

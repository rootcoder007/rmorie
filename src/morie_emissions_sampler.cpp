// SPDX-License-Identifier: AGPL-3.0-or-later
//
// morie_emissions_sampler.cpp -- background CPU-utilisation sampler for the
// emissions tracker (R/emissions.R).
//
// R is single-threaded, so the per-interval sampling that the Python
// tracker (morie.emissions, CodeCarbon methodology) does in a daemon
// thread is done here in a std::thread. The thread never touches the R
// API: it reads the kernel's aggregate CPU tick counters (/proc/stat on
// Linux, host_statistics on macOS) every `interval` seconds, converts each
// delta to a utilisation percentage and keeps (dt, util) pairs. The main
// thread collects them in stop(). Power is then integrated per interval,
// TDP * (0.1 + 0.9 * u^3) * dt, the same nonlinearity the Python arm
// applies to each of its samples.

#include <Rcpp.h>

#include <atomic>
#include <chrono>
#include <mutex>
#include <thread>
#include <vector>

#if defined(__linux__)
#include <fstream>
#include <sstream>
#include <string>
#elif defined(__APPLE__)
#include <mach/mach.h>
#include <mach/mach_host.h>
#endif

namespace {

struct Ticks {
  double idle = 0.0;
  double total = 0.0;
  bool ok = false;
};

Ticks read_ticks() {
  Ticks t;
#if defined(__linux__)
  std::ifstream in("/proc/stat");
  std::string line;
  if (!std::getline(in, line)) return t;
  std::istringstream ss(line);
  std::string cpu;
  ss >> cpu;
  double v[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  int n = 0;
  while (n < 8 && (ss >> v[n])) ++n;
  if (n < 4) return t;
  t.idle = v[3] + (n > 4 ? v[4] : 0.0);
  for (int i = 0; i < n; ++i) t.total += v[i];
  t.ok = true;
#elif defined(__APPLE__)
  host_cpu_load_info_data_t info;
  mach_msg_type_number_t count = HOST_CPU_LOAD_INFO_COUNT;
  if (host_statistics(mach_host_self(), HOST_CPU_LOAD_INFO,
                      reinterpret_cast<host_info_t>(&info), &count) == KERN_SUCCESS) {
    t.idle = static_cast<double>(info.cpu_ticks[CPU_STATE_IDLE]);
    t.total = t.idle + info.cpu_ticks[CPU_STATE_USER] + info.cpu_ticks[CPU_STATE_SYSTEM] +
              info.cpu_ticks[CPU_STATE_NICE];
    t.ok = true;
  }
#endif
  return t;
}

struct Sampler {
  std::thread thread;
  std::atomic<bool> stop{false};
  std::atomic<bool> running{false};
  std::mutex mu;
  std::vector<double> dts;
  std::vector<double> utils;
  Ticks last;
  std::chrono::steady_clock::time_point last_time;
  double interval = 1.0;

  // the static instance is destroyed at process exit: a thread still running there would make
  // std::thread's destructor call std::terminate (seen as a core dump after an R-level error)
  ~Sampler() {
    stop.store(true);
    if (thread.joinable()) thread.join();
  }

  void sample_once() {
    const auto now = std::chrono::steady_clock::now();
    const double dt = std::chrono::duration<double>(now - last_time).count();
    const Ticks cur = read_ticks();
    double util = 0.0;
    if (cur.ok && last.ok && cur.total > last.total) {
      const double didle = cur.idle - last.idle;
      const double dtotal = cur.total - last.total;
      util = 100.0 * (1.0 - didle / dtotal);
      if (util < 0.0) util = 0.0;
      if (util > 100.0) util = 100.0;
    } else if (!cur.ok) {
      util = -1.0;  // no counters on this platform: the R side falls back
    }
    last = cur;
    last_time = now;
    std::lock_guard<std::mutex> lock(mu);
    dts.push_back(dt);
    utils.push_back(util);
  }

  void loop() {
    const auto step = std::chrono::milliseconds(50);
    while (!stop.load()) {
      auto deadline = std::chrono::steady_clock::now() +
                      std::chrono::duration_cast<std::chrono::steady_clock::duration>(
                          std::chrono::duration<double>(interval));
      while (!stop.load() && std::chrono::steady_clock::now() < deadline) {
        std::this_thread::sleep_for(step);
      }
      if (stop.load()) break;
      sample_once();
    }
  }
};

Sampler& sampler() {
  static Sampler s;
  return s;
}

}  // namespace

//' @noRd
// [[Rcpp::export(.emissions_sampler_start)]]
bool emissions_sampler_start(double interval) {
  Sampler& s = sampler();
  if (s.running.load()) return false;
  if (!(interval > 0.0)) interval = 1.0;
  s.interval = interval;
  s.stop.store(false);
  {
    std::lock_guard<std::mutex> lock(s.mu);
    s.dts.clear();
    s.utils.clear();
  }
  s.last = read_ticks();
  s.last_time = std::chrono::steady_clock::now();
  if (!s.last.ok) return false;  // nothing to sample here; the R side uses the fallback
  s.running.store(true);
  try {
    s.thread = std::thread([&s]() { s.loop(); });
  } catch (...) {
    s.running.store(false);
    return false;
  }
  return true;
}

//' @noRd
// [[Rcpp::export(.emissions_sampler_stop)]]
Rcpp::NumericMatrix emissions_sampler_stop() {
  Sampler& s = sampler();
  if (!s.running.load()) return Rcpp::NumericMatrix(0, 2);
  s.stop.store(true);
  if (s.thread.joinable()) s.thread.join();
  s.sample_once();  // the final partial interval
  s.running.store(false);
  std::lock_guard<std::mutex> lock(s.mu);
  const int n = static_cast<int>(s.dts.size());
  Rcpp::NumericMatrix out(n, 2);
  for (int i = 0; i < n; ++i) {
    out(i, 0) = s.dts[i];
    out(i, 1) = s.utils[i];
  }
  Rcpp::colnames(out) = Rcpp::CharacterVector::create("dt", "util_pct");
  return out;
}

//' @noRd
// [[Rcpp::export(.emissions_sampler_running)]]
bool emissions_sampler_running() {
  return sampler().running.load();
}

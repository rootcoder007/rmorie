// SIGPIPE is ignored for the lifetime of this object and restored after it.
//
// The handles here set CURLOPT_NOSIGNAL (so that the timeouts of libcurl do
// not use SIGALRM); with that option libcurl no longer ignores SIGPIPE
// itself. A write to a TLS connection the peer has closed then raises
// SIGPIPE, which R reports as the error "ignoring SIGPIPE signal" instead of
// libcurl returning its own send error. With the signal ignored, the write
// fails with EPIPE and libcurl reports the failed transfer.
#ifndef MORIE_NOPIPE_H
#define MORIE_NOPIPE_H

#ifndef _WIN32
#include <signal.h>

struct MorieNoPipe {
  struct sigaction old_;
  bool set_;
  MorieNoPipe() : set_(false) {
    struct sigaction ign;
    ign.sa_handler = SIG_IGN;
    sigemptyset(&ign.sa_mask);
    ign.sa_flags = 0;
    set_ = sigaction(SIGPIPE, &ign, &old_) == 0;
  }
  ~MorieNoPipe() {
    if (set_) sigaction(SIGPIPE, &old_, nullptr);
  }
  MorieNoPipe(const MorieNoPipe &) = delete;
  MorieNoPipe &operator=(const MorieNoPipe &) = delete;
};
#else
struct MorieNoPipe {};
#endif

#endif

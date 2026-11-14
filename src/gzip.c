/* SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * gzip.c -- the Parquet GZIP page codec (RFC 1952 members) through zlib,
 * which R itself links. Compression at level 9; decompression accepts a
 * gzip or zlib stream (windowBits 15 + 32 detects the header).
 */
#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <string.h>
#include <zlib.h>

SEXP C_rmd_gzip_compress(SEXP x) {
    if (TYPEOF(x) != RAWSXP) Rf_error("gzip: expected a raw vector");
    z_stream s;
    memset(&s, 0, sizeof s);
    if (deflateInit2(&s, 9, Z_DEFLATED, 15 + 16, 9, Z_DEFAULT_STRATEGY) != Z_OK)
        Rf_error("gzip: deflateInit2 failed");
    /* avail_in and avail_out are 32-bit: a larger page would be cut short silently */
    if ((double) XLENGTH(x) > 4.0e9) {
        deflateEnd(&s);
        Rf_error("gzip: a page of %.0f bytes is more than this codec takes", (double) XLENGTH(x));
    }
    uLong bound = deflateBound(&s, (uLong) XLENGTH(x)) + 32;
    unsigned char *buf = (unsigned char *) R_alloc(bound, 1);
    s.next_in = (Bytef *) RAW(x);
    s.avail_in = (uInt) XLENGTH(x);
    s.next_out = buf;
    s.avail_out = (uInt) bound;
    int rc = deflate(&s, Z_FINISH);
    if (rc != Z_STREAM_END) { deflateEnd(&s); Rf_error("gzip: deflate failed (%d)", rc); }
    size_t n = (size_t) s.total_out;
    deflateEnd(&s);
    if ((double) n > (double) R_XLEN_T_MAX) Rf_error("gzip: the compressed page is more than this R can allocate");
    SEXP out = PROTECT(allocVector(RAWSXP, (R_xlen_t) n));
    memcpy(RAW(out), buf, n);
    UNPROTECT(1);
    return out;
}

SEXP C_rmd_gzip_decompress(SEXP x, SEXP size) {
    if (TYPEOF(x) != RAWSXP) Rf_error("gzip: expected a raw vector");
    double want = asReal(size);
    if (!R_FINITE(want) || want < 0 || want != floor(want)) Rf_error("gzip: bad uncompressed size");
    /* checked before allocating, as in snappy.c: deflate expands at most 1032 to 1, and avail_out is 32-bit */
    if (want > 4294967295.0 || want > (double) XLENGTH(x) * 1032.0 + 1024.0)
        Rf_error("gzip: page states %.0f bytes, more than %.0f compressed bytes can hold", want, (double) XLENGTH(x));
    /* before the cast: where R_xlen_t is 32 bits, a larger size is undefined behaviour */
    if (want > (double) R_XLEN_T_MAX) Rf_error("gzip: page states %.0f bytes, more than this R can allocate", want);
    SEXP out = PROTECT(allocVector(RAWSXP, (R_xlen_t) want));
    z_stream s;
    memset(&s, 0, sizeof s);
    if (inflateInit2(&s, 15 + 32) != Z_OK) Rf_error("gzip: inflateInit2 failed");
    s.next_in = (Bytef *) RAW(x);
    s.avail_in = (uInt) XLENGTH(x);
    s.next_out = RAW(out);
    s.avail_out = (uInt) want;
    int rc = inflate(&s, Z_FINISH);
    size_t got = (size_t) s.total_out;
    inflateEnd(&s);
    if (rc != Z_STREAM_END || got != (size_t) want)
        Rf_error("gzip: page did not decompress to its stated %.0f bytes", want);
    UNPROTECT(1);
    return out;
}

/* CRC-32 (zlib's, the polynomial Parquet uses for PageHeader.crc) of a raw vector, as a
 * double so the unsigned 32-bit value survives. */
SEXP C_rmd_crc32(SEXP x) {
    if (TYPEOF(x) != RAWSXP) Rf_error("crc32: `x` must be a raw vector");
    uLong c = crc32(0L, Z_NULL, 0);
    R_xlen_t n = XLENGTH(x);
    const Bytef *p = (const Bytef *) RAW(x);
    while (n > 0) {
        uInt take = n > (R_xlen_t) 0x40000000 ? 0x40000000u : (uInt) n;
        c = crc32(c, p, take);
        p += take;
        n -= take;
    }
    return Rf_ScalarReal((double) c);
}

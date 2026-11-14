/* SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * snappy.c -- Snappy (the Parquet store's page codec), compression and
 * decompression in C. The format is Google's snappy framing-free stream:
 * a varint of the uncompressed length, then literals and back-references.
 * The compressor is the reference greedy one: a 4-byte hash table per
 * 64 KiB block, literals for unmatched bytes, copies with a 2-byte offset.
 * A copy may be longer than its offset (a run), which every snappy decoder
 * reads by copying byte by byte.
 */
#include <R.h>
#include <Rinternals.h>
#include <stdint.h>
#include <string.h>

#define SNP_BLOCK 65536
#define SNP_HBITS 14

static inline uint32_t snp_load32(const unsigned char *p) {
    uint32_t v; memcpy(&v, p, 4); return v;
}
static inline uint32_t snp_hash(uint32_t v) {
    return (v * 0x1e35a7bdu) >> (32 - SNP_HBITS);
}
static unsigned char *snp_literal(unsigned char *op, const unsigned char *src, size_t len) {
    size_t n = len - 1;
    if (n < 60) {
        *op++ = (unsigned char) (n << 2);
    } else if (n < 256) {
        *op++ = 60 << 2; *op++ = (unsigned char) n;
    } else if (n < 65536) {
        *op++ = 61 << 2; *op++ = (unsigned char) (n & 0xff); *op++ = (unsigned char) (n >> 8);
    } else {
        *op++ = 62 << 2; *op++ = (unsigned char) (n & 0xff); *op++ = (unsigned char) ((n >> 8) & 0xff);
        *op++ = (unsigned char) (n >> 16);
    }
    memcpy(op, src, len);
    return op + len;
}
static unsigned char *snp_copy(unsigned char *op, size_t offset, size_t len) {
    /* 2-byte-offset copies carry 1..64 bytes; keep every piece >= 4 */
    while (len >= 68) {
        *op++ = (unsigned char) (((64 - 1) << 2) | 2);
        *op++ = (unsigned char) (offset & 0xff); *op++ = (unsigned char) (offset >> 8);
        len -= 64;
    }
    if (len > 64) {
        *op++ = (unsigned char) (((60 - 1) << 2) | 2);
        *op++ = (unsigned char) (offset & 0xff); *op++ = (unsigned char) (offset >> 8);
        len -= 60;
    }
    *op++ = (unsigned char) (((len - 1) << 2) | 2);
    *op++ = (unsigned char) (offset & 0xff); *op++ = (unsigned char) (offset >> 8);
    return op;
}

SEXP C_rmd_snappy_compress(SEXP x) {
    if (TYPEOF(x) != RAWSXP) Rf_error("snappy: expected a raw vector");
    const unsigned char *src = RAW(x);
    size_t n = (size_t) XLENGTH(x);
    /* the stream starts with the length, which a decoder reads as 32 bits */
    if ((double) n > 4294967295.0)
        Rf_error("snappy: a page of %.0f bytes is more than a Snappy stream can state", (double) n);
    /* worst case: varint + literals with a 3-byte header per 64 KiB + slack */
    size_t cap = 32 + n + n / 6 + 16;
    unsigned char *buf = (unsigned char *) R_alloc(cap, 1);
    unsigned char *op = buf;
    size_t m = n;
    while (m >= 128) { *op++ = (unsigned char) ((m & 127) | 128); m >>= 7; }
    *op++ = (unsigned char) m;
    int32_t table[1 << SNP_HBITS];
    for (size_t base = 0; base < n; base += SNP_BLOCK) {
        size_t blen = n - base < SNP_BLOCK ? n - base : SNP_BLOCK;
        const unsigned char *b = src + base;
        for (int i = 0; i < (1 << SNP_HBITS); i++) table[i] = -1;
        size_t ip = 0, next_emit = 0;
        while (ip + 4 <= blen) {
            uint32_t v = snp_load32(b + ip);
            uint32_t h = snp_hash(v);
            int32_t cand = table[h];
            table[h] = (int32_t) ip;
            if (cand >= 0 && snp_load32(b + cand) == v) {
                size_t offset = ip - (size_t) cand;
                size_t len = 4;
                while (ip + len < blen && b[cand + len] == b[ip + len]) len++;
                if (next_emit < ip) op = snp_literal(op, b + next_emit, ip - next_emit);
                op = snp_copy(op, offset, len);
                ip += len;
                next_emit = ip;
            } else {
                ip++;
            }
        }
        if (next_emit < blen) op = snp_literal(op, b + next_emit, blen - next_emit);
    }
    size_t outn = (size_t) (op - buf);
    if ((double) outn > (double) R_XLEN_T_MAX) Rf_error("snappy: the compressed page is more than this R can allocate");
    SEXP out = PROTECT(allocVector(RAWSXP, (R_xlen_t) outn));
    memcpy(RAW(out), buf, outn);
    UNPROTECT(1);
    return out;
}

SEXP C_rmd_snappy_decompress(SEXP x) {
    if (TYPEOF(x) != RAWSXP) Rf_error("snappy: expected a raw vector");
    const unsigned char *ip = RAW(x), *end = ip + XLENGTH(x);
    uint64_t n = 0; int shift = 0;
    while (1) {
        if (ip >= end || shift > 28) Rf_error("snappy: bad length header");
        unsigned char c = *ip++;
        n |= (uint64_t) (c & 127) << shift;
        if (!(c & 128)) break;
        shift += 7;
    }
    /* checked before allocating: a hostile header must not reach the allocator (a sanitizer build
       aborts on a request it cannot satisfy instead of letting R raise an error) */
    uint64_t room = (uint64_t) (end - ip);
    if (n > 0xFFFFFFFFu || n > room * 64 / 3 + 64)
        Rf_error("snappy: expected %llu bytes, more than %llu compressed bytes can hold",
                 (unsigned long long) n, (unsigned long long) room);
    /* before the cast: where R_xlen_t is 32 bits, a larger size is undefined behaviour */
    if ((double) n > (double) R_XLEN_T_MAX)
        Rf_error("snappy: expected %llu bytes, more than this R can allocate", (unsigned long long) n);
    SEXP out = PROTECT(allocVector(RAWSXP, (R_xlen_t) n));
    unsigned char *o = RAW(out);
    uint64_t pos = 0;
    while (ip < end) {
        unsigned char tag = *ip++;
        int kind = tag & 3;
        if (kind == 0) {
            uint64_t len = tag >> 2;
            if (len >= 60) {
                int extra = (int) len - 59;
                if (ip + extra > end) Rf_error("snappy: truncated literal header");
                len = 0;
                for (int i = 0; i < extra; i++) len |= (uint64_t) ip[i] << (8 * i);
                ip += extra;
            }
            len += 1;
            if (ip + len > end || pos + len > n) Rf_error("snappy: literal runs past the end");
            memcpy(o + pos, ip, len);
            ip += len; pos += len;
            continue;
        }
        uint64_t len, off;
        if (kind == 1) {
            if (ip >= end) Rf_error("snappy: truncated copy");
            len = 4 + ((tag >> 2) & 7);
            off = ((uint64_t) (tag >> 5) << 8) | *ip++;
        } else if (kind == 2) {
            if (ip + 2 > end) Rf_error("snappy: truncated copy");
            len = (tag >> 2) + 1;
            off = (uint64_t) ip[0] | ((uint64_t) ip[1] << 8);
            ip += 2;
        } else {
            if (ip + 4 > end) Rf_error("snappy: truncated copy");
            len = (tag >> 2) + 1;
            off = (uint64_t) ip[0] | ((uint64_t) ip[1] << 8) | ((uint64_t) ip[2] << 16) | ((uint64_t) ip[3] << 24);
            ip += 4;
        }
        if (off == 0 || off > pos || pos + len > n) Rf_error("snappy: bad copy offset %llu", (unsigned long long) off);
        /* byte by byte: an overlapping copy reads bytes it has just written */
        for (uint64_t i = 0; i < len; i++) o[pos + i] = o[pos - off + i];
        pos += len;
    }
    if (pos != n) Rf_error("snappy: expected %llu bytes, decoded %llu", (unsigned long long) n, (unsigned long long) pos);
    UNPROTECT(1);
    return out;
}

/* SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * zstd.c -- a Zstandard decoder for the Parquet reader's ZSTD pages,
 * written from RFC 8878 (Zstandard Compression and the application/zstd
 * Media Type) and the xxHash specification (XXH64). Decode only, and no
 * dictionaries.
 *
 * It reads one or more concatenated frames, skipping skippable frames;
 * raw, RLE and compressed blocks; literals that are raw, RLE, Huffman
 * compressed or treeless, in 1 or 4 streams, with direct or FSE-compressed
 * Huffman weights; sequences whose literal-length, offset and match-length
 * codes use predefined, RLE, FSE-compressed or repeated tables; repeat
 * offsets; overlapping matches; and the optional content checksum (the low
 * 32 bits of XXH64, seed 0).
 *
 * It refuses, each with its own message: a frame that names a dictionary;
 * a reserved bit in a frame header or sequences section, or the reserved
 * block type; a window above max(output size, 8 MiB) or above 512 MiB; a
 * block, or a block's literals, above min(window, 128 KiB); a frame content
 * size that is not what the frame produces, and output that is not exactly
 * the size the caller states; a checksum mismatch; FSE tables above the
 * format's accuracy logs (9 literal lengths, 8 offsets, 9 match lengths,
 * 6 Huffman weights) or whose probabilities do not add up; Huffman codes
 * longer than 11 bits or weights that do not complete a code; bitstreams
 * not consumed exactly; repeat or treeless modes with no earlier table in
 * the frame; an offset beyond the window or before the start of the
 * frame's output; and bytes after a frame that are not another frame.
 *
 * The input is untrusted. Every read is checked against the input and
 * every write against the caller's output size, which is also the hard cap
 * on the output. The history is the output buffer itself, so no window is
 * allocated; the working memory is one fixed-size block the caller supplies
 * (zs_work), and nothing here allocates, so no error path can leak. There
 * is no recursion. Errors are codes; the R error is raised by the wrapper
 * at the end, after the decoder has returned.
 */
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define ZS_BLOCK_MAX 131072u
#define ZS_WIN_FLOOR ((uint64_t) 8 << 20)
#define ZS_WIN_CEIL ((uint64_t) 512 << 20)
#define ZS_HUF_BITS 11
#define ZS_HW_AL 6

enum {
    ZS_OK = 0, ZS_E_TRUNC, ZS_E_MAGIC, ZS_E_RESERVED, ZS_E_DICT, ZS_E_WINDOW,
    ZS_E_FCS, ZS_E_SIZE, ZS_E_CHECKSUM, ZS_E_BLOCKTYPE, ZS_E_BLOCKSIZE,
    ZS_E_LITERALS, ZS_E_HUFTABLE, ZS_E_HUFSTREAM, ZS_E_FSE, ZS_E_SEQ,
    ZS_E_OFFSET, ZS_E_NOTABLE, ZS_E_EMPTY, ZS_E_TRAIL, ZS_E_N
};

static const char *const zs_msgs[ZS_E_N] = {
    "no error",
    "the input ends inside a frame (truncated)",
    "not a Zstandard frame (bad magic number)",
    "a reserved bit is set",
    "the frame needs a dictionary, which is not supported",
    "the frame's window is larger than allowed",
    "the frame's content size does not match its data",
    "the data does not decompress to the stated size",
    "content checksum mismatch",
    "reserved block type",
    "a block is larger than its maximum size",
    "corrupt literals section",
    "corrupt Huffman table",
    "corrupt Huffman-coded literals",
    "corrupt FSE table",
    "corrupt sequences section",
    "a match offset is beyond the window or before the start of the output",
    "a repeat or treeless mode has no earlier table",
    "the input holds no frame",
    "bytes after the last frame are not a frame"
};

static const char *zs_errmsg(int rc) {
    return (rc > 0 && rc < ZS_E_N) ? zs_msgs[rc] : "unknown error";
}

/* Coverage counters for the test harness only (dev/fuzz, -DZS_TRACE). */
#ifdef ZS_TRACE
enum {
    ZT_BLK_RAW, ZT_BLK_RLE, ZT_BLK_CMP, ZT_LIT_RAW, ZT_LIT_RLE, ZT_LIT_CMP1,
    ZT_LIT_CMP4, ZT_LIT_TL1, ZT_LIT_TL4, ZT_HUF_DIRECT, ZT_HUF_FSE, ZT_SEQ_NONE,
    ZT_MODE, /* 12 slots: 4 * {LL, OF, ML} + {predefined, RLE, FSE, repeat} */
    ZT_REP = ZT_MODE + 12, /* 4 slots: repeat offset 1, 2, 3, offset 1 minus 1 */
    ZT_NEWOFF = ZT_REP + 4, ZT_OVERLAP, ZT_SKIP, ZT_CHECKSUM, ZT_NSEQ2, ZT_NSEQ3,
    ZT_N
};
unsigned long zs_trace[ZT_N];
#define ZS_T(i) (zs_trace[(i)]++)
#else
#define ZS_T(i) ((void) 0)
#endif

struct zs_fse { uint16_t base; uint8_t sym; uint8_t nb; };

typedef struct {
    struct zs_fse tab[3][512];   /* literal lengths, offsets, match lengths */
    int al[3], have[3];
    uint16_t huf[1 << ZS_HUF_BITS]; /* symbol | code length << 8 */
    int hufbits, have_huf;
    uint64_t rep[3];
    uint8_t lit[ZS_BLOCK_MAX];
} zs_work;

typedef struct {
    uint8_t *dst;
    size_t cap, pos, fstart, blim;
    uint64_t window;
} zs_out;

static uint64_t zs_le(const uint8_t *p, size_t n) {
    uint64_t v = 0;
    while (n > 0) { n--; v = (v << 8) | p[n]; }
    return v;
}

static int zs_hibit(uint32_t x) {
    int r = 0;
    while (x >>= 1) r++;
    return r;
}

/* ------------------------------------------------------------ bitstreams */

/* Backward bitstream (RFC 8878 4.1): read from the end, after the highest set
 * bit of the last byte. pos counts the bits not yet read; reads past the start
 * give zeros and drive pos negative, which callers treat as corruption. */
typedef struct { const uint8_t *p; int64_t pos; } zs_bits;

static int zs_binit(zs_bits *b, const uint8_t *p, size_t n) {
    uint8_t last;
    if (n == 0 || n > ZS_BLOCK_MAX) return 1; /* every bitstream lies in one block */
    last = p[n - 1];
    if (last == 0) return 1;
    b->p = p;
    b->pos = (int64_t) (n - 1) * 8 + zs_hibit(last);
    return 0;
}

static uint32_t zs_peek(const zs_bits *b, int nb) { /* 0 <= nb <= 32 */
    int64_t start = b->pos - nb, lo = start < 0 ? 0 : start, cnt = b->pos - lo;
    uint64_t v = 0;
    size_t i, i0, i1;
    if (nb == 0 || cnt <= 0) return 0;
    i0 = (size_t) (lo >> 3);
    i1 = (size_t) ((lo + cnt - 1) >> 3);
    for (i = i1 + 1; i-- > i0;) v = (v << 8) | b->p[i];
    v >>= (lo & 7);
    v &= ((uint64_t) 1 << cnt) - 1;
    return (uint32_t) (v << (lo - start));
}

static uint32_t zs_read(zs_bits *b, int nb) {
    uint32_t v = zs_peek(b, nb);
    b->pos -= nb;
    return v;
}

/* Forward little-endian bit reader for FSE table descriptions; bits past the
 * end read as zero and the caller checks how far it went. */
typedef struct { const uint8_t *p; size_t n; uint64_t bp; } zs_fwd;

static uint32_t zs_fpeek(const zs_fwd *f, int nb) { /* nb <= 24 */
    uint32_t v = 0;
    size_t i = (size_t) (f->bp >> 3);
    int k;
    for (k = 3; k >= 0; k--) {
        size_t j = i + (size_t) k;
        v = (v << 8) | (j < f->n ? f->p[j] : 0u);
    }
    return (v >> (f->bp & 7)) & (((uint32_t) 1 << nb) - 1);
}

/* ------------------------------------------------------------ FSE */

/* FSE table description (RFC 8878 4.1.1): accuracy log, then a probability
 * per symbol, with 2-bit repeat flags after a zero probability. */
static int zs_ncount(const uint8_t *p, size_t n, int16_t *prob, int maxsym, int maxal,
                     int *alp, size_t *used) {
    zs_fwd f;
    int32_t remaining, threshold;
    int nbits, sym = 0, prev0 = 0, al, s;
    if (n == 0) return ZS_E_FSE;
    f.p = p; f.n = n; f.bp = 0;
    al = (int) zs_fpeek(&f, 4) + 5;
    f.bp += 4;
    if (al > maxal) return ZS_E_FSE;
    threshold = (int32_t) 1 << al;
    remaining = threshold + 1;
    nbits = al + 1;
    while (remaining > 1 && sym <= maxsym) {
        int32_t max, count, v, a;
        if (prev0) {
            for (;;) {
                uint32_t r = zs_fpeek(&f, 2), k;
                f.bp += 2;
                for (k = 0; k < r; k++) {
                    if (sym > maxsym) return ZS_E_FSE;
                    prob[sym++] = 0;
                }
                if (r != 3) break;
            }
            if (sym > maxsym) return ZS_E_FSE;
            prev0 = 0;
        }
        max = 2 * threshold - 1 - remaining;
        if (max < 0) return ZS_E_FSE;
        v = (int32_t) zs_fpeek(&f, nbits);
        if ((v & (threshold - 1)) < max) {
            count = v & (threshold - 1);
            f.bp += (uint64_t) (nbits - 1);
        } else {
            count = v & (2 * threshold - 1);
            if (count >= threshold) count -= max;
            f.bp += (uint64_t) nbits;
        }
        count--;
        a = count < 0 ? 1 : count;
        if (a >= remaining) return ZS_E_FSE;
        remaining -= a;
        prob[sym++] = (int16_t) count;
        prev0 = (count == 0);
        while (remaining < threshold) { nbits--; threshold >>= 1; }
    }
    if (remaining != 1 || f.bp > (uint64_t) n * 8) return ZS_E_FSE;
    for (s = sym; s <= maxsym; s++) prob[s] = 0;
    *alp = al;
    *used = (size_t) ((f.bp + 7) >> 3);
    return ZS_OK;
}

/* Decoding table from probabilities (RFC 8878 4.1.1, table construction). */
static int zs_fse_build(struct zs_fse *t, const int16_t *prob, int nsym, int al) {
    uint32_t size = (uint32_t) 1 << al, mask = size - 1;
    uint32_t step = (size >> 1) + (size >> 3) + 3, pos = 0, u;
    int32_t high = (int32_t) size - 1;
    uint16_t next[64];
    int s, i;
    if (nsym > 64) return ZS_E_FSE;
    for (s = 0; s < nsym; s++) {
        if (prob[s] < -1) return ZS_E_FSE;
        if (prob[s] == -1) {
            if (high < 0) return ZS_E_FSE;
            t[high--].sym = (uint8_t) s;
            next[s] = 1;
        } else {
            next[s] = (uint16_t) prob[s];
        }
    }
    for (s = 0; s < nsym; s++) {
        for (i = 0; i < prob[s]; i++) {
            if (high < 0) return ZS_E_FSE;
            t[pos].sym = (uint8_t) s;
            do pos = (pos + step) & mask; while (pos > (uint32_t) high);
        }
    }
    if (pos != 0) return ZS_E_FSE;
    for (u = 0; u < size; u++) {
        uint32_t x = next[t[u].sym]++;
        int nb;
        if (x == 0 || x >= 2 * size) return ZS_E_FSE;
        nb = al - zs_hibit(x);
        t[u].nb = (uint8_t) nb;
        t[u].base = (uint16_t) ((x << nb) - size);
    }
    return ZS_OK;
}

/* Predefined distributions (RFC 8878 3.1.1.3.2.2). */
static const int16_t zs_ll_def[36] = {
    4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 1,
    1, 1, 1, 1, -1, -1, -1, -1
};
static const int16_t zs_of_def[29] = {
    1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1,
    -1, -1
};
static const int16_t zs_ml_def[53] = {
    1, 4, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1
};

/* Code to value tables (RFC 8878 3.1.1.3.2.1). */
static const uint32_t zs_ll_base[36] = {
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 22, 24, 28, 32, 40,
    48, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536
};
static const uint8_t zs_ll_bits[36] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8,
    9, 10, 11, 12, 13, 14, 15, 16
};
static const uint32_t zs_ml_base[53] = {
    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 37, 39, 41, 43, 47, 51, 59, 67, 83, 99, 131,
    259, 515, 1027, 2051, 4099, 8195, 16387, 32771, 65539
};
static const uint8_t zs_ml_bits[53] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
};

/* ------------------------------------------------------------ Huffman */

/* Code from weights (RFC 8878 4.2.1): the last weight is implied; codes go
 * to symbols by increasing weight, then increasing symbol value. wt has room
 * for nw + 1 entries. */
static int zs_huf_build(zs_work *w, uint8_t *wt, int nw) {
    uint32_t total = 0, rem, idx = 0;
    int s, k, maxbits;
    for (s = 0; s < nw; s++) {
        if (wt[s] > ZS_HUF_BITS) return ZS_E_HUFTABLE;
        if (wt[s]) total += (uint32_t) 1 << (wt[s] - 1);
    }
    if (total == 0) return ZS_E_HUFTABLE;
    maxbits = zs_hibit(total) + 1;
    if (maxbits > ZS_HUF_BITS) return ZS_E_HUFTABLE;
    rem = ((uint32_t) 1 << maxbits) - total;
    if (rem & (rem - 1)) return ZS_E_HUFTABLE;
    wt[nw] = (uint8_t) (zs_hibit(rem) + 1);
    for (k = 1; k <= maxbits; k++) {
        for (s = 0; s <= nw; s++) {
            if (wt[s] == k) {
                uint32_t cnt = (uint32_t) 1 << (k - 1), j;
                uint16_t e = (uint16_t) (s | ((maxbits + 1 - k) << 8));
                if (cnt > ((uint32_t) 1 << maxbits) - idx) return ZS_E_HUFTABLE;
                for (j = 0; j < cnt; j++) w->huf[idx++] = e;
            }
        }
    }
    if (idx != (uint32_t) 1 << maxbits) return ZS_E_HUFTABLE;
    w->hufbits = maxbits;
    w->have_huf = 1;
    return ZS_OK;
}

/* Huffman tree description (RFC 8878 4.2.1.1-2): direct 4-bit weights, or
 * weights FSE-coded with two interleaved states sharing one table. */
static int zs_huf_desc(zs_work *w, const uint8_t *p, size_t n, size_t *used) {
    uint8_t wt[256];
    int nw = 0;
    uint8_t h;
    w->have_huf = 0;
    if (n < 1) return ZS_E_HUFTABLE;
    h = p[0];
    if (h >= 128) {
        size_t nbytes;
        int i;
        nw = h - 127;
        nbytes = ((size_t) nw + 1) / 2;
        if (nbytes > n - 1) return ZS_E_HUFTABLE;
        for (i = 0; i < nw; i++) {
            uint8_t byte = p[1 + i / 2];
            wt[i] = (uint8_t) ((i & 1) ? (byte & 15) : (byte >> 4));
        }
        *used = 1 + nbytes;
        ZS_T(ZT_HUF_DIRECT);
    } else {
        int16_t prob[13];
        struct zs_fse t[1 << ZS_HW_AL];
        int al;
        size_t hu;
        zs_bits b;
        uint32_t s1, s2;
        if (h == 0 || h > n - 1) return ZS_E_HUFTABLE;
        if (zs_ncount(p + 1, h, prob, 12, ZS_HW_AL, &al, &hu) != ZS_OK) return ZS_E_HUFTABLE;
        if (zs_fse_build(t, prob, 13, al) != ZS_OK) return ZS_E_HUFTABLE;
        if (hu >= h || zs_binit(&b, p + 1 + hu, h - hu)) return ZS_E_HUFTABLE;
        s1 = zs_read(&b, al);
        s2 = zs_read(&b, al);
        if (b.pos < 0) return ZS_E_HUFTABLE;
        for (;;) {
            if (nw >= 255) return ZS_E_HUFTABLE;
            wt[nw++] = t[s1].sym;
            s1 = t[s1].base + zs_read(&b, t[s1].nb);
            if (b.pos < 0) {
                if (nw >= 255) return ZS_E_HUFTABLE;
                wt[nw++] = t[s2].sym;
                break;
            }
            if (nw >= 255) return ZS_E_HUFTABLE;
            wt[nw++] = t[s2].sym;
            s2 = t[s2].base + zs_read(&b, t[s2].nb);
            if (b.pos < 0) {
                if (nw >= 255) return ZS_E_HUFTABLE;
                wt[nw++] = t[s1].sym;
                break;
            }
        }
        *used = 1 + (size_t) h;
        ZS_T(ZT_HUF_FSE);
    }
    return zs_huf_build(w, wt, nw);
}

/* One Huffman-coded stream: exactly cnt symbols, consuming every bit. */
static int zs_huf_stream(const zs_work *w, const uint8_t *p, size_t n, uint8_t *out,
                         size_t cnt) {
    zs_bits b;
    size_t i;
    if (zs_binit(&b, p, n)) return ZS_E_HUFSTREAM;
    for (i = 0; i < cnt; i++) {
        uint16_t e = w->huf[zs_peek(&b, w->hufbits)];
        out[i] = (uint8_t) (e & 255);
        b.pos -= e >> 8;
        if (b.pos < 0) return ZS_E_HUFSTREAM;
    }
    return b.pos == 0 ? ZS_OK : ZS_E_HUFSTREAM;
}

/* ------------------------------------------------------------ blocks */

static int zs_room(const zs_out *o, size_t n) {
    if (n > o->cap - o->pos) return ZS_E_SIZE;
    if (n > o->blim - o->pos) return ZS_E_BLOCKSIZE;
    return ZS_OK;
}

/* Literals section (RFC 8878 3.1.1.3.1). */
static int zs_literals(zs_work *w, const uint8_t *p, size_t n, size_t bms,
                       const uint8_t **lit, size_t *litn, size_t *used) {
    int type, sf;
    size_t hdr, regen, csize;
    if (n < 1) return ZS_E_LITERALS;
    type = p[0] & 3;
    sf = (p[0] >> 2) & 3;
    if (type <= 1) {
        hdr = (sf & 1) ? (sf == 1 ? 2 : 3) : 1;   /* size format 0 or 2: 1 byte */
        if (hdr > n) return ZS_E_LITERALS;
        if (hdr == 1) regen = (size_t) (p[0] >> 3);
        else if (hdr == 2) regen = (size_t) (p[0] >> 4) + ((size_t) p[1] << 4);
        else regen = (size_t) (p[0] >> 4) + ((size_t) p[1] << 4) + ((size_t) p[2] << 12);
        if (regen > bms) return ZS_E_LITERALS;
        if (type == 0) {
            if (regen > n - hdr) return ZS_E_LITERALS;
            *lit = p + hdr;
            *used = hdr + regen;
            ZS_T(ZT_LIT_RAW);
        } else {
            if (n - hdr < 1) return ZS_E_LITERALS;
            if (regen) memset(w->lit, p[hdr], regen);
            *lit = w->lit;
            *used = hdr + 1;
            ZS_T(ZT_LIT_RLE);
        }
        *litn = regen;
        return ZS_OK;
    } else {
        int streams = sf == 0 ? 1 : 4, rc;
        const uint8_t *q;
        size_t m;
        hdr = sf <= 1 ? 3 : (size_t) sf + 2;
        if (hdr > n) return ZS_E_LITERALS;
        if (hdr == 3) {
            uint64_t c = zs_le(p, 3);
            regen = (size_t) ((c >> 4) & 0x3FF);
            csize = (size_t) ((c >> 14) & 0x3FF);
        } else if (hdr == 4) {
            uint64_t c = zs_le(p, 4);
            regen = (size_t) ((c >> 4) & 0x3FFF);
            csize = (size_t) ((c >> 18) & 0x3FFF);
        } else {
            uint64_t c = zs_le(p, 5);
            regen = (size_t) ((c >> 4) & 0x3FFFF);
            csize = (size_t) ((c >> 22) & 0x3FFFF);
        }
        if (regen > bms || csize > n - hdr) return ZS_E_LITERALS;
        q = p + hdr;
        m = csize;
        if (type == 2) {
            size_t tsize;
            rc = zs_huf_desc(w, q, m, &tsize);
            if (rc != ZS_OK) return rc;
            q += tsize;
            m -= tsize;
        } else if (!w->have_huf) {
            return ZS_E_NOTABLE;
        }
        if (streams == 1) {
            rc = zs_huf_stream(w, q, m, w->lit, regen);
            if (rc != ZS_OK) return rc;
            if (type == 2) ZS_T(ZT_LIT_CMP1); else ZS_T(ZT_LIT_TL1);
        } else {
            size_t s[4], seg = (regen + 3) / 4, off = 0;
            int k;
            if (m < 6) return ZS_E_HUFSTREAM;
            s[0] = (size_t) zs_le(q, 2);
            s[1] = (size_t) zs_le(q + 2, 2);
            s[2] = (size_t) zs_le(q + 4, 2);
            if (s[0] + s[1] + s[2] > m - 6) return ZS_E_HUFSTREAM;
            s[3] = m - 6 - s[0] - s[1] - s[2];
            if (3 * seg > regen) return ZS_E_HUFSTREAM;
            q += 6;
            for (k = 0; k < 4; k++) {
                size_t cnt = k < 3 ? seg : regen - 3 * seg;
                rc = zs_huf_stream(w, q, s[k], w->lit + off, cnt);
                if (rc != ZS_OK) return rc;
                q += s[k];
                off += cnt;
            }
            if (type == 2) ZS_T(ZT_LIT_CMP4); else ZS_T(ZT_LIT_TL4);
        }
        *lit = w->lit;
        *litn = regen;
        *used = hdr + csize;
        return ZS_OK;
    }
}

/* Sequences section and sequence execution (RFC 8878 3.1.1.3.2, 3.1.1.4-5). */
static int zs_sequences(zs_work *w, const uint8_t *p, size_t n, const uint8_t *lit,
                        size_t litn, zs_out *o) {
    static const int maxsym[3] = {35, 31, 52}, maxal[3] = {9, 8, 9}, defal[3] = {6, 5, 6};
    static const int16_t *const def[3] = {zs_ll_def, zs_of_def, zs_ml_def};
    static const int defn[3] = {36, 29, 53};
    size_t pos = 1, lp = 0;
    uint32_t nseq, i, st[3];
    int k, rc;
    zs_bits b;
    uint8_t m;
    if (n < 1) return ZS_E_SEQ;
    if (p[0] == 0) {
        nseq = 0;
        ZS_T(ZT_SEQ_NONE);
    } else if (p[0] < 128) {
        nseq = p[0];
    } else if (p[0] < 255) {
        if (n < 2) return ZS_E_SEQ;
        nseq = ((uint32_t) (p[0] - 128) << 8) + p[1];
        pos = 2;
        ZS_T(ZT_NSEQ2);
    } else {
        if (n < 3) return ZS_E_SEQ;
        nseq = (uint32_t) p[1] + ((uint32_t) p[2] << 8) + 0x7F00u;
        pos = 3;
        ZS_T(ZT_NSEQ3);
    }
    /* no sequences: the section ends there, in either form of the count (0x00, or
     * 0x80 0x00), as in libzstd; bytes after it are not part of the block */
    if (nseq == 0 && pos != n) return ZS_E_SEQ;
    if (nseq > 0) {
        if (pos >= n) return ZS_E_SEQ;
        m = p[pos++];
        if (m & 3) return ZS_E_RESERVED;
        for (k = 0; k < 3; k++) {
            int mode = (m >> (6 - 2 * k)) & 3, al;
            struct zs_fse *t = w->tab[k];
            if (mode == 0) {
                rc = zs_fse_build(t, def[k], defn[k], defal[k]);
                if (rc != ZS_OK) return rc;
                w->al[k] = defal[k];
            } else if (mode == 1) {
                if (pos >= n) return ZS_E_SEQ;
                if (p[pos] > maxsym[k]) return ZS_E_SEQ;
                t[0].sym = p[pos++];
                t[0].nb = 0;
                t[0].base = 0;
                w->al[k] = 0;
            } else if (mode == 2) {
                int16_t prob[53];
                size_t used;
                if (pos >= n) return ZS_E_FSE;
                rc = zs_ncount(p + pos, n - pos, prob, maxsym[k], maxal[k], &al, &used);
                if (rc != ZS_OK) return rc;
                rc = zs_fse_build(t, prob, maxsym[k] + 1, al);
                if (rc != ZS_OK) return rc;
                w->al[k] = al;
                pos += used;
            } else if (!w->have[k]) {
                return ZS_E_NOTABLE;
            }
            w->have[k] = 1;
            ZS_T(ZT_MODE + 4 * k + mode);
        }
        if (zs_binit(&b, p + pos, n - pos)) return ZS_E_SEQ;
        for (k = 0; k < 3; k++) st[k] = zs_read(&b, w->al[k]);
        for (i = 0; i < nseq; i++) {
            struct zs_fse lle = w->tab[0][st[0]], ofe = w->tab[1][st[1]], mle = w->tab[2][st[2]];
            uint64_t ofv, off;
            size_t ll, ml;
            ofv = ((uint64_t) 1 << ofe.sym) + zs_read(&b, ofe.sym);
            ml = (size_t) zs_ml_base[mle.sym] + zs_read(&b, zs_ml_bits[mle.sym]);
            ll = (size_t) zs_ll_base[lle.sym] + zs_read(&b, zs_ll_bits[lle.sym]);
            if (i + 1 < nseq) {
                st[0] = lle.base + zs_read(&b, lle.nb);
                st[2] = mle.base + zs_read(&b, mle.nb);
                st[1] = ofe.base + zs_read(&b, ofe.nb);
            }
            if (b.pos < 0) return ZS_E_SEQ;
            if (ofv > 3) {
                off = ofv - 3;
                w->rep[2] = w->rep[1];
                w->rep[1] = w->rep[0];
                w->rep[0] = off;
                ZS_T(ZT_NEWOFF);
            } else {
                int idx = (int) ofv - 1 + (ll == 0);
                ZS_T(ZT_REP + idx);
                if (idx == 0) {
                    off = w->rep[0];
                } else if (idx == 1) {
                    off = w->rep[1];
                    w->rep[1] = w->rep[0];
                    w->rep[0] = off;
                } else {
                    off = idx == 2 ? w->rep[2] : w->rep[0] - 1;
                    if (off == 0) return ZS_E_OFFSET;
                    w->rep[2] = w->rep[1];
                    w->rep[1] = w->rep[0];
                    w->rep[0] = off;
                }
            }
            if (ll > litn - lp) return ZS_E_SEQ;
            if ((rc = zs_room(o, ll)) != ZS_OK) return rc;
            if (ll) memcpy(o->dst + o->pos, lit + lp, ll);
            o->pos += ll;
            lp += ll;
            if (off > (uint64_t) (o->pos - o->fstart) || off > o->window) return ZS_E_OFFSET;
            if ((rc = zs_room(o, ml)) != ZS_OK) return rc;
            {
                uint8_t *d = o->dst + o->pos;
                const uint8_t *s = d - (size_t) off;
                if (off >= ml) {
                    memcpy(d, s, ml);
                } else {
                    size_t j;
                    for (j = 0; j < ml; j++) d[j] = s[j];
                    ZS_T(ZT_OVERLAP);
                }
            }
            o->pos += ml;
        }
        if (b.pos != 0) return ZS_E_SEQ;
    }
    if ((rc = zs_room(o, litn - lp)) != ZS_OK) return rc;
    if (litn > lp) memcpy(o->dst + o->pos, lit + lp, litn - lp);
    o->pos += litn - lp;
    return ZS_OK;
}

/* ------------------------------------------------------------ XXH64 */

#define ZS_P1 11400714785074694791ULL
#define ZS_P2 14029467366897019727ULL
#define ZS_P3 1609587929392839161ULL
#define ZS_P4 9650029242287828579ULL
#define ZS_P5 2870177450012600261ULL

static uint64_t zs_rotl(uint64_t x, int r) { return (x << r) | (x >> (64 - r)); }

static uint64_t zs_round(uint64_t acc, uint64_t lane) {
    acc += lane * ZS_P2;
    return zs_rotl(acc, 31) * ZS_P1;
}

static uint64_t zs_merge(uint64_t acc, uint64_t v) {
    acc ^= zs_round(0, v);
    return acc * ZS_P1 + ZS_P4;
}

static uint64_t zs_xxh64(const uint8_t *p, size_t n) { /* seed 0 */
    uint64_t h;
    size_t i = 0;
    if (n >= 32) {
        uint64_t v1 = ZS_P1 + ZS_P2, v2 = ZS_P2, v3 = 0, v4 = 0 - ZS_P1;
        for (; n - i >= 32; i += 32) {
            v1 = zs_round(v1, zs_le(p + i, 8));
            v2 = zs_round(v2, zs_le(p + i + 8, 8));
            v3 = zs_round(v3, zs_le(p + i + 16, 8));
            v4 = zs_round(v4, zs_le(p + i + 24, 8));
        }
        h = zs_rotl(v1, 1) + zs_rotl(v2, 7) + zs_rotl(v3, 12) + zs_rotl(v4, 18);
        h = zs_merge(h, v1);
        h = zs_merge(h, v2);
        h = zs_merge(h, v3);
        h = zs_merge(h, v4);
    } else {
        h = ZS_P5;
    }
    h += (uint64_t) n;
    for (; n - i >= 8; i += 8) {
        h ^= zs_round(0, zs_le(p + i, 8));
        h = zs_rotl(h, 27) * ZS_P1 + ZS_P4;
    }
    if (n - i >= 4) {
        h ^= zs_le(p + i, 4) * ZS_P1;
        h = zs_rotl(h, 23) * ZS_P2 + ZS_P3;
        i += 4;
    }
    for (; i < n; i++) {
        h ^= (uint64_t) p[i] * ZS_P5;
        h = zs_rotl(h, 11) * ZS_P1;
    }
    h ^= h >> 33;
    h *= ZS_P2;
    h ^= h >> 29;
    h *= ZS_P3;
    h ^= h >> 32;
    return h;
}

/* ------------------------------------------------------------ frames */

/* One Zstandard frame (RFC 8878 3.1.1), magic number already checked. */
static int zs_frame(zs_work *w, const uint8_t *src, size_t n, size_t *ip, zs_out *o) {
    static const size_t didsz[4] = {0, 1, 2, 4};
    size_t p = *ip + 4, fcsn, didn, bms;
    uint64_t window = 0, fcs, wlim;
    int fcsf, ss, chk, rc;
    uint8_t fhd;
    if (p >= n) return ZS_E_TRUNC;
    fhd = src[p++];
    fcsf = fhd >> 6;
    ss = (fhd >> 5) & 1;
    chk = (fhd >> 2) & 1;
    if (fhd & 8) return ZS_E_RESERVED;
    if (!ss) {
        uint8_t wd;
        uint64_t base;
        if (p >= n) return ZS_E_TRUNC;
        wd = src[p++];
        base = (uint64_t) 1 << (10 + (wd >> 3));
        window = base + (base >> 3) * (wd & 7);
    }
    didn = didsz[fhd & 3];
    if (didn > n - p) return ZS_E_TRUNC;
    if (zs_le(src + p, didn) != 0) return ZS_E_DICT;
    p += didn;
    fcsn = fcsf == 0 ? (size_t) ss : (size_t) 1 << fcsf;
    if (fcsn > n - p) return ZS_E_TRUNC;
    fcs = zs_le(src + p, fcsn);
    if (fcsn == 2) fcs += 256;
    p += fcsn;
    if (ss) window = fcs;
    wlim = o->cap > ZS_WIN_FLOOR ? (uint64_t) o->cap : ZS_WIN_FLOOR;
    if (wlim > ZS_WIN_CEIL) wlim = ZS_WIN_CEIL;
    if (window > wlim) return ZS_E_WINDOW;
    if (fcsn && fcs > (uint64_t) (o->cap - o->pos)) return ZS_E_FCS;
    o->fstart = o->pos;
    o->window = window;
    w->have[0] = w->have[1] = w->have[2] = 0;
    w->have_huf = 0;
    w->rep[0] = 1;
    w->rep[1] = 4;
    w->rep[2] = 8;
    bms = window < ZS_BLOCK_MAX ? (size_t) window : ZS_BLOCK_MAX;
    for (;;) {
        uint32_t bh;
        size_t bs;
        int last, bt;
        if (n - p < 3) return ZS_E_TRUNC;
        bh = (uint32_t) zs_le(src + p, 3);
        p += 3;
        last = bh & 1;
        bt = (bh >> 1) & 3;
        bs = bh >> 3;
        if (bt == 3) return ZS_E_BLOCKTYPE;
        if (bs > bms) return ZS_E_BLOCKSIZE;
        o->blim = o->pos + (bms < o->cap - o->pos ? bms : o->cap - o->pos);
        if (bt == 0) {
            if (bs > n - p) return ZS_E_TRUNC;
            if ((rc = zs_room(o, bs)) != ZS_OK) return rc;
            if (bs) memcpy(o->dst + o->pos, src + p, bs);
            o->pos += bs;
            p += bs;
            ZS_T(ZT_BLK_RAW);
        } else if (bt == 1) {
            if (n - p < 1) return ZS_E_TRUNC;
            if ((rc = zs_room(o, bs)) != ZS_OK) return rc;
            if (bs) memset(o->dst + o->pos, src[p], bs);
            o->pos += bs;
            p += 1;
            ZS_T(ZT_BLK_RLE);
        } else {
            const uint8_t *lit;
            size_t litn, used;
            if (bs > n - p) return ZS_E_TRUNC;
            rc = zs_literals(w, src + p, bs, bms, &lit, &litn, &used);
            if (rc != ZS_OK) return rc;
            rc = zs_sequences(w, src + p + used, bs - used, lit, litn, o);
            if (rc != ZS_OK) return rc;
            p += bs;
            ZS_T(ZT_BLK_CMP);
        }
        if (last) break;
    }
    if (fcsn && (uint64_t) (o->pos - o->fstart) != fcs) return ZS_E_FCS;
    if (chk) {
        if (n - p < 4) return ZS_E_TRUNC;
        if ((uint32_t) zs_xxh64(o->dst + o->fstart, o->pos - o->fstart) !=
            (uint32_t) zs_le(src + p, 4))
            return ZS_E_CHECKSUM;
        p += 4;
        ZS_T(ZT_CHECKSUM);
    }
    *ip = p;
    return ZS_OK;
}

/* Whether the k < 4 bytes at p are the start of a frame's or a skippable frame's magic. */
static int zs_magic_prefix(const uint8_t *p, size_t k) {
    static const uint8_t zm[3] = {0x28, 0xB5, 0x2F}, sm[3] = {0x50, 0x2A, 0x4D};
    size_t i;
    int z = 1, s = 1;
    for (i = 0; i < k; i++) {
        if (p[i] != zm[i]) z = 0;
        if ((i == 0 ? (p[i] & 0xF0) : p[i]) != sm[i]) s = 0;
    }
    return z || s;
}

/* Decompress all frames of src into dst, which must come out exactly cap
 * bytes long. work points at sizeof(zs_work) bytes, suitably aligned. */
static int zs_decompress(const uint8_t *src, size_t n, uint8_t *dst, size_t cap, void *work) {
    zs_work *w = (zs_work *) work;
    zs_out o;
    size_t ip = 0;
    int rc;
    if (n == 0) return ZS_E_EMPTY;
    o.dst = dst;
    o.cap = cap;
    o.pos = o.fstart = o.blim = 0;
    o.window = 0;
    while (ip < n) {
        uint32_t magic;
        /* after a frame, 1 to 3 bytes are trailing data unless they begin a magic number */
        if (n - ip < 4) return ip > 0 && !zs_magic_prefix(src + ip, n - ip) ? ZS_E_TRAIL : ZS_E_TRUNC;
        magic = (uint32_t) zs_le(src + ip, 4);
        if ((magic & 0xFFFFFFF0u) == 0x184D2A50u) {
            uint64_t sz;
            if (n - ip < 8) return ZS_E_TRUNC;
            sz = zs_le(src + ip + 4, 4);
            if (sz > (uint64_t) (n - ip - 8)) return ZS_E_TRUNC;
            ip += 8 + (size_t) sz;
            ZS_T(ZT_SKIP);
            continue;
        }
        if (magic != 0xFD2FB528u) return ZS_E_MAGIC;
        rc = zs_frame(w, src, n, &ip, &o);
        if (rc != ZS_OK) return rc;
    }
    return o.pos == cap ? ZS_OK : ZS_E_SIZE;
}

/* ------------------------------------------------------------ R */

#ifndef ZS_NO_R
#include <R.h>
#include <Rinternals.h>
#include <math.h>

SEXP C_rmd_zstd_decompress(SEXP x, SEXP size) {
    double want, n;
    SEXP out;
    void *work;
    int rc;
    if (TYPEOF(x) != RAWSXP) Rf_error("zstd: expected a raw vector");
    want = asReal(size);
    if (!R_FINITE(want) || want < 0 || want != floor(want))
        Rf_error("zstd: bad uncompressed size");
    n = (double) XLENGTH(x);
    /* checked before allocating: an RLE block (4 bytes) is the most a byte of
     * input can yield, 32768 bytes */
    if (want > n * 32768.0 || want > 4503599627370496.0)
        Rf_error("zstd: page states %.0f bytes, more than %.0f compressed bytes can hold",
                 want, n);
    /* before the cast: where R_xlen_t is 32 bits, a larger size is undefined behaviour */
    if (want > (double) R_XLEN_T_MAX)
        Rf_error("zstd: page states %.0f bytes, more than this R can allocate", want);
    out = PROTECT(allocVector(RAWSXP, (R_xlen_t) want));
    work = R_alloc(sizeof(zs_work), 1);
    rc = zs_decompress(RAW(x), (size_t) XLENGTH(x), RAW(out), (size_t) want, work);
    if (rc != ZS_OK) Rf_error("zstd: %s", zs_errmsg(rc));
    UNPROTECT(1);
    return out;
}
#endif

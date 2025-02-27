/*
* Copyright (c) 2022 Calvin Rose
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to
* deal in the Software without restriction, including without limitation the
* rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
* sell copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
* IN THE SOFTWARE.
*/

#ifndef JANET_AMALG
#include "features.h"
#include <janet.h>
#include <stdlib.h>
#include <errno.h>
#include "gc.h"
#include "util.h"
#include "state.h"
#endif

#include <string.h>

/* Begin building a string */
uint8_t *janet_string_begin(int32_t length) {
    JanetStringHead *head = janet_gcalloc(JANET_MEMORY_STRING, sizeof(JanetStringHead) + (size_t) length + 1);
    head->length = length;
    uint8_t *data = (uint8_t *)head->data;
    data[length] = 0;
    return data;
}

/* Finish building a string */
const uint8_t *janet_string_end(uint8_t *str) {
    janet_string_hash(str) = janet_string_calchash(str, janet_string_length(str));
    return str;
}

/* Load a buffer as a string */
const uint8_t *janet_string(const uint8_t *buf, int32_t len) {
    JanetStringHead *head = janet_gcalloc(JANET_MEMORY_STRING, sizeof(JanetStringHead) + (size_t) len + 1);
    head->length = len;
    head->hash = janet_string_calchash(buf, len);
    uint8_t *data = (uint8_t *)head->data;
    safe_memcpy(data, buf, len);
    data[len] = 0;
    return data;
}

/* Compare two strings */
int janet_string_compare(const uint8_t *lhs, const uint8_t *rhs) {
    int32_t xlen = janet_string_length(lhs);
    int32_t ylen = janet_string_length(rhs);
    int32_t len = xlen > ylen ? ylen : xlen;
    int res = memcmp(lhs, rhs, len);
    if (res) return res > 0 ? 1 : -1;
    if (xlen == ylen) return 0;
    return xlen < ylen ? -1 : 1;
}

/* Compare a janet string with a piece of memory */
int janet_string_equalconst(const uint8_t *lhs, const uint8_t *rhs, int32_t rlen, int32_t rhash) {
    int32_t lhash = janet_string_hash(lhs);
    int32_t llen = janet_string_length(lhs);
    if (lhs == rhs)
        return 1;
    if (lhash != rhash || llen != rlen)
        return 0;
    return !memcmp(lhs, rhs, rlen);
}

/* Check if two strings are equal */
int janet_string_equal(const uint8_t *lhs, const uint8_t *rhs) {
    return janet_string_equalconst(lhs, rhs,
                                   janet_string_length(rhs), janet_string_hash(rhs));
}

/* Load a c string */
const uint8_t *janet_cstring(const char *str) {
    return janet_string((const uint8_t *)str, (int32_t)strlen(str));
}

/* Knuth Morris Pratt Algorithm */

struct kmp_state {
    int32_t i;
    int32_t j;
    int32_t textlen;
    int32_t patlen;
    int32_t *lookup;
    const uint8_t *text;
    const uint8_t *pat;
};

static void kmp_init(
    struct kmp_state *s,
    const uint8_t *text, int32_t textlen,
    const uint8_t *pat, int32_t patlen) {
    if (patlen == 0) {
        janet_panic("expected non-empty pattern");
    }
    int32_t *lookup = janet_calloc(patlen, sizeof(int32_t));
    if (!lookup) {
        JANET_OUT_OF_MEMORY;
    }
    s->lookup = lookup;
    s->i = 0;
    s->j = 0;
    s->text = text;
    s->pat = pat;
    s->textlen = textlen;
    s->patlen = patlen;
    /* Init state machine */
    {
        int32_t i, j;
        for (i = 1, j = 0; i < patlen; i++) {
            while (j && pat[j] != pat[i]) j = lookup[j - 1];
            if (pat[j] == pat[i]) j++;
            lookup[i] = j;
        }
    }
}

static void kmp_deinit(struct kmp_state *state) {
    janet_free(state->lookup);
}

static void kmp_seti(struct kmp_state *state, int32_t i) {
    state->i = i;
    state->j = 0;
}

static int32_t kmp_next(struct kmp_state *state) {
    int32_t i = state->i;
    int32_t j = state->j;
    int32_t textlen = state->textlen;
    int32_t patlen = state->patlen;
    const uint8_t *text = state->text;
    const uint8_t *pat = state->pat;
    int32_t *lookup = state->lookup;
    while (i < textlen) {
        if (text[i] == pat[j]) {
            if (j == patlen - 1) {
                state->i = i + 1;
                state->j = lookup[j];
                return i - j;
            } else {
                i++;
                j++;
            }
        } else {
            if (j > 0) {
                j = lookup[j - 1];
            } else {
                i++;
            }
        }
    }
    return -1;
}

/* CFuns */

JANET_CORE_FN(cfun_string_slice,
              "(string/slice bytes &opt start end)",
              "Returns a substring from a byte sequence. The substring is from "
              "index start inclusive to index end exclusive. All indexing "
              "is from 0. 'start' and 'end' can also be negative to indicate indexing "
              "from the end of the string. Note that index -1 is synonymous with "
              "index (length bytes) to allow a full negative slice range. ") {
    JanetByteView view = janet_getbytes(argv, 0);
    JanetRange range = janet_getslice(argc, argv);
    return janet_stringv(view.bytes + range.start, range.end - range.start);
}

JANET_CORE_FN(cfun_symbol_slice,
              "(symbol/slice bytes &opt start end)",
              "Same a string/slice, but returns a symbol.") {
    JanetByteView view = janet_getbytes(argv, 0);
    JanetRange range = janet_getslice(argc, argv);
    return janet_symbolv(view.bytes + range.start, range.end - range.start);
}

JANET_CORE_FN(cfun_keyword_slice,
              "(keyword/slice bytes &opt start end)",
              "Same a string/slice, but returns a keyword.") {
    JanetByteView view = janet_getbytes(argv, 0);
    JanetRange range = janet_getslice(argc, argv);
    return janet_keywordv(view.bytes + range.start, range.end - range.start);
}

JANET_CORE_FN(cfun_string_repeat,
              "(string/repeat bytes n)",
              "Returns a string that is n copies of bytes concatenated.") {
    janet_fixarity(argc, 2);
    JanetByteView view = janet_getbytes(argv, 0);
    int32_t rep = janet_getinteger(argv, 1);
    if (rep < 0) janet_panic("expected non-negative number of repetitions");
    if (rep == 0) return janet_cstringv("");
    int64_t mulres = (int64_t) rep * view.len;
    if (mulres > INT32_MAX) janet_panic("result string is too long");
    uint8_t *newbuf = janet_string_begin((int32_t) mulres);
    uint8_t *end = newbuf + mulres;
    for (uint8_t *p = newbuf; p < end; p += view.len) {
        safe_memcpy(p, view.bytes, view.len);
    }
    return janet_wrap_string(janet_string_end(newbuf));
}

JANET_CORE_FN(cfun_string_bytes,
              "(string/bytes str)",
              "Returns a tuple of integers that are the byte values of the string.") {
    janet_fixarity(argc, 1);
    JanetByteView view = janet_getbytes(argv, 0);
    Janet *tup = janet_tuple_begin(view.len);
    int32_t i;
    for (i = 0; i < view.len; i++) {
        tup[i] = janet_wrap_integer((int32_t) view.bytes[i]);
    }
    return janet_wrap_tuple(janet_tuple_end(tup));
}

JANET_CORE_FN(cfun_string_frombytes,
              "(string/from-bytes & byte-vals)",
              "Creates a string from integer parameters with byte values. All integers "
              "will be coerced to the range of 1 byte 0-255.") {
    int32_t i;
    uint8_t *buf = janet_string_begin(argc);
    for (i = 0; i < argc; i++) {
        int32_t c = janet_getinteger(argv, i);
        buf[i] = c & 0xFF;
    }
    return janet_wrap_string(janet_string_end(buf));
}

JANET_CORE_FN(cfun_string_asciilower,
              "(string/ascii-lower str)",
              "Returns a new string where all bytes are replaced with the "
              "lowercase version of themselves in ASCII. Does only a very simple "
              "case check, meaning no unicode support.") {
    janet_fixarity(argc, 1);
    JanetByteView view = janet_getbytes(argv, 0);
    uint8_t *buf = janet_string_begin(view.len);
    for (int32_t i = 0; i < view.len; i++) {
        uint8_t c = view.bytes[i];
        if (c >= 65 && c <= 90) {
            buf[i] = c + 32;
        } else {
            buf[i] = c;
        }
    }
    return janet_wrap_string(janet_string_end(buf));
}

JANET_CORE_FN(cfun_string_asciiupper,
              "(string/ascii-upper str)",
              "Returns a new string where all bytes are replaced with the "
              "uppercase version of themselves in ASCII. Does only a very simple "
              "case check, meaning no unicode support.") {
    janet_fixarity(argc, 1);
    JanetByteView view = janet_getbytes(argv, 0);
    uint8_t *buf = janet_string_begin(view.len);
    for (int32_t i = 0; i < view.len; i++) {
        uint8_t c = view.bytes[i];
        if (c >= 97 && c <= 122) {
            buf[i] = c - 32;
        } else {
            buf[i] = c;
        }
    }
    return janet_wrap_string(janet_string_end(buf));
}

JANET_CORE_FN(cfun_string_reverse,
              "(string/reverse str)",
              "Returns a string that is the reversed version of str.") {
    janet_fixarity(argc, 1);
    JanetByteView view = janet_getbytes(argv, 0);
    uint8_t *buf = janet_string_begin(view.len);
    int32_t i, j;
    for (i = 0, j = view.len - 1; i < view.len; i++, j--) {
        buf[i] = view.bytes[j];
    }
    return janet_wrap_string(janet_string_end(buf));
}

static void findsetup(int32_t argc, Janet *argv, struct kmp_state *s, int32_t extra) {
    janet_arity(argc, 2, 3 + extra);
    JanetByteView pat = janet_getbytes(argv, 0);
    JanetByteView text = janet_getbytes(argv, 1);
    int32_t start = 0;
    if (argc >= 3) {
        start = janet_getinteger(argv, 2);
        if (start < 0) janet_panic("expected non-negative start index");
    }
    kmp_init(s, text.bytes, text.len, pat.bytes, pat.len);
    s->i = start;
}

JANET_CORE_FN(cfun_string_find,
              "(string/find patt str &opt start-index)",
              "Searches for the first instance of pattern patt in string "
              "str. Returns the index of the first character in patt if found, "
              "otherwise returns nil.") {
    int32_t result;
    struct kmp_state state;
    findsetup(argc, argv, &state, 0);
    result = kmp_next(&state);
    kmp_deinit(&state);
    return result < 0
           ? janet_wrap_nil()
           : janet_wrap_integer(result);
}

JANET_CORE_FN(cfun_string_hasprefix,
              "(string/has-prefix? pfx str)",
              "Tests whether str starts with pfx.") {
    janet_fixarity(argc, 2);
    JanetByteView prefix = janet_getbytes(argv, 0);
    JanetByteView str = janet_getbytes(argv, 1);
    return str.len < prefix.len
           ? janet_wrap_false()
           : janet_wrap_boolean(memcmp(prefix.bytes, str.bytes, prefix.len) == 0);
}

JANET_CORE_FN(cfun_string_hassuffix,
              "(string/has-suffix? sfx str)",
              "Tests whether str ends with sfx.") {
    janet_fixarity(argc, 2);
    JanetByteView suffix = janet_getbytes(argv, 0);
    JanetByteView str = janet_getbytes(argv, 1);
    return str.len < suffix.len
           ? janet_wrap_false()
           : janet_wrap_boolean(memcmp(suffix.bytes,
                                       str.bytes + str.len - suffix.len,
                                       suffix.len) == 0);
}

JANET_CORE_FN(cfun_string_findall,
              "(string/find-all patt str &opt start-index)",
              "Searches for all instances of pattern patt in string "
              "str. Returns an array of all indices of found patterns. Overlapping "
              "instances of the pattern are counted individually, meaning a byte in str "
              "may contribute to multiple found patterns.") {
    int32_t result;
    struct kmp_state state;
    findsetup(argc, argv, &state, 0);
    JanetArray *array = janet_array(0);
    while ((result = kmp_next(&state)) >= 0) {
        janet_array_push(array, janet_wrap_integer(result));
    }
    kmp_deinit(&state);
    return janet_wrap_array(array);
}

struct replace_state {
    struct kmp_state kmp;
    const uint8_t *subst;
    int32_t substlen;
};

static void replacesetup(int32_t argc, Janet *argv, struct replace_state *s) {
    janet_arity(argc, 3, 4);
    JanetByteView pat = janet_getbytes(argv, 0);
    JanetByteView subst = janet_getbytes(argv, 1);
    JanetByteView text = janet_getbytes(argv, 2);
    int32_t start = 0;
    if (argc == 4) {
        start = janet_getinteger(argv, 3);
        if (start < 0) janet_panic("expected non-negative start index");
    }
    kmp_init(&s->kmp, text.bytes, text.len, pat.bytes, pat.len);
    s->kmp.i = start;
    s->subst = subst.bytes;
    s->substlen = subst.len;
}

JANET_CORE_FN(cfun_string_replace,
              "(string/replace patt subst str)",
              "Replace the first occurrence of patt with subst in the string str. "
              "Will return the new string if patt is found, otherwise returns str.") {
    int32_t result;
    struct replace_state s;
    uint8_t *buf;
    replacesetup(argc, argv, &s);
    result = kmp_next(&s.kmp);
    if (result < 0) {
        kmp_deinit(&s.kmp);
        return janet_stringv(s.kmp.text, s.kmp.textlen);
    }
    buf = janet_string_begin(s.kmp.textlen - s.kmp.patlen + s.substlen);
    safe_memcpy(buf, s.kmp.text, result);
    safe_memcpy(buf + result, s.subst, s.substlen);
    safe_memcpy(buf + result + s.substlen,
                s.kmp.text + result + s.kmp.patlen,
                s.kmp.textlen - result - s.kmp.patlen);
    kmp_deinit(&s.kmp);
    return janet_wrap_string(janet_string_end(buf));
}

JANET_CORE_FN(cfun_string_replaceall,
              "(string/replace-all patt subst str)",
              "Replace all instances of patt with subst in the string str. Overlapping "
              "matches will not be counted, only the first match in such a span will be replaced. "
              "Will return the new string if patt is found, otherwise returns str.") {
    int32_t result;
    struct replace_state s;
    JanetBuffer b;
    int32_t lastindex = 0;
    replacesetup(argc, argv, &s);
    janet_buffer_init(&b, s.kmp.textlen);
    while ((result = kmp_next(&s.kmp)) >= 0) {
        janet_buffer_push_bytes(&b, s.kmp.text + lastindex, result - lastindex);
        janet_buffer_push_bytes(&b, s.subst, s.substlen);
        lastindex = result + s.kmp.patlen;
        kmp_seti(&s.kmp, lastindex);
    }
    janet_buffer_push_bytes(&b, s.kmp.text + lastindex, s.kmp.textlen - lastindex);
    const uint8_t *ret = janet_string(b.data, b.count);
    janet_buffer_deinit(&b);
    kmp_deinit(&s.kmp);
    return janet_wrap_string(ret);
}

JANET_CORE_FN(cfun_string_split,
              "(string/split delim str &opt start limit)",
              "Splits a string str with delimiter delim and returns an array of "
              "substrings. The substrings will not contain the delimiter delim. If delim "
              "is not found, the returned array will have one element. Will start searching "
              "for delim at the index start (if provided), and return up to a maximum "
              "of limit results (if provided).") {
    int32_t result;
    JanetArray *array;
    struct kmp_state state;
    int32_t limit = -1, lastindex = 0;
    if (argc == 4) {
        limit = janet_getinteger(argv, 3);
    }
    findsetup(argc, argv, &state, 1);
    array = janet_array(0);
    while ((result = kmp_next(&state)) >= 0 && --limit) {
        const uint8_t *slice = janet_string(state.text + lastindex, result - lastindex);
        janet_array_push(array, janet_wrap_string(slice));
        lastindex = result + state.patlen;
        kmp_seti(&state, lastindex);
    }
    const uint8_t *slice = janet_string(state.text + lastindex, state.textlen - lastindex);
    janet_array_push(array, janet_wrap_string(slice));
    kmp_deinit(&state);
    return janet_wrap_array(array);
}

JANET_CORE_FN(cfun_string_checkset,
              "(string/check-set set str)",
              "Checks that the string str only contains bytes that appear in the string set. "
              "Returns true if all bytes in str appear in set, false if some bytes in str do "
              "not appear in set.") {
    uint32_t bitset[8] = {0, 0, 0, 0, 0, 0, 0, 0};
    janet_fixarity(argc, 2);
    JanetByteView set = janet_getbytes(argv, 0);
    JanetByteView str = janet_getbytes(argv, 1);
    /* Populate set */
    for (int32_t i = 0; i < set.len; i++) {
        int index = set.bytes[i] >> 5;
        uint32_t mask = 1 << (set.bytes[i] & 0x1F);
        bitset[index] |= mask;
    }
    /* Check set */
    for (int32_t i = 0; i < str.len; i++) {
        int index = str.bytes[i] >> 5;
        uint32_t mask = 1 << (str.bytes[i] & 0x1F);
        if (!(bitset[index] & mask)) {
            return janet_wrap_false();
        }
    }
    return janet_wrap_true();
}

JANET_CORE_FN(cfun_string_join,
              "(string/join parts &opt sep)",
              "Joins an array of strings into one string, optionally separated by "
              "a separator string sep.") {
    janet_arity(argc, 1, 2);
    JanetView parts = janet_getindexed(argv, 0);
    JanetByteView joiner;
    if (argc == 2) {
        joiner = janet_getbytes(argv, 1);
    } else {
        joiner.bytes = NULL;
        joiner.len = 0;
    }
    /* Check args */
    int32_t i;
    int64_t finallen = 0;
    for (i = 0; i < parts.len; i++) {
        const uint8_t *chunk;
        int32_t chunklen = 0;
        if (!janet_bytes_view(parts.items[i], &chunk, &chunklen)) {
            janet_panicf("item %d of parts is not a byte sequence, got %v", i, parts.items[i]);
        }
        if (i) finallen += joiner.len;
        finallen += chunklen;
        if (finallen > INT32_MAX)
            janet_panic("result string too long");
    }
    uint8_t *buf, *out;
    out = buf = janet_string_begin((int32_t) finallen);
    for (i = 0; i < parts.len; i++) {
        const uint8_t *chunk = NULL;
        int32_t chunklen = 0;
        if (i) {
            safe_memcpy(out, joiner.bytes, joiner.len);
            out += joiner.len;
        }
        janet_bytes_view(parts.items[i], &chunk, &chunklen);
        safe_memcpy(out, chunk, chunklen);
        out += chunklen;
    }
    return janet_wrap_string(janet_string_end(buf));
}

JANET_CORE_FN(cfun_string_format,
              "(string/format format & values)",
              "Similar to snprintf, but specialized for operating with Janet values. Returns "
              "a new string.") {
    janet_arity(argc, 1, -1);
    JanetBuffer *buffer = janet_buffer(0);
    const char *strfrmt = (const char *) janet_getstring(argv, 0);
    janet_buffer_format(buffer, strfrmt, 0, argc, argv);
    return janet_stringv(buffer->data, buffer->count);
}

static int trim_help_checkset(JanetByteView set, uint8_t x) {
    for (int32_t j = 0; j < set.len; j++)
        if (set.bytes[j] == x)
            return 1;
    return 0;
}

static int32_t trim_help_leftedge(JanetByteView str, JanetByteView set) {
    for (int32_t i = 0; i < str.len; i++)
        if (!trim_help_checkset(set, str.bytes[i]))
            return i;
    return str.len;
}

static int32_t trim_help_rightedge(JanetByteView str, JanetByteView set) {
    for (int32_t i = str.len - 1; i >= 0; i--)
        if (!trim_help_checkset(set, str.bytes[i]))
            return i + 1;
    return 0;
}

static void trim_help_args(int32_t argc, Janet *argv, JanetByteView *str, JanetByteView *set) {
    janet_arity(argc, 1, 2);
    *str = janet_getbytes(argv, 0);
    if (argc >= 2) {
        *set = janet_getbytes(argv, 1);
    } else {
        set->bytes = (const uint8_t *)(" \t\r\n\v\f");
        set->len = 6;
    }
}

JANET_CORE_FN(cfun_string_trim,
              "(string/trim str &opt set)",
              "Trim leading and trailing whitespace from a byte sequence. If the argument "
              "set is provided, consider only characters in set to be whitespace.") {
    JanetByteView str, set;
    trim_help_args(argc, argv, &str, &set);
    int32_t left_edge = trim_help_leftedge(str, set);
    int32_t right_edge = trim_help_rightedge(str, set);
    if (right_edge < left_edge)
        return janet_stringv(NULL, 0);
    return janet_stringv(str.bytes + left_edge, right_edge - left_edge);
}

JANET_CORE_FN(cfun_string_triml,
              "(string/triml str &opt set)",
              "Trim leading whitespace from a byte sequence. If the argument "
              "set is provided, consider only characters in set to be whitespace.") {
    JanetByteView str, set;
    trim_help_args(argc, argv, &str, &set);
    int32_t left_edge = trim_help_leftedge(str, set);
    return janet_stringv(str.bytes + left_edge, str.len - left_edge);
}

JANET_CORE_FN(cfun_string_trimr,
              "(string/trimr str &opt set)",
              "Trim trailing whitespace from a byte sequence. If the argument "
              "set is provided, consider only characters in set to be whitespace.") {
    JanetByteView str, set;
    trim_help_args(argc, argv, &str, &set);
    int32_t right_edge = trim_help_rightedge(str, set);
    return janet_stringv(str.bytes, right_edge);
}

/*****************/
/* JSON Decoding */
/*****************/

#define JSON_KEYWORD_KEY 0x10000
#define JSON_NULL_TO_NIL 0x20000

/* Check if a character is whitespace */
static int white(uint8_t c) {
    return c == '\t' || c == '\n' || c == ' ' || c == '\r';
}

/* Skip whitespace */
static void skipwhite(const char **p) {
    const char *cp = *p;
    for (;;) {
        if (white(*cp))
            cp++;
        else
            break;
    }
    *p = cp;
}

/* Get a hex digit value */
static int hexdig(char dig) {
    if (dig >= '0' && dig <= '9')
        return dig - '0';
    if (dig >= 'a' && dig <= 'f')
        return 10 + dig - 'a';
    if (dig >= 'A' && dig <= 'F')
        return 10 + dig - 'A';
    return -1;
}

/* Convert integer to hex character */
static const char hex_digits[] = "0123456789ABCDEF";
#define tohex(x) (hex_digits[x])

/* Read the hex value for a unicode escape */
static const char *decode_utf16_escape(const char *p, uint32_t *outpoint) {
    if (!p[0] || !p[1] || !p[2] || !p[3])
        return "unexpected end of source";
    int d1 = hexdig(p[0]);
    int d2 = hexdig(p[1]);
    int d3 = hexdig(p[2]);
    int d4 = hexdig(p[3]);
    if (d1 < 0 || d2 < 0 || d3 < 0 || d4 < 0)
        return "invalid hex digit";
    *outpoint = d4 | (d3 << 4) | (d2 << 8) | (d1 << 12);
    return NULL;
}

/* Parse a string. Also handles the conversion of utf-16 to
 * utf-8. */
static const char *decode_string(const char **p, Janet *out) {
    JanetBuffer *buffer = janet_buffer(0);
    const char *cp = *p;
    while (*cp != '"') {
        uint8_t b = (uint8_t) *cp;
        if (b < 32) return "invalid character in string";
        if (b == '\\') {
            cp++;
            switch(*cp) {
            default:
                return "unknown string escape";
            case 'b':
                b = '\b';
                break;
            case 'f':
                b = '\f';
                break;
            case 'n':
                b = '\n';
                break;
            case 'r':
                b = '\r';
                break;
            case 't':
                b = '\t';
                break;
            case '"':
                b = '"';
                break;
            case '\\':
                b = '\\';
                break;
            case '/':
                b = '/';
                break;
            case 'u':
                {
                    /* Get codepoint and check for surrogate pair */
                    uint32_t codepoint;
                    const char *err = decode_utf16_escape(cp + 1, &codepoint);
                    if (err) return err;
                    if (codepoint >= 0xDC00 && codepoint <= 0xDFFF) {
                        return "unexpected utf-16 low surrogate";
                    } else if (codepoint >= 0xD800 && codepoint <= 0xDBFF) {
                        if (cp[5] != '\\') return "expected utf-16 low surrogate pair";
                        if (cp[6] != 'u') return "expected utf-16 low surrogate pair";
                        uint32_t lowsur;
                        const char *err = decode_utf16_escape(cp + 7, &lowsur);
                        if (err) return err;
                        if (lowsur < 0xDC00 || lowsur > 0xDFFF)
                            return "expected utf-16 low surrogate pair";
                        codepoint = ((codepoint - 0xD800) << 10) +
                            (lowsur - 0xDC00) + 0x10000;
                        cp += 11;
                    } else {
                        cp += 5;
                    }
                    /* Write codepoint */
                    if (codepoint <= 0x7F) {
                        janet_buffer_push_u8(buffer, codepoint);
                    } else if (codepoint <= 0x7FF) {
                        janet_buffer_push_u8(buffer, ((codepoint >>  6) & 0x1F) | 0xC0);
                        janet_buffer_push_u8(buffer, ((codepoint >>  0) & 0x3F) | 0x80);
                    } else if (codepoint <= 0xFFFF) {
                        janet_buffer_push_u8(buffer, ((codepoint >> 12) & 0x0F) | 0xE0);
                        janet_buffer_push_u8(buffer, ((codepoint >>  6) & 0x3F) | 0x80);
                        janet_buffer_push_u8(buffer, ((codepoint >>  0) & 0x3F) | 0x80);
                    } else {
                        janet_buffer_push_u8(buffer, ((codepoint >> 18) & 0x07) | 0xF0);
                        janet_buffer_push_u8(buffer, ((codepoint >> 12) & 0x3F) | 0x80);
                        janet_buffer_push_u8(buffer, ((codepoint >>  6) & 0x3F) | 0x80);
                        janet_buffer_push_u8(buffer, ((codepoint >>  0) & 0x3F) | 0x80);
                    }
                }
                continue;
            }
        }
        janet_buffer_push_u8(buffer, b);
        cp++;
    }
    *out = janet_stringv(buffer->data, buffer->count);
    *p = cp + 1;
    return NULL;
}

static const char *decode_one(const char **p, Janet *out, int depth) {

    /* Prevent stack overflow */
    if ((depth & 0xFFFF) > JANET_RECURSION_GUARD) goto recurdepth;

    /* Skip leading whitepspace */
    skipwhite(p);

    /* Main switch */
    switch (**p) {
    default:
        goto badchar;
    case '\0':
        goto eos;
        /* Numbers */
    case '-': case '0': case '1' : case '2': case '3' : case '4':
    case '5': case '6': case '7' : case '8': case '9':
        {
            errno = 0;
            char *end = NULL;
            double x = strtod(*p, &end);
            if (end == *p) goto badnum;
            *p = end;
            *out = janet_wrap_number(x);
            break;
        }
        /* false, null, true */
    case 'f':
        {
            const char *cp = *p;
            if (cp[1] != 'a' || cp[2] != 'l' || cp[3] != 's' || cp[4] != 'e')
                goto badident;
            *out = janet_wrap_false();
            *p = cp + 5;
            break;
        }
    case 'n':
        {
            const char *cp = *p;

            if (cp[1] != 'u' || cp[2] != 'l' || cp[3] != 'l')
                goto badident;
            if (depth & JSON_NULL_TO_NIL) {
                *out = janet_wrap_nil();
            } else {
                *out = janet_ckeywordv("null");
            }
            *p = cp + 4;
            break;
        }
    case 't':
        {
            const char *cp = *p;
            if (cp[1] != 'r' || cp[2] != 'u' || cp[3] != 'e')
                goto badident;
            *out = janet_wrap_true();
            *p = cp + 4;
            break;
        }
        /* String */
    case '"':
        {
            const char *cp = *p + 1;
            const char *start = cp;
            while ((*cp >= 32 || *cp < 0) && *cp != '"' && *cp != '\\')
                cp++;
            /* Only use a buffer for strings with escapes, else just copy
             * memory from source */
            if (*cp == '\\') {
                *p = *p + 1;
                const char *err = decode_string(p, out);
                if (err) return err;
                break;
            }
            if (*cp != '"') goto badchar;
            *p = cp + 1;
            *out = janet_stringv((const uint8_t *)start, cp - start);
            break;
        }
        /* Array */
    case '[':
        {
            *p = *p + 1;
            JanetArray *array = janet_array(0);
            const char *err;
            Janet subval;
            skipwhite(p);
            while (**p != ']') {
                err = decode_one(p, &subval, depth + 1);
                if (err) return err;
                janet_array_push(array, subval);
                skipwhite(p);
                if (**p == ']') break;
                if (**p != ',') goto wantcomma;
                *p = *p + 1;
            }
            *p = *p + 1;
            *out = janet_wrap_array(array);
        }
        break;
        /* Object */
    case '{':
        {
            *p = *p + 1;
            JanetTable *table = janet_table(0);
            const char *err;
            Janet subkey, subval;
            skipwhite(p);
            while (**p != '}') {
                skipwhite(p);
                if (**p != '"') goto wantstring;
                err = decode_one(p, &subkey, depth + 1);
                if (err) return err;
                skipwhite(p);
                if (**p != ':') goto wantcolon;
                *p = *p + 1;
                err = decode_one(p, &subval, depth + 1);
                if (err) return err;
                if (depth & JSON_KEYWORD_KEY) {
                    JanetString str = janet_unwrap_string(subkey);
                    subkey = janet_keywordv(str, janet_string_length(str));
                }
                janet_table_put(table, subkey, subval);
                skipwhite(p);
                if (**p == '}') break;
                if (**p != ',') goto wantcomma;
                *p = *p + 1;
            }
            *p = *p + 1;
            *out = janet_wrap_table(table);
            break;
        }
    }

    /* Good return */
    return NULL;

    /* Errors */
 recurdepth:
    return "recured too deeply";
 eos:
    return "unexpected end of source";
 badident:
    return "bad identifier";
 badnum:
    return "bad number";
 wantcomma:
    return "expected comma";
 wantcolon:
    return "expected colon";
 badchar:
    return "unexpected character";
 wantstring:
    return "expected json string";
}

static Janet json_decode(int32_t argc, Janet *argv) {
    janet_arity(argc, 1, 3);
    Janet ret = janet_wrap_nil();
    const char *err;
    const char *start;
    const char *p;
    if (janet_checktype(argv[0], JANET_BUFFER)) {
        JanetBuffer *buffer = janet_unwrap_buffer(argv[0]);
        /* Ensure 0 padded */
        janet_buffer_push_u8(buffer, 0);
        buffer->count--;
        start = p = (const char *)buffer->data;
    } else {
        JanetByteView bytes = janet_getbytes(argv, 0);
        start = p = (const char *)bytes.bytes;
    }
    int flags = 0;
    if (argc > 1 && janet_truthy(argv[1])) flags |= JSON_KEYWORD_KEY;
    if (argc > 2 && janet_truthy(argv[2])) flags |= JSON_NULL_TO_NIL;
    err = decode_one(&p, &ret, flags);
    /* Check trailing values */
    if (!err) {
        skipwhite(&p);
        if (*p) err = "unexpected extra token";
    }
    if (err)
        janet_panicf("decode error at position %d: %s", p - start, err);
    return ret;
}

/*****************/
/* JSON Encoding */
/*****************/

typedef struct {
    JanetBuffer *buffer;
    int32_t indent;
    const uint8_t *tab;
    const uint8_t *newline;
    int32_t tablen;
    int32_t newlinelen;
} Encoder;

static void encode_newline(Encoder *e) {
    janet_buffer_push_bytes(e->buffer, e->newline, e->newlinelen);
    /* Skip loop if no tab string */
    if (!e->tablen) return;
    for (int32_t i = 0; i < e->indent; i++)
        janet_buffer_push_bytes(e->buffer, e->tab, e->tablen);
}

static const char *encode_one(Encoder *e, Janet x, int depth) {
    switch(janet_type(x)) {
    default:
        goto badtype;
    case JANET_NIL:
        janet_buffer_push_cstring(e->buffer, "null");
        break;
    case JANET_BOOLEAN:
        janet_buffer_push_cstring(e->buffer,
                                  janet_unwrap_boolean(x) ? "true" : "false");
        break;
    case JANET_NUMBER:
        {
            char cbuf[25];
            sprintf(cbuf, "%.17g", janet_unwrap_number(x));
            janet_buffer_push_cstring(e->buffer, cbuf);
        }
        break;
    case JANET_STRING:
    case JANET_SYMBOL:
    case JANET_KEYWORD:
    case JANET_BUFFER:
        {
            const uint8_t *bytes;
            const uint8_t *c;
            const uint8_t *end;
            int32_t len;
            janet_bytes_view(x, &bytes, &len);
            janet_buffer_push_u8(e->buffer, '"');
            c = bytes;
            end = bytes + len;
            while (c < end) {

                /* get codepoint */
                uint32_t codepoint;
                if (*c < 0x80) {
                    /* one byte */
                    codepoint = *c++;
                } else if (*c < 0xE0) {
                    /* two bytes */
                    if (c + 2 > end) goto invalidutf8;
                    if ((c[1] >> 6) != 2) goto invalidutf8;
                    codepoint = ((c[0] & 0x1F) << 6) |
                        (c[1] & 0x3F);
                    c += 2;
                } else if (*c < 0xF0) {
                    /* three bytes */
                    if (c + 3 > end) goto invalidutf8;
                    if ((c[1] >> 6) != 2) goto invalidutf8;
                    if ((c[2] >> 6) != 2) goto invalidutf8;
                    codepoint = ((c[0] & 0x0F) << 12) |
                        ((c[1] & 0x3F) << 6) |
                        (c[2] & 0x3F);
                    c += 3;
                } else if (*c < 0xF8) {
                    /* four bytes */
                    if (c + 4 > end) goto invalidutf8;
                    if ((c[1] >> 6) != 2) goto invalidutf8;
                    if ((c[2] >> 6) != 2) goto invalidutf8;
                    if ((c[3] >> 6) != 2) goto invalidutf8;
                    codepoint = ((c[0] & 0x07) << 18) |
                        ((c[1] & 0x3F) << 12) |
                        ((c[3] & 0x3F) << 6) |
                        (c[3] & 0x3F);
                    c += 4;
                } else {
                    /* invalid */
                    goto invalidutf8;
                }

                /* write codepoint */
                if (codepoint > 0x1F && codepoint < 0x80) {
                    /* Normal, no escape */
                    if (codepoint == '\\' || codepoint == '"')
                        janet_buffer_push_u8(e->buffer, '\\');
                    janet_buffer_push_u8(e->buffer, (uint8_t) codepoint);
                } else if (codepoint < 0x10000) {
                    /* One unicode escape */
                    uint8_t buf[6];
                    buf[0] = '\\';
                    buf[1] = 'u';
                    buf[2] = tohex((codepoint >> 12) & 0xF);
                    buf[3] = tohex((codepoint >> 8) & 0xF);
                    buf[4] = tohex((codepoint >> 4) & 0xF);
                    buf[5] = tohex(codepoint & 0xF);
                    janet_buffer_push_bytes(e->buffer, buf, sizeof(buf));
                } else {
                    /* Two unicode escapes (surrogate pair) */
                    uint32_t hi, lo;
                    uint8_t buf[12];
                    hi = ((codepoint - 0x10000) >> 10) + 0xD800;
                    lo = ((codepoint - 0x10000) & 0x3FF) + 0xDC00;
                    buf[0] = '\\';
                    buf[1] = 'u';
                    buf[2] = tohex((hi >> 12) & 0xF);
                    buf[3] = tohex((hi >> 8) & 0xF);
                    buf[4] = tohex((hi >> 4) & 0xF);
                    buf[5] = tohex(hi & 0xF);
                    buf[6] = '\\';
                    buf[7] = 'u';
                    buf[8] = tohex((lo >> 12) & 0xF);
                    buf[9] = tohex((lo >> 8) & 0xF);
                    buf[10] = tohex((lo >> 4) & 0xF);
                    buf[11] = tohex(lo & 0xF);
                    janet_buffer_push_bytes(e->buffer, buf, sizeof(buf));
                }
            }
            janet_buffer_push_u8(e->buffer, '"');
        }
        break;
    case JANET_TUPLE:
    case JANET_ARRAY:
        {
            const char *err;
            const Janet *items;
            int32_t len;
            janet_indexed_view(x, &items, &len);
            janet_buffer_push_u8(e->buffer, '[');
            e->indent++;
            for (int32_t i = 0; i < len; i++) {
                encode_newline(e);
                if ((err = encode_one(e, items[i], depth + 1))) return err;
                janet_buffer_push_u8(e->buffer, ',');
            }
            e->indent--;
            if (e->buffer->data[e->buffer->count - 1] == ',') {
                e->buffer->count--;
                encode_newline(e);
            }
            janet_buffer_push_u8(e->buffer, ']');
        }
        break;
    case JANET_TABLE:
    case JANET_STRUCT:
        {
            const char *err;
            const JanetKV *kvs;
            int32_t count, capacity;
            janet_dictionary_view(x, &kvs, &count, &capacity);
            janet_buffer_push_u8(e->buffer, '{');
            e->indent++;
            for (int32_t i = 0; i < capacity; i++) {
                if (janet_checktype(kvs[i].key, JANET_NIL))
                    continue;
                if (!janet_checktypes(kvs[i].key, JANET_TFLAG_BYTES))
                    return "object key must be a byte sequence";
                encode_newline(e);
                if ((err = encode_one(e, kvs[i].key, depth + 1)))
                    return err;
                const char *sep = e->tablen ? ": " : ":";
                janet_buffer_push_cstring(e->buffer, sep);
                if ((err = encode_one(e, kvs[i].value, depth + 1)))
                    return err;
                janet_buffer_push_u8(e->buffer, ',');
            }
            e->indent--;
            if (e->buffer->data[e->buffer->count - 1] == ',') {
                e->buffer->count--;
                encode_newline(e);
            }
            janet_buffer_push_u8(e->buffer, '}');
        }
        break;
    }
    return NULL;

    /* Errors */

 badtype:
    return "type not supported";
 invalidutf8:
    return "string contains invalid utf-8";
}

static Janet json_encode(int32_t argc, Janet *argv) {
    janet_arity(argc, 1, 4);
    Encoder e;
    e.indent = 0;
    e.buffer = janet_optbuffer(argv, argc, 3, 10);
    e.tab = NULL;
    e.newline = NULL;
    e.tablen = 0;
    e.newlinelen = 0;
    if (argc >= 2) {
        JanetByteView tab = janet_getbytes(argv, 1);
        e.tab = tab.bytes;
        e.tablen = tab.len;
        if (argc >= 3) {
            JanetByteView newline = janet_getbytes(argv, 2);
            e.newline = newline.bytes;
            e.newlinelen = newline.len;
        } else {
            e.newline = (const uint8_t *)"\r\n";
            e.newlinelen = 2;
        }
    }
    const char *err = encode_one(&e, argv[0], 0);
    if (err) janet_panicf("encode error: %s", err);
    return janet_wrap_buffer(e.buffer);
}


JANET_CORE_FN(cfun_json_decode,
              "(string/trim str &opt set)",
              "Trim leading and trailing whitespace from a byte sequence. If the argument "
              "set is provided, consider only characters in set to be whitespace.") {
    return json_decode(argc, argv);
}

JANET_CORE_FN(cfun_json_encode,
              "(string/trim str &opt set)",
              "Trim leading and trailing whitespace from a byte sequence. If the argument "
              "set is provided, consider only characters in set to be whitespace.") {
    return json_encode(argc, argv);
}


/* Module entry point */
void janet_lib_string(JanetTable *env) {
    JanetRegExt string_cfuns[] = {
        JANET_CORE_REG("s:", cfun_string_slice),
        JANET_CORE_REG("key:", cfun_keyword_slice),
        JANET_CORE_REG("sym:", cfun_symbol_slice),
        JANET_CORE_REG("s*", cfun_string_repeat),
        JANET_CORE_REG("s-bytes", cfun_string_bytes),
        JANET_CORE_REG("s-from-bytes", cfun_string_frombytes),
        JANET_CORE_REG("s_", cfun_string_asciilower),
        JANET_CORE_REG("s^", cfun_string_asciiupper),
        JANET_CORE_REG("s<->", cfun_string_reverse),
        JANET_CORE_REG("s>", cfun_string_find),
        JANET_CORE_REG("s>*", cfun_string_findall),
        JANET_CORE_REG("s-prefix?", cfun_string_hasprefix),
        JANET_CORE_REG("s-suffix?", cfun_string_hassuffix),
        JANET_CORE_REG("s/>", cfun_string_replace),
        JANET_CORE_REG("s/>*", cfun_string_replaceall),
        JANET_CORE_REG("s/", cfun_string_split),
        JANET_CORE_REG("s-check-set", cfun_string_checkset),
        JANET_CORE_REG("s-join", cfun_string_join),
        JANET_CORE_REG("s-fmt", cfun_string_format),
        JANET_CORE_REG("s//", cfun_string_trim),
        JANET_CORE_REG("s/-", cfun_string_triml),
        JANET_CORE_REG("s-/", cfun_string_trimr),
        JANET_CORE_REG("json-encode", cfun_json_encode),
        JANET_CORE_REG("json-decode", cfun_json_decode),
        JANET_REG_END
    };
    janet_core_cfuns_ext(env, NULL, string_cfuns);
}

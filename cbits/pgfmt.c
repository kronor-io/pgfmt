/*
 * The libpg_query calls pgfmt makes, reshaped for Haskell's FFI, which cannot
 * take or return C structs by value. Every function returns 0 on success and
 * hands back a malloc'd buffer in *out / *out_len, which the caller frees with
 * free(). On failure it returns 1, *out holds the error message (also malloc'd,
 * NUL-terminated) and *cursor the 1-based byte position libpg_query reported,
 * or 0.
 */
#include <pg_query.h>
#include <stdlib.h>
#include <string.h>

static char *copy_bytes(const char *src, size_t len)
{
    char *dst = malloc(len + 1);
    if (dst == NULL)
        return NULL;
    if (len > 0)
        memcpy(dst, src, len);
    dst[len] = '\0';
    return dst;
}

static int fail(const PgQueryError *error, char **out, size_t *out_len, int *cursor)
{
    const char *message = error->message ? error->message : "unknown libpg_query error";
    *out_len = strlen(message);
    *out = copy_bytes(message, *out_len);
    *cursor = error->cursorpos;
    return 1;
}

/* The protobuf parse tree of sql. */
int pgfmt_parse(const char *sql, char **out, size_t *out_len, int *cursor)
{
    PgQueryProtobufParseResult result = pg_query_parse_protobuf(sql);
    int rc;
    if (result.error) {
        rc = fail(result.error, out, out_len, cursor);
    } else {
        *out_len = result.parse_tree.len;
        *out = copy_bytes(result.parse_tree.data, result.parse_tree.len);
        *cursor = 0;
        rc = 0;
    }
    pg_query_free_protobuf_parse_result(result);
    return rc;
}

/*
 * The JSON parse tree of sql. Unlike the protobuf, it tells an empty string
 * from a missing one (comment on t is '' from comment on t is null).
 */
int pgfmt_parse_json(const char *sql, char **out, size_t *out_len, int *cursor)
{
    PgQueryParseResult result = pg_query_parse(sql);
    int rc;
    if (result.error) {
        rc = fail(result.error, out, out_len, cursor);
    } else {
        *out_len = strlen(result.parse_tree);
        *out = copy_bytes(result.parse_tree, *out_len);
        *cursor = 0;
        rc = 0;
    }
    pg_query_free_parse_result(result);
    return rc;
}

/* The protobuf token stream of sql, comments included. */
int pgfmt_scan(const char *sql, char **out, size_t *out_len, int *cursor)
{
    PgQueryScanResult result = pg_query_scan(sql);
    int rc;
    if (result.error) {
        rc = fail(result.error, out, out_len, cursor);
    } else {
        *out_len = result.pbuf.len;
        *out = copy_bytes(result.pbuf.data, result.pbuf.len);
        *cursor = 0;
        rc = 0;
    }
    pg_query_free_scan_result(result);
    return rc;
}

/* libpg_query's JSON for the PL/pgSQL functions sql creates. */
int pgfmt_parse_plpgsql(const char *sql, char **out, size_t *out_len, int *cursor)
{
    PgQueryPlpgsqlParseResult result = pg_query_parse_plpgsql(sql);
    int rc;
    if (result.error) {
        rc = fail(result.error, out, out_len, cursor);
    } else {
        const char *json = result.plpgsql_funcs ? result.plpgsql_funcs : "[]";
        *out_len = strlen(json);
        *out = copy_bytes(json, *out_len);
        *cursor = 0;
        rc = 0;
    }
    pg_query_free_plpgsql_parse_result(result);
    return rc;
}

/*
 * SQL for a protobuf parse tree. With pretty set, libpg_query's pretty printer
 * lays it out, and the comments of comment_source (the text the tree was parsed
 * from, so their locations line up) are put back; comment_source may be NULL.
 */
int pgfmt_deparse(const char *tree, size_t tree_len, int pretty, int indent_size,
                  int max_line_length, const char *comment_source,
                  char **out, size_t *out_len, int *cursor)
{
    PgQueryProtobuf protobuf = {tree_len, (char *)tree};
    PostgresDeparseOpts opts;
    memset(&opts, 0, sizeof opts);

    PgQueryDeparseCommentsResult comments = {0};
    int have_comments = 0;
    if (pretty) {
        opts.pretty_print = true;
        opts.indent_size = indent_size;
        opts.max_line_length = max_line_length;
        if (comment_source != NULL) {
            comments = pg_query_deparse_comments_for_query(comment_source);
            have_comments = 1;
            if (comments.error) {
                int rc = fail(comments.error, out, out_len, cursor);
                pg_query_free_deparse_comments_result(comments);
                return rc;
            }
            opts.comments = comments.comments;
            opts.comment_count = comments.comment_count;
        }
    }

    PgQueryDeparseResult result = pretty
        ? pg_query_deparse_protobuf_opts(protobuf, opts)
        : pg_query_deparse_protobuf(protobuf);
    int rc;
    if (result.error) {
        rc = fail(result.error, out, out_len, cursor);
    } else {
        *out_len = strlen(result.query);
        *out = copy_bytes(result.query, *out_len);
        *cursor = 0;
        rc = 0;
    }
    pg_query_free_deparse_result(result);
    if (have_comments)
        pg_query_free_deparse_comments_result(comments);
    return rc;
}

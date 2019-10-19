
#include "lisp.hpp"

#include "stream_impl.hpp"

static Bool   stream_inited    = 0;
static I32    free_list = -1; /* intrusive free list */
static Stream streams[MAX_STREAMS];
static I32    num_streams = 0;

static Stream * lookup(I32 index)
{
    ASSERT(index < MAX_STREAMS);
    return &streams[index];
}

static void maybe_init_free_list()
{
    if (stream_inited)
    {
        return;
    }

    for (U32 i = 0; i < MAX_STREAMS - 1; ++i)
    {
        streams[i].next = i + 1;
    }
    streams[MAX_STREAMS - 1].next = -1;
    free_list = 0;

    stream_inited = 1;
}

static I32 alloc_index()
{
    maybe_init_free_list();

    if (free_list == -1)
    {
        return -1;
    }
    else
    {
        I32 index = free_list;
        free_list = streams[index].next;
        ++num_streams;
        return index;
    }
}

static void release_index(I32 index)
{
    maybe_init_free_list();

    ASSERT(index < MAX_STREAMS);
    streams[index].next = free_list;
    free_list = index;
    --num_streams;
}

static I32 next_index(I32 index)
{
    Stream * stream = lookup(index);
    DEBUG_ASSERT(stream);
    return stream->next;
}

static I32 stream_index(Expr exp)
{
    DEBUG_ASSERT(is_stream(exp));
    return (I32) expr_data(exp);
}

Bool is_stream(Expr exp)
{
    return expr_type(exp) == TYPE_STREAM;
}

Expr make_file_input_stream(FILE * file, char const * name, Bool close_on_free)
{
    I32 index = alloc_index();
    if (index == -1)
    {
        return ERROR("ran out of streams");
    }

    Stream * stream = lookup(index);
    _make_file_input_stream(stream, file, name, close_on_free);
    stream->index = index; // actually unused
    return make_expr(TYPE_STREAM, (U64) index);
}

Expr make_file_input_stream_from_path(char const * ifn)
{
    FILE * file = fopen(ifn, "rb");
    if (!file)
    {
        return ERROR("cannot open %s", ifn);
    }

    return make_file_input_stream(file, ifn, 1);
}

Expr make_file_output_stream(FILE * file, char const * name, Bool close_on_free)
{
    I32 index = alloc_index();
    if (index == -1)
    {
        return ERROR("ran out of streams");
    }

    Stream * stream = lookup(index);
    _make_file_output_stream(stream, file, name, close_on_free);
    stream->index = index; // actually unused
    return make_expr(TYPE_STREAM, (U64) index);
}

Expr make_file_output_stream_from_path(char const * ofn)
{
    FILE * file = fopen(ofn, "wb");
    if (!file)
    {
        return ERROR("cannot open %s\n", ofn);
    }

    return make_file_output_stream(file, ofn, 1);
}

Expr make_string_input_stream(char const * str)
{
    I32 index = alloc_index();
    if (index == -1)
    {
        return ERROR("ran out of streams");
    }

    Stream * stream = lookup(index);
    _make_string_input_stream(stream, str);
    stream->index = index;
    return make_expr(TYPE_STREAM, (U64) index);
}

Expr make_string_output_stream()
{
    I32 index = alloc_index();
    ASSERT(index != -1);
    if (index == -1)
    {
        return ERROR("ran out of streams");
    }

    Stream * stream = lookup(index);
    _make_string_output_stream(stream);
    stream->index = index; // actually unused
    return make_expr(TYPE_STREAM, (U64) index); /* we know the index is not negative */
}

// TODO as we cannot write to exp, this leaves dangling references
void stream_close(Expr exp)
{
    ASSERT_VOID(is_stream(exp));

    I32 const index = stream_index(exp);
    Stream * stream = lookup(index);
    _free_stream(stream);
    release_index(index);
}

Stream * stream_impl(Expr exp)
{
    if (!is_stream(exp))
    {
        ERROR("expression is not a stream");
    }

    I32 const index = stream_index(exp);
    return lookup(index);
}

char const * stream_to_string(Expr exp)
{
    return _stream_to_string(stream_impl(exp));
}

void stream_put_cstring(Expr exp, char const * str)
{
    _stream_put_cstring(stream_impl(exp), str);
}

void stream_put_char(Expr exp, U32 ch)
{
    _stream_put_char(stream_impl(exp), ch);
}

void stream_put_u64(Expr exp, U64 val)
{
    _stream_put_u64(stream_impl(exp), val);
}

void stream_put_i64(Expr exp, I64 val)
{
    _stream_put_i64(stream_impl(exp), val);
}

void stream_put_x64(Expr exp, U64 val)
{
    _stream_put_x64(stream_impl(exp), val);
}

void stream_put_ptr(Expr exp, void * ptr)
{
    _stream_put_ptr(stream_impl(exp), ptr);
}

void stream_put_f32(Expr exp, F32 val)
{
    _stream_put_f32(stream_impl(exp), val);
}

char stream_peek_char(Expr in)
{
    return _stream_peek_char(stream_impl(in));
}

void stream_skip_char(Expr in)
{
    _stream_skip_char(stream_impl(in));
}

char stream_get_char(Expr in)
{
    return _stream_get_char(stream_impl(in));
}

Bool stream_at_end(Expr in)
{
    return _stream_at_end(stream_impl(in));
}

void stream_show_status()
{
    printf("STREAM STATUS:\n");
    printf("in use: %d/%d\n", num_streams, MAX_STREAMS);
    printf("free list:");
    for (I32 index = free_list; index != -1; index = next_index(index))
    {
        printf(" %d", index);
    }
    printf("\n");
}

char const * stream_name(Expr exp)
{
    char const * name = _stream_name(stream_impl(exp));
    return name;
}

I64 stream_offset(Expr exp)
{
    return _stream_offset(stream_impl(exp));
}

static Expr b_make_file_input_stream(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments");
    }

    char const * name = string_value(exp);
    return make_file_input_stream(fopen(name, "rt"), name, 1);
}

static Expr b_make_file_output_stream(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments");
    }

    char const * name = string_value(exp);
    return make_file_output_stream(fopen(name, "wt"), name, 1);
}

static Expr f_make_string_input_stream(Expr string)
{
    return make_string_input_stream(string_value(string));
}

static Expr b_make_string_input_stream(Expr args, Expr env, void * user)
{
    Expr exp = nil;
    if (!unpack_all_args(args, "x", &exp))
    {
        return ERROR("illegal arguments");
    }

    return f_make_string_input_stream(exp);
}

static Expr b_make_string_output_stream(Expr args, Expr env, void * user)
{
    if (!unpack_all_args(args, ""))
    {
        return ERROR("illegal arguments");
    }

    return make_string_output_stream();
}

static Expr b_stream_close(Expr args, Expr env, void * user)
{
    stream_close(car(args));
    return nil;
}

static Expr b_stream_name(Expr args, Expr env, void * user)
{
    char const * name = stream_name(car(args));
    if (!name)
    {
        return nil;
    }
    return make_string(name);
}

static Expr f_stream_offset(Expr stream)
{
    return i64_to_num(stream_offset(stream));
}

static Expr b_stream_offset(Expr args, Expr env, void * user)
{
    return f_stream_offset(car(args));
}

static Expr f_stream_put_string(Expr stream, Expr string)
{
    stream_put_cstring(stream, string_value(string));
    return nil;
}

static Expr b_stream_put_string(Expr args, Expr env, void * user)
{
    return f_stream_put_string(car(args), cadr(args));
}

static Expr f_stream_put_char(Expr stream, Expr ch)
{
    stream_put_char(stream, num_to_u32(ch));
    return nil;
}

static Expr b_stream_put_char(Expr args, Expr env, void * user)
{
    return f_stream_put_char(car(args), cadr(args));
}

Expr f_stream_to_string(Expr exp)
{
    return make_string(stream_to_string(exp));
}

static Expr b_stream_to_string(Expr args, Expr env, void * user)
{
    return f_stream_to_string(car(args));
}

static Expr f_stream_peek_char(Expr stream)
{
    // TODO use char_to_num
    return int_to_num((int) stream_peek_char(stream));
}

static Expr b_stream_peek_char(Expr args, Expr env, void * user)
{
    return f_stream_peek_char(car(args));
}

static Expr f_stream_skip_char(Expr stream)
{
    stream_skip_char(stream);
    return nil;
}

static Expr b_stream_skip_char(Expr args, Expr env, void * user)
{
    return f_stream_skip_char(car(args));
}

void bind_stream(Expr env)
{
    env_def(env, QUOTE(make-file-input-stream   ), make_builtin_fun(b_make_file_input_stream   , NULL));
    env_def(env, QUOTE(make-file-output-stream  ), make_builtin_fun(b_make_file_output_stream  , NULL));
    env_def(env, QUOTE(make-string-input-stream ), make_builtin_fun(b_make_string_input_stream , NULL));
    env_def(env, QUOTE(make-string-output-stream), make_builtin_fun(b_make_string_output_stream, NULL));

    env_def(env, QUOTE(stream-close             ), make_builtin_fun(b_stream_close             , NULL));
    env_def(env, QUOTE(stream-name              ), make_builtin_fun(b_stream_name              , NULL));
    env_def(env, QUOTE(stream-offset            ), make_builtin_fun(b_stream_offset            , NULL));

    env_def(env, QUOTE(stream-put-string        ), make_builtin_fun(b_stream_put_string        , NULL));
    env_def(env, QUOTE(stream-put-char          ), make_builtin_fun(b_stream_put_char          , NULL));
    env_def(env, QUOTE(stream-to-string         ), make_builtin_fun(b_stream_to_string         , NULL));

    env_def(env, QUOTE(stream-peek-char         ), make_builtin_fun(b_stream_peek_char         , NULL));
    env_def(env, QUOTE(stream-skip-char         ), make_builtin_fun(b_stream_skip_char         , NULL));
}

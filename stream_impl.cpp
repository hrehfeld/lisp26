
#include "lisp.hpp"

#include "stream_impl.hpp"

enum
{
    STREAM_TYPE_NONE,
    STREAM_TYPE_FILE_OUTPUT,
    STREAM_TYPE_FILE_OUTPUT_CLOSE,
    STREAM_TYPE_FILE_INPUT,
    STREAM_TYPE_FILE_INPUT_CLOSE,
    STREAM_TYPE_STRING_OUTPUT,
    STREAM_TYPE_STRING_INPUT,
};

#define DEFAULT_CAPACITY  4

static void ensure_capacity(Stream * stream, size_t cap)
{
    DEBUG_ASSERT(stream);

    size_t const old_cap = stream->cap;
    while (stream->cap < cap)
    {
        stream->cap *= 2;
    }
    if (stream->cap != old_cap)
    {
        stream->buf = (U8 *) realloc(stream->buf, stream->cap);
        memset(stream->buf + old_cap, 0, stream->cap - old_cap);
    }
}

void _make_file_input_stream(Stream * stream, FILE * file, Bool close_on_free)
{
    ASSERT(stream);

    memset(stream, 0, sizeof(Stream));

    stream->type = close_on_free ?
        STREAM_TYPE_FILE_INPUT_CLOSE :
        STREAM_TYPE_FILE_INPUT;
    stream->file = file;
}

void _make_file_output_stream(Stream * stream, FILE * file, Bool close_on_free)
{
    ASSERT(stream);

    memset(stream, 0, sizeof(Stream));

    stream->type = close_on_free ?
        STREAM_TYPE_FILE_OUTPUT_CLOSE :
        STREAM_TYPE_FILE_OUTPUT;
    stream->file = file;
}

// TODO add option to copy the value?
void _make_string_input_stream(Stream * stream, char const * str)
{
    ASSERT(stream);

    memset(stream, 0, sizeof(Stream));

    stream->type = STREAM_TYPE_STRING_INPUT;
    stream->buf  = (U8 *) str; // TODO this cast is somewhat suboptimal
    stream->pos  = 0;
    stream->cap  = strlen(str) + 1;
}

void _make_string_output_stream(Stream * stream)
{
    ErrorContext ec("make-string-output-stream");
    ASSERT(stream);

    ASSERT(DEFAULT_CAPACITY);

    memset(stream, 0, sizeof(Stream));
    stream->type = STREAM_TYPE_STRING_OUTPUT;
    stream->pos  = 0;
    stream->cap  = DEFAULT_CAPACITY;
    stream->buf  = (U8 *) malloc(stream->cap);
    memset(stream->buf, 0, stream->cap);
}

void _free_stream(Stream * stream)
{
    ASSERT(stream);

    switch (stream->type)
    {
    case STREAM_TYPE_FILE_INPUT_CLOSE:
    case STREAM_TYPE_FILE_OUTPUT_CLOSE:
        ASSERT(stream->file);
        fclose(stream->file);
        break;
    case STREAM_TYPE_STRING_OUTPUT:
        ASSERT(stream->buf);
        free(stream->buf);
        break;
    }

    memset(stream, 0, sizeof(Stream));
}

void _stream_put_cstring(Stream * out, char const * str)
{
    ASSERT(out);

    switch (out->type)
    {
    case STREAM_TYPE_FILE_OUTPUT:
    case STREAM_TYPE_FILE_OUTPUT_CLOSE:
        fputs(str, out->file);
        break;
    case STREAM_TYPE_STRING_OUTPUT:
    {
        size_t const len = strlen(str);
        ensure_capacity(out, out->pos + len + 1);
        memcpy(out->buf + out->pos, str, len + 1);
        out->pos += len;
        break;
    }
    default:
        ERROR("cannot write to stream of type %u", out->type);
        break;
    }
}

void _stream_put_char(Stream * out, U32 ch)
{
    /* don't do an assert for every char in release */
    DEBUG_ASSERT(out);

    switch (out->type)
    {
    case STREAM_TYPE_FILE_OUTPUT:
    case STREAM_TYPE_FILE_OUTPUT_CLOSE:
        fputc((char) ch, out->file);
        break;

    case STREAM_TYPE_STRING_OUTPUT:
        /* TODO only put zeros in stream_string? */
        /* TODO better: preset buffer to zero! */
#if 1 /* TODO utf-8 test hack */
        if (ch == 231)
        {
            ensure_capacity(out, out->pos + 3);
            out->buf[out->pos++] = 195;
            out->buf[out->pos++] = 167;
            out->buf[out->pos] = '\0';
        }
        else if (ch == 12363)
        {
            ensure_capacity(out, out->pos + 4);
            out->buf[out->pos++] = 227;
            out->buf[out->pos++] = 129;
            out->buf[out->pos++] = 139;
            out->buf[out->pos] = '\0';
        }
        else
#endif
        {
            ensure_capacity(out, out->pos + 2);
            out->buf[out->pos++] = (U8) ch;
            out->buf[out->pos] = '\0';
        }
        break;

    default:
        ERROR("cannot write to stream of type %u", out->type);
        break;
    }
}

void _stream_put_ptr(Stream * out, void const * ptr)
{
    ASSERT(out);

    /* TODO roll your own */
    char tmp[32];
    sprintf(tmp, "%p", ptr);
    _stream_put_cstring(out, tmp);
}

void _stream_put_u64(Stream * out, U64 num)
{
    ASSERT(out);

    /* TODO roll your own */
    char tmp[32];
    sprintf(tmp, "%" PRIu64, num);
    _stream_put_cstring(out, tmp);
}

void _stream_put_i64(Stream * out, I64 num)
{
    ASSERT(out);

    /* TODO roll your own */
    char tmp[32];
    sprintf(tmp, "%" PRIi64, num);
    _stream_put_cstring(out, tmp);
}

void _stream_put_x64(Stream * out, U64 num)
{
    ASSERT(out);

    /* TODO roll your own */
    char tmp[32];
    sprintf(tmp, "%016" PRIx64, num);
    _stream_put_cstring(out, tmp);
}

void _stream_put_f32(Stream * out, F32 num)
{
    ASSERT(out);

    /* TODO roll your own */
    char tmp[32];
    sprintf(tmp, "%f", num);
    _stream_put_cstring(out, tmp);
}

char const * _stream_to_string(Stream * stream)
{
    ASSERT(stream);

    switch (stream->type)
    {
    case STREAM_TYPE_STRING_OUTPUT:
    case STREAM_TYPE_STRING_INPUT:
        return (char const *) stream->buf; // TODO
    default:
        ERROR("cannot get string from stream of type %u", stream->type);
        return NULL;
    }
}

char _stream_peek_char(Stream * in)
{
    /* don't do an assert for every char in release */
    DEBUG_ASSERT(in);

    /* TODO might as well use the buffer for file input */

    switch (in->type)
    {
    case STREAM_TYPE_FILE_INPUT:
    case STREAM_TYPE_FILE_INPUT_CLOSE:
    {
        int ch = getc(in->file);
        if (ch == EOF)
        {
            return 0;
        }
        ungetc(ch, in->file);
        return (char) ch;
    }
    case STREAM_TYPE_STRING_INPUT:
        return in->buf[in->pos];
    default:
        ERROR("cannot read from stream of type %u", in->type);
        return 0;
    }
}

void _stream_skip_char(Stream * in)
{
    /* don't do an assert for every char in release */
    DEBUG_ASSERT(in);

    /* TODO might as well use the buffer for file input */

    switch (in->type)
    {
    case STREAM_TYPE_FILE_INPUT:
    case STREAM_TYPE_FILE_INPUT_CLOSE:
        getc(in->file);
        break;
    case STREAM_TYPE_STRING_INPUT:
        if (in->pos < in->cap)
        {
            ++in->pos;
        }
        break;
    default:
        ERROR("cannot read from stream of type %u", in->type);
        break;
    }
}

char _stream_get_char(Stream * in)
{
    /* don't do an assert for every char in release */
    DEBUG_ASSERT(in);

    /* TODO might as well use the buffer for file input */

    switch (in->type)
    {
    case STREAM_TYPE_FILE_INPUT:
    case STREAM_TYPE_FILE_INPUT_CLOSE:
        return (char) getc(in->file);
    case STREAM_TYPE_STRING_INPUT:
    {
        char ret = _stream_peek_char(in);
        _stream_skip_char(in);
        return ret;
    }
    default:
        ERROR("cannot read from stream of type %u", in->type);
        return 0;
    }
}

Bool _stream_at_end(Stream * in)
{
    return _stream_peek_char(in) == 0;
}

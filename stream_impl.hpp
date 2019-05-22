
#pragma once

typedef struct
{
    FILE * file;

    U8 *   buf;
    size_t pos;
    size_t cap;

    U32    type;

    union
    {
        I32 index;
        I32 next;
    };
} Stream;

void _make_file_input_stream(Stream * stream, FILE * file, Bool close_on_free);
void _make_file_output_stream(Stream * stream, FILE * file, Bool close_on_free);

void _make_string_input_stream(Stream * stream, char const * str);
void _make_string_output_stream(Stream * stream);

void _free_stream(Stream * stream);

/* output-only */

void _stream_put_cstring(Stream * out, char const * str); // TODO rename to put_cstring

void _stream_put_char(Stream * out, U32 ch);
void _stream_put_ptr(Stream * out, void const * ptr);
void _stream_put_u64(Stream * out, U64 num);
void _stream_put_i64(Stream * out, I64 num);
void _stream_put_x64(Stream * out, U64 num);
void _stream_put_f32(Stream * out, F32 num);
char const * _stream_to_string(Stream * stream);

/* input-only */

char _stream_peek_char(Stream * in);
void _stream_skip_char(Stream * in);
char _stream_get_char(Stream * in);
Bool _stream_at_end(Stream * in);

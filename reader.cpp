
#include "lisp.hpp"

#if READER_BACKQUOTE && !ENABLE_BACKQUOTE
#define READER_BACKQUOTE 0
#endif

#if READER_FIXNUM && !ENABLE_FIXNUM
#define READER_FIXNUM 0
#endif

#if READER_VECTOR && !ENABLE_VECTOR
#define READER_VECTOR 0
#endif

inline static char peek(Expr in)
{
    return stream_peek_char(in);
}

inline static void skip(Expr in)
{
    stream_skip_char(in);
}

inline static char consume(Expr in)
{
    return stream_get_char(in);
}

inline static void write(Expr out, char ch)
{
    stream_put_char(out, ch);
}

static Bool is_ws(char ch)
{
    return
        ch == ' ' ||
        ch == '\n' ||
        ch == '\t';
}

static Bool is_symbol_start(char ch)
{
    return
        ch != 0 &&
        !is_ws(ch) &&
        ch != '"' &&
        ch != '(' && ch != ')' &&
        ch != ';' && ch != '\'';
}

static Bool is_symbol_part(char ch)
{
    return is_symbol_start(ch);
}

#if READER_FIXNUM

static Bool is_number_part(char ch)
{
    return ch >= '0' && ch <= '9';
}

static Bool is_number_start(char ch)
{
    return is_number_part(ch) || ch == '-' || ch == '+';
}

static Bool is_number_stop(char ch)
{
    return ch == 0 || ch == ')' || is_ws(ch);
}

#endif

static void skip_ws_or_comment(Expr in)
{
whitespace:
    while (is_ws(peek(in)))
    {
        skip(in);
    }

    if (peek(in) != ';')
    {
        return;
    }

    skip(in);

comment:
    if (peek(in) == 0)
    {
        return;
    }

    if (peek(in) == '\n')
    {
        skip(in);
        goto whitespace;
    }

    skip(in);
    goto comment;
}

static Expr parse_expr(Expr in);

static char parse_hex_digit(Expr in, char val)
{
#if TRACE_READ
    printf("PARSE-HEX-DIGIT %c\n", peek(in));
#endif
    char const ch = consume(in);

    if (ch >= '0' && ch <= '9')
    {
        val *= 16;
        val += ch - '0';
    }

    else if (ch >= 'a' && ch <= 'f')
    {
        val *= 16;
        val += 10 + ch - 'a';
    }

    else if (ch >= 'A' && ch <= 'F')
    {
        val *= 16;
        val += 10 + ch - 'A';
    }

    else
    {
        ERROR("malformed string");
    }

    return val;
}

static Expr parse_list(Expr in)
{
#if TRACE_READ
    printf("PARSE-LIST %c\n", peek(in));
#endif
    Expr exp  = nil;
    Expr head = nil;
    Expr tail = nil;

    if (peek(in) != '(')
    {
        return ERROR("expected '(', got '%c'", peek(in));
    }

    skip(in);

list_loop:
    skip_ws_or_comment(in);

    if (peek(in) == 0)
    {
        return ERROR("unexpected eof");
    }

    if (peek(in) == ')')
    {
        goto list_done;
    }

    exp = parse_expr(in);

    if (exp == SYM_DOT)
    {
        exp = parse_expr(in);
        set_cdr(tail, exp);

        skip_ws_or_comment(in);

        goto list_done;
    }
    else
    {
        list_append(&head, &tail, exp);
    }

    goto list_loop;

list_done:
    if (peek(in) != ')')
    {
        return ERROR("missing ')'");
    }
    skip(in);

    return head;
}

static Expr parse_string(Expr in, Expr tok)
{
#if TRACE_READ
    printf("PARSE-STRING %c\n", peek(in));
#endif
    enum
    {
        STATE_DEFAULT,
        STATE_ESCAPE,
    } state = STATE_DEFAULT;

    if (peek(in) != '"')
    {
        return ERROR("missing '\"'");
    }
    skip(in);

string_loop:
    if (peek(in) == 0)
    {
        return ERROR("unexpected eof in string");
    }

    else if (state == STATE_DEFAULT)
    {
        if (peek(in) == '"')
        {
            skip(in);
            goto string_done;
        }
        else if (peek(in) == '\\')
        {
            skip(in);
            state = STATE_ESCAPE;
        }
        else
        {
            write(tok, consume(in));
        }
    }

    else if (state == STATE_ESCAPE)
    {
        if (peek(in) == 'n')
        {
            skip(in);
            write(tok, '\n');
        }

        else if (peek(in) == 't')
        {
            skip(in);
            write(tok, '\t');
        }

        else if (peek(in) == 'x')
        {
            skip(in);

            char val = 0;
            val = parse_hex_digit(in, val);
            val = parse_hex_digit(in, val);

            /* TODO check for more digits? */

            write(tok, val);
        }

        else
        {
            write(tok, consume(in));
        }

        state = STATE_DEFAULT;
    }

    else
    {
        return ERROR("internal error");
    }

    goto string_loop;

string_done:
    write(tok, 0);

#if TRACE_READ
    printf("STRING => %s\n", stream_to_string(tok));
#endif
    return f_stream_to_string(tok);
}

static inline Expr do_parse_expr(Expr in, Expr tok)
{
    skip_ws_or_comment(in);

    if (peek(in) == '(')
    {
        return parse_list(in);
    }

#if READER_QUOTE
    else if (/*option_read_quote && */peek(in) == '\'')
    {
        skip(in);
        return LIST2(SYM_quote, parse_expr(in));
    }
#endif

#if READER_BACKQUOTE
    else if (peek(in) == '`')
    {
        skip(in);
        return LIST2(SYM_backquote, parse_expr(in));
    }

    else if (peek(in) == ','
#if READER_TILDE_IS_UNQUOTE
             || peek(in) == '~'
#endif
        )
    {
        skip(in);
        if (peek(in) == '@')
        {
            skip(in);
            return LIST2(SYM_unquote_splicing, parse_expr(in));
        }
        return LIST2(SYM_unquote, parse_expr(in));
    }
#endif

#if READER_VECTOR
    else if (peek(in) == '#')
    {
        skip(in);
        if (peek(in) != '(')
        {
            // TODO hash should be a proper reader macro
            //return ERROR("expected list of arguments for vector constructor");
            write(tok, '#');
            goto symbol_loop; // TODO have some sort of speculative streams?
        }
        Expr args = parse_list(in);
        //println(args);

        U64 len = 0; // TODO len should fall out of parse_list
        for (Expr iter = args; iter; iter = cdr(iter))
        {
            ++len;
        }
        //printf("len: %" PRIu64"\n", len);

        Expr vec = make_vector(len);
        //println(vec);

        U64 idx = 0;
        for (Expr iter = args; iter; iter = cdr(iter))
        {
            vector_set(vec, idx++, car(iter));
        }

        //println(vec);
        return vec;
    }

#endif

    else if (/*option_read_string && */peek(in) == '"')
    {
        return parse_string(in, tok);
    }

#if READER_FIXNUM
    else if (is_number_start(peek(in)))
    {
#if TRACE_READ
        printf("PARSE-NUMBER %c\n", peek(in));
#endif
        Bool neg = 0;
        if (peek(in) == '-')
        {
            neg = 1;
            write(tok, consume(in));
        }
        else if (peek(in) == '+')
        {
            write(tok, consume(in));
        }

        if (is_number_part(peek(in)))
        {
#if READER_USE_NUMBER
            Expr val = make_number(0);
            Expr ten = make_number(10);

            while (1)
            {
	            char const ch = peek(in);
	            if (!is_number_part(ch))
	            {
		            break;
	            }

	            I64 const digit = ch - '0';

	            val = number_mul(val, ten);
	            val = number_add(val, make_number(digit));

	            write(tok, consume(in));
            }
            
            if (neg)
            {
	            val = number_neg(val);
            }

            if (!is_number_stop(peek(in)))
            {
                goto symbol_loop;
            }

            return val;
#else
            I64 val = 0;

            /* TODO check for overflows? */
            if (neg)
            {
                while (1)
                {
                    char const ch = peek(in);
                    if (ch < '0' || ch > '9')
                    {
                        break;
                    }

                    I64 const digit = ch - '0';

                    val = val * 10;
                    val = val - digit;

                    write(tok, consume(in));
                }
            }
            else
            {
                while (1)
                {
                    char const ch = peek(in);
                    if (ch < '0' || ch > '9')
                    {
                        break;
                    }

                    I64 const digit = ch - '0';

                    val = val * 10;
                    val = val + digit;

                    write(tok, consume(in));
                }
            }

            if (!is_number_stop(peek(in)))
            {
                goto symbol_loop;
            }

            return make_fixnum(val);
#endif
        }

        else
        {
            goto symbol_loop;
        }
    }
#endif

    else if (is_symbol_start(peek(in)))
    {
#if TRACE_READ
        printf("PARSE-SYMBOL %c\n", peek(in));
#endif
        write(tok, consume(in));

    symbol_loop:
        if (is_symbol_part(peek(in)))
        {
            write(tok, consume(in));
            goto symbol_loop;
        }
        else
        {
            goto symbol_done;
        }

    symbol_done:
        write(tok, 0);

#if TRACE_READ
        printf("SYMBOL => %s\n", stream_to_string(tok));
#endif
        char const * str = stream_to_string(tok);
#if READER_SPLIT_ATTR
        Expr ret = nil;
        for (char const * pch = str, * beg = str; ; ++pch)
        {
            char const ch = *pch;
            if ((pch > str && ch == '.') || (ret && !ch))
            {
                if (!ret)
                {
                    ret = cons(QUOTE(attr), ret);
                }

                ret = cons(intern_range(beg, pch), ret);
                beg = pch + 1;

            }
            if (!ch)
            {
                break;
            }
        }
        if (ret)
        {
            ret = nreverse(ret);
            return ret;
        }
        else
#endif
        {
            return intern(str);
        }
    }

    else
    {
        ERROR("unexpected %c", peek(in));
    }

    return nil;
}

static Expr parse_expr(Expr in)
{
#if TRACE_READ
    printf("PARSE-EXPR %c\n", peek(in));
#endif
    Expr tok = make_string_output_stream();
    Expr ret = do_parse_expr(in, tok);
    stream_close(tok);
    return ret;
}

Expr read_with_env(Expr env)
{
    Expr stream;
    if (env_maybe_lookup(env, QUOTE(*read-stream*), &stream))
    {
        return read_from_stream(stream);
    }
    else
    {
        // TODO make a proper prompt input stream
        Expr in = make_file_input_stream(stdin, 0);
        Expr ret = read_from_stream(in);
        stream_close(in);
        return ret;
    }
}

Expr read_from_string(char const * str)
{
    Expr in = make_string_input_stream(str);
    Expr ret = read_from_stream(in);
    stream_close(in);
    return ret;
}

Expr read_from_stream(Expr in)
{
#if TRACE_READ
    printf("READ-FROM-STREAM %c\n", peek(in));
#endif

    Expr const ret = parse_expr(in);
#if TRACE_READ
    printf("=> "); println(ret);
#endif
    return ret;
}

Bool maybe_read_from_stream(Expr in, Expr * exp)
{
#if TRACE_READ
    printf("MAYBE-READ-FROM-STREAM %c\n", peek(in));
#endif

    skip_ws_or_comment(in);
    if (stream_at_end(in))
    {
        return 0;
    }

    *exp = read_from_stream(in);
    return 1;
}

Expr read_all_from_stream(Expr in)
{
    Expr ret = nil;
    Expr exp = nil;
    while (maybe_read_from_stream(in, &exp))
    {
        ret = cons(exp, ret);
    }

    ret = nreverse(ret);
    return ret;
}


#include "lisp.hpp"

#if ENABLE_SDL2

static Expr b_SDL_Init(Expr args, Expr env, void * user)
{
    return int_to_num(SDL_Init(num_to_u32(car(args))));
}

static Expr b_SDL_Quit(Expr args, Expr env, void * user)
{
    SDL_Quit();
    return nil;
}

static Expr b_SDL_CreateWindow(Expr args, Expr env, void * user)
{
    show_backtrace();
    //char const * title;
    Expr title;
    int x, y, w, h;
    U32 flags;
    println(args);
    title = make_string("bla");
    x = 0; y = 0; w = 640; h = 400; flags = 0;
//    if (!unpack_all_args(args, "x", &title))
//    if (!unpack_all_args(args, "siiiiu32", &title, &x, &y, &w, &h, &flags))
    {
//        return make_error("illegal arguments -- SDL_CreateWindow");
    }
    println(args);
    return make_pointer(SDL_CreateWindow(string_value(title), x, y, w, h, flags));
}

static Expr b_SDL_GetTicks(Expr args, Expr env, void * user)
{
    // TODO
    //if (!unpack_all_args(""))
    //{
    //    return make_error("illegal arguments -- SDL_GETTICKS");
    //}
    return u32_to_num(SDL_GetTicks());
}

static Expr b_SDL_CreateRenderer(Expr args, Expr env, void * user)
{
    SDL_Window * window = (SDL_Window *) pointer_value(car(args));
    int          index  = num_to_int(cadr(args));
    Uint32       flags  = num_to_u32(caddr(args));
    return make_pointer(SDL_CreateRenderer(window, index, flags));
}

static Expr b_SDL_DestroyRenderer(Expr args, Expr env, void * user)
{
    SDL_Renderer * renderer = (SDL_Renderer *) pointer_value(car(args));
    SDL_DestroyRenderer(renderer);
    return nil;
}

#endif

#define FUN2(sym, name) env_def(env, QUOTE(sym), make_builtin_fun(name, NULL));
#define FUN1(sym)       FUN2(sym, b_##sym)
#define VARI(sym)       env_def(env, intern(#sym), int_to_num(sym));

void system_bind_sdl2(Expr env)
{
#if ENABLE_SDL2
    FUN1(SDL_CreateRenderer);
    FUN1(SDL_CreateWindow);
    FUN1(SDL_DestroyRenderer);
    FUN1(SDL_GetTicks);
    FUN1(SDL_Init);
    FUN1(SDL_Quit);

    VARI(SDL_INIT_VIDEO);
    VARI(SDL_RENDERER_ACCELERATED);
    VARI(SDL_RENDERER_PRESENTVSYNC);
#endif
}

#undef FUN2
#undef FUN1
#undef VARI

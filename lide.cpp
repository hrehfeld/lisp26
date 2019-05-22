
#define LISP_IMPLEMENTATION
#include "runtime.hpp"

#define FONT_IMPLEMENTATION
#include "font.hpp"

#define FILE_NAME "eval.lisp"

#include <algorithm>
#include <sstream>
#include <string>
#include <vector>

#include <SDL2/SDL.h>

void draw_string_bgrx32(font_t font, U32 flags, I32 x, I32 y, I32 w, I32 h, U32 * pixels, char const * string, U32 bgrx)
{
    U32 gw, gh;
    get_metrics(font, &gw, &gh);

    I32 const x0 = x;
    for (char const * p = string; *p; ++p)
    {
        /* TODO do an early-out if we're below the bottom */
        U32 const cp = (U32) *p;
        switch (cp)
        {
        case '\n':
            x = x0;
            y += gh;
            break;
        default:
            draw_glyph_bgrx32(font, flags, x, y, w, h, pixels, cp, bgrx);
            x += gw;
            break;
        }
    }
}

void fill_rect_bgrx32(I32 x0, I32 x1, I32 y0, I32 y1, I32 w, I32 h, U32 * pixels, U8 r, U8 g, U8 b)
{
    for (I32 y = y0; y < y1; ++y)
    {
        for (I32 x = x0; x < x1; ++x)
        {
            set_pixel_bgrx32_unsafe(x, y, w, h, pixels, r, g, b);
        }
    }
}

void fill_rect_bgrx32(I32 x0, I32 x1, I32 y0, I32 y1, I32 w, I32 h, U32 * pixels, U32 color)
{
    fill_rect_bgrx32(x0, x1, y0, y1, w, h, pixels,
                     (U8) ((color >> 16) & 0xff),
                     (U8) ((color >>  8) & 0xff),
                     (U8) ( color        & 0xff));
}

class ListWidget
{
public:
    ListWidget(int x0, int x1, int y0, int y1) :
        x0(x0), x1(x1), y0(y0), y1(y1), sel(-1)
    {
    }

    std::string selected_item() const
    {
        if (sel >= 0 && sel < (int) items.size())
        {
            return items[sel];
        }
        return "";
    }

    void draw(int mx, int my, U32 w, U32 h, U32 * pixels)
    {
        if (mx >= x0 && mx < x1 && my >= y0 && my < y1)
        {
            fill_rect_bgrx32(x0, x1, y0, y1, w, h, pixels, 0x101010);
        }
        int x = x0 + 4;
        int y = y0 + 4;

        for (int index = 0; index < (int) items.size(); ++index)
        {
            if (index + 2 < sel)
            {
                continue;
            }

            auto item = items[index];

            // TODO clip x
            U32 color = index == sel ? 0x0000ffff : 0xaabbffff;
            draw_string_bgrx32(font_8x12, 0, x, y, w, h, pixels, item.c_str(), color);

            y += 16;

            if (y >= this->y1)
            {
                break;
            }
        }
    }

    int x0, x1, y0, y1;

    std::vector<std::string> items;
    int sel;
};

class EditWidget
{
public:
    EditWidget(int x0, int x1, int y0, int y1) : x0(x0), x1(x1), y0(y0), y1(y1)
    {
    }

    void draw(int mx, int my, U32 w, U32 h, U32 * pixels)
    {
        if (mx >= x0 && mx < x1 && my >= y0 && my < y1)
        {
            fill_rect_bgrx32(x0, x1, y0, y1, w, h, pixels, 0x101010);
        }
        draw_string_bgrx32(font_8x12, 0, x0 + 4, y0 + 4, w, h,
                           pixels, text.c_str(), 0xffffff);
    }

    int x0, x1, y0, y1;

    std::string text;
};

int main(int argc, char ** argv)
{
    system_init();
    Expr env = make_env(nil);
    system_bind_core(env);

    char const * ifn = FILE_NAME;

    for (int i = 1; i < argc; ++i)
    {
        ifn = argv[i];
    }
    try
    {
        load_file(ifn, env);
    }
    catch (Expr err)
    {
        fprintf(stderr, "%s\n", repr(err));
    }

    U32 const w = 640;
    U32 const h = 400;

    ListWidget list_widget(  0, 200, 0, h);
    EditWidget edit_widget(200,   w, 0, h);

    // TODO do this on the fly
    for (Expr iter = env_vars(env); iter; iter = cdr(iter))
    {
        list_widget.items.push_back(repr(car(iter)));
    }

    std::sort(list_widget.items.begin(), list_widget.items.end());

    if (SDL_Init(SDL_INIT_VIDEO) < 0)
    {
        exit(1);
    }

    SDL_Window * window = SDL_CreateWindow("LIDE",
                                           SDL_WINDOWPOS_UNDEFINED,
                                           SDL_WINDOWPOS_UNDEFINED,
                                           w, h,
                                           0/*SDL_WINDOW_ALLOW_HIGHDPI*/);
    if (!window)
    {
        exit(1);
    }

    SDL_Renderer * renderer = SDL_CreateRenderer(window, -1,
                                                 SDL_RENDERER_ACCELERATED |
                                                 SDL_RENDERER_PRESENTVSYNC);
    if (!renderer)
    {
        exit(1);
    }

    SDL_Texture * texture = SDL_CreateTexture(renderer,
                                              SDL_PIXELFORMAT_ARGB8888,
                                              SDL_TEXTUREACCESS_STREAMING, w, h);
    if (!texture)
    {
        exit(1);
    }

    U32 pixels[w * h];
    SDL_Event event;

    int display_index = 0;

    int mx = -1;
    int my = -1;

loop:
    while (SDL_PollEvent(&event))
    {
        bool const no_mods = event.key.keysym.mod == 0;

        //bool const shift_mod =
        //    event.key.keysym.mod == KMOD_LSHIFT ||
        //    event.key.keysym.mod == KMOD_RSHIFT ||
        //    event.key.keysym.mod == (KMOD_LSHIFT | KMOD_RSHIFT);

        bool const ctrl_mod =
            event.key.keysym.mod == KMOD_LCTRL ||
            event.key.keysym.mod == KMOD_RCTRL ||
            event.key.keysym.mod == (KMOD_LCTRL | KMOD_RCTRL);

        //bool const alt_mod =
        //    event.key.keysym.mod == KMOD_LALT ||
        //    event.key.keysym.mod == KMOD_RALT ||
        //    event.key.keysym.mod == (KMOD_LALT | KMOD_RALT);

        switch (event.type)
        {
        case SDL_QUIT:
            goto done;
        case SDL_MOUSEMOTION:
            mx = event.motion.x;
            my = event.motion.y;
            break;
        case SDL_KEYDOWN:
            switch (event.key.keysym.sym)
            {
            case SDLK_ESCAPE:
                if (no_mods)
                {
                    event.type = SDL_QUIT;
                    SDL_PushEvent(&event);
                }
                break;

            case SDLK_DOWN:
                ++display_index;
                break;

            case SDLK_UP:
                --display_index;
                break;

            case SDLK_n:
                if (ctrl_mod)
                {
                    ++display_index;
                }
                break;
            case SDLK_p:
                if (ctrl_mod)
                {
                    --display_index;
                }
                break;
            }
            break;
        }
    }

    memset(pixels, 0, sizeof(pixels));

    {
        if (list_widget.sel != display_index)
        {
            list_widget.sel = display_index;

            std::string sel = list_widget.selected_item();
            if (sel.empty())
            {
                edit_widget.text = "";
            }
            else
            {
                Expr exp = env_lookup(env, intern(sel.c_str()));
                if (is_function(exp) || is_macro(exp))
                {
                    std::stringstream ss;
                    ss << "(";
#if 0
                    if (is_macro(exp))
                    {
                        ss << "syntax";
                    }
                    else
                    {
                        ss << "lambda";
                    }
#else
                    if (is_macro(exp))
                    {
                        ss << "defmacro";
                    }
                    else
                    {
                        ss << "defun";
                    }
                    ss << " ";
                    ss << sel;
#endif
                    ss << " ";
                    ss << repr(closure_params(exp));
                    for (Expr iter = closure_body(exp); iter; iter = cdr(iter))
                    {
                        Expr stmt = car(iter);
                        ss << "\n";
                        ss << "  ";
                        ss << repr(stmt);
                    }
                    ss << ")";
                    edit_widget.text = ss.str();
                }
                else
                {
                    edit_widget.text = repr(exp);
                }
            }
        }

        list_widget.draw(mx, my, w, h, pixels);
        edit_widget.draw(mx, my, w, h, pixels);
    }

    system_gc(&env);

    SDL_Delay(2);

    SDL_UpdateTexture(texture, NULL, pixels, w * sizeof(U32));
    SDL_RenderCopy(renderer, texture, NULL, NULL);
    SDL_RenderPresent(renderer);

    goto loop;

done:
    if (texture)
    {
        SDL_DestroyTexture(texture);
    }

    if (renderer)
    {
        SDL_DestroyRenderer(renderer);
    }

    if (window)
    {
        SDL_DestroyWindow(window);
    }

    SDL_Quit();

    system_quit();
    return 0;
}

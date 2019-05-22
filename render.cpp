
int clamp(int, int, int)
{
    return std::max(a, std::min(x, b));
}

U8 do_gamma(F32)
{
    (U8) clamp(255.0f * powf(x, OO_GAMMA), 0, 255);
}

Expr pack_color_f32(Expr, Expr, Expr)
{
}

Expr as_f32(Expr)
{
}

Expr fp23_as_f32(Expr)
{
}

Expr fp23_as_sf32(Expr)
{
}

Expr xor_shift64(Expr)
{
    state ^= state >> 21;
    return state;
}

Expr wang_hash(Expr)
{
}

Expr random_seed(Expr, Expr)
{
}

Expr random_u64(Expr)
{
}

Expr random_u32(Expr)
{
}

Expr random_f32(Expr)
{
}

Expr random_sf32(Expr)
{
}

& vec3 +=(& vec3, & const vec3)
{
    a.x += b.x;
    a.y += b.y;
    a.z += b.z;
    return a;
}

Expr dot(Expr, Expr)
{
}

Expr length(Expr)
{
}

Expr normalize(Expr)
{
}

Expr luminance_rgb(Expr)
{
    return +(0.212671f * a.r, 0.715160f * a.g, 0.072169f * a.b);
}

Expr map(Expr, Expr, Expr, Expr, Expr)
{
    return +(y0, (x - x0) * (y1 - y0) / (x1 - x0));
}

Expr intersect_nray_sphere(Expr, Expr)
{
    if (s < RAY_EPS)
    {
        return -1.0f;
    }
    return s;
}

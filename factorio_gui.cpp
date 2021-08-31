
#define PROGRAM_NAME "cppsat"
#define PROGRAM_TITLE "Belt Balancers"

#ifndef PLATFORM_INCLUDES
#error "You did not compile the right file."
#endif

#include <unistd.h>
#include <signal.h>
#include <sys/prctl.h>

#include "hashmap.cpp"
#include "asset.cpp"
#include "opengl.cpp"
#include "spline.cpp"
#include "font.cpp"
#include "shapes.cpp"
#include "gui.cpp"

#include "array_linux.cpp"
#include "sat.cpp"
#include "sat_linux.cpp"
#include "factorio_sat.cpp"


struct Backend {
    Shader circle, spline;
    s64 screen_w = 0, screen_h = 0;
    
    Font_data fonts;
    s64 font_sans_base, font_sans;
    s64 font_solution;

    Gui gui;
    Shape_drawer shapes;

    bool redraw_internal = false;

    Array_dyn<u8> temp_u8;
    Array_dyn<u64> temp_u64;
};

namespace Factorio_gui_state {
    enum States: u8 {
        INVALID, CHOOSE_PARAMS, RUN_SOLVER, DRAW_PROPAGATION, DRAW_SOLUTION, CHECK_SOLUTIONS
    };
}

struct Factorio_solution_state {
    Sat_instance* inst;
    Sat_solution* sol;
    bool sol_detail;
    s64 sol_detail_x, sol_detail_y;
};

void factorio_solution_init(Factorio_solution_state* fsol, Sat_instance* inst, Sat_solution* sol) {
    memset(fsol, 0, sizeof(*fsol));
    fsol->inst = inst;
    fsol->sol = sol;
}

void base64_encode(Array_dyn<u8>* into, Array_t<u8> data) {
    u8* table = (u8*)"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    u64 buf = 0;
    s64 buf_size = 0;
    for (u8 c: data) {
        buf = buf << 8 | c;
        buf_size += 8;
        while (buf_size >= 6) {
            array_push_back(into, table[buf >> (buf_size - 6) & 0x3f]);
            buf_size -= 6;
            buf &= (1ull << buf_size) - 1;
        }
    }
    if (buf_size) {
        array_push_back(into, table[(buf << (6 - buf_size)) & 0x3f]);
        array_push_back(into, table[buf & 0x3f]);
        if (buf_size == 2) array_push_back(into, table[buf & 0x3f]);
    }
}

void factorio_solution_draw_image(Backend* backend, Factorio_solution_state* draw, Vec2 p, float* x_out, float* y_out) {
    GUI_TIMER(&backend->gui);

    float border_size = 16;
    float border_pad = 4;
    Vec2 bar_size_min = {5, 3};
    float arrow_fac = 0.8f;
    font_instance_scale(&backend->fonts, backend->font_solution, border_size);

    s64 nx = hashmap_get(&draw->inst->params, Factorio::fpar_nx);
    s64 ny = hashmap_get(&draw->inst->params, Factorio::fpar_ny);
    s64 n_lines = hashmap_get(&draw->inst->params, Factorio::fpar_n_lines);
    s64 n_linedim = hashmap_get(&draw->inst->params, Factorio::fpar_n_linedim);
    s64 n_linelen = hashmap_get(&draw->inst->params, Factorio::fpar_n_linelen);

    if (gui_shortcut(&backend->gui, Key::create_special(Key::C_SAVE, Key::MOD_CTRL))) {
        auto* sol = draw->sol;

        Array_dyn<u8> str;
        defer { array_free(&str); };

        array_printf(&str, "solution ? ? {\n");
        for (s64 yi = 0; yi < ny; ++yi) {
            array_printf(&str, "    ");
            for (s64 xi = 0; xi < nx; ++xi) {
                Field f {xi, yi};

                u8 c_inp = '.';
                if (sol->istrue(f.dirs[2].inp)) c_inp = 'v';
                if (sol->istrue(f.dirs[3].inp)) c_inp = '>';
                if (sol->istrue(f.dirs[0].inp)) c_inp = '^';
                if (sol->istrue(f.dirs[1].inp)) c_inp = '<';

                u8 c_out = '.';
                if (sol->istrue(f.dirs[0].out)) c_out = 'v';
                if (sol->istrue(f.dirs[1].out)) c_out = '>';
                if (sol->istrue(f.dirs[2].out)) c_out = '^';
                if (sol->istrue(f.dirs[3].out)) c_out = '<';

                if (draw->sol->istrue(f.empty)) {
                    array_printf(&str, "..");
                } else if (draw->sol->istrue(f.belt)) {
                    array_printf(&str, ".%c", c_out);
                } else if (draw->sol->istrue(f.split)) {
                    array_printf(&str, "S%c", c_out);
                } else if (draw->sol->istrue(f.under)) {
                    if (c_out != '.') array_printf(&str, "u%c", c_out);
                    else              array_printf(&str, "%cu", c_inp);
                }
            }
            array_push_back(&str, '\n');
        }
        array_printf(&str, "}\n");
        platform_clipboard_set(Platform_clipboard::CONTROL_C, str);
    }
    if (gui_shortcut(&backend->gui, Key::create_special(Key::C_SAVEAS, Key::MOD_CTRL | Key::MOD_SHIFT))) {
        auto* sol = draw->sol;

        Array_dyn<u8> str;
        defer { array_free(&str); };

        array_printf(&str, "{\"blueprint\":{\"entities\":[");
        s64 count = 1;
        for (s64 yi = 0; yi < ny; ++yi) {
            for (s64 xi = 0; xi < nx; ++xi) {
                Field f {xi, yi};

                int d_inp = -1;
                if (sol->istrue(f.dirs[0].inp)) d_inp = Dir::BEG + 0;
                if (sol->istrue(f.dirs[1].inp)) d_inp = Dir::BEG + 1;
                if (sol->istrue(f.dirs[2].inp)) d_inp = Dir::BEG + 2;
                if (sol->istrue(f.dirs[3].inp)) d_inp = Dir::BEG + 3;

                int d_out = -1;
                if (sol->istrue(f.dirs[0].out)) d_out = Dir::BEG + 0;
                if (sol->istrue(f.dirs[1].out)) d_out = Dir::BEG + 1;
                if (sol->istrue(f.dirs[2].out)) d_out = Dir::BEG + 2;
                if (sol->istrue(f.dirs[3].out)) d_out = Dir::BEG + 3;

                if (draw->sol->istrue(f.belt)) {
                    array_printf(
                        &str, "{\"entity_number\":%lld,\"name\":\"transport-belt\","
                        "\"position\":{\"x\":%lld.5,\"y\":-%lld.5},\"direction\":%d},",
                        count, xi, yi, (d_out - Dir::BEG) * 2
                    );
                } else if (draw->sol->istrue(f.splitl)) {
                    float xa = 0.5f * (1 + (d_out == Dir::TOP)  - (d_out == Dir::BOTTOM));
                    float ya = 0.5f * (1 + (d_out == Dir::LEFT) - (d_out == Dir::RIGHT) );
                    
                    array_printf(
                        &str, "{\"entity_number\":%lld,\"name\":\"splitter\","
                        "\"position\":{\"x\":%.1f,\"y\":-%.1f},\"direction\":%d},",
                        count, (float)xi + xa, (float)yi + ya, (d_out - Dir::BEG) * 2
                    );
                } else if (draw->sol->istrue(f.under)) {
                    char const* type; int d;
                    if (d_inp != -1) {
                        type = "input";
                        d = d_inp ^ 2;
                    } else {
                        type = "output";
                        d = d_out;
                    }

                    array_printf(
                        &str, "{\"entity_number\":%lld,\"name\":\"express-underground-belt\","
                        "\"position\":{\"x\":%lld.5,\"y\":-%lld.5},\"direction\":%d,\"type\":\"%s\"},",
                        count, xi, yi, (d - Dir::BEG) * 2, type
                    );
                } else {
                    continue;
                }
                ++count;
            }
        }
        if (count > 1) --str.size;
        array_printf(&str, "]}}");

        Array_t<u8> str_defl = array_create<u8>(compressBound(str.size));
        defer { array_free(&str_defl); };

        int code = compress2(str_defl.data, (long unsigned int*)&str_defl.size, str.data, str.size, 9);
        if (code == Z_OK) {
            Array_dyn<u8> str_b64;
            defer { array_free(&str_b64); };

            array_push_back(&str_b64, '0');
            base64_encode(&str_b64, str_defl);
            platform_clipboard_set(Platform_clipboard::CONTROL_C, str_b64);
        }
    }
    
    float center_size = max(max(bar_size_min.x * n_lines, bar_size_min.y * n_linelen), 24.f);
    float size = (border_size + border_pad) * 2 + center_size;
    Vec2 bar_size = center_size / Vec2 {(float)n_lines, (float)n_linelen};

    if (x_out) *x_out = p.x + (nx + 2) * size;
    if (y_out) *y_out = p.y + (ny + 2) * size;
    
    for (s64 yi = -1; yi <= ny; ++yi) {
        for (s64 xi = -1; xi <= nx; ++xi) {
            u32 click_flags = gui_pointable(&backend->gui, "draw_solution"_arr, {xi, yi});
            if (click_flags & Gui::EVENT_CLICKED) {
                if (draw->sol_detail and draw->sol_detail_x == xi and draw->sol_detail_y == yi) {
                    draw->sol_detail = false;
                } else {
                    draw->sol_detail = true;
                    draw->sol_detail_x = xi;
                    draw->sol_detail_y = yi;
                }
            }
        }
    }
    
    for (s64 yi = -1; yi <= ny; ++yi) {
        for (s64 xi = -1; xi <= nx; ++xi) {
            Vec2 pi = p + size * Vec2 {xi + 1.f, yi + 1.f};
            Field f {xi, yi};

            u32 click_flags = gui_pointable(&backend->gui, "draw_solution"_arr, {xi, yi}, pi, {size, size}, 0.2f);
            bool is_detail = draw->sol_detail and draw->sol_detail_x == xi and draw->sol_detail_y == yi;
            bool is_active = (click_flags & Gui::DRAW_ACTIVE);
            
            bool flags[] = {draw->sol->istrue(f.empty), draw->sol->istrue(f.belt),
                            draw->sol->istrue(f.split), draw->sol->istrue(f.under)};
            Color colors[] = {Palette::BG, Palette::BGBLUE, Palette::BGGREEN, Palette::BGPURPLE};
            s64 flags_size = sizeof(flags) / sizeof(flags[0]);

            bool isset = false;
            Color bg_fill = lerp(Palette::WHITE, Palette::BLACK, 0.03f);;
            for (s64 i = 0; i < flags_size; ++i) {
                if (not flags[i]) continue;
                if (not isset) {
                    bg_fill = colors[i];
                    isset = true;
                } else {
                    bg_fill = Palette::REDLIGHT;
                }
            }
            if (is_active) {
                bg_fill = lerp(bg_fill, Palette::WHITE, 0.5f);
            }
            shape_rectangle(&backend->shapes, pi, {size, size}, bg_fill, 0.2f);

            if (is_detail) {
                Color c = lerp(Palette::WHITE, Palette::BLACK, .7f);
                shape_rectangle(&backend->shapes, pi-1, {size+2, 1}, c);
                shape_rectangle(&backend->shapes, pi-1, {1, size+2}, c);
                shape_rectangle(&backend->shapes, pi + Vec2{-1, size}, {size+2, 1}, c);
                shape_rectangle(&backend->shapes, pi + Vec2{size, -1}, {1, size+2}, c);
            }
            
            s64 dir_inp = -1;
            s64 dir_out = -1;

            for (Dir d: f.dirs) {
                u8 dd = d.dir - Dir::BEG;
                float arr[4] = {0, 1, 0, -1};
                Vec2 v1 = {arr[ dd       ], arr[(dd+1) & 3]};
                Vec2 v2 = {arr[(dd+1) & 3], arr[(dd+2) & 3]};

                Vec2 dp = pi + size/2 + v1 * (size - border_size - border_pad) / 2;

                bool inp = draw->sol->istrue(d.inp);
                bool out = draw->sol->istrue(d.out);
                bool sid = draw->sol->istrue(d.sid);
                bool iserr = inp + out + sid >= 2;
                Color fill = iserr ? Palette::RED : Palette::BLACK;

                if (inp) shape_arrowhead(&backend->shapes, dp, -v1 * border_size/2 * arrow_fac, fill);
                if (out) shape_arrowhead(&backend->shapes, dp,  v1 * border_size/2 * arrow_fac, fill);
                if (sid) {
                    Color fill2 = iserr ? Palette::RED : lerp(Palette::BLUE, Palette::BGBLUE, 0.3f);
                    float thickness = border_size * 0.15f;
                    Vec2 w = v2 * (0.5f * (size + center_size) + thickness) + v1 * thickness;
                    shape_rectangle(&backend->shapes, dp - w / 2.f, w, fill2);
                }

                if (out) dir_out = dd;
                if (inp) dir_inp = dd;
            }

            bool splitl = draw->sol->istrue(f.splitl);
            bool splitr = draw->sol->istrue(f.splitr);
            Color fill = splitl + splitr >= 2 ? Palette::RED : Palette::GREEN;

            bool draw_split_indicator[4] = {};
            if (splitl) {
                if (dir_out != -1) {
                    draw_split_indicator[(dir_out+1) & 3] = true;
                } else if (dir_inp != -1) {
                    draw_split_indicator[(dir_inp+3) & 3] = true;
                } else {
                    float w;
                    font_metrics_codepoint_get(&backend->fonts, backend->font_solution, 'L', &w);
                    Vec2 tp = pi + Vec2 {(border_size + border_pad - w) / 2, size - border_pad};
                    font_draw_codepoint(&backend->fonts, backend->font_solution, 'L', tp, fill);
                }
            }
            if (splitr) {
                if (dir_out != -1) {
                    draw_split_indicator[(dir_out+3) & 3] = true;
                } else if (dir_inp != -1) {
                    draw_split_indicator[(dir_inp+1) & 3] = true;
                } else {
                    float w;
                    font_metrics_codepoint_get(&backend->fonts, backend->font_solution, 'R', &w);
                    Vec2 tp = pi + size - Vec2 {(border_size + border_pad + w) / 2, border_pad};
                    font_draw_codepoint(&backend->fonts, backend->font_solution, 'R', tp, fill);
                }
            }

            for (u8 dd = 0; dd < 4; ++dd) {
                if (not draw_split_indicator[dd]) continue;
                float arr[4] = {0, 1, 0, -1};
                Vec2 v1 = {arr[ dd       ], arr[(dd+1) & 3]};
                Vec2 v2 = {arr[(dd+1) & 3], arr[(dd+2) & 3]};
                Vec2 dpp = pi + size/2 + v1 * size/2;

                float size = border_size * 0.6f;
                shape_rectangle(&backend->shapes, dpp - (v1 + v2)*size, v1*size + 2*v2*size, fill);
            }

            Color line_color_off[] = {Palette::BLUELIGHT, Color {109, 201, 96}, Palette::REDLIGHT};
            Color line_color_on[] = {Palette::BLUE, Palette::GREEN, Palette::RED};
            
            for (s64 i = 0; i < n_lines; ++i) {
                u64 line = i <  n_linedim ? f.line_first | (i << 8) :
                           i == n_linedim ? f.line_sum :
                                            f.line_block;
                Color c_off = line_color_off[(i >= n_linedim) + (i > n_linedim)];
                Color c_on  = line_color_on [(i >= n_linedim) + (i > n_linedim)];
                
                backend->temp_u64.size = 0;
                auto arr = sat_expand(draw->inst, line, &backend->temp_u64);
                for (s64 j = 0; j < arr.size; ++j) {
                    Color col;
                    switch (draw->sol->get(arr[j])) {
                    case Sat_solution::L_UNASSIGNED: col = Palette::GREY; break;
                    case Sat_solution::L_FALSE:      col = c_off; break;
                    case Sat_solution::L_TRUE:       col = c_on; break;
                    default: assert(false);
                    }

                    Vec2 v = pi + border_size + border_pad + Vec2 {(float)i, (float)(arr.size-1 - j)} * bar_size;
                    shape_rectangle(&backend->shapes, v, bar_size - 0.5f, col);
                }
            }
        }
    }
}

void factorio_solution_draw_detail(Backend* backend, Factorio_solution_state* draw, Vec2 p, float* out_x) {
    GUI_TIMER(&backend->gui);
    
    s64 n_linedim = hashmap_get(&draw->inst->params, Factorio::fpar_n_linedim);
    bool do_blocking = hashmap_get(&draw->inst->params, Factorio::fpar_do_blocking);

    auto font_sans = font_instance_get(&backend->fonts, backend->font_sans);

    s64 xi = draw->sol_detail_x;
    s64 yi = draw->sol_detail_y;
    Field f {xi, yi};

    float max_x = p.x;
    float max_xx;
    
    backend->temp_u8.size = 0;
    array_printf(&backend->temp_u8, "Field (%lld,%lld):", xi, yi);
    font_draw_string(&backend->fonts, backend->font_sans, backend->temp_u8, p, Palette::BLACK, &max_xx, &p.y);
    p.y += font_sans.newline * .5f;
    if (max_x < max_xx) max_x = max_xx;

    float base_x = p.x;

    u32 chars_[3] = {0x2013, '0', '1'};
    Array_t<u32> chars {chars_, 3};
    
    float chars_w = 0;
    for (u32 c: chars) {
        float f;
        font_metrics_codepoint_get(&backend->fonts, backend->font_sans, c, &f);
        if (chars_w < f) chars_w = f;
    }


    auto do_field = [&](Array_t<u8> name, u64 lit, bool newline = false) {
        Color col = Palette::BLACK;
        
        float w;
        font_metrics_string_get(&backend->fonts, backend->font_sans, name, &w);
        u32 flags = gui_pointable(&backend->gui, "solution_draw_details,do_field"_arr, {(s64)lit}, p, {w, font_sans.newline}, 0.f);
        if (flags & Gui::DRAW_ACTIVE) {
            col = lerp(col, Palette::WHITE, 0.5f);
        }
        if (flags & Gui::EVENT_CLICKED_R) {
            backend->temp_u8.size = 0;
            array_printf(&backend->temp_u8, "0x%llx", lit);
            platform_clipboard_set(Platform_clipboard::MIDDLE_BUTTON, backend->temp_u8);
        }
        
        font_draw_string(&backend->fonts, backend->font_sans, name, p, col, &p.x);
        p.x += font_sans.space * 0.5f;
        font_draw_string(&backend->fonts, backend->font_sans, "="_arr, p, Palette::BLACK, &p.x);
        p.x += font_sans.space * 0.5f;
        
        u32 c = chars[draw->sol->get(lit)];
        font_draw_codepoint(&backend->fonts, backend->font_sans, c, p, Palette::BLACK);
        p.x += chars_w;

        if (max_x < p.x) max_x = p.x;
        
        if (newline) {
            p.x = base_x; p.y += font_sans.newline;
        } else {
            p.x += font_sans.space * 1.5f;
        }
    };

    do_field("empty"_arr, f.empty);
    do_field("belt"_arr, f.belt, true);
    do_field("split"_arr, f.split);
    do_field("splitl"_arr, f.splitl);
    do_field("splitr"_arr, f.splitr, true);
    do_field("under"_arr, f.under);
    do_field("underh"_arr, f.underh);
    do_field("underv"_arr, f.underv, true);

    p.y += font_sans.newline * 0.2f;
    
    Array_t<u8> dirtitle[4] = {"Top:"_arr, "Right:"_arr, "Bottom:"_arr, "Left:"_arr};
    float dirtitle_w = 0;
    for (u8 d = 0; d < 4; ++d) {
        float f;
        font_metrics_string_get(&backend->fonts, backend->font_sans, dirtitle[d], &f);
        if (dirtitle_w < f) dirtitle_w = f;
    }        
    for (u8 d = 0; d < 4; ++d) {
        font_draw_string(&backend->fonts, backend->font_sans, dirtitle[d], p, Palette::BLACK);
        p.x += dirtitle_w + font_sans.space;
        do_field("inp"_arr, f.dirs[d].inp);
        do_field("out"_arr, f.dirs[d].out);
        do_field("sid"_arr, f.dirs[d].sid);
        do_field("und"_arr, f.dirs[d].und, true);
    }
    
    p.y += font_sans.newline * 0.2f;
    
    backend->temp_u8.size = 0;
    array_printf(&backend->temp_u8, "line[%lld]", n_linedim - 1);
    float line_w;
    font_metrics_string_get(&backend->fonts, backend->font_sans, backend->temp_u8, &line_w);

    auto do_line = [&](Array_t<u8> name, u64 line) {
        font_draw_string(&backend->fonts, backend->font_sans, name,    p, Palette::BLACK);
        p.x += line_w + font_sans.space * 0.5f;
        font_draw_string(&backend->fonts, backend->font_sans, "="_arr, p, Palette::BLACK, &p.x);
        p.x += font_sans.space * 0.5f;

        backend->temp_u64.size = 0;
        auto arr = sat_expand(draw->inst, line, &backend->temp_u64);
        s64 v_min = 0, v_max = arr.size;
        for (s64 i = 0; i < arr.size; ++i) {
            if (draw->sol->istrue(arr[i])  and v_min < i+1) v_min = i+1;
            if (draw->sol->isfalse(arr[i]) and v_max > i) v_max = i;
            u32 c = chars[draw->sol->get(arr[i])];
            font_draw_codepoint(&backend->fonts, backend->font_sans, c, p, Palette::BLACK);
            p.x += chars_w;
        }

        backend->temp_u8.size = 0;
        array_printf(&backend->temp_u8, "  [%lld,%lld]", v_min, v_max);
        font_draw_string(&backend->fonts, backend->font_sans, backend->temp_u8, p, Palette::BLACK, &p.x);
        
        if (max_x < p.x) max_x = p.x;
        
        p.x = base_x; p.y += font_sans.newline;
    };

    auto do_lines = [&](u64 line_first) {
        for (s64 i = 0; i < n_linedim; ++i) {
            backend->temp_u8.size = 0;
            array_printf(&backend->temp_u8, "line[%lld]", i);
            do_line(backend->temp_u8, line_first | (i << 8));
        }
        do_line("sum"_arr, line_first | 0xfe00);
        if (do_blocking) {
            do_line("block"_arr, line_first | 0xff00);
            do_line("cyclic"_arr, line_first | 0xfd00 | Field::FVAR_CYCLE);
        }
    };

    do_lines(f.line_first);

    p.y += font_sans.newline * 0.2f;
    font_draw_string(&backend->fonts, backend->font_sans, "sumx"_arr, p, Palette::BLACK, nullptr, &p.y);
    do_lines(f.dirs[Dir::RIGHT - Dir::BEG].sum_line_first);

    p.y += font_sans.newline * 0.2f;
    font_draw_string(&backend->fonts, backend->font_sans, "sumy"_arr, p, Palette::BLACK, nullptr, &p.y);
    do_lines(f.dirs[Dir::TOP - Dir::BEG].sum_line_first);
    
    if (out_x) *out_x = max_x;
}

void factorio_solution_draw_props(Backend* context, Factorio_solution_state* draw, Array_t<Sat_propagation> arr, Vec2 pos, Vec2 size) {
    GUI_TIMER(&context->gui);

    if (gui_shortcut_special(&context->gui, Key::F4)) {
        draw->inst->debug_explain_vars_raw ^= true;
    }
    
    auto font_sans = font_instance_get(&context->fonts, context->font_sans);
    Vec2 posmax = pos + size;
    float pad = 10.f; // TODO?

    u64 only_mask = 0;
    u64 only_val = 0;
    if (draw->sol_detail) {
        only_mask = Sat::MASK_SUBTYPE | Field::COORD_MASK;
        only_val = Factorio::VAR_FACTORIO | (u64)(draw->sol_detail_x + Field::COORD_OFFSET) << 40
            | (u64)(draw->sol_detail_y + Field::COORD_OFFSET) << 24;
        assert((only_val & ~only_mask) == 0);
    }

    Vec2 orig = pos;
    float next_x = pos.x;
    for (Sat_propagation i: arr) {
        u64 var = i.lit ^ -(i.lit >> 63);
        Color base = (var & only_mask) == only_val ? Palette::BLACK : Palette::GREY;
        
        context->temp_u8.size = 0;
        sat_explain(draw->inst, i.lit, &context->temp_u8);

        bool is_conflict = not (*draw->sol)[i.lit];
        float dot_size = font_sans.height;
        
        Vec2 rect_p = {pos.x, pos.y};
        Vec2 rect_size = {0.f, font_sans.newline};
        font_metrics_string_get(&context->fonts, context->font_sans, context->temp_u8, &rect_size.x);
        if (is_conflict) rect_size.x += dot_size;

        u32 flags = gui_pointable(&context->gui, "lit_detail"_arr, {(s64)i.lit, i.clause}, rect_p, rect_size, 0.1f);
        if (flags & Gui::EVENT_CLICKED) {
            auto* r = gui_pointable_get(&context->gui, "lit_detail"_arr, {(s64)i.lit, i.clause});
            r->flags ^= Gui::MOD_USER1;
            flags = r->flags;
        }
        bool is_expanded = flags & Gui::MOD_USER1;

        Vec2 text_p = pos;
        if (is_conflict) {
            shape_circle(&context->shapes, rect_p + dot_size/2.f, dot_size*0.32f, Palette::RED);
            shape_rectangle(&context->shapes, rect_p, rect_size, Palette::BGRED);
            text_p.x += dot_size;
        }
        
        Color c = is_expanded ? Palette::RED : base;
        c = flags & Gui::DRAW_ACTIVE ? lerp(c, Palette::WHITE, 0.5f) : c;
        {float x;
        font_draw_string(&context->fonts, context->font_sans, context->temp_u8, text_p, c, &x, &pos.y);
        if (next_x < x) next_x = x;}


        if (is_expanded) {
            u64 constraint = draw->inst->clause_constraint[i.clause];
            while (constraint != -1) {
                context->temp_u8.size = 0;
                array_printf(&context->temp_u8, u8"↳ ");
                sat_explain_constraint(draw->inst, constraint, &context->temp_u8);

                float w;
                font_metrics_string_get(&context->fonts, context->font_sans, context->temp_u8, &w);
                u32 flags = gui_pointable(&context->gui, "lit_detail_constraint"_arr,
                    {(s64)i.lit, i.clause, (s64)constraint}, pos, {w, font_sans.newline});
                Color cc = flags & Gui::DRAW_ACTIVE ? lerp(Palette::BLACK, Palette::WHITE, 0.5f) : Palette::BLACK;
                
                {float x;
                font_draw_string(&context->fonts, context->font_sans, context->temp_u8, pos, cc, &x, &pos.y);
                if (next_x < x) next_x = x;}

                if (flags & Gui::EVENT_CLICKED_R) {
                    context->temp_u8.size = 0;
                    array_printf(&context->temp_u8, "%lld\n", constraint);
                    platform_clipboard_set(Platform_clipboard::MIDDLE_BUTTON, context->temp_u8);
                }
                
                if (pos.y >= posmax.y) {
                    pos = Vec2 {next_x + pad, orig.y};
                }

                constraint = draw->inst->constraint_parent[constraint];
            }
        }
        if (flags & Gui::EVENT_CLICKED_R) {
            context->temp_u8.size = 0;
            array_printf(&context->temp_u8, "0x%llx", i.lit);
            platform_clipboard_set(Platform_clipboard::MIDDLE_BUTTON, context->temp_u8);
        }

        if (pos.y + font_sans.height >= posmax.y) {
            pos = Vec2 {next_x + pad, orig.y};
        }
    }
}

void factorio_solution_draw(Backend* backend, Factorio_solution_state* fsol, Vec2 pos, float* out_x=nullptr) {
    float next_x;
    factorio_solution_draw_image(backend, fsol, pos, &next_x, &pos.y);
    float pad = 10.f;

    if (fsol->sol_detail) {
        pos.y += pad;
        float next_xx;
        factorio_solution_draw_detail(backend, fsol, pos, &next_xx);
        if (next_x < next_xx) next_x = next_xx;
    }

    if (out_x) *out_x = next_x;
}

struct Factorio_propagation_state {
    Sat_instance inst;
    Sat_solution sol;

    Array_t<u8> name_instance, name_solution;

    Array_dyn<s64> anim_indices;
    Array_dyn<Sat_propagation> anim_data;
    s64 anim_frame;

    Array_dyn<u64> frame_conflicts;
    s64 total_conflicts;

    Factorio_solution_state fsol;
};

void factorio_propagation_init(Factorio_propagation_state* prop, Factorio_instance finst, Factorio_solution* opt_sol) {
    sat_solution_free(&prop->sol);

    Factorio_params p = finst.params;
    prop->name_instance = finst.name;

    prop->anim_indices.size = 0;
    prop->anim_data.size = 0;

    sat_init(&prop->inst);
    factorio_balancer(&prop->inst, p);
    if (opt_sol) {
        prop->name_solution = opt_sol->name;
        factorio_clauses_from_solution(&prop->inst, opt_sol);
    } else {
        prop->name_solution = {};
    }

    prop->sol = sat_solution_from_instance(&prop->inst, &prop->anim_indices, &prop->anim_data);
    prop->anim_frame = 0;

    s64 frame_len_max = 700;
    for (s64 i = 1; i < prop->anim_indices.size; ++i) {
        if (prop->anim_indices[i] - prop->anim_indices[i-1] > frame_len_max) {
            array_push_back(&prop->anim_indices, 0ll);
            for (s64 j = prop->anim_indices.size-1; j > i; --j) {
                prop->anim_indices[j] = prop->anim_indices[j-1];
            }
            prop->anim_indices[i] = prop->anim_indices[i-1] + frame_len_max;
        }
    }
    
    Array_dyn<u8> text_temp;
    defer { array_free(&text_temp); };
    for (s64 i = 0; i+1 < prop->anim_indices.size; ++i) {
        auto arr = array_subindex(prop->anim_indices, prop->anim_data, i);
        std::sort(arr.begin(), arr.end(), [prop, &text_temp](Sat_propagation a, Sat_propagation b) {
            text_temp.size = 0;
            sat_explain(&prop->inst, a.lit, &text_temp);
            s64 index = text_temp.size;
            sat_explain(&prop->inst, b.lit, &text_temp);
            return array_cmp(array_subarray(text_temp, 0, index), array_subarray(text_temp, index)) < 0;
        });
    }

    factorio_solution_init(&prop->fsol, &prop->inst, &prop->sol);

    prop->total_conflicts = 0;
    prop->frame_conflicts.size = 0;
    array_append_zero(&prop->frame_conflicts, (prop->anim_indices.size-1 + 63)/64);
    
    for (s64 frame = 1; frame < prop->anim_indices.size; ++frame) {
        auto arr = array_subindex(prop->anim_indices, prop->anim_data, frame-1);
        s64 n_conflicts = 0;
        for (Sat_propagation i: arr) n_conflicts += not prop->sol[i.lit];
        
        if (n_conflicts) {
            bitset_set(&prop->frame_conflicts, frame, true);
        }
        prop->total_conflicts += n_conflicts;
    }
}

void factorio_propagation_draw(Backend* context, Factorio_propagation_state* prop, Vec2 pos, Vec2 size) {
    float pad = 10.f; // TODO?

    if (gui_shortcut_special(&context->gui, Key::ARROW_L)) {
        prop->anim_frame -= prop->anim_frame > 0;
    }
    if (gui_shortcut_special(&context->gui, Key::ARROW_R)) {
        prop->anim_frame += prop->anim_frame < prop->anim_indices.size-1;
    }
    if (gui_shortcut_special(&context->gui, Key::HOME)) {
        prop->anim_frame = 0;
    }
    if (gui_shortcut_special(&context->gui, Key::END)) {
        prop->anim_frame = prop->anim_indices.size-1;
    }
    if (gui_shortcut_special(&context->gui, Key::PAGE_U)) {
        while (prop->anim_frame < prop->anim_indices.size-1) {
            ++prop->anim_frame;
            if (bitset_get(prop->frame_conflicts, prop->anim_frame)) break;
        }
    }
    if (gui_shortcut_special(&context->gui, Key::PAGE_D)) {
        while (prop->anim_frame > 0) {
            --prop->anim_frame;
            if (bitset_get(prop->frame_conflicts, prop->anim_frame)) break;
        }
    }
    
    sat_solution_init(&prop->sol);
    auto arr = array_subarray(prop->anim_data, 0, prop->anim_indices[prop->anim_frame]);
    for (Sat_propagation i: arr) {
        if (prop->sol.get(i.lit) == Sat_solution::L_UNASSIGNED) prop->sol.set(i.lit);
    }

    context->temp_u8.size = 0;
    array_append(&context->temp_u8, prop->name_instance);
    if (prop->name_solution.size) {
        array_push_back(&context->temp_u8, '/');
        array_append(&context->temp_u8, prop->name_solution);
    }
    array_printf(&context->temp_u8, " (total conflicts %lld)", prop->total_conflicts);
    
    Vec2 p = pos;
    font_draw_string(&context->fonts, context->font_sans, context->temp_u8, p, Palette::BLACK, nullptr, &p.y); 
    
    context->temp_u8.size = 0;
    array_printf(&context->temp_u8, "Frame %lld/%lld", prop->anim_frame, prop->anim_indices.size - 1);
        
    if (prop->anim_frame != 0) {
        auto arr = array_subindex(prop->anim_indices, prop->anim_data, prop->anim_frame-1);
        s64 n_lits = arr.size;
        s64 n_conflicts = 0;
        for (Sat_propagation i: arr) n_conflicts += not prop->sol[i.lit];
        array_printf(&context->temp_u8, " (props: %lld, conflicts: %lld)", n_lits, n_conflicts);
    }

    font_draw_string(&context->fonts, context->font_sans, context->temp_u8, p, Palette::BLACK, nullptr, &p.y); 
    p.y += pad;

    factorio_solution_draw(context, &prop->fsol, p, &p.x);

    p.x += pad;
    p.y = pos.y;

    if (prop->anim_frame != 0) {
        auto arr = array_subindex(prop->anim_indices, prop->anim_data, prop->anim_frame-1);
        factorio_solution_draw_props(context, &prop->fsol, arr, p, size - (p - pos));
    }
}

struct Factorio_solver_state {
    Sat_instance inst;
    Sat_solver_state solver;
    Array_dyn<u32> codepoint_temp;
    Array_dyn<s32> lines;

    u64 redraw_fd_token;
    bool show_solution;
    bool flag_new_lines;
    Factorio_solution_state fsol;
};

void factorio_solver_init(Factorio_solver_state* solver) {
    solver->codepoint_temp.size = 0;
    solver->lines.size = 0;
    solver->redraw_fd_token = 0;

    sat_solver_init(&solver->solver, &solver->inst);
    
    solver->show_solution = false;
    factorio_solution_init(&solver->fsol, &solver->inst, &solver->solver.sol);
    
    //{Array_dyn<u8> human;
    //defer { array_free(&human); };
    //sat_write_human(&solver->inst, &human);
    //FILE* f = fopen("human.out", "w");
    //fwrite(human.data, 1, human.size, f);
    //fclose(f);}
}

void factorio_solver_doinput(Factorio_solver_state* solver, Font_data* fonts, s64 font_line, Gui* gui) {
    GUI_TIMER(gui);

    if (solver->redraw_fd_token) {
        pollfd pfd = platform_redraw_fd_result(solver->redraw_fd_token);
        solver->redraw_fd_token = 0;
        sat_solver_process(&solver->solver, pfd);
    }

    if (solver->solver.output_fd != -1) {
        solver->redraw_fd_token = platform_redraw_fd({solver->solver.output_fd, POLLIN, 0});
    }

    for (s64 i = 0; i+1 < solver->solver.output_lines.size; ++i) {
        auto line = array_subindex(solver->solver.output_lines, solver->solver.output_data, i);
        if (line.size < 3) continue;
        auto str = array_subarray(line, 2, line.size - 1);
        u32 word = font_word_create_utf8(fonts, font_line, str);
        solver->flag_new_lines = true;
        array_push_back(&solver->lines, word);        
    }

    s64 last = solver->solver.output_lines.back();
    if (0 < last and last < solver->solver.output_data.size) {
        memmove(&solver->solver.output_data[0], &solver->solver.output_data[last], solver->solver.output_data.size - last);
    }
    solver->solver.output_data.size -= last;
    solver->solver.output_lines.size = 1;

    if (solver->solver.state == Sat_solver_state::SAT) {
        solver->show_solution = true;
    }
}

void factorio_solver_stop(Factorio_solver_state* solver) {
    sat_solver_stop(&solver->solver);
    solver->redraw_fd_token = 0;
}

void factorio_solver_draw(Backend* context, Factorio_solver_state* solver, Vec2 pos, Vec2 size, Vec2 pad) {
    GUI_TIMER(&context->gui);

    if (solver->solver.output_fd != -1) {
        factorio_solver_doinput(solver, &context->fonts, context->font_sans, &context->gui);
    }
    auto font_sans = font_instance_get(&context->fonts, context->font_sans);

    Vec2 p = pos;
    
    if (solver->show_solution) {
        factorio_solution_draw(context, &solver->fsol, p + pad, &p.x);
        size.x -= p.x - pos.x;
    }

    bool was_flush = gui_scrollbar_is_at_bottom(&context->gui, "solver_draw,scroll"_arr, {});
    
    float total_height = solver->lines.size * font_sans.newline + 2*pad.y + 50;
    gui_scrollbar_set_height(&context->gui, "solver_draw,scroll"_arr, {}, total_height);

    if (was_flush and solver->flag_new_lines) {
        gui_scrollbar_move_to_bottom(&context->gui, "solver_draw,scroll"_arr, {});
    }
    
    gui_scrollbar(&context->gui, "solver_draw,scroll"_arr, {}, p, size, 0.f, nullptr, &p.y);

    p.x += pad.x;
    p.y += font_sans.ascent;
    for (s64 word: solver->lines) {
        font_draw_word(&context->fonts, word, p, Palette::BLACK);
        p.y += font_sans.newline;
    }
}

struct Factorio_params_state {    
    Factorio_db fdb;
    s64 current_instance = -1;
    s64 current_solution = -1;
    float pad = 10.f;
};

struct Factorio_params_result {
    u8 action = Factorio_gui_state::INVALID;
    s64 instance = -1;
    s64 solution = -1;
};

Factorio_params_result factorio_params_draw(Backend* context, Factorio_params_state* draw, Vec2 p, Vec2 size) {
    GUI_TIMER(&context->gui);
    
    auto font_sans = font_instance_get(&context->fonts, context->font_sans);    
    float pad = 10.f; // TODO?

    float name_w = 0.f;
    for (Factorio_instance i: draw->fdb.instances) {
        float w;
        font_metrics_string_get(&context->fonts, context->font_sans, i.name, &w);
        if (name_w < w) name_w = w;
    }
    
    bool action_draw_instance = false;
    bool action_run_solver = false;
    for (s64 i = 0; i < draw->fdb.instances.size; ++i) {
        {u32 flags = gui_pointable(&context->gui, "choose_instance,instance"_arr, {i});
        if (flags & Gui::EVENT_CLICKED) {
            draw->current_instance = i;
        }}
        {u32 flags = gui_pointable(&context->gui, "choose_instance,instance_show"_arr, {i});
        if (flags & Gui::EVENT_CLICKED) {
            draw->current_instance = i;
            action_draw_instance = true;
            break;
        }}
        {u32 flags = gui_pointable(&context->gui, "choose_instance,instance_run"_arr, {i});
        if (flags & Gui::EVENT_CLICKED) {
            draw->current_instance = i;
            action_run_solver = true;
            break;
        }}
    }

    if (action_draw_instance) {
        return {Factorio_gui_state::DRAW_PROPAGATION, draw->current_instance, draw->current_solution};
    } else if (action_run_solver) {
        return {Factorio_gui_state::RUN_SOLVER, draw->current_instance, draw->current_solution};
    }

    gui_scrollbar(&context->gui, "choose_instance,scroll"_arr, {}, p, size, 0.f, &size.x, &p.y);
    float orig_y = p.y;
    
    bool action_check_solutions = gui_button(
        &context->gui, "check_solutions"_arr, {}, p + pad, "Check solutions"_arr, 0.f, nullptr, &p.y
    );
    if (action_check_solutions) {
        context->redraw_internal = true;
        return {Factorio_gui_state::CHECK_SOLUTIONS};
    }

    p += pad;
    font_draw_string(&context->fonts, context->font_sans, "Choose instance:"_arr, p, Palette::BLACK, nullptr, &p.y);    
    p.y += font_sans.newline * 0.5;
    p.x += font_sans.space * 2;
    
    for (s64 i = 0; i < draw->fdb.instances.size; ++i) {
        Factorio_instance i_inst = draw->fdb.instances[i];
        for (s64 j = -1; j < draw->fdb.solutions.size; ++j) {
            if (j >= 0 and not array_equal(draw->fdb.solutions[j].instance_name, i_inst.name)) continue;

            u32 flags = gui_pointable(&context->gui, "choose_instance,solution"_arr, {i, j});
            if (flags & Gui::EVENT_CLICKED) {
                draw->current_instance = i;
                draw->current_solution = j;
            }
        }
    }
    
    for (s64 i = 0; i < draw->fdb.instances.size; ++i) {
        Factorio_instance i_inst = draw->fdb.instances[i];
        
        Vec2 v = p;
        float w0, w1;

        font_draw_string(&context->fonts, context->font_sans, i_inst.name, p, Palette::BLACK);
        v.x += name_w + 2 * font_sans.space;

        context->temp_u8.size = 0;
        array_printf(&context->temp_u8, "nx = %lld, ny = %lld, n_under = %lld, scale_fac = %lld",
                     i_inst.params.nx, i_inst.params.ny, i_inst.params.n_under, i_inst.params.scale_fac);
        font_draw_string(&context->fonts, context->font_sans, context->temp_u8, v, Palette::BLUE, &w0, &v.y);
        
        context->temp_u8.size = 0;
        array_printf(&context->temp_u8, "do_blocking = %d, yoff_output = [", i_inst.params.do_blocking);
        {bool first = true;
        for (s64 j: i_inst.params.yoff_output) {
            if (first) first = false;
            else       array_printf(&context->temp_u8, ", ");
            array_printf(&context->temp_u8, "%lld", j);
        }}
        array_printf(&context->temp_u8, "], yoff_input = [");
        {bool first = true;
        for (s64 j: i_inst.params.yoff_input) {
            if (first) first = false;
            else       array_printf(&context->temp_u8, ", ");
            array_printf(&context->temp_u8, "%lld", j);
        }}
        array_printf(&context->temp_u8, "]");
        font_draw_string(&context->fonts, context->font_sans, context->temp_u8, v, Palette::BLUE, &w1, &v.y);
        v.y += font_sans.newline * 0.25f;

        float wsol;
        font_draw_string(&context->fonts, context->font_sans, "Solution: "_arr, v, Palette::BLACK, &wsol);
        wsol -= v.x;

        bool anyhover = false;
        for (s64 j = -1; j < draw->fdb.solutions.size; ++j) {
            if (j >= 0 and not array_equal(draw->fdb.solutions[j].instance_name, i_inst.name)) continue;

            Array_t<u8> name = j >= 0 ? draw->fdb.solutions[j].name : "none"_arr;

            Vec2 vv = {v.x+wsol, v.y};
            float w_name;
            font_metrics_string_get(&context->fonts, context->font_sans, name, &w_name);
            wsol += w_name + font_sans.space;

            u32 flags = gui_pointable(&context->gui, "choose_instance,solution"_arr, {i, j}, vv, {w_name, font_sans.height});
            
            Color c = i == draw->current_instance and j == draw->current_solution ? Palette::RED :
                flags & Gui::DRAW_ACTIVE ? lerp(Palette::REDLIGHT, Palette::BLACK, 0.6f)
                : Palette::BLACK;
            anyhover |= flags & Gui::DRAW_ACTIVE;

            font_draw_string(&context->fonts, context->font_sans, name, vv, c);
        }
        v.y += font_sans.newline;

        {Vec2 vv = v;
        if (draw->current_instance == i) {
            gui_button(&context->gui, "choose_instance,instance_show"_arr, {i}, vv, "Show"_arr, 0.f, &vv.x, nullptr);
            gui_button(&context->gui, "choose_instance,instance_run"_arr, {i}, vv, "Run solver"_arr, 0.f, nullptr, &v.y);
            v.y += font_sans.newline - font_sans.height;
        }}
        
        Vec2 click_r {v.x + max(w0, w1), v.y - p.y};
        {u32 flags = gui_pointable(&context->gui, "choose_instance,instance"_arr, {i}, p, click_r, 0.02f);
        if (draw->current_instance == i) {
            shape_rectangle(&context->shapes, p, click_r, Palette::BGBLUE, 0.03f);
        } else if ((flags & Gui::DRAW_ACTIVE) or anyhover) {
            shape_rectangle(&context->shapes, p, click_r, lerp(Palette::BGBLUE, Palette::BG, 0.6f), 0.03f);            
        }}
        
        p.y = v.y + font_sans.newline * 0.25f;
    }

    p.y += pad;
    gui_scrollbar_set_height(&context->gui, "choose_instance,scroll"_arr, {}, p.y - orig_y);
    
    return {};
}

struct Factorio_checksol_state {
    enum Results: u8 {
        NONE = 0, PENDING, SAT, UNSAT
    };
    
    struct Row {
        s64 instance, solution;
        bool expect_unsat = false;
        u8 result = 0;
    };

    Factorio_db* fdb;
    Array_dyn<Row> rows;

    Sat_instance inst;
    s64 cur_row;

    Sat_solver_state solver;
    s64 redraw_fd_token = 0;
};

void factorio_checksol_init(Factorio_checksol_state* check, Factorio_db* fdb) {
    check->fdb = fdb;
    check->rows.size = 0;
    
    for (s64 i = 0; i < fdb->solutions.size; ++i) {
        auto fsol = fdb->solutions[i];

        s64 j;
        for (j = 0; j < fdb->instances.size; ++j) {
            if (array_equal(fsol.instance_name, fdb->instances[j].name)) break;
        }
        assert(j < fdb->instances.size);

        array_push_back(&check->rows, {j, i});
        
        if (fsol.name.size >= 3 and array_equal_str(array_subarray(fsol.name, 0, 3), "err")) {
            check->rows.back().expect_unsat = true;
        }
    }

    check->cur_row = 0;
    check->redraw_fd_token = 0;
}

void factorio_checksol_doinput(Factorio_checksol_state* check) {
    while (check->cur_row < check->rows.size) {
        auto* row = &check->rows[check->cur_row];
        
        if (row->result == Factorio_checksol_state::NONE) {
            sat_init(&check->inst);
            factorio_balancer(&check->inst, check->fdb->instances[row->instance].params);
            factorio_clauses_from_solution(&check->inst, &check->fdb->solutions[row->solution]);

            sat_solver_init(&check->solver, &check->inst);
            row->result = Factorio_checksol_state::PENDING;
        }
        if (row->result == Factorio_checksol_state::PENDING) {
            if (check->redraw_fd_token) {
                pollfd pfd = platform_redraw_fd_result(check->redraw_fd_token);
                check->redraw_fd_token = 0;
                sat_solver_process(&check->solver, pfd);
            }
            
            if (check->solver.state == Sat_solver_state::RUNNING) {
                check->redraw_fd_token = platform_redraw_fd({check->solver.output_fd, POLLIN, 0});
            }
            if (check->solver.state == Sat_solver_state::SAT) {
                row->result = Factorio_checksol_state::SAT;
                ++check->cur_row;
                continue;
            } else if (check->solver.state == Sat_solver_state::UNSAT) {
                row->result = Factorio_checksol_state::UNSAT;
                ++check->cur_row;
                continue;
            } else {
                break;
            }

            assert(false);
        }
    }
}

void factorio_checksol_stop(Factorio_checksol_state* check) {
    sat_solver_stop(&check->solver);
    check->redraw_fd_token = 0;
}

void factorio_checksol_draw(Backend* backend, Factorio_checksol_state* check, Vec2 p, Vec2 size) {
    using Row = Factorio_checksol_state::Row;
    
    float pad = 10.f;
    auto font_sans = font_instance_get(&backend->fonts, backend->font_sans);
    Array_t<u8> strings[] = {"-"_arr, "pending"_arr, "SAT"_arr, "UNSAT"_arr};
    Color colors[] = {Palette::BLACK, Palette::BLACK, Palette::GREEN, Palette::RED};

    auto* fdb = check->fdb;
    
    p += pad;
    Vec2 p0 = p;

    auto cell = [backend](Array_t<u8> s, Vec2* p, float* out_w, Color c = Palette::BLACK) {
        float x;
        font_draw_string(&backend->fonts, backend->font_sans, s, *p, c, &x, &p->y);
        if (out_w) *out_w = max(*out_w, x - p->x);
    };
    
    
    float w0 = 0.f;
    cell("instance"_arr, &p, &w0);
    p.y += pad;
    for (Row i: check->rows) cell(fdb->instances[i.instance].name, &p, &w0);
    p.x += w0 + pad; p.y = p0.y;

    float w1 = 0.f;
    cell("solution"_arr, &p, &w1);
    p.y += pad;
    for (Row i: check->rows) cell(fdb->solutions[i.solution].name, &p, &w1);
    p.x += w1 + pad; p.y = p0.y;
    
    float w2 = 0.f;
    cell("result"_arr, &p, &w2);
    p.y += pad;
    for (Row i: check->rows) cell(strings[i.result], &p, &w2, colors[i.result ^ i.expect_unsat]);
    p.x += w2 + pad;

    p.x = p0.x;

    shape_rectangle(&backend->shapes, p0 + Vec2 {0.f, font_sans.newline + pad/2}, {w0 + w1 + w2 + 2*pad, 1.f}, Palette::BLACK);
}

void factorio_checksol_update(Backend* backend, Factorio_checksol_state* check, Vec2 p, Vec2 size) {
    factorio_checksol_doinput(check);
    factorio_checksol_draw(backend, check, p, size);
}


struct Factorio_gui {
    u8 state = Factorio_gui_state::INVALID;
    
    Factorio_params_state      params;
    Factorio_solution_state    solution;
    Factorio_propagation_state propagation;
    Factorio_solver_state      solver;
    Factorio_checksol_state    checksol;
};

void factorio_gui_init(Factorio_gui* fgui, Asset_store* assets) {
    // Initialise instance
    Array_t<u8> fdb_data = asset_get(assets, "factorio_db"_arr);
    factorio_db_parse(&fgui->params.fdb, fdb_data);

    fgui->state = Factorio_gui_state::CHOOSE_PARAMS;
}

void factorio_gui_reset(Factorio_gui* fgui) {
    // Cleanup
    switch (fgui->state) {
    case Factorio_gui_state::CHOOSE_PARAMS:
    case Factorio_gui_state::DRAW_PROPAGATION:
    case Factorio_gui_state::DRAW_SOLUTION:
        // nothing
        break;
            
    case Factorio_gui_state::RUN_SOLVER:
        factorio_solver_stop(&fgui->solver);
        break;

    case Factorio_gui_state::CHECK_SOLUTIONS:
        factorio_checksol_stop(&fgui->checksol);
        break;
        
    default: assert(false);
    }

    fgui->state = Factorio_gui_state::CHOOSE_PARAMS;
}

void factorio_gui_reload(Factorio_gui* fgui, Asset_store* assets) {
    if (asset_try_reload(assets, "factorio_db"_arr)) {
        factorio_gui_reset(fgui);

        factorio_db_clear(&fgui->params.fdb);
        Array_t<u8> fdb_data = asset_get(assets, "factorio_db"_arr);
        factorio_db_parse(&fgui->params.fdb, fdb_data);
    }
}

void factorio_gui_draw(Backend* backend, Factorio_gui* fgui) {
    Vec2 screen {(float)backend->screen_w, (float)backend->screen_h};
    float pad = 10.f;
    Vec2 padd = {pad, pad};

    if (gui_shortcut_special(&backend->gui, Key::ESCAPE)) {
        factorio_gui_reset(fgui);
    }
    
    if (fgui->state == Factorio_gui_state::CHOOSE_PARAMS) {
        auto result = factorio_params_draw(backend, &fgui->params, {}, screen);
        
        if (result.action == Factorio_gui_state::INVALID) {
            goto gui_was_rendered;
            
        } else if (result.action == Factorio_gui_state::DRAW_PROPAGATION) {
            Factorio_solution* sol = result.solution >= 0 ? &fgui->params.fdb.solutions[result.solution] : nullptr;
            factorio_propagation_init(&fgui->propagation, fgui->params.fdb.instances[result.instance], sol);
            
        } else if (result.action == Factorio_gui_state::RUN_SOLVER) {
            Factorio_solution* sol = result.solution >= 0 ? &fgui->params.fdb.solutions[result.solution] : nullptr;
            
            sat_init(&fgui->solver.inst);
            factorio_balancer(&fgui->solver.inst, fgui->params.fdb.instances[result.instance].params);
            if (sol) {
                factorio_clauses_from_solution(&fgui->solver.inst, sol);
            }
            
            factorio_solver_init(&fgui->solver);
            
        } else if (result.action == Factorio_gui_state::CHECK_SOLUTIONS) {
            factorio_checksol_init(&fgui->checksol, &fgui->params.fdb);
            
        } else {
            assert(false);
        }
        
        fgui->state = result.action;
    }
    if (fgui->state == Factorio_gui_state::RUN_SOLVER) {
        factorio_solver_draw(backend, &fgui->solver, {}, screen, padd);
        goto gui_was_rendered;
    }
    if (fgui->state == Factorio_gui_state::DRAW_PROPAGATION) {
        factorio_propagation_draw(backend, &fgui->propagation, padd, screen - 2.f * padd);
        goto gui_was_rendered;
    }
    if (fgui->state == Factorio_gui_state::DRAW_SOLUTION) {
        factorio_solution_draw(backend, &fgui->solution, padd);
        goto gui_was_rendered;
    }
    if (fgui->state == Factorio_gui_state::CHECK_SOLUTIONS) {
        factorio_checksol_update(backend, &fgui->checksol, {}, screen);
        goto gui_was_rendered;
    }

    assert(false);
  gui_was_rendered:
    return;
}

// Keeps the necessary data to manage OpenGL and other data for the ui layer
struct Application {
    // Data required by the platform layer
    Array_dyn<Key> input_queue;

    // Other data
    Backend backend;
    
    Asset_store assets;
    bool asset_is_pack = false;

    Factorio_gui factorio_gui;
};

void application_handle_resize(Application* context, s64 width, s64 height) {
    if (width  != -1) context->backend.screen_w = width;
    if (height != -1) context->backend.screen_h = height;

    glViewport(0.0, 0.0, context->backend.screen_w, context->backend.screen_h);
}

void application_init_assets(Application* context, bool flag_pack_assets) {
    context->asset_is_pack = asset_init(&context->assets);
    if (not context->asset_is_pack) {
        asset_load_source(&context->assets, "gui.cpp"_arr);
        asset_load_source(&context->assets, "font.cpp"_arr);
        asset_load_source(&context->assets, "shapes.cpp"_arr);
        asset_load_source(&context->assets, "spline.cpp"_arr);
        asset_load_file(&context->assets, "font"_arr, "fonts_stripped/DejaVuSans.ttf"_arr);
        asset_load_file(&context->assets, "font_license"_arr, "fonts_stripped/LICENSE"_arr);
        asset_load_file(&context->assets, "factorio_db"_arr, "instances.lst"_arr);
    }
    asset_finalize(&context->assets, flag_pack_assets);
}

void application_init(Application* context) {
    auto* backend = &context->backend;
    
    // Initialise the fonts
    font_init(&backend->fonts, &context->assets, GL_TEXTURE2);
    backend->font_sans_base = font_add(&backend->fonts, asset_get(&context->assets, "font"_arr));
    backend->font_sans = font_instantiate(&backend->fonts, backend->font_sans_base, 15.f);
    font_instantiate_defaults(&backend->fonts, backend->font_sans_base,
        backend->font_sans_base, backend->font_sans_base, backend->font_sans_base);

    font_instance_scale_defaults(&backend->fonts, 0.75f);

    backend->font_solution = font_instantiate(&backend->fonts, backend->font_sans_base);

    // Initialise the shapes and gui
    shape_init(&backend->shapes, &context->assets);
    gui_init(&backend->gui, &context->assets, &backend->shapes, &backend->fonts, backend->font_sans);
    factorio_gui_init(&context->factorio_gui, &context->assets);
    
    // Now we load our shaders
    backend->spline = opengl_shader_assets(&context->assets, "spline"_arr);
}

void backend_frame_draw(Backend* backend) {
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    
    opengl_clear_color(Palette::BG);
    glClear(GL_COLOR_BUFFER_BIT);
    glClear(GL_DEPTH_BUFFER_BIT);
    
    shape_frame_draw(&backend->shapes, backend->screen_w, backend->screen_h);

    gui_frame_draw  (&backend->gui,    backend->screen_w, backend->screen_h);

    gui_timer(&backend->gui, "font_frame_draw"_arr);
    font_frame_draw (&backend->fonts,  backend->screen_w, backend->screen_h);
}


void backend_clear(Backend* backend) {
    shape_clear(&backend->shapes);
    gui_clear(&backend->gui);
    font_clear(&backend->fonts);
}

void application_render(Application* context) {
    gui_frame_init(&context->backend.gui);

    auto* fgui = &context->factorio_gui;
    
    for (Key i: context->input_queue) {
        bool consumed = false;
        if (i.type == Key::SPECIAL) {
            consumed = true;
            switch (i.special) {
            case Key::F5:      factorio_gui_reload(fgui, &context->assets); break;
            case Key::F8:      context->backend.gui.debug_draw_timers ^= true; break;
            case Key::C_QUIT:  exit(0);
            default: consumed = false;
            }
        }

        if (not consumed) {
            gui_process_input(&context->backend.gui, i);
        }
    }
    context->input_queue.size = 0;
    
    context->backend.shapes.z_level_add = 0.1f;
    context->backend.redraw_internal = false;
    
    factorio_gui_draw(&context->backend, fgui);
    
    if (context->backend.redraw_internal) {
        backend_clear(&context->backend);
        factorio_gui_draw(&context->backend, fgui);
    }
    
    backend_frame_draw(&context->backend);
}


void application_print_help(Application* context, char* argv0) {
    printf("Usage:\n  %s [--fullscreen]\n", argv0);
    if (context->asset_is_pack) {
        printf("  %s --font-license\n", argv0);
    } else {
        printf("  %s --pack\n", argv0);
    }
    printf("  %s --help\n\n", argv0);

    puts("This is " PROGRAM_NAME " written by Philipp Czerner in 2019. Running the program without any arguments starts the GUI, which is the main part of this application.\n");
    puts("  --fullscreen  You may be able to guess the meaning of this option from context.");
    if (context->asset_is_pack) {
        puts("  --font-license  You are running the packed version of " PROGRAM_NAME ", which means that the font data is included in the binary. To view the license under which the fonts are distributed, run " PROGRAM_NAME " with the --font-license option.");
    } else {
        puts("  --pack  You are running the unpacked version of the binary, which means that it will load assets from its CWD. (The source files may contain assets as well.) You can run this programm with the '--pack' option, which will create a '" PROGRAM_NAME "_packed' binary in the CWD that contains the asset data.");
    }
    puts("  --help  Prints this help.");
}

void application_print_license(Application* context) {
    Array_t<u8> license = asset_get(&context->assets, "font_license"_arr);
    fwrite(license.data, 1, license.size, stdout);
    puts("");
}

int old_main() {
    Array_dyn<s64> yoff_output, yoff_input;
    array_append(&yoff_output, {0});
    array_append(&yoff_input, {0, 1});
    Factorio_params p {3, 2, 10, 1, false, yoff_output, yoff_input};
    
    Sat_instance inst;
    sat_init(&inst);
    inst.debug_forbidden_id = 0x2880118012300000ull;

    factorio_balancer(&inst, p);

    Sat_dimacs dimacs;
    sat_write_dimacs(&inst, &dimacs);

    fwrite(dimacs.text.data, 1, dimacs.text.size, stdout);

    Array_dyn<u8> human;
    sat_write_human(&inst, &human);
    //fwrite(human.data, 1, human.size, stdout);
    return 0;
}


#define PROGRAM_NAME "cppsat"
#define PROGRAM_TITLE "Belt Balancers"

#ifndef PLATFORM_INCLUDES
#error "You did not compile the right file."
#endif

#include <unistd.h>
#include <poll.h>

#include "hashmap.cpp"
#include "asset.cpp"
#include "opengl.cpp"
#include "spline.cpp"
#include "font.cpp"
#include "shapes.cpp"
#include "gui.cpp"

#include "array_linux.cpp"
#include "sat.cpp"
#include "factorio_sat.cpp"


struct Backend {
    Shader circle, spline;
    s64 screen_w = 0, screen_h = 0;
    
    Font_data fonts;
    s64 font_sans_base, font_sans;
    s64 font_solution;

    Gui gui;
    Shape_drawer shapes;

    Array_dyn<u8> temp_u8;
    Array_dyn<u64> temp_u64;
};


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

void factorio_solution_draw_image(Backend* backend, Factorio_solution_state* draw, Vec2 p, float* x_out, float* y_out) {
    GUI_TIMER(backend);

    float border_size = 16;
    float border_pad = 4;
    Vec2 bar_size_min = {8, 4};
    float arrow_fac = 0.8f;
    font_instance_scale(&backend->fonts, backend->font_solution, border_size);

    s64 nx = hashmap_get(&draw->inst->params, Factorio::fpar_nx);
    s64 ny = hashmap_get(&draw->inst->params, Factorio::fpar_ny);
    s64 n_lines = hashmap_get(&draw->inst->params, Factorio::fpar_n_lines);
    s64 n_linedim = hashmap_get(&draw->inst->params, Factorio::fpar_n_linedim);
    s64 n_linelen = hashmap_get(&draw->inst->params, Factorio::fpar_n_linelen);
    
    float center_size = max(bar_size_min.x * n_lines, bar_size_min.y * n_linelen);
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

            for (s64 i = 0; i < n_lines; ++i) {
                u64 line = i <  n_linedim ? f.line_first | (i << 8) :
                           i == n_linedim ? f.line_sum :
                                            f.line_block;
                backend->temp_u64.size = 0;
                auto arr = sat_expand(draw->inst, line, &backend->temp_u64);
                for (s64 j = 0; j < arr.size; ++j) {
                    Color col;
                    switch (draw->sol->get(arr[j])) {
                    case Sat_solution::L_UNASSIGNED: col = Palette::GREY; break;
                    case Sat_solution::L_FALSE:      col = Palette::BLUELIGHT; break;
                    case Sat_solution::L_TRUE:       col = Palette::BLUE; break;
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
    GUI_TIMER(backend);
    
    s64 n_linedim = hashmap_get(&draw->inst->params, Factorio::fpar_n_linedim);
    bool do_block = hashmap_get(&draw->inst->params, Factorio::fpar_do_block);

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
    
    for (s64 i = 0; i < n_linedim; ++i) {
        backend->temp_u8.size = 0;
        array_printf(&backend->temp_u8, "line[%lld]", i);
        do_line(backend->temp_u8, f.line_first | (i << 8));
    }
    do_line("sum"_arr,   f.line_sum);
    if (do_block) {
        do_line("block"_arr, f.line_block);
    }

    if (out_x) *out_x = max_x;
}

void factorio_solution_draw_props(Backend* context, Factorio_solution_state* draw, Array_t<u64> arr, Vec2 pos, Vec2 size) {
    GUI_TIMER(&context->gui);
    
    auto font_sans = font_instance_get(&context->fonts, context->font_sans);
    Vec2 posmax = pos + size;
        
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
        sat_explain(&draw->inst, i.lit, &context->temp_u8);

        bool is_conflict = not draw->sol[i.lit];
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
                sat_explain_constraint(&draw->inst, constraint, &context->temp_u8);

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
                    pos = Vec2 {next_x + draw->pad, orig.y};
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
            pos = Vec2 {next_x + draw->pad, orig.y};
        }
    }
}

void factorio_solution_draw(Backend* backend, Factorio_solution_state* fsol, Vec2 pos, float* out_x) {
    float next_x;
    factorio_solution_draw_image(backend, fsol, pos, &next_x, &pos.y);

    if (draw->sol_detail) {
        p.y += draw->pad;
        float next_xx;
        factorio_solution_draw_detail(backend, fsol, p, &next_xx);
        if (next_x < next_xx) next_x = next_xx;
    }

    if (out_x) *out_x = next_x;
}

struct Factorio_propagation_state {
    Sat_instance inst;
    Sat_solution sol;

    Array_dyn<s64> anim_indices;
    Array_dyn<Sat_propagation> anim_data;
    s64 anim_frame;

    Factorio_solution_state fsol;
};

void factorio_propagation_init(Factorio_propagation_state* prop, Factorio_params p, Factorio_solution* opt_sol) {
    sat_init(&prop->inst);

    factorio_balancer(&prop->inst, p);
    if (opt_sol) {
        factorio_clauses_from_diagram(&prop->inst, opt_sol->ascii_diagram);
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
}

void factorio_propagation_draw(Backend* context, Factorio_solution_draw* draw, Vec2 pos, Vec2 size) {
    if (draw->do_animation) {
        sat_solution_init(&draw->sol);
        auto arr = array_subarray(draw->anim_data, 0, draw->anim_indices[draw->anim_frame]);
        for (Sat_propagation i: arr) {
            if (draw->sol->get(i.lit) == Sat_solution::L_UNASSIGNED) draw->sol->set(i.lit);
        }
    }
    
    context->temp_u8.size = 0;
    array_printf(&context->temp_u8, "Frame %lld/%lld", draw->anim_frame, draw->anim_indices.size - 1);
        
    if (draw->anim_frame != 0) {
        auto arr = array_subindex(draw->anim_indices, draw->anim_data, draw->anim_frame-1);
        s64 n_lits = arr.size;
        s64 n_conflicts = 0;
        for (Sat_propagation i: arr) n_conflicts += not draw->sol[i.lit];
        array_printf(&context->temp_u8, " (props: %lld, conflicts: %lld)", n_lits, n_conflicts);
    }

    Vec2 p = pos;
    font_draw_string(&context->fonts, context->font_sans, context->temp_u8, p, Palette::BLACK, nullptr, &p.y); 
    p.y += draw->pad;

    factorio_solution_draw(context, &draw->fsol, p, &p.x);

    p.x += draw->pad;
    p.y = pos.y;

    if (draw->anim_frame != 0) {
        auto arr = array_subindex(draw->anim_indices, draw->anim_data, draw->anim_frame-1);
        factorio_solution_draw_props(context, draw, p, size - (p - pos));
    }
}

struct Factorio_solver_state {
    Sat_instance inst;
    Sat_dimacs dimacs;
    pid_t solver_pid;
    int output_fd;
    u64 next_check;
    Array_dyn<u8> output_data;
    Array_dyn<u32> codepoint_temp;
    Array_dyn<s32> lines;

    bool show_solution;
    Sat_solution sol;
    Factorio_solution_state fsol;
};

void factorio_solution_init_from_inst(Factorio_solution_draw* draw, Sat_instance* inst, Array_t<u64> sol_lits) {
    draw->inst = *inst;
    for (u64 lit: sol_lits) {
        draw->sol->set(lit);
    }

    draw->do_animation = false;
}

void factorio_solver_init(Factorio_solver_state* solver, Factorio_params p) {
    sat_init(&solver->inst);
    factorio_balancer(&solver->inst, p);

    sat_write_dimacs(&solver->inst, &solver->dimacs);
    
    solver->output_data.size = 0;
    solver->codepoint_temp.size = 0;
    solver->lines.size = 0;
    
    solver->show_solution = false;
    sat_solution_init(&solver->sol);
    factorio_solution_init(&solver->fsol, &solver->inst, &solver->sol);

    // TODO this can probably be removed
    {FILE* f = fopen("dimacs.out", "w");
    fwrite(solver->dimacs.text.data, 1, solver->dimacs.text.size, f);
    fclose(f);}
    
    {Array_dyn<u8> human;
    defer { array_free(&human); };
    sat_write_human(&solver->inst, &human);
    FILE* f = fopen("human.out", "w");
    fwrite(human.data, 1, human.size, f);
    fclose(f);}

    int pipe_solver_to  [2] = {};
    int pipe_solver_from[2] = {};
    if (pipe(pipe_solver_to  )) goto error;
    if (pipe(pipe_solver_from)) goto error;
    
    {pid_t pid = fork();
    if (pid == 0) {
        if (dup2(pipe_solver_to[0],   STDIN_FILENO)  == -1) goto error;
        if (dup2(pipe_solver_from[1], STDOUT_FILENO) == -1) goto error;
        if (dup2(pipe_solver_from[1], STDERR_FILENO) == -1) goto error;
        if (platform_close_try(pipe_solver_to[1])) goto error;
        if (platform_close_try(pipe_solver_from[0])) goto error;
        
        {char* argv[] = {"kissat", nullptr};
        execvp("kissat", argv);
        platform_error_printf("$ while trying to execute solver in child");
        platform_error_print();
        exit(12);}
    } else if (pid != -1) {
        solver->solver_pid = pid;
        solver->output_fd = pipe_solver_from[0];
        solver->next_check = 0;
        
        if (platform_read_all_try(solver->output_fd, &solver->output_data)) goto error2;
        if (platform_write_try(pipe_solver_to[1], solver->dimacs.text)) goto error2;
        if (platform_close_try(pipe_solver_to[1])) goto error2;
        if (platform_close_try(pipe_solver_from[1])) goto error2;
    } else {
        goto error3;
    }}

    return;

  error:
    platform_error_printf("$ while initialising child");
    platform_error_print();
    exit(8);
  error2:
    platform_error_printf("$ while talking to the child");
    platform_error_print();
    exit(9);
  error3:
    platform_error_printf("$ while trying to fork");
    platform_error_print();
    exit(15);
}

void factorio_solver_doinput(Factorio_solver_state* solver, Font_data* fonts, s64 font_line, Gui* gui) {
    GUI_TIMER(gui);
    
    if (platform_now() + 1000000 >= solver->next_check) {
        pollfd pfd {solver->output_fd, POLLIN, 0};
        int code = poll(&pfd, 1, 0);
        if (code > 0) {
            if (pfd.revents & POLLIN) {
                if (platform_read_all_try(solver->output_fd, &solver->output_data)) goto error;
            }
            if (pfd.revents & POLLHUP) {
                if (platform_close_try(solver->output_fd)) goto error;
                if (solver->output_data.size and solver->output_data.back() != '\n') {
                    array_push_back(&solver->output_data, '\n');
                }
                solver->output_fd = -1;
            }
            if (pfd.revents & POLLERR) {
                fprintf(stderr, "Warning: polling returned POLLERR, I do not know how to handle this\n");
            }
            solver->next_check = 0;
        } else if (code == 0) {
            solver->next_check = platform_now() + 100 * 1000 * 1000;
        } else {
            platform_error_printf("$ while trying to call poll");
            platform_error_print();
            exit(10);
        }
    }
    platform_redraw(solver->next_check);

    {s64 last = 0;
    for (s64 i = 0; i < solver->output_data.size; ++i) {
        if (solver->output_data[i] == '\n') {
            if (solver->output_data[last] == 'v') {
                s64 val = 0;
                bool isneg = false;
                for (s64 j = last+2; j <= i; ++j) {
                    u8 c = j < i ? solver->output_data[j] : ' ';
                    if (c == '-') {
                        isneg ^= true;
                    } else if ('0' <= c and c <= '9') {
                        val = 10 * val + (c - '0');
                    } else if (c == ' ') {
                        if (val) {
                            u64 var = solver->dimacs.map_back[val];
                            solver->show_solution = true;
                            solver->sol.set(isneg ? ~var : var);
                            val = 0; isneg = false;
                        }
                    }
                }
            } else {
                auto str = array_subarray(solver->output_data, last, i);
                u32 word = font_word_create_utf8(fonts, font_line, str);
                array_push_back(&solver->lines, word);
            }
            last = i+1;
        }
    }
    if (0 < last and last < solver->output_data.size) {
        memmove(&solver->output_data[0], &solver->output_data[last], solver->output_data.size - last);
    }
    solver->output_data.size -= last;}
    
    return;
  error:
    platform_error_print();
    exit(11);
}

void factorio_solver_draw(Backend* context, Factorio_solver_state* solver, Vec2 pos, Vec2 size, Vec2 pad) {
    GUI_TIMER(&context->gui);

    if (solver->output_fd != -1) {
        factorio_solver_doinput(solver, &context->fonts, context->font_sans, &context->gui);
    }
    auto font_sans = font_instance_get(&context->fonts, context->font_sans);

    Vec2 p = pos;
    
    if (solver->show_solution) {
        factorio_solution_draw(context, &solver->fsol, p, &p.x);
        size.x -= p.x - pos.x;
    }

    float total_height = solver->lines.size * font_sans.newline + 2*pad.y + 50;
    gui_scrollbar_set_height(&context->gui, "solver_draw,scroll"_arr, {}, total_height);
    gui_scrollbar(&context->gui, "solver_draw,scroll"_arr, {}, p, size, 0.f, nullptr, &p.y);

    p += pad;
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
    enum Actions: u8 {
        NOTHING, DRAW_PROPAGATION, RUN_SOLVER
    };
    u8 action = NOTHING;
    s64 instance = -1;
    s64 solution = -1;
};

Factorio_params_result factorio_params_draw(Backend* context, Factorio_params_state* draw, Vec2 p, Vec2 size) {
    GUI_TIMER(&context->gui);
    
    auto font_sans = font_instance_get(&context->fonts, context->font_sans);    

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
        return {Factorio_params_result::DRAW_SOLUTION, draw->current_instance, draw->current_solution};
    } else if (action_run_solver) {
        return {Factorio_params_result::RUN_SOLVER, draw->current_instance};
    }

    gui_scrollbar(&context->gui, "choose_instance,scroll"_arr, {}, p, size, 0.f, &size.x, &p.y);
    float orig_y = p.y;
    p += draw->pad;
    
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
        for (s64 j: i_inst.params.yoff_output) {
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

    p.y += draw->pad;
    gui_scrollbar_set_height(&context->gui, "choose_instance,scroll"_arr, {}, p.y - orig_y);
    
    return {};
}

void factorio_solution_frame_change(Factorio_solution_draw* draw, s64 diff) {
    draw->anim_frame = min(max(draw->anim_frame + diff, 0ll), draw->anim_indices.size-1);
}

struct Factorio_gui {
    enum Factorio_gui_state: u8 {
        INVALID, CHOOSE_PARAMS, RUN_SOLVER, DRAW_PROPAGATION, DRAW_SOLUTION
    };
    u8 state = INVALID;
    
    Factorio_params_state      params;
    Factorio_solution_state    solution;
    Factorio_propagation_state propagation;
    Factorio_solver_state      solver;
};

void factorio_gui_init(Factorio_gui* fgui, Assets* assets) {
    // Initialise instance
    Array_t<u8> fdb_data = asset_get(assets, "factorio_db"_arr);
    factorio_db_parse(&context->factorio_params_draw.fdb, fdb_data);
}

void factorio_gui_draw(Backend* backend, Factorio_gui* fgui) {
    Vec2 screen {(float)backend->screen_w, (float)backend->screen_h};
    float pad = 10.f;
    Vec2 padd = {pad, pad};

    if (fgui->state == Application::CHOOSE_PARAMS) {
        auto result = factorio_params_draw(backend, &fgui->params, {}, screen);
        
        if (result.action == result.NOTHING) {
            goto gui_was_rendered;
            
        } else if (result.action == result.DRAW_PROPAGATION) {
            Factorio_solution* sol = result.solution >= 0 ? &draw->fdb.solutions[result.solution] : nullptr;
            factorio_propagation_init(&fgui->propagation, draw->fdb.instances[result.instance].params, sol);
            
        } else if (result.action == result.RUN_SOLVER) {
            factorio_solver_init(&fgui->solver, draw->fdb.instances[result.instance].params);
        } else {
            assert(false);
        }
        fgui->state = result.action;
    }
    if (fgui->state == Application::RUN_SOLVER) {
        factorio_solver_draw(backend, &fgui->solver, {}, screen, padd);
        goto gui_was_rendered;
    }
    if (fgui->state == Application::DRAW_SOLUTION) {
        fgui->solution.pad = pad;
        factorio_solution_draw(backend, &fgui->solution, padd, screen - 2*padd);
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

void application_render(Application* context) {
    gui_frame_init(&context->backend.gui);
    
    for (Key i: context->input_queue) {
        if (i.type == Key::SPECIAL and i.special == Key::C_QUIT) {
            exit(0);
        } else if (i.type == Key::SPECIAL and i.special == Key::ARROW_R) {
            factorio_solution_frame_change(&context->factorio_solution_draw, 1);
        } else if (i.type == Key::SPECIAL and i.special == Key::ARROW_L) {
            factorio_solution_frame_change(&context->factorio_solution_draw, -1);
        } else if (i.type == Key::SPECIAL and i.special == Key::END) {
            factorio_solution_frame_change(&context->factorio_solution_draw, 1000000);
        } else if (i.type == Key::SPECIAL and i.special == Key::HOME) {
            factorio_solution_frame_change(&context->factorio_solution_draw, -1000000);
        } else if (i.type == Key::SPECIAL and i.special == Key::F4) {
            context->factorio_solution_draw.inst.debug_explain_vars_raw ^= true;
        } else if (i.type == Key::SPECIAL and i.special == Key::F5) {
            context->backend.gui.debug_draw_timers ^= true;
        } else if (i.type == Key::SPECIAL and i.special == Key::ESCAPE) {
            context->state = Application::CHOOSE_PARAMS;
        } else {
            gui_process_input(&context->backend.gui, i);
        }
    }
    context->input_queue.size = 0;
    
    context->backend.shapes.z_level_add = 0.1f;
    factorio_gui(context);
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

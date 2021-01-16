
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


struct Factorio_solution_draw {
    Sat_instance inst;
    Sat_solution sol;
    
    Array_dyn<s64> anim_indices;
    Array_dyn<Sat_propagation> anim_data;
    s64 anim_frame;
    
    bool sol_detail;
    s64 sol_detail_x, sol_detail_y;

    float pad = 5.f;

    Array_dyn<u64> lit_temp;
};

struct Factorio_params_draw {    
    Factorio_db fdb;
    s64 current_instance = -1;
    s64 current_solution = -1;
};

struct Factorio_solver_draw {
    Sat_instance inst;
    Sat_dimacs dimacs;
    pid_t solver_pid;
    int output_fd;
    u64 next_check;
    Array_dyn<u8> output_data;
    Array_dyn<u32> codepoint_temp;
    Array_dyn<s32> lines;
    Array_dyn<s64> output_lits;
};

// Keeps the necessary data to manage OpenGL and other data for the uil layer
struct Application {
    // Data required by the platform layer
    Array_dyn<Key> input_queue;

    // Other data
    Asset_store assets;
    bool asset_is_pack = false;
    
    Shader circle, spline;

    s64 screen_w = 0, screen_h = 0;
    float scale = 1.f; // Ratio of world-coordinates and pixels (world * scale = pixels)

    Font_data fonts;
    s64 font_sans_base, font_sans;
    s64 font_solution;
    Array_dyn<u8> string_temp;

    Gui gui;
    Shape_drawer shapes;

    enum Application_state: u8 {
        CHOOSE_PARAMS = 0, RUN_SOLVER, DRAW_SOLUTION
    };
    u8 state = CHOOSE_PARAMS;
    
    Factorio_params_draw factorio_params_draw;
    Factorio_solution_draw factorio_solution_draw;
    Factorio_solver_draw factorio_solver_draw;
};

void factorio_solution_draw_image(Application* context, Factorio_solution_draw* draw, Vec2 p, float* x_out, float* y_out) {
    float border_size = 16;
    float border_pad = 4;
    float bar_w = 8;
    float arrow_fac = 0.8f;
    font_instance_scale(&context->fonts, context->font_solution, border_size);

    s64 nx = hashmap_get(&draw->inst.params, Factorio::fpar_nx);
    s64 ny = hashmap_get(&draw->inst.params, Factorio::fpar_ny);
    s64 n_lines = hashmap_get(&draw->inst.params, Factorio::fpar_n_lines);
    s64 n_linedim = hashmap_get(&draw->inst.params, Factorio::fpar_n_linedim);
    
    float center_size = bar_w * n_lines;
    float size = (border_size + border_pad) * 2 + center_size;

    for (s64 yi = -1; yi <= ny; ++yi) {
        for (s64 xi = -1; xi <= nx; ++xi) {
            u32 click_flags = gui_pointable(&context->gui, "draw_solution"_arr, {xi, yi});
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

            u32 click_flags = gui_pointable(&context->gui, "draw_solution"_arr, {xi, yi}, pi, {size, size}, 0.2f);
            bool is_detail = draw->sol_detail and draw->sol_detail_x == xi and draw->sol_detail_y == yi;
            bool is_active = (click_flags & Gui::DRAW_ACTIVE);
            
            bool flags[] = {draw->sol.istrue(f.empty), draw->sol.istrue(f.belt),
                            draw->sol.istrue(f.split), draw->sol.istrue(f.under)};
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
            shape_rectangle(&context->shapes, pi, {size, size}, bg_fill, 0.2f);

            if (is_detail) {
                Color c = lerp(Palette::WHITE, Palette::BLACK, .7f);
                shape_rectangle(&context->shapes, pi-1, {size+2, 1}, c);
                shape_rectangle(&context->shapes, pi-1, {1, size+2}, c);
                shape_rectangle(&context->shapes, pi + Vec2{-1, size}, {size+2, 1}, c);
                shape_rectangle(&context->shapes, pi + Vec2{size, -1}, {1, size+2}, c);
            }
            
            s64 dir_inp = -1;
            s64 dir_out = -1;

            for (Dir d: f.dirs) {
                u8 dd = d.dir - Dir::BEG;
                float arr[4] = {0, 1, 0, -1};
                Vec2 v1 = {arr[ dd       ], arr[(dd+1) & 3]};
                Vec2 v2 = {arr[(dd+1) & 3], arr[(dd+2) & 3]};

                Vec2 dp = pi + size/2 + v1 * (size - border_size - border_pad) / 2;

                bool inp = draw->sol.istrue(d.inp);
                bool out = draw->sol.istrue(d.out);
                bool sid = draw->sol.istrue(d.sid);
                bool iserr = inp + out + sid >= 2;
                Color fill = iserr ? Palette::RED : Palette::BLACK;

                if (inp) shape_arrowhead(&context->shapes, dp, -v1 * border_size/2 * arrow_fac, fill);
                if (out) shape_arrowhead(&context->shapes, dp,  v1 * border_size/2 * arrow_fac, fill);
                if (sid) {
                    Color fill2 = iserr ? Palette::RED : lerp(Palette::BLUE, Palette::BGBLUE, 0.3f);
                    float thickness = border_size * 0.15f;
                    Vec2 w = v2 * (0.5f * (size + center_size) + thickness) + v1 * thickness;
                    shape_rectangle(&context->shapes, dp - w / 2.f, w, fill2);
                }

                if (out) dir_out = dd;
                if (inp) dir_inp = dd;
            }

            bool splitl = draw->sol.istrue(f.splitl);
            bool splitr = draw->sol.istrue(f.splitr);
            Color fill = splitl + splitr >= 2 ? Palette::RED : Palette::GREEN;

            bool draw_split_indicator[4] = {};
            if (splitl) {
                if (dir_out != -1) {
                    draw_split_indicator[(dir_out+1) & 3] = true;
                } else if (dir_inp != -1) {
                    draw_split_indicator[(dir_inp+3) & 3] = true;
                } else {
                    float w;
                    font_metrics_codepoint_get(&context->fonts, context->font_solution, 'L', &w);
                    Vec2 tp = pi + Vec2 {(border_size + border_pad - w) / 2, size - border_pad};
                    font_draw_codepoint(&context->fonts, context->font_solution, 'L', tp, fill);
                }
            }
            if (splitr) {
                if (dir_out != -1) {
                    draw_split_indicator[(dir_out+3) & 3] = true;
                } else if (dir_inp != -1) {
                    draw_split_indicator[(dir_inp+1) & 3] = true;
                } else {
                    float w;
                    font_metrics_codepoint_get(&context->fonts, context->font_solution, 'R', &w);
                    Vec2 tp = pi + size - Vec2 {(border_size + border_pad + w) / 2, border_pad};
                    font_draw_codepoint(&context->fonts, context->font_solution, 'R', tp, fill);
                }
            }

            for (u8 dd = 0; dd < 4; ++dd) {
                if (not draw_split_indicator[dd]) continue;
                float arr[4] = {0, 1, 0, -1};
                Vec2 v1 = {arr[ dd       ], arr[(dd+1) & 3]};
                Vec2 v2 = {arr[(dd+1) & 3], arr[(dd+2) & 3]};
                Vec2 dpp = pi + size/2 + v1 * size/2;

                float size = border_size * 0.6f;
                shape_rectangle(&context->shapes, dpp - (v1 + v2)*size, v1*size + 2*v2*size, fill);
            }

            for (s64 i = 0; i < n_lines; ++i) {
                u64 line = i <  n_linedim ? f.line_first | (i << 8) :
                           i == n_linedim ? f.line_sum :
                                            f.line_block;
                draw->lit_temp.size = 0;
                auto arr = sat_expand(&draw->inst, line, &draw->lit_temp);
                for (s64 j = 0; j < arr.size; ++j) {
                    Color col;
                    switch (draw->sol.get(arr[j])) {
                    case Sat_solution::L_UNASSIGNED: col = Palette::GREY; break;
                    case Sat_solution::L_FALSE:      col = Palette::BLUELIGHT; break;
                    case Sat_solution::L_TRUE:       col = Palette::BLUE; break;
                    default: assert(false);
                    }

                    Vec2 v = pi + border_size + border_pad;
                    v.x += i * bar_w;
                    v.y += center_size * (arr.size-1 - j) / arr.size;

                    shape_rectangle(&context->shapes, v, {bar_w - .5f, center_size / arr.size - .5f}, col);
                }
            }
        }
    }

    if (x_out) *x_out = p.x + (nx + 2) * size;
    if (y_out) *y_out = p.y + (ny + 2) * size;
}

void factorio_solution_draw_detail(Application* context, Factorio_solution_draw* draw, Vec2 p) {
    s64 n_linedim = hashmap_get(&draw->inst.params, Factorio::fpar_n_linedim);
    bool do_block = hashmap_get(&draw->inst.params, Factorio::fpar_do_block);

    auto font_sans = font_instance_get(&context->fonts, context->font_sans);

    s64 xi = draw->sol_detail_x;
    s64 yi = draw->sol_detail_y;
    Field f {xi, yi};

    context->string_temp.size = 0;
    array_printf(&context->string_temp, "Field (%lld,%lld):", xi, yi);
    font_draw_string(&context->fonts, context->font_sans, context->string_temp, p, Palette::BLACK, nullptr, &p.y);
    p.y += font_sans.newline * .5f;

    float base_x = p.x;

    u32 chars_[3] = {0x2013, '0', '1'};
    Array_t<u32> chars {chars_, 3};
    
    float chars_w = 0;
    for (u32 c: chars) {
        float f;
        font_metrics_codepoint_get(&context->fonts, context->font_sans, c, &f);
        if (chars_w < f) chars_w = f;
    }

    auto do_field = [&](Array_t<u8> name, u64 lit, bool newline = false) {
        font_draw_string(&context->fonts, context->font_sans, name,     p, Palette::BLACK, &p.x);
        p.x += font_sans.space * 0.5f;
        font_draw_string(&context->fonts, context->font_sans, "="_arr,  p, Palette::BLACK, &p.x);
        p.x += font_sans.space * 0.5f;
        
        u32 c = chars[draw->sol.get(lit)];
        font_draw_codepoint(&context->fonts, context->font_sans, c, p, Palette::BLACK);
        p.x += chars_w;
        
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
        font_metrics_string_get(&context->fonts, context->font_sans, dirtitle[d], &f);
        if (dirtitle_w < f) dirtitle_w = f;
    }        
    for (u8 d = 0; d < 4; ++d) {
        font_draw_string(&context->fonts, context->font_sans, dirtitle[d], p, Palette::BLACK);
        p.x += dirtitle_w + font_sans.space;
        do_field("inp"_arr, f.dirs[d].inp);
        do_field("out"_arr, f.dirs[d].out);
        do_field("sid"_arr, f.dirs[d].sid);
        do_field("und"_arr, f.dirs[d].und, true);
    }
    
    p.y += font_sans.newline * 0.2f;
    
    context->string_temp.size = 0;
    array_printf(&context->string_temp, "line[%lld]", n_linedim - 1);
    float line_w;
    font_metrics_string_get(&context->fonts, context->font_sans, context->string_temp, &line_w);

    auto do_line = [&](Array_t<u8> name, u64 line) {
        font_draw_string(&context->fonts, context->font_sans, name,    p, Palette::BLACK);
        p.x += line_w + font_sans.space * 0.5f;
        font_draw_string(&context->fonts, context->font_sans, "="_arr, p, Palette::BLACK, &p.x);
        p.x += font_sans.space * 0.5f;

        draw->lit_temp.size = 0;
        for (u64 lit: sat_expand(&draw->inst, line, &draw->lit_temp)) {
            u32 c = chars[draw->sol.get(lit)];
            font_draw_codepoint(&context->fonts, context->font_sans, c, p, Palette::BLACK);
            p.x += chars_w;
        }
        
        p.x = base_x; p.y += font_sans.newline;
    };
    
    for (s64 i = 0; i < n_linedim; ++i) {
        context->string_temp.size = 0;
        array_printf(&context->string_temp, "line[%lld]", i);
        do_line(context->string_temp, f.line_first | (i << 8));
    }
    do_line("sum"_arr,   f.line_sum);
    if (do_block) {
        do_line("block"_arr, f.line_block);
    }
}

void factorio_solution_draw_props(Application* context, Factorio_solution_draw* draw, Vec2 pos, Vec2 size) {
    if (draw->anim_frame == 0) return;

    auto font_sans = font_instance_get(&context->fonts, context->font_sans);
    Vec2 posmax = pos + size;
    
    auto arr = array_subindex(draw->anim_indices, draw->anim_data, draw->anim_frame-1);

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
        
        context->string_temp.size = 0;
        sat_explain(&draw->inst, i.lit, &context->string_temp);

        Vec2 rect_p = {pos.x, pos.y};
        Vec2 rect_size = {0.f, font_sans.newline};
        font_metrics_string_get(&context->fonts, context->font_sans, context->string_temp, &rect_size.x);

        u32 flags = gui_pointable(&context->gui, "lit_detail"_arr, {(s64)i.lit, i.clause}, rect_p, rect_size, 0.1f);
        if (flags & Gui::EVENT_CLICKED) {
            auto* r = gui_pointable_get(&context->gui, "lit_detail"_arr, {(s64)i.lit, i.clause});
            r->flags ^= Gui::MOD_USER1;
            flags = r->flags;
        }
        bool is_expanded = flags & Gui::MOD_USER1;

        Color c = is_expanded ? Palette::RED : base;
        c = flags & Gui::DRAW_ACTIVE ? lerp(c, Palette::WHITE, 0.5f) : c;
        float y_prev = pos.y;
        {float x;
        font_draw_string(&context->fonts, context->font_sans, context->string_temp, pos, c, &x, &pos.y);
        if (next_x < x) next_x = x;}

        if (not draw->sol[i.lit]) {
            shape_rectangle(&context->shapes, rect_p, rect_size, Palette::BGRED);
        }

        if (is_expanded) {
            u64 constraint = draw->inst.clause_constraint[i.clause];
            while (constraint != -1) {
                context->string_temp.size = 0;
                array_printf(&context->string_temp, u8"↳ ");
                sat_explain_constraint(&draw->inst, constraint, &context->string_temp);
                {float x;
                font_draw_string(&context->fonts, context->font_sans, context->string_temp, pos, Palette::BLACK, &x, &pos.y);
                if (next_x < x) next_x = x;}

                if (pos.y >= posmax.y) {
                    pos = Vec2 {next_x + draw->pad, orig.y};
                }

                constraint = draw->inst.constraint_parent[constraint];
            }
        }
        if (flags & Gui::EVENT_CLICKED_R) {
            context->string_temp.size = 0;
            array_printf(&context->string_temp, "0x%llx", i.lit);
            platform_clipboard_set(Platform_clipboard::MIDDLE_BUTTON, context->string_temp);
        }

        if (pos.y + font_sans.height >= posmax.y) {
            pos = Vec2 {next_x + draw->pad, orig.y};
        }
    }
}

void factorio_solution_draw(Application* context, Factorio_solution_draw* draw, Vec2 pos, Vec2 size) {
    hashmap_clear(&draw->sol.values);
    {auto arr = array_subarray(draw->anim_data, 0, draw->anim_indices[draw->anim_frame]);
    for (Sat_propagation i: arr) draw->sol.set(i.lit);}
    
    float next_x;
    {Vec2 p = pos;
    
    context->string_temp.size = 0;
    array_printf(&context->string_temp, "Frame %lld/%lld", draw->anim_frame, draw->anim_indices.size - 1);
    font_draw_string(&context->fonts, context->font_sans, context->string_temp, p, Palette::BLACK, nullptr, &p.y); 
    p.y += draw->pad;

    factorio_solution_draw_image(context, draw, p, &next_x, &p.y);

    if (draw->sol_detail) {
        p.y += draw->pad;
        factorio_solution_draw_detail(context, draw, p);
    }}

    Vec2 p {next_x, pos.y};
    p.x += draw->pad;
    factorio_solution_draw_props(context, draw, p, size - (p - pos));
}

void factorio_solution_init(Factorio_solution_draw* draw, Factorio_params p, Factorio_solution* opt_sol) {
    sat_init(&draw->inst);
    //inst.debug_forbidden_id = 0x2880118012300000ull;

    factorio_balancer(&draw->inst, p);
    if (opt_sol) {
        factorio_clauses_from_diagram(&draw->inst, opt_sol->ascii_diagram);
    }

    draw->sol = sat_solution_from_instance(&draw->inst, &draw->anim_indices, &draw->anim_data);
    draw->anim_frame = 0;

    Array_dyn<u8> text_temp;
    defer { array_free(&text_temp); };
    for (s64 i = 0; i+1 < draw->anim_indices.size; ++i) {
        auto arr = array_subindex(draw->anim_indices, draw->anim_data, i);
        std::sort(arr.begin(), arr.end(), [draw, &text_temp](Sat_propagation a, Sat_propagation b) {
            text_temp.size = 0;
            sat_explain(&draw->inst, a.lit, &text_temp);
            s64 index = text_temp.size;
            sat_explain(&draw->inst, b.lit, &text_temp);
            return array_cmp(array_subarray(text_temp, 0, index), array_subarray(text_temp, index)) < 0;
        });
    }
    
    {Array_dyn<u8> human;
    defer { array_free(&human); };
    sat_write_human(&draw->inst, &human);
    FILE* f = fopen("human.out", "w");
    fwrite(human.data, 1, human.size, f);
    fclose(f);}
}

void factorio_solver_init(Factorio_solver_draw* draw, Factorio_params p) {
    sat_init(&draw->inst);
    factorio_balancer(&draw->inst, p);

    sat_write_dimacs(&draw->inst, &draw->dimacs);

    {FILE* f = fopen("dimacs.out", "w");
    fwrite(draw->dimacs.text.data, 1, draw->dimacs.text.size, f);
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
        draw->solver_pid = pid;
        draw->output_fd = pipe_solver_from[0];
        draw->next_check = 0;
        
        if (platform_read_all_try(draw->output_fd, &draw->output_data)) goto error2;
        if (platform_write_try(pipe_solver_to[1], draw->dimacs.text)) goto error2;
        if (platform_close_try(pipe_solver_to[1])) goto error2;
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

void factorio_db_choose_params_draw(Application* context, Factorio_params_draw* draw, Sat_instance* into_inst, Vec2 p) {
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
        s64 i = draw->current_instance;
        Factorio_solution* sol = draw->current_solution >= 0 ? &draw->fdb.solutions[i] : nullptr;
        factorio_solution_init(&context->factorio_solution_draw, draw->fdb.instances[i].params, sol);
        context->state = Application::DRAW_SOLUTION;
        return;
    }
    if (action_run_solver) {
        s64 i = draw->current_instance;
        factorio_solver_init(&context->factorio_solver_draw, draw->fdb.instances[i].params);
        context->state = Application::RUN_SOLVER;
        return;
    }
    
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

        context->string_temp.size = 0;
        array_printf(&context->string_temp, "nx = %lld, ny = %lld, n_under = %lld, scale_fac = %lld",
                     i_inst.params.nx, i_inst.params.ny, i_inst.params.n_under, i_inst.params.scale_fac);
        font_draw_string(&context->fonts, context->font_sans, context->string_temp, v, Palette::BLUE, &w0, &v.y);
        
        context->string_temp.size = 0;
        array_printf(&context->string_temp, "do_blocking = %d, yoff_output = [", i_inst.params.do_blocking);
        {bool first = true;
        for (s64 j: i_inst.params.yoff_output) {
            if (first) first = false;
            else       array_printf(&context->string_temp, ", ");
            array_printf(&context->string_temp, "%lld", j);
        }}
        array_printf(&context->string_temp, "], yoff_input = [");
        {bool first = true;
        for (s64 j: i_inst.params.yoff_output) {
            if (first) first = false;
            else       array_printf(&context->string_temp, ", ");
            array_printf(&context->string_temp, "%lld", j);
        }}
        array_printf(&context->string_temp, "]");
        font_draw_string(&context->fonts, context->font_sans, context->string_temp, v, Palette::BLUE, &w1, &v.y);
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
}

void factorio_solver_doinput(Factorio_solver_draw* draw) {
    if (platform_now() + 1000000 >= draw->next_check) {
        pollfd pfd {draw->output_fd, POLLIN, 0};
        int code = poll(&pfd, 1, 0);
        if (code > 0) {
            if (pfd.revents & POLLIN) {
                if (platform_read_all_try(draw->output_fd, &draw->output_data)) goto error;
            }
            if (pfd.revents & POLLHUP) {
                if (platform_close_try(draw->output_fd)) goto error;
                if (draw->output_data.size and draw->output_data.back() != '\n') {
                    array_push_back(&draw->output_data, '\n');
                }
                array_printf(&draw->output_data, "Exited.\n");
                draw->output_fd = -1;
            }
            if (pfd.revents & POLLERR) {
                fprintf(stderr, "Warning: polling returned POLLERR, I do not know how to handle this\n");
            }
            draw->next_check = 0;
        } else if (code == 0) {
            draw->next_check = platform_now() + 1000 * 1000 * 1000;
        } else {
            platform_error_printf("$ while trying to call poll");
            platform_error_print();
            exit(10);
        }
    }
    platform_redraw(draw->next_check);

    {s64 last = 0;
    s64 i_out = 0;
    for (s64 i = 0; i < draw->output_data.size; ++i) {
        if (draw->output_data[i] == '\n') {
            if (draw->output_data[last] == 'v') {
                s64 val = 0;
                bool isneg = false;
                for (s64 j = last+2; j < i; ++j) {
                    u8 c = draw->output_data[j];
                    if (c == '-') {
                        isneg ^= true;
                    } else if ('0' <= c and c <= '9') {
                        val = 10 * val + (c - '0');
                    } else if (c == ' ') {
                        if (val) {
                            array_push_back(&draw->output_lits, isneg ? -val : val);
                            val = 0; isneg = false;
                        }
                    }
                }
            } else {
                memcpy(&draw->output_data[i_out], &draw->output_data[last], i+1 - last);
                i_out += i+1 - last;
            }
            last = i+1;
        }
    }
    draw->output_data.size = i_out;}
    
    return;
  error:
    platform_error_print();
    exit(11);
}

void factorio_solver_draw(Application* context, Factorio_solver_draw* draw, Vec2 p) {
    if (draw->output_fd != -1) {
        factorio_solver_doinput(draw);
    }

    {s64 last = 0;
    for (s64 i = 0; i < draw->output_data.size; ++i) {
        if (draw->output_data[i] == '\n') {
            auto str = array_subarray(draw->output_data, last, i);
            u32 word = font_word_create_utf8(&context->fonts, context->font_sans, str);
            array_push_back(&draw->lines, word);
            last = i+1;
        }
    }
    memcpy(draw->output_data.data, draw->output_data.data + last, draw->output_data.size - last);
    draw->output_data.size -= last;}

    auto font_sans = font_instance_get(&context->fonts, context->font_sans);
    p.y += font_sans.ascent;
    for (s64 word: draw->lines) {
        font_draw_word(&context->fonts, word, p, Palette::BLACK);
        p.y += font_sans.newline;
    }
}

void factorio_solution_frame_change(Factorio_solution_draw* draw, s64 diff) {
    draw->anim_frame = min(max(draw->anim_frame + diff, 0ll), draw->anim_indices.size-1);
}

void application_handle_resize(Application* context, s64 width, s64 height) {
    if (width  != -1) context->screen_w = width;
    if (height != -1) context->screen_h = height;

    glViewport(0.0, 0.0, context->screen_w, context->screen_h);
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
    // Initialise instance
    Array_t<u8> fdb_data = asset_get(&context->assets, "factorio_db"_arr);
    factorio_db_parse(&context->factorio_params_draw.fdb, fdb_data);
    
    // Initialise the fonts
    font_init(&context->fonts, &context->assets, GL_TEXTURE2);
    context->font_sans_base = font_add(&context->fonts, asset_get(&context->assets, "font"_arr));
    context->font_sans = font_instantiate(&context->fonts, context->font_sans_base, 15.f);
    font_instantiate_defaults(&context->fonts, context->font_sans_base,
        context->font_sans_base, context->font_sans_base, context->font_sans_base);

    font_instance_scale_defaults(&context->fonts, 0.75f);

    context->font_solution = font_instantiate(&context->fonts, context->font_sans_base);

    // Initialise the shapes and gui
    shape_init(&context->shapes, &context->assets);
    gui_init(&context->gui, &context->assets, &context->fonts, context->font_sans);
    
    
    // Now we load our shaders
    context->spline = opengl_shader_assets(&context->assets, "spline"_arr);
}

void application_render(Application* context) {
    gui_frame_init(&context->gui);
    
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
        } else {
            gui_process_input(&context->gui, i);
        }
    }
    context->input_queue.size = 0;
    
    
    context->shapes.z_level_add = 0.1f;

    Vec2 screen {(float)context->screen_w, (float)context->screen_h};
    float pad = 10.f;
    Vec2 padd = {pad, pad};

    if (context->state == Application::CHOOSE_PARAMS) {
        factorio_db_choose_params_draw(
            context, &context->factorio_params_draw, &context->factorio_solution_draw.inst, padd
        );
    }
    if (context->state == Application::RUN_SOLVER) {
        factorio_solver_draw(context, &context->factorio_solver_draw, padd);
    }
    if (context->state == Application::DRAW_SOLUTION) {
        context->factorio_solution_draw.pad = pad;
        factorio_solution_draw(context, &context->factorio_solution_draw, padd, screen - 2*padd);
    }
    
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    
    opengl_clear_color(Palette::BG);
    glClear(GL_COLOR_BUFFER_BIT);
    glClear(GL_DEPTH_BUFFER_BIT);
    
    shape_frame_draw(&context->shapes, context->screen_w, context->screen_h);
    gui_frame_draw  (&context->gui,    context->screen_w, context->screen_h);
    font_frame_draw (&context->fonts,  context->screen_w, context->screen_h);
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


#define PROGRAM_NAME "cppsat"
#define PROGRAM_TITLE "Belt Balancers"

#ifndef PLATFORM_INCLUDES
#error "You did not compile the right file."
#endif

#include "hashmap.cpp"
#include "asset.cpp"
#include "opengl.cpp"
#include "spline.cpp"
#include "font.cpp"
#include "shapes.cpp"

#include "array_linux.cpp"
#include "cppsat.cpp"


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

    Shape_drawer shapes;

    Sat_instance sat_inst;
    Sat_solution sat_sol;
    Array_dyn<s64> sat_sol_anim_indices;
    Array_dyn<Sat_propagation> sat_sol_anim_data;
    s64 sat_sol_anim_frame;
    bool sat_sol_detail;
    s64 sat_sol_detail_x, sat_sol_detail_y;

    enum Click_rect_flags: u8 {
        CLICK_RECT_CLICKED = 1,
        CLICK_RECT_CLICKED_TOGGLE = 4,
        CLICK_RECT_DEAD = 2,
        CLICK_RECT_CLICKED_RIGHT = 8,
    };
    
    struct Click_rect {
        u64 id;
        Vec2 p, size;
        float z;
        u8 flags;
    };
    Array_dyn<Click_rect> clickable;
};

u8 application_clickable(Application* context, Array_t<u8> name, Array_t<u64> name_args, Vec2 p, Vec2 size, float z) {
    u64 id = hash_u64(hash_str(name)) ^ hash_arr(name_args);
    Application::Click_rect r {id, p, size, z, 0};
    
    for (auto& i: context->clickable) {
        if (i.id == r.id) {
            r.flags = i.flags & ~Application::CLICK_RECT_DEAD;
            i = r;
            return i.flags;
        }
    }
    array_push_back(&context->clickable, r);
    return r.flags;
}

void factorio_draw_solution(Application* context, Sat_solution* sol, Vec2 p, float* x_out, float* y_out) {
    Sat_instance* inst = &context->sat_inst;
    
    float border_size = 16;
    float border_pad = 4;
    float bar_w = 8;
    float arrow_fac = 0.8f;
    font_instance_scale(&context->fonts, context->font_solution, border_size);

    s64 nx = hashmap_get(&inst->params, Sat::fpar_nx);
    s64 ny = hashmap_get(&inst->params, Sat::fpar_ny);
    s64 n_lines = hashmap_get(&inst->params, Sat::fpar_n_lines);
    s64 n_linedim = hashmap_get(&inst->params, Sat::fpar_n_linedim);
    
    float center_size = bar_w * n_lines;
    float size = (border_size + border_pad) * 2 + center_size;

    Array_dyn<u64> lit_temp;
    array_reserve(&lit_temp, 32);
    defer { array_free(&lit_temp); };

    for (s64 yi = -1; yi <= ny; ++yi) {
        for (s64 xi = -1; xi <= nx; ++xi) {
            Vec2 pi = p + size * Vec2 {xi + 1.f, yi + 1.f};

            u8 click_flags = application_clickable(context, "draw_solution"_arr, {(u64)xi, (u64)yi}, pi, {size, size}, 0.2f);
            if (click_flags & Application::CLICK_RECT_CLICKED) {
                if (context->sat_sol_detail and context->sat_sol_detail_x == xi and context->sat_sol_detail_y == yi) {
                    context->sat_sol_detail = false;
                } else {
                    context->sat_sol_detail = true;
                    context->sat_sol_detail_x = xi;
                    context->sat_sol_detail_y = yi;
                }
            }
        }
    }
    
    for (s64 yi = -1; yi <= ny; ++yi) {
        for (s64 xi = -1; xi <= nx; ++xi) {
            Vec2 pi = p + size * Vec2 {xi + 1.f, yi + 1.f};
            Field f {xi, yi};
            
            bool flags[] = {sol->istrue(f.empty), sol->istrue(f.belt),
                            sol->istrue(f.split), sol->istrue(f.under)};
            Color colors[] = {Palette::NONE, Palette::BGBLUE, Palette::BGGREEN, Palette::BGPURPLE};
            s64 flags_size = sizeof(flags) / sizeof(flags[0]);

            bool isset = false;
            Color bg_fill = Palette::WHITE;
            for (s64 i = 0; i < flags_size; ++i) {
                if (not flags[i]) continue;
                if (not isset) {
                    bg_fill = colors[i];
                    isset = true;
                } else {
                    bg_fill = Palette::REDLIGHT;
                }
            }
            shape_rectangle(&context->shapes, pi, {size, size}, bg_fill, 0.2f);

            if (context->sat_sol_detail and context->sat_sol_detail_x == xi and context->sat_sol_detail_y == yi) {
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

                bool inp = sol->istrue(d.inp);
                bool out = sol->istrue(d.out);
                bool sid = sol->istrue(d.sid);
                Color fill = inp + out + sid >= 2 ? Palette::RED : Palette::BLACK;

                if (inp) shape_arrowhead(&context->shapes, dp, -v1 * border_size/2 * arrow_fac, fill);
                if (out) shape_arrowhead(&context->shapes, dp,  v1 * border_size/2 * arrow_fac, fill);
                if (sid) {
                    Vec2 w = v2 * center_size * 0.8f + v1 * border_size * 0.2f;
                    shape_rectangle(&context->shapes, dp - w / 2.f, w, fill);
                }

                if (out) dir_out = dd;
                if (inp) dir_inp = dd;
            }

            bool splitl = sol->istrue(f.splitl);
            bool splitr = sol->istrue(f.splitr);
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
                auto arr = sat_expand(sol->inst, line, &lit_temp);
                for (s64 j = 0; j < arr.size; ++j) {
                    Color col;
                    switch (sol->get(arr[j])) {
                    case Sat_solution::L_UNASSIGNED: col = Palette::GREY; break;
                    case Sat_solution::L_FALSE:      col = Palette::BLUELIGHT; break;
                    case Sat_solution::L_TRUE:       col = Palette::BLUE; break;
                    default: assert(false);
                    }

                    Vec2 v = pi + border_size + border_pad;
                    v.x += i * bar_w;
                    v.y += center_size * (arr.size-1 - j) / n_lines;

                    shape_rectangle(&context->shapes, v, {bar_w, center_size / n_lines - .5f}, col);
                }
            }
        }
    }

    p.y += (ny + 2) * size;

    Array_dyn<u8> string_temp;
    defer { array_free(&string_temp); };

    if (context->sat_sol_detail) {
        auto font_sans = font_instance_get(&context->fonts, context->font_sans);
        p.y += font_sans.ascent + font_sans.newline * 0.5f;

        s64 xi = context->sat_sol_detail_x;
        s64 yi = context->sat_sol_detail_y;
        Field f {xi, yi};

        string_temp.size = 0;
        array_printf(&string_temp, "Field (%lld,%lld):", xi, yi);
        font_draw_string(&context->fonts, context->font_sans, string_temp, p, Palette::BLACK, nullptr, &p.y);
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
            
            u32 c = chars[context->sat_sol.get(lit)];
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
        
        string_temp.size = 0;
        array_printf(&string_temp, "line[%lld]", n_linedim - 1);
        float line_w;
        font_metrics_string_get(&context->fonts, context->font_sans, string_temp, &line_w);

        auto do_line = [&](Array_t<u8> name, u64 line) {
            font_draw_string(&context->fonts, context->font_sans, name,    p, Palette::BLACK);
            p.x += line_w + font_sans.space * 0.5f;
            font_draw_string(&context->fonts, context->font_sans, "="_arr, p, Palette::BLACK, &p.x);
            p.x += font_sans.space * 0.5f;

            lit_temp.size = 0;
            for (u64 lit: sat_expand(context->sat_sol.inst, line, &lit_temp)) {
                u32 c = chars[context->sat_sol.get(lit)];
                font_draw_codepoint(&context->fonts, context->font_sans, c, p, Palette::BLACK);
                p.x += chars_w;
            }
            
            p.x = base_x; p.y += font_sans.newline;
        };
        
        for (s64 i = 0; i < n_linedim; ++i) {
            string_temp.size = 0;
            array_printf(&string_temp, "line[%lld]", i);
            do_line(string_temp, f.line_first | (i << 8));
        }
        do_line("sum"_arr,   f.line_sum);
        do_line("block"_arr, f.line_block);
    }

    if (x_out) *x_out = p.x + (nx + 2) * size;
    if (y_out) *y_out = p.y;
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
    }
    asset_finalize(&context->assets, flag_pack_assets);
}

void application_init(Application* context) {
    // Initialise instance
    Array_dyn<s64> yoff_output, yoff_input;
    array_append(&yoff_output, {0});
    array_append(&yoff_input, {0, 1});
    Factorio_params p {3, 2, 10, 1, false, yoff_output, yoff_input};
    
    sat_init(&context->sat_inst);
    //inst.debug_forbidden_id = 0x2880118012300000ull;

    factorio_balancer(&context->sat_inst, p);
    factorio_clauses_from_diagram(&context->sat_inst,
        " >S> >\n"
        "  S> >"_arr
    );

    context->sat_sol = sat_solution_from_instance(
        &context->sat_inst, &context->sat_sol_anim_indices, &context->sat_sol_anim_data
    );
    context->sat_sol_anim_frame = 0;

    Array_dyn<u8> text_temp;
    defer { array_free(&text_temp); };
    for (s64 i = 0; i+1 < context->sat_sol_anim_indices.size; ++i) {
        auto arr = array_subindex(context->sat_sol_anim_indices, context->sat_sol_anim_data, i);
        std::sort(arr.begin(), arr.end(), [context, &text_temp](Sat_propagation a, Sat_propagation b) {
            text_temp.size = 0;
            sat_explain(context->sat_sol.inst, a.lit, &text_temp);
            s64 index = text_temp.size;
            sat_explain(context->sat_sol.inst, b.lit, &text_temp);
            return array_cmp(array_subarray(text_temp, 0, index), array_subarray(text_temp, index)) < 0;
        });
    }
    
    {Array_dyn<u8> human;
    sat_write_human(&context->sat_inst, &human);
    FILE* f = fopen("human.out", "w");
    fwrite(human.data, 1, human.size, f);
    fclose(f);}
    
    // Initialise the fonts
    font_init(&context->fonts, &context->assets, GL_TEXTURE2);
    context->font_sans_base = font_add(&context->fonts, asset_get(&context->assets, "font"_arr));
    context->font_sans = font_instantiate(&context->fonts, context->font_sans_base, 15.f);
    font_instantiate_defaults(&context->fonts, context->font_sans_base,
        context->font_sans_base, context->font_sans_base, context->font_sans_base);

    font_instance_scale_defaults(&context->fonts, 0.75f);

    context->font_solution = font_instantiate(&context->fonts, context->font_sans_base);

    // Initialise the shapes
    shape_init(&context->shapes, &context->assets);
    
    // Now we load our shaders
    context->spline = opengl_shader_assets(&context->assets, "spline"_arr);
}

void application_render(Application* context) {
    for (auto& r: context->clickable) {
        r.flags |= Application::CLICK_RECT_DEAD;
        r.flags &= ~Application::CLICK_RECT_CLICKED;
    }

    for (Key i: context->input_queue) {
        if (i.type == Key::SPECIAL and i.special == Key::C_QUIT) {
            exit(0);
        } else if (i.type == Key::SPECIAL and i.special == Key::ARROW_R) {
            ++context->sat_sol_anim_frame;
        } else if (i.type == Key::SPECIAL and i.special == Key::ARROW_L) {
            --context->sat_sol_anim_frame;
        } else if (i.type == Key::MOUSE) {
            u8 action; s64 x, y;
            i.get_mouse_param(&action, &x, &y);
            
            float best_z = 1.f;
            Application::Click_rect* best_r = nullptr;
            for (auto& r: context->clickable) {
                if (x < r.p.x or y < r.p.y or x >= r.p.x+r.size.x or y >= r.p.y+r.size.y) continue;
                if (r.z <= best_z) { best_z = r.z; best_r = &r; }
            }

            if (best_r and action == Key::LEFT_DOWN) {
                best_r->flags |= Application::CLICK_RECT_CLICKED;
                best_r->flags ^= Application::CLICK_RECT_CLICKED_TOGGLE;
            } else if (best_r and action == Key::RIGHT_DOWN) {
                best_r->flags |= Application::CLICK_RECT_CLICKED_RIGHT;
            }
        } else if (i.type == Key::SPECIAL and i.special == Key::F4) {
            context->sat_inst.debug_explain_vars_raw ^= true;
        } 
    }
    context->input_queue.size = 0;
    
    context->sat_sol_anim_frame = min(max(context->sat_sol_anim_frame, 0ll), context->sat_sol_anim_indices.size - 1);
    hashmap_clear(&context->sat_sol.values);
    auto arr = array_subarray(context->sat_sol_anim_data, 0,
        context->sat_sol_anim_indices[context->sat_sol_anim_frame]);
    for (Sat_propagation i: arr) context->sat_sol.set(i.lit);
    
    context->shapes.z_level_add = 0.1f;

    Array_dyn<u8> string_temp;
    defer { array_free(&string_temp); };

    float pad = 10.f;
    Vec2 orig = {2*pad, 2*pad};
    float next_x = orig.x;
    auto font_sans = font_instance_get(&context->fonts, context->font_sans);

    {
        Vec2 pos = orig;

        string_temp.size = 0;
        array_printf(&string_temp, "Frame %lld/%lld", context->sat_sol_anim_frame, context->sat_sol_anim_indices.size - 1);
        pos.y += font_sans.ascent;
        font_draw_string(&context->fonts, context->font_sans, string_temp, pos, Palette::BLACK, nullptr, &pos.y); 

        factorio_draw_solution(context, &context->sat_sol, pos, &next_x, &pos.y);
    }
    
    {
        Vec2 pos = {next_x + pad, orig.y + font_sans.ascent};
        if (context->sat_sol_anim_frame > 0) {
            auto arr = array_subindex(context->sat_sol_anim_indices, context->sat_sol_anim_data, context->sat_sol_anim_frame-1);

            u64 only_mask = 0;
            u64 only_val = 0;
            if (context->sat_sol_detail) {
                only_mask = Sat::MASK_SUBTYPE | Field::COORD_MASK;
                only_val = Sat::VAR_FACTORIO | (u64)(context->sat_sol_detail_x + Field::COORD_OFFSET) << 40
                    | (u64)(context->sat_sol_detail_y + Field::COORD_OFFSET) << 24;
                assert((only_val & ~only_mask) == 0);
            }
            
            float next_x = pos.x;
            for (Sat_propagation i: arr) {
                u64 var = i.lit ^ -(i.lit >> 63);
                Color base = (var & only_mask) == only_val ? Palette::BLACK : Palette::GREY;
                
                string_temp.size = 0;
                sat_explain(context->sat_sol.inst, i.lit, &string_temp);

                Vec2 rect_p = {pos.x, pos.y - font_sans.ascent};
                Vec2 rect_size = {0.f, font_sans.height};
                font_metrics_string_get(&context->fonts, context->font_sans, string_temp, &rect_size.x);

                u8 flags = application_clickable(context, "lit_detail"_arr, {i.lit, (u64)i.clause}, rect_p, rect_size, 0.1f);

                Color c = flags & Application::CLICK_RECT_CLICKED_TOGGLE ? Palette::RED : base;

                float y_prev = pos.y;
                {float x;
                font_draw_string(&context->fonts, context->font_sans, string_temp, pos, c, &x, &pos.y);
                if (next_x < x) next_x = x;}

                if (not context->sat_sol[i.lit]) {
                    shape_rectangle(&context->shapes, rect_p, rect_size, Palette::BGRED);
                }

                if (flags & Application::CLICK_RECT_CLICKED_TOGGLE) {
                    u64 constraint = context->sat_sol.inst->clause_constraint[i.clause];
                    while (constraint != -1) {
                        string_temp.size = 0;
                        array_printf(&string_temp, u8"↳ ");
                        sat_explain_constraint(context->sat_sol.inst, constraint, &string_temp);
                        {float x;
                        font_draw_string(&context->fonts, context->font_sans, string_temp, pos, Palette::BLACK, &x, &pos.y);
                        if (next_x < x) next_x = x;}

                        if (pos.y >= context->screen_h - 2*pad) {
                            pos = Vec2 {next_x + pad, orig.y + font_sans.ascent};
                        }

                        constraint = context->sat_sol.inst->constraint_parent[constraint];
                    }
                }
                if (flags & Application::CLICK_RECT_CLICKED_RIGHT) {
                    string_temp.size = 0;
                    array_printf(&string_temp, "0x%llx", i.lit);
                    platform_clipboard_set(Platform_clipboard::MIDDLE_BUTTON, string_temp);
                }

                if (pos.y >= context->screen_h - 2*pad) {
                    pos = Vec2 {next_x + pad, orig.y + font_sans.ascent};
                }
            }
        }
    }

    {s64 i_out = 0;
    for (auto& r: context->clickable) {
        if (r.flags & Application::CLICK_RECT_DEAD) continue;
        context->clickable[i_out++] = r;
    }
    context->clickable.size = i_out;}
    
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    
    opengl_clear_color(Palette::BG);
    glClear(GL_COLOR_BUFFER_BIT);
    glClear(GL_DEPTH_BUFFER_BIT);
    
    shape_frame_draw(&context->shapes, context->screen_w, context->screen_h);
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
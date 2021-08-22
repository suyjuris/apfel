

struct Smoothstep {
    u64 begin = 0, duration = 1000000000ull;
    float value0 = 0.f, value1 = 0.f, speed0 = 0.f, speed1 = 0.f;
};

// Return the coefficients for the polynomial of the smoothstep. a is the constant term. The
// polynomial p is the unique third-degree polynomial with
//     p (0) = step->value0
//     p (1) = step->speed0
//     p'(0) = step->value1
//     p'(1) = step->speed1
void _smoothstep_coeff(Smoothstep* step, float* a, float* b, float* c, float* d) {
    *a = step->value0;
    *b = step->speed0;
    *c = 3.f*(step->value1 - *a) - 2.f*(*b) - step->speed1;
    *d = 2.f*(*a - step->value1) + *b + step->speed1;
}

void smoothstep_add(Smoothstep* step, float add, float vmin, float vmax) {
    u64 now = platform_now();
    if (step->begin == 0) {
        float value1 = min(max(step->value0+add, vmin), vmax);
        if (step->value0 == value1) return;
        
        step->begin = now;
        step->speed0 = 0.f;
        // value0 remains
        step->value1 = value1;
        step->speed1 = 0.f;
    } else {
        float value1 = min(max(step->value1+add, vmin), vmax);
        if (step->value1 == value1) return;

        float x = (float)(now - step->begin) / (float)step->duration;
            
        float a, b, c, d;
        _smoothstep_coeff(step, &a, &b, &c, &d);
            
        step->begin = now;
        step->value0 = ((d*x + c)*x + b)*x + a;
        step->speed0 = (3.f*d*x + 2.f*c)*x + b;
        step->value1 = value1;
        step->speed1 = 0.f;
    }
}

float smoothstep_set(Smoothstep* step, float value, float vmin = -INFINITY, float vmax = INFINITY) {
    float v = min(max(value, vmin), vmax);
    step->begin = 0;
    step->value0 = v;
    step->value1 = v;
    return v;
}

float smoothstep_get(Smoothstep* step, bool* need_redraw = nullptr) {
    if (step->begin == 0) {
        if (need_redraw) *need_redraw = false;
        return step->value0;
    }

    u64 now = platform_now();
    float x;
    if (now >= step->begin + step->duration) {
        step->begin = 0.f;
        step->value0 = step->value1;
        return step->value0;
    } else {
        if (need_redraw) *need_redraw = true;
        
        float x = (float)(now - step->begin) / (float)step->duration;
        float a, b, c, d;
        _smoothstep_coeff(step, &a, &b, &c, &d);
        return ((d*x + c)*x + b)*x + a;
    }

}

struct Scrollbar {
    u64 id;
    Smoothstep step {};
    float total_height = 0;
    u32 flags = 0;
};

struct Gui {
    struct Pointable {
        u64 id;
        Vec2 p, size;
        float z;
        u32 flags;
    };

    enum Gui_flags: u32 {
        DRAW_FOCUSED    = 1,
        DRAW_ACTIVE     = 2,
        DRAW_PRESSED    = 4,
        DRAW_DISABLED   = 8,
        DRAW_BUTTON     = 16,
        DRAW_ENTRY      = 32,
        DRAW_RADIO      = 64,
        DRAW_SCROLL     = 128,
        DRAW_RESIZER    = 256,
        DRAW_AREA       = 512,
        DRAW_COMPACT    = 1024,
        EVENT_CLICKED   = 2048,
        EVENT_CLICKED_R = 4096,
        EVENT_MOTION    = 8192, 
        EVENT_SCROLL    = 16384,
        MOD_BUTTONLIKE  = 32768,
        MOD_DRAGGABLE   = 65536,
        MOD_SCROLLABLE  = 1<<17,
        MOD_USER1       = 1<<18,
        MOD_USER2       = 1<<19,
        _DEAD           = 1<<20,
        MASK_MOD = MOD_BUTTONLIKE | MOD_DRAGGABLE | MOD_SCROLLABLE | MOD_USER1 | MOD_USER2,
        MASK_EVENT = EVENT_CLICKED | EVENT_CLICKED_R | EVENT_MOTION | EVENT_SCROLL,
    };

    Hashmap<Pointable> pointables;
    u64 pointable_drag_id = 0;
    Vec2 pointable_drag_last, pointable_drag_diff;
    s64 pointable_scroll_diff;
    u32 clear_once_flags;

    Array_dyn<Scrollbar> scrollbars;
    float scrollbar_step = 50.f;

    Shader buttonlike;
    float buttonlike_width_max = 40.f;
    float z_level_add = 0.f;

    Shape_drawer* shapes;
    Font_data* fonts;
    s64 font_gui;

    struct Timer {
        Gui* gui;
        s64 index_to_write;
        ~Timer() {
            gui->timer_data[index_to_write].duration += platform_now_real();
        }
    };
    struct Timer_data {
        u64 id, duration;
        Array_t<u8> name;
    };
    Array_dyn<Timer_data> timer_data;
    bool debug_draw_timers = false;

    Array_dyn<u8> str_temp;
    Array_dyn<s64> index_temp;
};

#ifdef ASSET_gui_buttonlike_v
    #version 150 core
    in vec2 pos;
    in vec2 x;
    in float z;
    in float size;
    in vec3 border;
    in vec3 color;
    in vec4 grad;
    in vec2 circle;
    out vec2 v_p;
    out float v_size;
    out vec3 v_border;
    out vec3 v_color;
    out vec4 v_grad;
    out vec2 v_circle;
    uniform vec2 origin;
    uniform vec2 scale;
    void main() {
        gl_Position = vec4((pos - origin)*scale, z, 1);
        v_p = x;
        v_size = size;
        v_border = border;
        v_color = color;
        v_grad = grad;
        v_circle = circle;
    };
#endif
    
#ifdef ASSET_gui_buttonlike_f
    #version 150 core
    in vec2 v_p;
    in float v_size;
    in vec3 v_border;
    in vec3 v_color;
    in vec4 v_grad;
    in vec2 v_circle;
    out vec4 color;
    void main() {
        float dp = v_circle[0];
        float f = pow(abs(v_p.x)/v_size, dp) + pow(abs(v_p.y)/v_size, dp) - 1.0;
        vec2 df = vec2(dp/v_size*pow(abs(v_p.x)/v_size, dp-1.0), dp/v_size*pow(abs(v_p.y)/v_size, dp-1.0));
        float d = f / length(df);
        if (d > v_border[0] + 0.5) discard;
        if (0.0 < d && d < 3.0) {
            vec2 pp = v_p - f * df / dot(df, df) * vec2(sign(v_p.x), sign(v_p.y));
            float f2 = pow(abs(pp.x)/v_size, dp) + pow(abs(pp.y)/v_size, dp) - 1.0;
            vec2 df2 = vec2(dp/v_size*pow(abs(pp.x)/v_size, dp-1.0), dp/v_size*pow(abs(pp.y)/v_size, dp-1.0));
            d += f2 / length(df2);
        }
        
        float t = (v_p.y/v_size+1.0) / 2.0;
        vec3 col1 = v_color * mix(v_border[1], v_border[2], clamp(t, 0.0, 1.0));
        float l = t < v_grad[0] ? mix(v_grad[1], v_grad[2], t / v_grad[0])
                : mix(v_grad[2], v_grad[3], clamp((t - v_grad[0]) / (1.0 - v_grad[0]), 0.0, 1.0));
        vec3 col2 = vec3(l, l, l);
        vec3 col3 = mix(col2, col1, clamp(d+0.5, 0.0, 1.0));
        vec3 col4 = mix(col3*v_circle[1], col3, clamp(length(v_p) - v_size*0.5 + 0.5, 0.0, 1.0));
        float a = 1.0 - max(0.0, d - v_border[0] + 0.5);
        color = vec4(col4, a);
    };
#endif


Gui::Pointable* gui_pointable_get(Gui* gui, Array_t<u8> name, Array_t<s64> name_args) {
    u64 id = hash_u64(hash_str(name)) ^ hash_arr({(u64*)name_args.data, name_args.size});
    auto* p = hashmap_getcreate(&gui->pointables, id, {id, {}, {}, 0.f, 0});
    p->flags &= ~Gui::_DEAD;
    return p;
}

u32 gui_pointable(Gui* gui, Array_t<u8> name, Array_t<s64> name_args, Vec2 p = {}, Vec2 size = {}, float z = 0.f) {
    Gui::Pointable* r = gui_pointable_get(gui, name, name_args);
    r->p = p;
    r->size = size;
    r->z = z;
    return r->flags;
}


#define GUI_CONCAT_(x,y) x##y
#define GUI_CONCAT(x,y) GUI_CONCAT_(x,y)
#define GUI_TIMER(gui) auto GUI_CONCAT(unnamed_timer, __LINE__) {gui_timer((gui), {(u8*)__func__, (s64)sizeof(__func__)-1})};

Gui::Timer gui_timer(Gui* gui, Array_t<u8> name) {
    assert(gui);
    u64 id = hash_str(name);
    for (s64 i = 0; i < gui->timer_data.size; ++i) {
        auto* p = &gui->timer_data[i];
        if (p->id == id) {
            p->duration -= platform_now_real();
            return {gui, i};
        }
    }
    s64 i = gui->timer_data.size;
    array_push_back(&gui->timer_data, {id, -platform_now_real(), name});
    return {gui, i};
}

void gui_draw_timers(Gui* gui, Vec2 p, Vec2 size) {
    float w_val;
    font_metrics_string_get(gui->fonts, gui->font_gui, "999.999ms "_arr, &w_val);

    float w_name = 0.f;
    for (auto i: gui->timer_data) {
        float w;
        font_metrics_string_get(gui->fonts, gui->font_gui, i.name, &w);
        if (w_name < w) w_name = w;
    }

    p.x += size.x - w_name - w_val - font_instance_get(gui->fonts, gui->font_gui).space;
    for (auto i: gui->timer_data) {
        gui->str_temp.size = 0;
        array_printf(&gui->str_temp, "%lld.%03lld ", i.duration / 1000000, (i.duration + 500) / 1000 % 1000);
        float w;
        font_metrics_string_get(gui->fonts, gui->font_gui, gui->str_temp, &w);
        font_draw_string(gui->fonts, gui->font_gui, gui->str_temp, {p.x + w_val - w, p.y});
        font_draw_string(gui->fonts, gui->font_gui, i.name, {p.x + w_val, p.y}, Palette::BLACK, nullptr, &p.y);
    }
}

void gui_frame_init(Gui* gui) {
    gui->index_temp.size = 0;
    for (auto& slot: gui->pointables.slots) {
        if (slot.key == gui->pointables.empty) continue;
        if (~slot.val.flags & Gui::_DEAD) {
            slot.val.flags |= Gui::_DEAD;
            slot.val.flags &= ~Gui::MASK_EVENT;
        } else {
            array_push_back(&gui->index_temp, slot.key);
        }
    }
    for (s64 key: gui->index_temp) {
        hashmap_delete(&gui->pointables, key);
    }

    {s64 i_out = 0;
    for (s64 i = 0; i < gui->scrollbars.size; ++i) {
        Scrollbar r = gui->scrollbars[i];
        if (~r.flags & Gui::_DEAD) {
            r.flags |= Gui::_DEAD;
            gui->scrollbars[i_out++] = r;
        }
    }
    gui->scrollbars.size = i_out;}

    gui->pointable_drag_diff = {};
    gui->pointable_scroll_diff = 0;
    gui->clear_once_flags = 0;
}

void gui_process_input(Gui* gui, Key i) {
    GUI_TIMER(gui);

    if (i.type == Key::MOUSE) {
        u8 action; s64 x, y;
        i.get_mouse_param(&action, &x, &y);
            
        float best_z = 1000.f;
        float best_z_scroll = 1000.f;
        Gui::Pointable* under = nullptr;
        Gui::Pointable* under_scroll = nullptr;
        Gui::Pointable* dragged = nullptr;
        for (auto& slot: gui->pointables.slots) {
            if (slot.key == gui->pointables.empty) continue;
            Gui::Pointable* r = &slot.val;
            r->flags &= Gui::_DEAD | Gui::MASK_MOD | gui->clear_once_flags;
            if (r->id == gui->pointable_drag_id) dragged = r;
            if (x < r->p.x or y < r->p.y or x >= r->p.x+r->size.x or y >= r->p.y+r->size.y) continue;
            if (r->z <= best_z) { best_z = r->z; under = r; }
            if ((r->flags & Gui::MOD_SCROLLABLE) and r->z <= best_z_scroll) { best_z_scroll = r->z; under_scroll = r; }
        }
        gui->clear_once_flags |= Gui::DRAW_ACTIVE;
        
        if (under) under->flags |= Gui::DRAW_ACTIVE;
        
        if (action == Key::LEFT_DOWN) {
            if (under) {
                if (under->flags & (Gui::MOD_BUTTONLIKE | Gui::MOD_DRAGGABLE)) {
                    under->flags |= Gui::DRAW_PRESSED;
                    gui->pointable_drag_id = under->id;
                    gui->pointable_drag_last = {(float)x, (float)y};
                    gui->pointable_drag_diff = {};
                } else {
                    under->flags |= Gui::EVENT_CLICKED;
                }
            }
        } else if (action == Key::LEFT_UP) {
            gui->pointable_drag_id = 0;
            if (under and dragged and under->id == dragged->id) {
                assert(under == dragged);
                dragged->flags &= ~Gui::DRAW_PRESSED;
                dragged->flags |= Gui::EVENT_CLICKED;
            }
        } else if (action == Key::MOTION) {
            if (dragged) {
                bool is_pressed = dragged->flags & Gui::MOD_BUTTONLIKE
                    ? under and under->id == dragged->id : true;
                if (is_pressed) dragged->flags |= Gui::DRAW_PRESSED;

                Vec2 p {(float)x, (float)y};
                gui->pointable_drag_diff += p - gui->pointable_drag_last;
                gui->pointable_drag_last = p;
                dragged->flags |= Gui::EVENT_MOTION;
            }
        } else if (action == Key::RIGHT_DOWN) {
            if (under) {
                under->flags |= Gui::EVENT_CLICKED_R;
            }
        } else if (action == Key::SCROLL_UPWARDS) {
            if (under_scroll) {
                under_scroll->flags |= Gui::EVENT_SCROLL;
                gui->pointable_scroll_diff += 1;
            }
        } else if (action == Key::SCROLL_DOWNWARDS) {
            if (under_scroll) {
                under_scroll->flags |= Gui::EVENT_SCROLL;
                gui->pointable_scroll_diff -= 1;
            }
        }
    }
}

void gui_draw_buttonlike(Gui* gui, Vec2 p, Vec2 size, Vec2 margin, u32 flags, float z) {
    assert(gui);
    z += gui->z_level_add;
    
    // Due to the way the shader is defined we have to offset everything by half a pixel.
    Vec2 inner = size - 2*margin;

    float f = min(min(inner.x, inner.y), gui->buttonlike_width_max);
    bool split_x = f < inner.x, split_y = f < inner.y;
    
    Vec2 p1 = p;
    Vec2 p4 = p1 + inner + 2.f * margin;
    Vec2 p2 = p1 + margin + f/2.f;
    Vec2 p3 = p4 - margin - f/2.f;
    Vec2 s = f * 0.5f + margin;

    s64 vertices;
    if (split_x and not split_y) {
        opengl_buffer_append(&gui->buttonlike, 0, "pos", {
            p1.x, p4.y, p1.x, p4.y, p1.x, p1.y, p2.x, p4.y, p2.x, p1.y, p3.x, p4.y, p3.x, p1.y, p4.x, p4.y, p4.x, p1.y, p4.x, p1.y
        });
        opengl_buffer_append(&gui->buttonlike, 1, "x", {
            -s.x, s.y, -s.x, s.y, -s.x, -s.y, 0.f, s.y, 0.f, -s.y, 0.f, s.y, 0.f, -s.y, s.x, s.y, s.x, -s.y, s.x, -s.y
        });
        vertices = 10;
    } else if (not split_x and split_y) {
        opengl_buffer_append(&gui->buttonlike, 0, "pos", {
            p1.x, p1.y, p1.x, p1.y, p4.x, p1.y, p1.x, p2.y, p4.x, p2.y, p1.x, p3.y, p4.x, p3.y, p1.x, p4.y, p4.x, p4.y, p4.x, p4.y
        });
        opengl_buffer_append(&gui->buttonlike, 1, "x", {
            -s.x, -s.y, -s.x, -s.y, s.x, -s.y, -s.x, 0.f, s.x, 0.f, -s.x, 0.f, s.x, 0.f, -s.x, s.y, s.x, s.y, s.x, s.y
        });
        vertices = 10;
    } else if (not split_x and not split_y) {
        opengl_buffer_append(&gui->buttonlike, 0, "pos", {
            p1.x, p1.y, p1.x, p1.y, p4.x, p1.y, p1.x, p4.y, p4.x, p4.y, p4.x, p4.y
        });
        opengl_buffer_append(&gui->buttonlike, 1, "x", {
            -s.x, -s.y, -s.x, -s.y, s.x, -s.y, -s.x, s.y, s.x, s.y, s.x, s.y
        });
        vertices = 6;
    } else {
        opengl_buffer_append(&gui->buttonlike, 0, "pos", {
            p1.x, p1.y, p1.x, p1.y, p2.x, p1.y, p1.x, p2.y, p2.x, p2.y, p1.x, p3.y, p2.x, p3.y, p1.x, p4.y, p2.x, p4.y, p3.x, p4.y,
            p2.x, p3.y, p3.x, p3.y, p2.x, p2.y, p3.x, p2.y, p2.x, p1.y, p3.x, p1.y, p4.x, p1.y, p3.x, p2.y, p4.x, p2.y, p3.x, p3.y,
            p4.x, p3.y, p3.x, p4.y, p4.x, p4.y, p4.x, p4.y
        });
        opengl_buffer_append(&gui->buttonlike, 1, "x", {
            -s.x, -s.y, -s.x, -s.y, 0.f, -s.y, -s.x, 0.f, 0.f, 0.f, -s.x, 0.f, 0.f, 0.f, -s.x, s.y, 0.f, s.y, 0.f, s.y, 0.f, 0.f,
            0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, -s.y, 0.f, -s.y, s.x, -s.y, 0.f, 0.f, s.x, 0.f, 0.f, 0.f, s.x, 0.f, 0.f, s.y,
            s.x, s.y, s.x, s.y
        });
        vertices = 24;
    }

    float grad_light_t = min(1.f, 15.f / (float)size.y);
    
    float border_normal[] = { 1.1, 0.72, 0.55 };
    float border_active[] = { 1.25, 0.67, 0.5 };
    float border_thick [] = { 1.5, 0.62, 0.45 };
    float border_entry [] = { 1.0, 0.55, 0.72 };
    float border_gray  [] = { 0.97, 0.78, 0.78 };
    float grad_normal  [] = { 0.85, 1.0, 0.92, 0.95 };
    float grad_pressed [] = { 0.15, 0.9, 0.87, 0.95 };
    float grad_light   [] = { grad_light_t, 0.95, 1.0, 1.0 };
    float grad_flat    [] = { 1.0, 0.95, 0.95, 0.95 };
    float color_normal [] = { 1.0, 1.0, 1.0 };
    float color_focused[] = { 1.32, 0.83, 0.28 };
    
    bool inwards = flags & (Gui::DRAW_ENTRY | Gui::DRAW_RADIO);
    float* border = flags & Gui::DRAW_DISABLED ? border_gray :
        flags & Gui::DRAW_ENTRY ? border_entry :
        flags & Gui::DRAW_PRESSED ? border_thick :
        flags & Gui::DRAW_ACTIVE ? border_active :
        flags & Gui::DRAW_RADIO ? border_entry
        : border_normal;
    float* color = flags & Gui::DRAW_FOCUSED ? color_focused : color_normal;
    float* grad = flags & Gui::DRAW_DISABLED ? grad_flat :
        inwards ? grad_light :
        flags & Gui::DRAW_PRESSED ? grad_pressed
        : grad_normal;
    float fsize = inwards ? f * 0.5f - 0.15f : f * 0.5f;
    float finner = ~flags & Gui::DRAW_RADIO ? 1.f :
        ~flags & Gui::DRAW_PRESSED ? 1.f :
        flags & Gui::DRAW_DISABLED ? 0.7f
        : 0.2f;
    float circle = flags & Gui::DRAW_RADIO ? 2.f : 8.f;
    
    for (s64 i = 0; i < vertices; ++i) {
        opengl_buffer_append(&gui->buttonlike, 2, "z",      {z});
        opengl_buffer_append(&gui->buttonlike, 3, "size",   {fsize});
        opengl_buffer_append<float>(&gui->buttonlike, 4, "border", {border, 3});
        opengl_buffer_append<float>(&gui->buttonlike, 5, "color",  {color, 3});
        opengl_buffer_append<float>(&gui->buttonlike, 6, "grad",   {grad, 4});
        opengl_buffer_append(&gui->buttonlike, 7, "circle", {circle, finner});
    }
}

bool gui_button(
    Gui* gui, Array_t<u8> name, Array_t<s64> name_args,
    Vec2 p, Array_t<u8> label, float z, float* out_x = nullptr, float* out_y = nullptr
) {
    auto font_gui = font_instance_get(gui->fonts, gui->font_gui);
    
    float w;
    font_metrics_string_get(gui->fonts, gui->font_gui, label, &w);

    Vec2 margin = {3.f, 3.f};
    Vec2 pad = {12.f, 3.f};
    Color grey1 { 70,  70,  70};
    Color grey2 {140, 140, 140};
    
    Vec2 inner = 2*pad + Vec2 {w, font_gui.height};
    Vec2 size = inner + 2*margin;
    Color c = 0 & Gui::DRAW_DISABLED ? grey2 : grey1;

    auto pt = gui_pointable_get(gui, name, name_args);
    pt->p = p;
    pt->size = size;
    pt->z = z;
    pt->flags |= Gui::MOD_BUTTONLIKE;
    
    gui_draw_buttonlike(gui, p, size, margin, Gui::DRAW_BUTTON | pt->flags, z);
    font_draw_string(gui->fonts, gui->font_gui, label, p + margin + pad, c);

    if (out_x) *out_x = p.x + size.x;
    if (out_y) *out_y = p.y + size.y;
    
    return pt->flags & Gui::EVENT_CLICKED;
}

Scrollbar* gui_scrollbar_get(Gui* gui, Array_t<u8> name, Array_t<s64> name_args) {
    u64 id = hash_u64(hash_str(name)) ^ hash_arr({(u64*)name_args.data, name_args.size});
    for (auto& i: gui->scrollbars) {
        if (i.id == id) {
            i.flags &= ~Gui::_DEAD;
            return &i;
        }
    }
    array_push_back(&gui->scrollbars, Scrollbar {id});
    return &gui->scrollbars.back();
}
    
void gui_scrollbar(
    Gui* gui, Array_t<u8> name, Array_t<s64> name_args,
    Vec2 p, Vec2 size, float z, float* out_w, float* out_y
) {
    Scrollbar* scroll = gui_scrollbar_get(gui, name, name_args);
    scroll->step.duration = 100000000ull;
    
    Gui::Pointable* point  = gui_pointable_get(gui, name, name_args);
    point->flags |= Gui::MOD_DRAGGABLE;

    Gui::Pointable* area  = gui_pointable_get(gui, "scrollbar_area"_arr, {(s64)point->id});
    area->flags |= Gui::MOD_SCROLLABLE;
    area->p = p;
    area->size = size;
    area->z = z + 0.5f;

    float bar_width_base = 6;
    float bar_height_min = 12;
    Color orange {242, 152,  51};
    Color black  {200, 200, 200};
    
    if (size.y >= scroll->total_height) {
        smoothstep_set(&scroll->step, 0.f);
        if (out_w) *out_w = size.x;
        if (out_y) *out_y = p.y;        
    } else {
        Vec2 bar_size = {
            2*bar_width_base,
            max(bar_height_min, size.y * size.y / scroll->total_height)
        };

        float offset_max = scroll->total_height - size.y;
        Color fill = black;
        if (point->flags & Gui::DRAW_PRESSED) fill = orange;

        bool need_redraw;
        float offset = smoothstep_get(&scroll->step, &need_redraw);
        
        if (area->flags & Gui::EVENT_SCROLL) {
            smoothstep_add(&scroll->step, -gui->scrollbar_step * gui->pointable_scroll_diff, 0.f, offset_max);
            need_redraw = true;
        }
        if (point->flags & Gui::EVENT_MOTION) {
            offset += gui->pointable_drag_diff.y * offset_max / (size.y - bar_size.y);
            offset = smoothstep_set(&scroll->step, offset, 0.f, offset_max);
        }
        
        if (need_redraw) platform_redraw(0);
        
        Vec2 bar_p = {
            size.x - bar_size.x,
            offset / offset_max * (size.y - bar_size.y)
        };
        point->p = p + bar_p;
        point->size = bar_size;
        point->z = z;
        
        if (not (point->flags & (Gui::DRAW_ACTIVE | Gui::DRAW_PRESSED))) {
            bar_size.x /= 2.f;
            bar_p.x += bar_size.x;
        }
        shape_rectangle(gui->shapes, p + bar_p, bar_size, fill, z);

        if (out_w) *out_w = size.x - bar_width_base;
        if (out_y) *out_y = p.y - offset;
    }
}

void gui_scrollbar_set_height(Gui* gui, Array_t<u8> name, Array_t<s64> name_args, float height) {
    Scrollbar* scroll = gui_scrollbar_get(gui, name, name_args);
    scroll->total_height = height;
}

bool gui_scrollbar_is_at_bottom(Gui* gui, Array_t<u8> name, Array_t<s64> name_args) {
    Scrollbar* scroll = gui_scrollbar_get(gui, name, name_args);
    Gui::Pointable* point  = gui_pointable_get(gui, name, name_args);
    Gui::Pointable* area  = gui_pointable_get(gui, "scrollbar_area"_arr, {(s64)point->id});

    if (area->size.y >= scroll->total_height) {
        return true;
    } else {
        float offset_max = scroll->total_height - area->size.y;
        return scroll->step.value1 >= offset_max - 1.f;
    }
}

void gui_scrollbar_move_to_bottom(Gui* gui, Array_t<u8> name, Array_t<s64> name_args) {
    Scrollbar* scroll = gui_scrollbar_get(gui, name, name_args);
    Gui::Pointable* point  = gui_pointable_get(gui, name, name_args);
    Gui::Pointable* area  = gui_pointable_get(gui, "scrollbar_area"_arr, {(s64)point->id});
    
    if (area->size.y >= scroll->total_height) {
        // nothing
    } else {
        float offset_max = scroll->total_height - area->size.y;
        smoothstep_add(&scroll->step, offset_max - scroll->step.value1, 0.f, offset_max);
    }
}

void gui_init(Gui* gui, Asset_store* assets, Shape_drawer* shapes, Font_data* fonts, s64 font) {
    gui->shapes = shapes;
    gui->fonts = fonts;
    gui->font_gui = font;
    gui->buttonlike = opengl_shader_assets(assets, "gui_buttonlike"_arr);

    gui->scrollbar_step = 6.f * font_instance_get(fonts, font).newline;
}

void gui_frame_draw(Gui* gui, s64 screen_w, s64 screen_h) {
    if (gui->debug_draw_timers) {
        gui_draw_timers(gui, {}, {(float)screen_w, (float)screen_h});
        for (auto& i: gui->timer_data) {
            i.duration = 0;
        }
    }
    
    opengl_shader_use_and_set_origin(&gui->buttonlike, screen_w, screen_h);
    opengl_shader_draw_and_clear(&gui->buttonlike, GL_TRIANGLE_STRIP);
}

void gui_clear(Gui* gui) {    
    opengl_shader_clear(&gui->buttonlike);
}

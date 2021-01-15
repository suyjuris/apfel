

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
        MOD_BUTTONLIKE  = 8192,
        MOD_USER1       = 16384,
        MOD_USER2       = 32768,
        _DEAD           = 65536,
        MASK_MOD = MOD_BUTTONLIKE | MOD_USER1 | MOD_USER2,
    };

    Array_dyn<Pointable> pointables;
    u64 pointable_drag_id = 0;

    Shader buttonlike;
    float buttonlike_width_max = 40.f;
    float z_level_add = 0.f;

    Font_data* fonts;
    s64 font_gui;
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
    for (auto& i: gui->pointables) {
        if (i.id == id) {
            i.flags &= ~Gui::_DEAD;
            return &i;
        }
    }
    array_push_back(&gui->pointables, {id, {}, {}, 0.f, 0});
    return &gui->pointables.back();
}

u32 gui_pointable(Gui* gui, Array_t<u8> name, Array_t<s64> name_args, Vec2 p = {}, Vec2 size = {}, float z = 0.f) {
    Gui::Pointable* r = gui_pointable_get(gui, name, name_args);
    r->p = p;
    r->size = size;
    r->z = z;
    return r->flags;
}

void gui_frame_init(Gui* gui) {
    s64 i_out = 0;
    for (s64 i = 0; i < gui->pointables.size; ++i) {
        Gui::Pointable r = gui->pointables[i];
        if (~r.flags & Gui::_DEAD) {
            r.flags = (r.flags & Gui::MASK_MOD) | Gui::_DEAD;
            gui->pointables[i_out++] = r;
        }
    }
    gui->pointables.size = i_out;
}

void gui_process_input(Gui* gui, Key i) {
    if (i.type == Key::MOUSE) {
        u8 action; s64 x, y;
        i.get_mouse_param(&action, &x, &y);
            
        float best_z = 1.f;
        Gui::Pointable* under = nullptr;
        Gui::Pointable* dragged = nullptr;
        for (auto& r: gui->pointables) {
            r.flags &= ~Gui::DRAW_ACTIVE;
            if (r.id == gui->pointable_drag_id) dragged = &r;
            if (x < r.p.x or y < r.p.y or x >= r.p.x+r.size.x or y >= r.p.y+r.size.y) continue;
            if (r.z <= best_z) { best_z = r.z; under = &r; }
        }
        
        if (under) {
            under->flags |= Gui::DRAW_ACTIVE;
        }

        
        if (action == Key::LEFT_DOWN) {
            if (under) {
                if (under->flags & Gui::MOD_BUTTONLIKE) {
                    under->flags |= Gui::DRAW_PRESSED;
                    gui->pointable_drag_id = under->id;
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
            if (under and dragged and under->id == dragged->id) {
                assert(under == dragged);
                dragged->flags |= Gui::DRAW_PRESSED;
            }
        } else if (action == Key::RIGHT_DOWN) {
            if (under) {
                under->flags |= Gui::EVENT_CLICKED_R;
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
    Vec2 p, Array_t<u8> label, float z, float* out_x, float* out_y
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

void gui_init(Gui* gui, Asset_store* assets, Font_data* fonts, s64 font) {
    gui->fonts = fonts;
    gui->font_gui = font;
    gui->buttonlike = opengl_shader_assets(assets, "gui_buttonlike"_arr);
}

void gui_frame_draw(Gui* gui, s64 screen_w, s64 screen_h) {
    opengl_shader_use_and_set_origin(&gui->buttonlike, screen_w, screen_h);
    opengl_shader_draw_and_clear(&gui->buttonlike, GL_TRIANGLE_STRIP);
}


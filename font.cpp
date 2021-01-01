
#include <algorithm>

typedef u8  stbtt_uint8;
typedef s8  stbtt_int8;
typedef u16 stbtt_uint16;
typedef s16 stbtt_int16;
typedef u32 stbtt_uint32;
typedef s32 stbtt_int32;
#define STBTT_assert(x) assert(x)
#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"

struct Font_data {
    constexpr static s64 MAGIC_INFOS     = 0x9e7efe9f622a9940ll;
    constexpr static s64 MAGIC_INSTANCES = 0x17919c962f9bcbe3ll;
    constexpr static s64 MAGIC_WORD      = 0x47c8c0a0a95218e1ll;

    struct Font_instance {
        s64 info;
        float scale, ascent, height, newline, space;
    };

    struct Glyph {
        u32 path0_beg, path1_beg;
        u8 type0, type1;
    };
    struct Glyph_handle {
        u16 index;
        u16 advance;
    };
    struct Outline {
        Array_dyn<s64> offsets;
        Array_dyn<Vec2> points;
    };
    struct Path {
        Vec2 p0, p1, p2;
        float sense, y;
    };
    struct Draw_glyph {
        u32 glyph;
        Vec2 pos, scale;
        Color fill;
    };
    struct Stitch_cmd {
        u8 type0, type1;
    };

    enum Default_font_names: s64 {
        FONT_NORMAL = 0 ^ MAGIC_INSTANCES,
        FONT_BOLD   = 1 ^ MAGIC_INSTANCES,
        FONT_ITALIC = 2 ^ MAGIC_INSTANCES,
        FONT_SANS   = 3 ^ MAGIC_INSTANCES,
        FONT_HEADER = 4 ^ MAGIC_INSTANCES,
        FONT_SMALL  = 5 ^ MAGIC_INSTANCES,
        FONT_BUTTON = 6 ^ MAGIC_INSTANCES,
        FONT_DEFAULT_SIZE = 7
    };
    
    Array_dyn<stbtt_fontinfo> infos;
    Array_dyn<Font_instance> instances;
    Shader path, blend;
    u32 path_fbo = 0;
    u32 tex_coverage = 0;
    s64 tex_coverage_w = -1;
    s64 tex_coverage_h = -1;
    u32 texture_unit;
    u32 tex_glyphs = 0;
    u32 tex_glyphs_buf = 0;
    s64 tex_glyphs_size = 0; // @Cleanup document

    Array_t<u8> type_len; // @Cleanup document
    Array_t<s64> type_counts;
    Array_dyn<s64> glyph_counts;
    Array_t<Stitch_cmd> len_to_stitch;
    
    Array_dyn<Glyph> glyphs;
    Array_dyn<Path> paths_data;
    Outline outline_buf;
    
    Array_dyn<Draw_glyph> draw_glyphs;
    Hashmap<Glyph_handle> codepoint_indices;
    Array_t<Glyph_handle> ascii_indices;

    struct Draw_word {
        u32 word;
        Vec2 pos;
        Color fill;
    };

    struct Word {
        u32 path_beg;
        u32 advance;
        s64 font_instance;
    };    

    enum Flags: u64 {
        PARAGRAPH = 1,       // Indicates a paragraph break at the end of the item
        PARAGRAPH_CLOSE = 2, // Same, but with less space
        NEWLINE = 4,
        SPACE = 8,           // Leave a space after this word
        STICKY = 16,         // Do not break up the word at this point
        HEADER = 32,         // Corresponds to <h4>, draw text as title
        BOLD = 64,
        ITALICS = 128,
        SMALL = 256,
        SANS = 512,
        BUTTON = 1024,
        COMPACT = 2048,
        RED = 4096,
        INDENTED = 8192,
        ITEMIZED = 16384,
        GROUP_SPACING = PARAGRAPH | PARAGRAPH_CLOSE | NEWLINE | SPACE | STICKY,
        GROUP_BREAKING = PARAGRAPH | PARAGRAPH_CLOSE | NEWLINE,
        GROUP_DRAWING = INDENTED | RED | ITEMIZED,
    };

    enum Default_slot_names: u64 {
        SLOT_TEMPORARY = 0xffffffffffffffffull // do not use this slot, it is really full usually
    };

    struct Text_box {
        u32 word;
        u64 flags = 0; // Same as the spacing flags in Text_fmt::Flags
        s64 word_beg = 0, word_end = 0; // Start and end offset of the word
    };
    
    struct Text {
        Array_dyn<Text_box> boxes;
        Array_dyn<u8> str;
    };
    
    Array_dyn<Word> words;
    Array_dyn<u8> word_type_count;
    Array_dyn<u32> word_paths;
    Array_dyn<u32> word_offsets;
    Array_dyn<Draw_word> draw_words;
    Hashmap<u32> word_lookup;
    Hashmap<Text> text_slots;
    u64 text_current_flags;
    Array_dyn<u32> buf_codepoints;

    bool default_fonts_are_init = false;
};

#ifdef ASSET_font_path_v
    #version 150 core
    in vec2 pos;
    in vec2 off;
    in vec2 size;
    in int p_base;
    in vec4 fill;
    uniform samplerBuffer p_data;
    uniform int p_length;
    uniform vec2 origin;
    uniform vec2 scale;
    
    out vec2 v_pos;
    out vec2 v_p1;
    out vec2 v_p2;
    out vec2 v_sense;
    out vec4 v_fill;
    
    void main() {
        int index = p_base + gl_InstanceID % p_length;
        vec4 dat0 = texelFetch(p_data, 2*index);
        vec4 dat1 = texelFetch(p_data, 2*index+1);
        vec2 p0 = dat0.xy;
        vec2 p1 = dat0.zw;
        vec2 p2 = dat1.xy;
        vec3 sense;
        sense.x = (dat1.z == 0 || dat1.z == 1) ? -1.0 : 1.0;
        sense.y = (dat1.z == 0 || dat1.z == 2) ? -1.0 : 1.0;
        sense.z = dat1.w;
        
        vec2 sp0  = (p0 * size + off);
        vec2 sp1  = (p1 * size + off);
        vec2 sp2  = (p2 * size + off);
        float sy  = sense.z * size.y + off.y;
        float sy1 = sense.y > 0 ? min(min(sp0.y, sp1.y), sp2.y)
                                : max(max(sp0.y, sp1.y), sp2.y);
        vec2 spos = vec2( mix(sp0.x-0.5, sp2.x+0.5, pos.x),
                          mix(sy, sy1-0.5*sense.y, pos.y) );

        gl_Position = vec4((spos - origin)*scale, vec2(0.05, 1));

        v_pos = spos-sp0;
        v_p1 = sp1-sp0;
        v_p2 = sp2-sp0;

        vec2 w = vec2(sp2.y-sp0.y, sp0.x-sp2.x);
        v_sense.x = sense.x * sign(v_p2.x);
        v_sense.y = sense.y * sign(dot(v_p1, w));

        v_fill = fill;
    }
#endif
    
#ifdef ASSET_font_path_f
    #version 150 core
    in vec2 v_pos;
    in vec2 v_p1;
    in vec2 v_p2;
    in vec2 v_sense;
    in vec4 v_fill;
    out vec4 color;
    
    void main() {
        float scale_p = 1.0;
        
        float pos_l = max(0.0,    v_pos.x-0.5/scale_p);
        float pos_r = min(v_p2.x, v_pos.x+0.5/scale_p);
        float x_fac = max(pos_r - pos_l, 0) * scale_p;
        vec2 w_pos = vec2((pos_l + pos_r) * 0.5 / x_fac, v_pos.y);
        vec2 w_p1 = vec2(v_p1.x / x_fac, v_p1.y);
        vec2 w_p2 = vec2(v_p2.x / x_fac, v_p2.y);
        
        mat2 A = mat2(0.5*w_p2.y, -w_p1.y, -0.5*w_p2.x, w_p1.x) / (w_p1.x*w_p2.y-w_p1.y*w_p2.x);
        mat2 At = mat2(A[0][0], A[1][0], A[0][1], A[1][1]);
        vec2 Apos = A*w_pos;
        vec3 v_v;
        v_v.y = Apos.y;
        v_v.z = Apos.x + Apos.y;
        vec2 v_dg = At*vec2(-2.0*v_v.z, -2.0*v_v.z+1.0);
        v_v.x = dot(At[0]+At[1], v_dg);        
        
        float f = v_v.y-v_v.z*v_v.z;
        float t = clamp(v_v.z-f*v_v.x/dot(v_dg, v_dg), 0, 1);
        vec2 w = vec2(w_p2.y, -w_p2.x);
        float v_e = -(2.0 * dot(w_pos, w) / dot(w_p1, w) - 1.0);
        if (abs(v_e) > 1.0) { f = v_e; t = dot(w_pos, w_p2) / dot(w_p2, w_p2); }
        float d = distance(mix(w_p1*t,mix(w_p1,w_p2,t),t), w_pos);
        float dist = d * sign(f) * v_sense.y;
                
        float d2 = clamp(dist * scale_p + 0.5, 0, 1);
        //float d2 = smoothstep(0, 1, dist * scale_p + 0.5);
        float c = d2 * x_fac * v_sense.x;
        color = vec4(v_fill * c/16.0);
    }
#endif
    
#ifdef ASSET_font_blend_v
    #version 150 core
    in vec2 pos;
    out vec2 v_tpos;
    
    void main() {
        v_tpos = (pos + vec2(1.0, 1.0)) / 2.0;
        gl_Position = vec4(pos, vec2(0.05, 1));
    }
#endif

#ifdef ASSET_font_blend_f
    #version 150 core
    uniform sampler2D sampler;
    in vec2 v_tpos;
    out vec4 color;
    
    void main() {
        color = texture2D(sampler, v_tpos) * 16.0;
    }
#endif

void font_init(Font_data* fonts, Asset_store* assets, u32 texture_unit) {
    fonts->infos.size = 0;
    fonts->instances.size = 0;
    fonts->texture_unit = texture_unit;
    
    fonts->path  = opengl_shader_assets(assets, "font_path"_arr);
    fonts->blend = opengl_shader_assets(assets, "font_blend"_arr);

    fonts->ascii_indices = array_create<Font_data::Glyph_handle>(128);
    array_memset(&fonts->ascii_indices, 0xff);

    {Array_dyn<u8> tmp;
    array_append<u8>(&tmp, {0, 2, 6, 14, 19, 44});
    fonts->type_len = tmp;}
    fonts->type_counts = array_create<s64>(fonts->type_len.size);
    {Array_dyn<Font_data::Stitch_cmd> tmp;
    array_append(&tmp, {
        {0,0},{1,0},{1,0},{1,1},{1,1},{2,0},{2,0},{1,2},{1,2},{2,2},{2,2},{2,2},{2,2},
        {3,0},{3,0},{1,3},{1,3},{4,0},{4,0},{4,0},{2,3},{1,4},{2,4},{2,4},{2,4},{2,4},
        {3,3},{3,3},{3,3},{3,4},{3,4},{3,4},{3,4},{3,4},{4,4},{4,4},{4,4},{4,4},{4,4},
        {5,0},{5,0},{5,0},{5,0},{5,0},{5,0},{1,5},{1,5},{2,5},{2,5},{2,5},{2,5},{3,5},
        {3,5},{3,5},{3,5},{3,5},{3,5},{3,5},{3,5},{4,5},{4,5},{4,5},{4,5},{4,5},{5,5},
        {5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},
        {5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},
        {5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},{5,5},
        {5,5},{5,5},{5,5},{5,5},{5,5}
    });
    fonts->len_to_stitch = tmp;}

    fonts->instances.size = 0;
    array_append_zero(&fonts->instances, Font_data::FONT_DEFAULT_SIZE);

    glGenFramebuffers(1, &fonts->path_fbo);
    glBindFramebuffer(GL_FRAMEBUFFER, fonts->path_fbo);
    glGenTextures(1, &fonts->tex_coverage);
    glGenTextures(1, &fonts->tex_glyphs);
    glGenBuffers(1, &fonts->tex_glyphs_buf);
    
    glActiveTexture(fonts->texture_unit);
    glBindTexture(GL_TEXTURE_2D, fonts->tex_coverage);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, fonts->tex_coverage, 0);
    glDrawBuffer(GL_COLOR_ATTACHMENT0);

    glActiveTexture(fonts->texture_unit + 1);
    glBindBuffer(GL_TEXTURE_BUFFER, fonts->tex_glyphs_buf);
    glBufferData(GL_TEXTURE_BUFFER, 65536 * 16, nullptr, GL_DYNAMIC_DRAW);
    glBindTexture(GL_TEXTURE_BUFFER, fonts->tex_glyphs);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_RGBA32F, fonts->tex_glyphs_buf);
}

s64 font_add(Font_data* fonts, Array_t<u8> data) {
    s64 index = fonts->infos.size;
    array_append_zero(&fonts->infos, 1);
    int code = stbtt_InitFont(&fonts->infos[index], data.data, 0);
    if (code == 0) {
        fprintf(stderr, "Error: Could not parse font data\n");
        exit(101);
    }
    return index ^ Font_data::MAGIC_INFOS;
}

void _font_instance_init(Font_data* fonts, s64 font_inst, s64 font_info, float size) {
    auto inst = &fonts->instances[font_inst];
    if (font_info != -1) {
        inst->info = font_info;
    }
    
    auto info = &fonts->infos[inst->info];

    inst->scale = stbtt_ScaleForPixelHeight(info, size);
    
    {int ascent, descent, linegap;
    stbtt_GetFontVMetrics(info, &ascent, &descent, &linegap);
    inst->ascent = (float)ascent * inst->scale;
    inst->height = (float)(ascent - descent) * inst->scale;
    inst->newline = (float)(ascent - descent + linegap) * inst->scale;}

    {int advance;
    stbtt_GetCodepointHMetrics(info, ' ', &advance, nullptr);
    inst->space = (float)advance * inst->scale;}
}

void font_instance_scale(Font_data* fonts, s64 font_inst, float size) {
    _font_instance_init(fonts, font_inst ^ Font_data::MAGIC_INSTANCES, -1, size);
}

s64 font_instantiate(Font_data* fonts, s64 font_info, float size) {
    s64 font_inst = fonts->instances.size;
    array_append_zero(&fonts->instances, 1);
    _font_instance_init(fonts, font_inst, font_info ^ Font_data::MAGIC_INFOS, size);
    return font_inst ^ Font_data::MAGIC_INSTANCES;
}

void font_instantiate_defaults(Font_data* fonts, s64 font_normal, s64 font_italic, s64 font_bold, s64 font_sans) {
    s64 x = Font_data::MAGIC_INSTANCES;
    s64 y = Font_data::MAGIC_INFOS;

    _font_instance_init(fonts, Font_data::FONT_NORMAL ^ x, font_normal ^ y, 20.f);
    _font_instance_init(fonts, Font_data::FONT_BOLD   ^ x, font_bold   ^ y, 20.f);
    _font_instance_init(fonts, Font_data::FONT_ITALIC ^ x, font_italic ^ y, 20.f);
    _font_instance_init(fonts, Font_data::FONT_SANS   ^ x, font_sans   ^ y, 20.f);
    _font_instance_init(fonts, Font_data::FONT_HEADER ^ x, font_bold   ^ y, 26.f);
    _font_instance_init(fonts, Font_data::FONT_SMALL  ^ x, font_normal ^ y, 15.f);
    _font_instance_init(fonts, Font_data::FONT_BUTTON ^ x, font_sans   ^ y, 16.7f);
}

void font_instance_scale_defaults(Font_data* fonts, float scale) {
    font_instance_scale(fonts, Font_data::FONT_NORMAL, scale * 20.f);
    font_instance_scale(fonts, Font_data::FONT_BOLD,   scale * 20.f);
    font_instance_scale(fonts, Font_data::FONT_ITALIC, scale * 20.f);
    font_instance_scale(fonts, Font_data::FONT_SANS,   scale * 20.f);
    font_instance_scale(fonts, Font_data::FONT_HEADER, scale * 26.f);
    font_instance_scale(fonts, Font_data::FONT_SMALL,  scale * 15.f);
    font_instance_scale(fonts, Font_data::FONT_BUTTON, scale * 16.7f);
}

void _font_outline_load(stbtt_fontinfo* font_info, int codepoint, Font_data::Outline* outline) {
    outline->offsets.size = 0;
    outline->points.size = 0;
    
    Array_t<stbtt_vertex> vertices;
    vertices.size = stbtt_GetCodepointShape(font_info, codepoint, &vertices.data);
    defer { stbtt_FreeShape(font_info, vertices.data); };

    Vec2 p;
    for (auto i: vertices) {
        Vec2 ip = {(float)i.x, (float)i.y};
        Vec2 ic = {(float)i.cx, (float)i.cy};
        
        if (i.type == STBTT_vmove) {
            array_push_back(&outline->offsets, outline->points.size);
            array_push_back(&outline->points, ip);
        } else if (i.type == STBTT_vline) {
            array_append(&outline->points, {(p+ip) / 2.f, ip});
        } else if (i.type == STBTT_vcurve) {
            array_append(&outline->points, {ic, ip});
        }
        p = ip;
    }
    array_push_back(&outline->offsets, outline->points.size);
}

s64 _font_glyph_from_outline(Font_data* fonts, Font_data::Outline o) {
    // Determine best y
    // Note that the median would be optimal
    float y = 0;
    for (Vec2 v: o.points) y += v.y;
    y /= (float)o.points.size;

    s64 paths_beg = fonts->paths_data.size;

    for (s64 o_i = 0; o_i+1 < o.offsets.size; ++o_i) {
        for (s64 j = o.offsets[o_i]; j+1 < o.offsets[o_i+1]; j += 2) {
            Vec2 p0 = o.points[j+2];
            Vec2 p1 = o.points[j+1];
            Vec2 p2 = o.points[j  ];

            // Ignore vertical lines
            if (p0.x == p1.x and p1.x == p2.x) continue;
            
            // Split the spline at y, if applicable
            float t[5] = {0.f};
            s64 n = spline_hit_y(p0, p1, p2, y, t+1) + 1;

            // Split the spline at vertical
            {float a = p0.x - p1.x, b = p0.x - 2*p1.x + p2.x;
            if (0.f < a and a < b) {
                assert(false);
                t[n] = a / b;
                ++n;
            }}

            t[n] = 1.f;

            for (s64 i = 0; i < n; ++i) {
                Vec2 p[3];
                spline_split_range(p0, p1, p2, t[i], t[i+1], p);
                bool above = (p[0] + p[1] + p[2]).y >= 3.f * y;
                bool cover = not above;
                if (p[0].x > p[2].x) {
                    simple_swap(p, p+2);
                    cover ^= 1;
                }

                // The approximation for the bezier curve is not well defined if the points are
                // colinear. So perturb them a bit in that case. Also put p1 into the middle, as the
                // approximation works better then. (It is bad mostly if p1 is near p0 or p2.)
                Vec2 u = (p[2] - p[0]).ortho().normalized();
                float d = dot(p[1] - p[0], u);
                if (abs(d) < 0.01) {
                    p[1] = 0.5f * (p[0] + p[2]) + u * 0.01f;
                }

                p[0].y = -p[0].y; p[1].y = -p[1].y; p[2].y = -p[2].y;

                float sense = 2*cover + above;
                array_push_back(&fonts->paths_data, {
                    p[0], p[1], p[2], sense, -y
                });
            }
        }
    }

    s64 len = fonts->paths_data.size - paths_beg;
    if (len >= fonts->len_to_stitch.size) {
        assert(false); // Cannot store a glyph with that many paths.
        len = fonts->len_to_stitch.size - 1;
    }

    if (len == 0) return 0xfffe;
    
    Font_data::Glyph glyph;
    glyph.type0 = fonts->len_to_stitch[len].type0;
    glyph.type1 = fonts->len_to_stitch[len].type1;
    glyph.path0_beg = paths_beg;
    glyph.path1_beg = paths_beg + fonts->type_len[glyph.type0];
    s64 pad = glyph.path1_beg + fonts->type_len[glyph.type1] - fonts->paths_data.size;
    assert(pad >= 0);
    array_append_zero(&fonts->paths_data, pad);
    
    array_push_back(&fonts->glyphs, glyph);
    array_push_back(&fonts->glyph_counts, 0ll);
    return fonts->glyphs.size - 1;
}

Font_data::Glyph_handle _font_glyph_get_handle(Font_data* fonts, s64 font_base, u32 codepoint) {
    Font_data::Glyph_handle handle = {0xffff, 0};
    u64 key = (u64)codepoint << 32 | font_base;
    if (codepoint < 128) {
        handle = fonts->ascii_indices.data[codepoint];
    } else if (auto* p = hashmap_getptr(&fonts->codepoint_indices, key)) {
        handle = *p;
    }
    if (handle.index == 0xffff) {
        _font_outline_load(&fonts->infos[font_base], codepoint, &fonts->outline_buf);
        handle.index = _font_glyph_from_outline(fonts, fonts->outline_buf);
        int advance;
        stbtt_GetCodepointHMetrics(&fonts->infos[font_base], codepoint, &advance, nullptr);
        handle.advance = advance;
        assert(handle.advance == advance);
        if (codepoint < 128) {
            fonts->ascii_indices[codepoint] = handle;;
        } else {
            hashmap_set(&fonts->codepoint_indices, key, handle);
        }
    }

    return handle;
}

void font_draw_codepoint_base(Font_data* fonts, s64 font_base, u32 codepoint, Vec2 pos, Vec2 scale, Color fill, float* out_advance=nullptr) {
    font_base ^= Font_data::MAGIC_INFOS;
    Font_data::Glyph_handle handle = _font_glyph_get_handle(fonts, font_base, codepoint);

    if (handle.index < 0xfffe) {
        ++fonts->glyph_counts.data[handle.index];
        array_push_back(&fonts->draw_glyphs, {handle.index, pos, scale, fill});
    }
    if (out_advance) *out_advance = handle.advance * scale.x;
}

void font_metrics_codepoint_get(
    Font_data* fonts, s64 font_instance, u32 codepoint,
    float* out_advance=nullptr, float* out_lsb=nullptr
) {
    auto inst = &fonts->instances[font_instance ^ Font_data::MAGIC_INSTANCES];
    auto info = &fonts->infos    [inst->info];
    int advance, lsb;
    stbtt_GetCodepointHMetrics(info, codepoint, &advance, &lsb);
    if (out_advance) *out_advance = advance * inst->scale;
    if (out_lsb)     *out_lsb     = lsb     * inst->scale;
}

// Decode the first unicode codepoint in buf, return the number of bytes it is long. The result value of the codepoint is written into c_out.
s64 _font_decode_utf8(Array_t<u8> buf, u32* c_out = nullptr) {
    static bool warning_was_given;
    
    u32 c = buf[0];
    s64 c_bytes = c&128 ? c&64 ? c&32 ? c&16 ? 4 : 3 : 2 : -1 : 1;
    if (buf.size < c_bytes) c_bytes = -1;
    
    if (c_bytes == 1) {
        // nothing
    } else if (c_bytes == 2) {
        c = (buf[0]&0x1f) << 6 | (buf[1]&0x3f);
    } else if (c_bytes == 3) {
        c = (buf[0]&0xf) << 12 | (buf[1]&0x3f) << 6 | (buf[2]&0x3f);
    } else if (c_bytes == 4) {
        c = (buf[0]&0x7) << 18 | (buf[1]&0x3f) << 12 | (buf[2]&0x3f) << 6 | (buf[3]&0x3f);
    } else {
        if (not warning_was_given) {
            fprintf(stderr, "Warning: encountered invalid utf-8 sequence (this warning will not show again)\n");
            warning_was_given = true;
        }
        assert(false);
        c = '?';
        c_bytes = 1;
    }
    if (c_out) *c_out = c;
    return c_bytes;
}

void font_metrics_string_get(
    Font_data* fonts, s64 font_instance, Array_t<u8> str,
    float* out_advance=nullptr, float* out_lsb=nullptr
) {
    float advance_total = 0.f;
    for (s64 i = 0; i < str.size;) {
        u32 c;
        s64 bytes = _font_decode_utf8(array_subarray(str, i, str.size), &c);
        i += bytes;
        float advance;
        font_metrics_codepoint_get(fonts, font_instance, c, &advance, i == 0 ? out_lsb : nullptr);
        advance_total += advance;
    }
    if (out_advance) *out_advance = advance_total;
}

void font_draw_codepoint(
    Font_data* fonts, s64 font_instance, u32 codepoint, Vec2 pos, Color fill,
    float* x_out=nullptr, float* yn_out=nullptr
) {
    auto inst = fonts->instances[font_instance ^ Font_data::MAGIC_INSTANCES];
    font_draw_codepoint_base(fonts, inst.info ^ Font_data::MAGIC_INFOS,
        codepoint, pos, {inst.scale, inst.scale}, fill, x_out);

    if (x_out) *x_out += pos.x;
    if (yn_out) *yn_out = pos.y + inst.newline;
}

void font_draw_string(
    Font_data* fonts, s64 font_instance, Array_t<u8> str, Vec2 pos, Color fill={0,0,0,255},
    float* x_out=nullptr, float* yn_out=nullptr
) {
    float yn;
    for (s64 i = 0; i < str.size;) {
        u32 c;
        s64 bytes = _font_decode_utf8(array_subarray(str, i, str.size), &c);
        i += bytes;
        font_draw_codepoint(fonts, font_instance, c, pos, fill, &pos.x, &yn);
    }

    if (x_out) *x_out = pos.x;
    if (yn_out) *yn_out = yn;
}

u32 font_word_create(Font_data* fonts, s64 font_instance, Array_t<u32> codepoints) {
    auto inst = fonts->instances[font_instance ^ Font_data::MAGIC_INSTANCES];

    u32 word = fonts->words.size;
    Font_data::Word w;
    w.path_beg = fonts->word_paths.size;
    w.font_instance = font_instance;

    s64 types = fonts->type_len.size - 1;
    array_append_zero(&fonts->word_type_count, types);
    Array_t<u8> counts = array_subarray(fonts->word_type_count, word * types, (word+1) * types);

    Array_t<s64> counts_tmp {nullptr, types + 1};
    counts_tmp.data = (s64*)alloca(counts_tmp.size * sizeof(counts_tmp[0]));
    array_memset(&counts_tmp);

    for (u32 c: codepoints) {
        Font_data::Glyph_handle handle = _font_glyph_get_handle(fonts, inst.info, c);
        if (handle.index >= 0xfffe) continue;
        Font_data::Glyph g = fonts->glyphs[handle.index];
        ++counts_tmp[g.type0];
        ++counts_tmp[g.type1];
    }

    counts_tmp[0] = 0;
    for (s64 i = 1; i < counts_tmp.size; ++i) {
        counts[i-1] = counts_tmp[i];
        assert(counts[i-1] == counts_tmp[i]);
        counts_tmp[i] += counts_tmp[i-1];
    }
    s64 count_total = counts_tmp[counts_tmp.size-1];

    array_append_zero(&fonts->word_paths, count_total);
    array_append_zero(&fonts->word_offsets, count_total);

    u32 offset = 0;
    for (u32 c: codepoints) {
        Font_data::Glyph_handle handle = _font_glyph_get_handle(fonts, inst.info, c);
        if (handle.index >= 0xfffe) continue;
        Font_data::Glyph g = fonts->glyphs[handle.index];
        
        s64 i0 = counts_tmp[g.type0-1]++;
        fonts->word_paths[w.path_beg + i0] = g.path0_beg;
        fonts->word_offsets[w.path_beg + i0] = offset;

        if (g.type1) {
            s64 i1 = counts_tmp[g.type1-1]++;
            fonts->word_paths[w.path_beg + i1] = g.path1_beg;
            fonts->word_offsets[w.path_beg + i1] = offset;
        }

        offset += handle.advance;
    }

    w.advance = offset;
    
    array_push_back(&fonts->words, w);
    return word ^ Font_data::MAGIC_WORD;
}


void font_draw_word(Font_data* fonts, u32 word, Vec2 pos, Color fill, float* out_advance=nullptr) {
    word ^= Font_data::MAGIC_WORD;
    array_push_back(&fonts->draw_words, {word, pos, fill});
    Font_data::Word w = fonts->words[word];
    if (out_advance) {
        auto inst = fonts->instances[w.font_instance ^ Font_data::MAGIC_INSTANCES];
        *out_advance = w.advance * inst.scale;
    }
}

void font_frame_draw(Font_data* fonts, s64 screen_w, s64 screen_h) {
    if (screen_w != fonts->tex_coverage_w or screen_h != fonts->tex_coverage_h) {
        glActiveTexture(fonts->texture_unit);
        glBindTexture(GL_TEXTURE_2D, fonts->tex_coverage);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16_SNORM, screen_w, screen_h, 0, GL_RED, GL_UNSIGNED_BYTE, 0);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

        fonts->tex_coverage_w = screen_w;
        fonts->tex_coverage_h = screen_h;
    }

    if (fonts->tex_glyphs_size < fonts->paths_data.size) {
        s64 offset = fonts->tex_glyphs_size * 2 * 16;
        s64 size   = fonts->paths_data.size * 2 * 16 - offset;
        if (offset + size > 65536 * 16) {
            static bool warning_was_given;
            if (not warning_was_given) {
                fprintf(stderr, "Warning: Glyph storage capacity exceeded.\n");
                warning_was_given = true;
            }
        } else {
            glBindBuffer(GL_TEXTURE_BUFFER, fonts->tex_glyphs_buf);
            glBufferSubData(GL_TEXTURE_BUFFER, offset, size, &fonts->paths_data[fonts->tex_glyphs_size]);
            fonts->tex_glyphs_size = fonts->paths_data.size;
        }
    }

    glBindFramebuffer(GL_FRAMEBUFFER, fonts->path_fbo);
    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);
    glBlendFunc(GL_ONE, GL_ONE);

    glActiveTexture(fonts->texture_unit + 1);
    glBindTexture(GL_TEXTURE_BUFFER, fonts->tex_glyphs);
    
    opengl_shader_use_and_set_origin(&fonts->path, screen_w, screen_h);
    opengl_uniform_set(&fonts->path, "p_data", (int)(fonts->texture_unit+1 - GL_TEXTURE0));

    opengl_shader_attrib_divisor(&fonts->path, 0, "pos", 0);
    
    Array_dyn<Vec2>*  target_pos  = opengl_buffer<Vec2> (&fonts->path, 0, "pos");
    Array_dyn<Vec2>*  target_off  = opengl_buffer<Vec2> (&fonts->path, 1, "off");
    Array_dyn<Vec2>*  target_size = opengl_buffer<Vec2> (&fonts->path, 2, "size");
    Array_dyn<u32>*   target_path = opengl_buffer<u32>  (&fonts->path, 3, "p_base");
    Array_dyn<Color>* target_fill = opengl_buffer<Color>(&fonts->path, 4, "fill");

    if (target_pos->size == 0) {
        array_append(target_pos, {
            {0.f, 1.f}, {0.f, 1.f}, {0.f, 0.f}, {1.f, 1.f}, {1.f, 0.f}, {1.f, 0.f}
        });
    }

    for (s64 i = 0; i < fonts->glyph_counts.size; ++i) {
        Font_data::Glyph g = fonts->glyphs[i];
        fonts->type_counts.data[g.type0] += fonts->glyph_counts.data[i];
        fonts->type_counts.data[g.type1] += fonts->glyph_counts.data[i];
    }
    for (auto wd: fonts->draw_words) {
        s64 types = fonts->type_len.size - 1;
        s64 w_counts_i = wd.word * types;
        Array_t<u8> w_counts = array_subarray(fonts->word_type_count, w_counts_i, w_counts_i + types);
        for (s64 type = 1; type < fonts->type_counts.size; ++type) {
            fonts->type_counts[type] += w_counts[type-1];
        }
    }
    
    fonts->type_counts[0] = 0;
    for (s64 i = 1; i < fonts->type_len.size; ++i) {
        fonts->type_counts[i] += fonts->type_counts[i-1];
    }
    
    array_resize(target_off,  fonts->type_counts[fonts->type_counts.size-1]);
    array_resize(target_size, fonts->type_counts[fonts->type_counts.size-1]);
    array_resize(target_path, fonts->type_counts[fonts->type_counts.size-1]);
    array_resize(target_fill, fonts->type_counts[fonts->type_counts.size-1]);

    for (auto i: fonts->draw_glyphs) {
        Font_data::Glyph g = fonts->glyphs.data[i.glyph];
        s64 index0 = fonts->type_counts.data[g.type0-1]++;
        (*target_off ).data[index0] = i.pos;
        (*target_size).data[index0] = i.scale;
        (*target_path).data[index0] = g.path0_beg;
        (*target_fill).data[index0] = i.fill;
        if (g.type1 == 0) continue;
        s64 index1 = fonts->type_counts.data[g.type1-1]++;
        (*target_off ).data[index1] = i.pos;
        (*target_size).data[index1] = i.scale;
        (*target_path).data[index1] = g.path1_beg;
        (*target_fill).data[index1] = i.fill;
    }

    for (auto wd: fonts->draw_words) {
        s64 types = fonts->type_len.size - 1;
        s64 w_counts_i = wd.word * types;
        Array_t<u8> w_counts = array_subarray(fonts->word_type_count, w_counts_i, w_counts_i + types);
        Font_data::Word w = fonts->words.data[wd.word];
        s64 path_index = w.path_beg;
        float scale = fonts->instances.data[w.font_instance ^ Font_data::MAGIC_INSTANCES].scale;
        
        for (s64 type = 1; type < fonts->type_counts.size; ++type) {
            s64 index = fonts->type_counts.data[type-1];
            s64 size = w_counts.data[type-1];
            fonts->type_counts.data[type-1] += size;
            for (s64 i = 0; i < size; ++i) {
                Vec2 tmp = wd.pos;
                tmp.x += fonts->word_offsets.data[path_index + i] * scale;
                (*target_off ).data[index + i] = tmp;
                (*target_size).data[index + i] = {scale, scale};
                (*target_path).data[index + i] = fonts->word_paths.data[path_index + i];
                (*target_fill).data[index + i] = wd.fill;
            }
            path_index += size;
        }
    }
    
    for (s64 i = fonts->type_counts.size-1; i > 0; --i) {
        fonts->type_counts[i] = fonts->type_counts[i-1];
    }
    fonts->type_counts[0] = 0;

    opengl_shader_buffers_copy(&fonts->path);

    for (s64 i = 1; i < fonts->type_len.size; ++i) {
        s64 len = fonts->type_len[i];
        s64 offset = fonts->type_counts[i-1];
        s64 count = fonts->type_counts[i] - offset;
        opengl_shader_attrib_divisor(&fonts->path, 1, "off",    len);
        opengl_shader_attrib_divisor(&fonts->path, 2, "size",   len);
        opengl_shader_attrib_divisor(&fonts->path, 3, "p_base", len);
        opengl_shader_attrib_divisor(&fonts->path, 4, "fill",   len);
        opengl_uniform_set(&fonts->path, "p_length", (u32)len);
        opengl_shader_buffers_enable(&fonts->path, 0, offset);
        glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 6, count * len);
    }
    
    array_memset(&fonts->glyph_counts);
    array_memset(&fonts->type_counts);
    fonts->draw_glyphs.size = 0;
    fonts->draw_words.size = 0;
    
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // @Cleanup move

    u8 black[4] = {0, 0, 0, 255};
    opengl_buffer_append<Vec2>(&fonts->blend, 0, "pos",
        {{-1.f, 1.f}, {-1.f, -1.f}, {1.f, 1.f}, {1.f, -1.f}}
    );
    
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    opengl_shader_use(&fonts->blend);
    opengl_uniform_set(&fonts->blend, "sampler", fonts->texture_unit - GL_TEXTURE0);
    opengl_shader_draw_and_clear(&fonts->blend, GL_TRIANGLE_STRIP);
}


void _convex_hull(Array_t<Vec2>* points) {
    if (points->size == 0) return;
    
    {Vec2 left_v {INFINITY, 0};
    s64 left = 0;
    for (s64 i = 1; i < points->size; ++i) {
        Vec2 v = (*points)[i];
        if (v.x < left_v.x or (v.x == left_v.x and v.y < left_v.y)) {
            left_v = v;
            left = i;
        }
    }
    simple_swap(&(*points)[0], &(*points)[left]);}

    for (s64 i = 0; i+1 < points->size; ++i) {
        Vec2 p = (*points)[i];
        Vec2 v = i == 0 ? Vec2 {0, 1} : p - (*points)[i-1];
        float best_val = i == 0 ? -INFINITY : dot(v, ((*points)[0] - p).normalized());
        float best_dist = 0.f;
        s64 best = 0;
        for  (s64 j = i+1; j < points->size; ++j) {
            Vec2 w = (*points)[j] - p;
            if (w.x == 0 and w.y == 0) {
                // Delete if identical
                simple_swap(&(*points)[j], &(*points)[points->size-1]);
                --points->size;
                --j;
                continue;
            }
            float val = dot(v, w.normalized());
            float dist = w.length2();
            if (best_val < val or (best_val == val and best_dist < dist)) {
                best_val = val;
                best_dist = dist;
                best = j;
            }
        }
        if (best == 0) {
            points->size = i+1;
            break;
        } else {
            simple_swap(&(*points)[i+1], &(*points)[best]);
        }
    }
}


void font_fmt_init(Font_data* fonts) {
    fonts->text_current_flags = 0;
    Font_data::Text* text = hashmap_getcreate(&fonts->text_slots, Font_data::SLOT_TEMPORARY);
    text->boxes.size = 0;
    text->str.size = 0;
}

void font_fmt_begin(Font_data* fonts, u64 flags) {
    fonts->text_current_flags |= flags;
}

void font_fmt_end(Font_data* fonts, u64 flags) {
    Font_data::Text* text = hashmap_getcreate(&fonts->text_slots, Font_data::SLOT_TEMPORARY);
    if (text->boxes.size) {
        text->boxes[text->boxes.size-1].flags |= flags & Font_data::GROUP_SPACING;
    }
    
    fonts->text_current_flags &= ~flags;
}


void _font_utf8_to_codepoints(Array_t<u8> str, Array_dyn<u32>* into) {
    for (s64 i = 0; i < str.size;) {
        u32 c = -1;
        i += _font_decode_utf8(array_subarray(str, i, str.size), &c);
        array_push_back(into, c);
    }
}

void font_fmt_text(Font_data* fonts, u64 flags_add, Array_t<u8> str) {
    font_fmt_begin(fonts, flags_add);
    u64 flags = fonts->text_current_flags;

    s64 font;
    if      (flags & Font_data::HEADER)  font = Font_data::FONT_HEADER;
    else if (flags & Font_data::BOLD)    font = Font_data::FONT_BOLD;
    else if (flags & Font_data::ITALICS) font = Font_data::FONT_ITALIC;
    else if (flags & Font_data::SMALL)   font = Font_data::FONT_SMALL;
    else if (flags & Font_data::SANS)    font = Font_data::FONT_SANS;
    else if (flags & Font_data::BUTTON)  font = Font_data::FONT_BUTTON;
    else                                font = Font_data::FONT_NORMAL;
    
    u8 letter_fac = flags & Font_data::COMPACT ? 95 : 100;
    u64 hash_base = hash_u64(font ^ (letter_fac << 16));

    Font_data::Text* into = hashmap_getcreate(&fonts->text_slots, Font_data::SLOT_TEMPORARY);
    array_append(&into->str, str);
            
    s64 first = -1;
    for (s64 i = 0; i <= str.size; ++i) {
        u8 c = i < str.size ? str[i] : 0;
        bool issplit = c == ' ' || c == '\t' || c == '\n' || c == 0;
        if (issplit and first >= 0) {
            Array_t<u8> word = array_subarray(str, first, i);
            u64 key = hash_u64_pair(hash_base, hash_str(word));

            u32* word_id = hashmap_getcreate(&fonts->word_lookup, key, -1u);
            if (*word_id == -1) {
                Array_dyn<u32>* buf = &fonts->buf_codepoints;
                buf->size = 0;
                _font_utf8_to_codepoints(word, buf);
                // TODO letter_fac
                *word_id = font_word_create(fonts, font, *buf);
            }
            
            Font_data::Text_box box;
            box.word = *word_id;
            box.flags =
                c == ' '  ? (u64)Font_data::SPACE    :
                c == '\n' ? (u64)Font_data::NEWLINE  :
                c == '\t' ? (u64)Font_data::INDENTED : 0;
            box.flags |= flags & Font_data::GROUP_DRAWING;
            box.word_beg = first;
            box.word_end = i;

            array_push_back(&into->boxes, box);
            first = -1;
        } else if (not issplit and first == -1) {
            first = i;
        }
    }
    
    font_fmt_end(fonts, flags_add);
}

void font_fmt_draw(
    Font_data* fonts, s64 slot, Vec2 pos, float w, Color fill={0,0,0,255},
    float* x_out=nullptr, float* y_out=nullptr, bool only_measure=false, float* xw_out=nullptr
) {
    Array_t<Font_data::Text_box> boxes = hashmap_get(&fonts->text_slots, slot).boxes;
    if (not boxes.size) return;

    float x = pos.x, y = pos.y;
    float orig_x = x, orig_y = y, max_x = x;
    if (w == -1) w = INFINITY;

    Color red   {112, 10,  19, 255};

    {Font_data::Word w0 = fonts->words[boxes[0].word ^ Font_data::MAGIC_WORD];
    y += fonts->instances[w0.font_instance ^ Font_data::MAGIC_INSTANCES].ascent;}

    for (s64 i = 0; i < boxes.size; ++i) {
        Font_data::Text_box box = boxes[i];
        auto word = fonts->words[box.word ^ Font_data::MAGIC_WORD];
        auto font_inst = fonts->instances[word.font_instance ^ Font_data::MAGIC_INSTANCES];

        float group_w = 0;
        bool addspace = false;
        for (s64 j = i; j < boxes.size; ++j) {
            Font_data::Text_box box_j = boxes[j];
            auto word_j = fonts->words[box_j.word ^ Font_data::MAGIC_WORD];
            auto font_inst_j = fonts->instances[word_j.font_instance ^ Font_data::MAGIC_INSTANCES];

            addspace = box_j.flags & Font_data::SPACE;
            group_w += word_j.advance * font_inst_j.scale;
            if (j+1 >= boxes.size or (box_j.flags & Font_data::GROUP_BREAKING)) {
                // There are no more words in this line, terminate group but do not add a space
                addspace = false;
                break;
            } else if (~box_j.flags & Font_data::STICKY) {
                // This is not sticky, so the group is terminated.
                break;
            } else if (addspace) {
                // We are sticky, and there is something after us, and we add a space
                group_w += font_inst_j.space;
            }
        }
        if (x > orig_x and x + group_w > orig_x + w) {
            x = orig_x;
            y += font_inst.newline;
            if (box.flags & (Font_data::INDENTED | Font_data::ITEMIZED)) {
                x += font_inst.space * 4;
            }
        } else if (x == orig_x and (box.flags & Font_data::ITEMIZED)) {
            x += font_inst.space * (4 - addspace) - group_w;
        }
        max_x = std::max(x + group_w, max_x);

        Color box_fill = box.flags & Font_data::RED ? red : fill;

        if (not only_measure) {
            font_draw_word(fonts, box.word, Vec2 {x, y}, box_fill);
        }
        
        x += word.advance * font_inst.scale;
        
        if (box.flags & Font_data::PARAGRAPH) {
            x = orig_x;
            y += font_inst.newline * 1.5f;
        } else if (box.flags & Font_data::PARAGRAPH_CLOSE) {
            x = orig_x;
            y += font_inst.newline * 1.2f;
        } else if (box.flags & Font_data::NEWLINE) {
            x = orig_x;
            y += font_inst.newline;
        } else if (box.flags & Font_data::SPACE) {
            x += font_inst.space;
        }
    }

    {Font_data::Word w1 = fonts->words[boxes[boxes.size-1].word ^ Font_data::MAGIC_WORD];
    y -= fonts->instances[w1.font_instance ^ Font_data::MAGIC_INSTANCES].ascent;}
    
    if (x_out)  *x_out  = x;
    if (y_out)  *y_out  = y;
    if (xw_out) *xw_out = max_x - orig_x;
}

void font_fmt_store(Font_data* fonts, u64 slot) {
    Font_data::Text from = hashmap_get(&fonts->text_slots, Font_data::SLOT_TEMPORARY);
    Font_data::Text* into = hashmap_getcreate(&fonts->text_slots, slot);
    
    into->boxes.size = 0;
    into->str.size = 0;

    array_append(&into->boxes, from.boxes);
    array_append(&into->str,   from.str  );
}

void font_fmt_store_simple(Font_data* fonts, u64 flags, Array_t<u8> str, u64 slot) {
    font_fmt_init(fonts);
    font_fmt_text(fonts, flags, str);
    font_fmt_store(fonts, slot);
}

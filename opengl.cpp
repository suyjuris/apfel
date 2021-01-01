
#ifndef PROGRAM_NAME
#error "Please define PROGRAM_NAME"
#endif

struct Vec2 {
    float x = 0.f, y = 0.f;

    Vec2& operator+= (Vec2 o) { x += o.x; y += o.y; return *this; }
    Vec2& operator-= (Vec2 o) { x -= o.x; y -= o.y; return *this; }
    Vec2& operator*= (Vec2 o) { x *= o.x; y *= o.y; return *this; }
    Vec2& operator/= (Vec2 o) { x /= o.x; y /= o.y; return *this; }
    Vec2 operator+ (Vec2 o) const { Vec2 v {*this}; v += o; return v; }
    Vec2 operator- (Vec2 o) const { Vec2 v {*this}; v -= o; return v; }
    Vec2 operator* (Vec2 o) const { Vec2 v {*this}; v *= o; return v; }
    Vec2 operator/ (Vec2 o) const { Vec2 v {*this}; v /= o; return v; }
    Vec2& operator+= (float o) { x += o; y += o; return *this; }
    Vec2& operator-= (float o) { x -= o; y -= o; return *this; }
    Vec2& operator*= (float o) { x *= o; y *= o; return *this; }
    Vec2& operator/= (float o) { x /= o; y /= o; return *this; }
    Vec2 operator+ (float o) const { Vec2 v {*this}; v += o; return v; }
    Vec2 operator- (float o) const { Vec2 v {*this}; v -= o; return v; }
    Vec2 operator* (float o) const { Vec2 v {*this}; v *= o; return v; }
    Vec2 operator/ (float o) const { Vec2 v {*this}; v /= o; return v; }
    Vec2 operator+ () const { return *this; }
    Vec2 operator- () const { return {-x, -y}; }
    float length2() const { return x*x + y*y; }
    float length() const { return std::sqrt(x*x + y*y); }
    Vec2 normalized() const { float len = length(); assert(len != 0.f); return {x/len, y/len}; }
    Vec2 ortho() const { return {-y, x}; }
    bool operator== (Vec2 o) { return x == o.x and y == o.y; }
    bool operator!= (Vec2 o) { return x != o.x or  y != o.y; }
 
};
Vec2 operator+ (float a, Vec2 b) { return b + a; }
Vec2 operator- (float a, Vec2 b) { return b - a; }
Vec2 operator* (float a, Vec2 b) { return b * a; }
Vec2 operator/ (float a, Vec2 b) { return b / a; }
float dot(Vec2 a, Vec2 b) { a *= b; return a.x + a.y; }
Vec2 lerp(Vec2 a, Vec2 b, float t) { return (1.f - t) * a + t * b; }

struct Mat2 {
    Vec2 x, y;

    Mat2& operator+= (Mat2 o) { x += o.x; y += o.y; return *this; }
    Mat2& operator-= (Mat2 o) { x -= o.x; y -= o.y; return *this; }
    Mat2 operator+ (Mat2 o) const { Mat2 v {*this}; v += o; return v; }
    Mat2 operator- (Mat2 o) const { Mat2 v {*this}; v -= o; return v; }
    Mat2& operator+= (float o) { x += o; y += o; return *this; }
    Mat2& operator-= (float o) { x -= o; y -= o; return *this; }
    Mat2& operator*= (float o) { x *= o; y *= o; return *this; }
    Mat2& operator/= (float o) { x /= o; y /= o; return *this; }
    Mat2 operator+ (float o) const { Mat2 v {*this}; v += o; return v; }
    Mat2 operator- (float o) const { Mat2 v {*this}; v -= o; return v; }
    Mat2 operator* (float o) const { Mat2 v {*this}; v *= o; return v; }
    Mat2 operator/ (float o) const { Mat2 v {*this}; v /= o; return v; }
    Mat2 operator+ () const { return *this; }
    Mat2 operator- () const { return {-x, -y}; }
    bool operator== (Mat2 o) { return x == o.x and y == o.y; }
    bool operator!= (Mat2 o) { return x != o.x or  y != o.y; }
    Mat2 T() const { return Mat2 {{x.x, y.x}, {x.y, y.y}}; }
    Mat2 operator* (Mat2 o) const {
        Mat2 t {T()}; return {{dot(t.x, o.x), dot(t.x, o.y)}, {dot(t.y, o.x), dot(t.y, o.y)}};
    }
    Mat2& operator*= (Mat2 o) { *this = *this * o; return *this; }
    Vec2 operator* (Vec2 o) const { Mat2 t {T()}; return {dot(t.x, o), dot(t.y, o)}; }
};

// Only as data
struct Vec3 { float x = 0.f, y = 0.f, z = 0.f; };
struct Vec4 { float x = 0.f, y = 0.f, z = 0.f, w = 0.f; };
struct Color { u8 r = 0, g = 0, b = 0, a = 255; };
Color lerp(Color a, Color b, float t) {
    float u = 1.f - t;
    return {(u8)(a.r*u+b.r*t), (u8)(a.g*u+b.g*t), (u8)(a.b*u+b.b*t), (u8)(a.a*u+b.a*t)};
}

namespace Palette {
constexpr Color BLACK {0, 0, 0};
constexpr Color GREY {100, 100, 100};
constexpr Color WHITE {255, 255, 255};
constexpr Color BG {246, 240, 228};
constexpr Color RED {127, 10, 19};
constexpr Color BGRED {242, 231, 232};
constexpr Color BLUE {16, 67, 84};
constexpr Color BGBLUE {232, 237, 238};
constexpr Color GREEN {33, 117, 22};
constexpr Color BGGREEN {233, 241, 232};
constexpr Color PURPLE {136, 75, 171};
constexpr Color BGPURPLE {243, 237, 247};
constexpr Color ORANGE {210, 124, 17};
constexpr Color BGORANGE {251, 242, 232};
constexpr Color PINK {233, 95, 159};
constexpr Color BGPINK {253, 239, 246};
constexpr Color REDLIGHT {201, 136, 141};
constexpr Color BLUELIGHT {120, 164, 184};
constexpr Color GREENLIGHT {118, 222, 104};
constexpr Color PURPLELIGHT {188, 135, 219};
constexpr Color REDBRIGHT {189, 3, 16};
constexpr Color BGREDBRIGHT {249, 230, 232};
constexpr Color BLUEBRIGHT {25, 123, 155};
constexpr Color BGBLUEBRIGHT {232, 242, 245};
}

struct Shader_var {
    s64 name_beg, name_end;
    u8 type;
};

struct Shader_buffer {
    enum Buffer_type: u8 {
        BTYPE_NONE = 0, BTYPE_float, BTYPE_s32, BTYPE_u32, BTYPE_u8,
        BTYPE_Vec2, BTYPE_Vec3, BTYPE_Vec4,
        BTYPE_Color,
        BTYPE_COUNT
    };
    static constexpr s64 btype_size[BTYPE_COUNT] = {
        (u32)-1, sizeof(float), sizeof(s32), sizeof(u32), sizeof(u8),
        sizeof(Vec2), sizeof(Vec3), sizeof(Vec4),
        sizeof(Color)
    };
    static constexpr u32 btype_type[BTYPE_COUNT] = {
        (u32)-1, GL_FLOAT, GL_INT, GL_UNSIGNED_INT, GL_UNSIGNED_BYTE,
        GL_FLOAT, GL_FLOAT, GL_FLOAT,
        GL_UNSIGNED_BYTE,
    };
    static constexpr bool btype_norm[BTYPE_COUNT] = {
        false, false, false, false, true,
        false, false, false,
        true
    };
    static constexpr char const* btype_name[BTYPE_COUNT] = {
        "none", "float", "s32", "u32", "u8",
        "Vec2", "Vec3", "Vec4",
        "Color"
    };
    static constexpr s64 btype_mult[BTYPE_COUNT] = {
        -1, 1, 1, 1, 1,
        2, 3, 4,
        4
    };
    
    Array_dyn<u8> data;
    u8 btype;
    bool instanced;
};
constexpr s64  Shader_buffer::btype_size[];
constexpr u32  Shader_buffer::btype_type[];
constexpr bool Shader_buffer::btype_norm[];
constexpr char const* Shader_buffer::btype_name[];
constexpr s64 Shader_buffer::btype_mult[];

template <typename T> constexpr static u8 Shader_buffer_btype_value = -1;
template <> u8 Shader_buffer_btype_value<float> = Shader_buffer::BTYPE_float;
template <> u8 Shader_buffer_btype_value<s32>   = Shader_buffer::BTYPE_s32;
template <> u8 Shader_buffer_btype_value<u32>   = Shader_buffer::BTYPE_u32;
template <> u8 Shader_buffer_btype_value<u8>    = Shader_buffer::BTYPE_u8;
template <> u8 Shader_buffer_btype_value<Vec2>  = Shader_buffer::BTYPE_Vec2;
template <> u8 Shader_buffer_btype_value<Vec3>  = Shader_buffer::BTYPE_Vec3;
template <> u8 Shader_buffer_btype_value<Vec4>  = Shader_buffer::BTYPE_Vec4;
template <> u8 Shader_buffer_btype_value<Color> = Shader_buffer::BTYPE_Color;

struct Shader {
    // These have to match!
    enum Var_type: u8 {
        TYPE_INT, TYPE_IVEC2, TYPE_IVEC3, TYPE_IVEC4,
        TYPE_SAMPLER1D, TYPE_SAMPLER2D, TYPE_SAMPLER3D, TYPE_SAMPLER4D, 
        TYPE_FLOAT, TYPE_VEC2, TYPE_VEC3, TYPE_VEC4,
        TYPE_SAMPLERBUFFER, TYPE_INDEX,
        TYPE_COUNT, TYPE_INVALID
    };
    static constexpr char* type_names[TYPE_COUNT] = {
        "int", "ivec2", "ivec3", "ivec4",
        "sampler1D", "sampler2D", "sampler3D", "sampler4D",
        "float", "vec2", "vec3", "vec4",
        "samplerBuffer", "index"
    };
    static constexpr s64 type_mult[TYPE_COUNT] = {
        1, 2, 3, 4,  1, 2, 3, 4,  1, 2, 3, 4,
        1, 1
    };
    static constexpr u8 type_conv[TYPE_COUNT] = {
        // 0 = float, 1 = int, 2 = double
        1, 1, 1, 1,  1, 1, 1, 1,
        0, 0, 0, 0,
        1, 1
    };

    enum Shader_type: u8 {
        VERTEX, FRAGMENT
    };

    constexpr static s64 BUFFER_INDICES = -1;

    u32 id_program, id_vertex, id_fragment;
    Array_t<u32> id_buffers;

    Array_dyn<u8> name_data;
    Array_dyn<Shader_var> attribs, uniforms;
    Array_t<s32> loc_attribs, loc_uniforms;
    Array_t<Shader_buffer> buffers;

    bool compiled = false;
    
#ifndef NDEBUG
    u64 check_counter = 0;
#endif
};

constexpr char* Shader::type_names[];
constexpr s64 Shader::type_mult[];
constexpr u8 Shader::type_conv[];

struct Shader_parse_error {
    s64 beg, end;
    Array_dyn<u8> str;
};

void opengl_shader_error_init(Shader_parse_error* error) {
    assert(error);
    memset(error, 0, sizeof(*error));
}
void opengl_shader_error_free(Shader_parse_error* error) {
    if (error) {
        array_free(&error->str);
    }
}
void opengl_shader_error_print(Shader_parse_error* error, Array_t<u8> source) {
    assert(error);
    fprintf(stderr, "Error: %s\n", error->str.data);
    fprintf(stderr, "Error: while parsing\n");
    fprintf(stderr, "\n    ");

    s64 line_beg = error->beg;
    while (line_beg > 0 and source[line_beg-1] != '\n') --line_beg;
    s64 line_end = error->end;
    while (line_end < source.size and source[line_end] != '\n') ++line_end;
    
    auto arr = array_subarray(source, line_beg, line_end);
    fwrite(arr.data, 1, arr.size, stderr);
    fprintf(stderr, "\n    ");
    for (s64 i = 0; i < error->beg - line_beg; ++i) {
        fprintf(stderr, " ");
    }
    fprintf(stderr, "^");
    for (s64 i = 1; i < error->end - error->beg; ++i) {
        fprintf(stderr, "~");
    }
    fprintf(stderr, "\n\n");
}

bool opengl_shader_parse(u8 type, Array_t<u8> source, Shader* out, Shader_parse_error* out_err) {
    assert(out);
    
    s64 i = 0;
    auto get_token = [&source, &i]() -> Array_t<u8> {
        s64 state = 0;
        s64 start = -1;
        for (; i < source.size; ++i) {
            u8 c = source[i];
            if (state == 0) {
                if (isspace(c)) {
                    continue;
                } else if (isalnum(c) or c == '_') {
                    state = 1;
                    start = i;
                } else if (c == '/' and i+1 < source.size and source[i+1] == '/') {
                    state = 2;
                } else if (c == '#') {
                    state = 2;
                } else if (c == '/' and i+1 < source.size and source[i+1] == '*') {
                    state = 3;
                } else {
                    ++i;
                    return array_subarray(source, i-1, i);
                }
            } else if (state == 1) {
                if (isalnum(c) or c == '_') continue;
                return array_subarray(source, start, i);
            } else if (state == 2) {
                if (c == '\n') state = 0;
            } else if (state == 3) {
                if (c == '*' and i+1 < source.size and source[i+1] == '/') {
                    state = 0;
                }
            } else {
                assert(false);
            }
        }
        return {};
    };

    auto set_error_pos = [&i, out_err] (Array_t<u8> tok) {
        if (not out_err) return;
        out_err->beg = i - tok.size;
        out_err->end = i;
    };

    s64 state = 0;
    u8 var_type = Shader::TYPE_INVALID;
    u8 var_kind = -1;
    while (true) {
        auto tok = get_token();
        if (tok.size == 0) break;

        if (state == 0) {
            if (array_equal_str(tok, "attribute")) {
                state = 1;
                var_kind = 1;
            } else if (array_equal_str(tok, "uniform")) {
                state = 1;
                var_kind = 2;
            } else if (array_equal_str(tok, "void")) {
                break;
            } else if (type == Shader::VERTEX and array_equal_str(tok, "in")) {
                state = 1;
                var_kind = 1;
            } else {
                state = -1;
            }
        } else if (state == 1) {
            var_type = Shader::TYPE_INVALID;
            for (u8 typ = 0; typ < Shader::TYPE_COUNT; ++typ) {
                if (array_equal_str(tok, Shader::type_names[typ])) {
                    var_type = typ; break;
                }
            }
            if (var_type == Shader::TYPE_INVALID) {
                if (out_err) {
                    array_printf(&out_err->str, "unknown type '");
                    array_append(&out_err->str, tok);
                    array_printf(&out_err->str, "'");
                }
                set_error_pos(tok);
                return false;
            }
            state = 2;
        } else if (state == 2) {
            auto var_list = var_kind == 1 ? &out->attribs : &out->uniforms;
            array_push_back(var_list, {out->name_data.size, out->name_data.size + tok.size, var_type});
            array_append(&out->name_data, tok);
            array_push_back(&out->name_data, (u8)0);
            state = 3;
        } else if (state == 3) {
            if (array_equal_str(tok, ";")) {
                state = 0;
            } else if (array_equal_str(tok, "[")) {
                state = 4;
            } else {
                if (out_err) array_printf(&out_err->str, "expected ';' or '['");
                set_error_pos(tok);
                return false;
            }
        } else if (state == 4) {
            state = 5;
        } else if (state == 5) {
            if (array_equal_str(tok, "]")) {
                state = 6;
            } else {
                if (out_err) array_printf(&out_err->str, "expected ']'");
                set_error_pos(tok);
                return false;
            }
        } else if (state == 6) {
            if (array_equal_str(tok, ";")) {
                state = 0;
            } else {
                if (out_err) array_printf(&out_err->str, "expected ';'");
                set_error_pos(tok);
                return false;
            }
        } else if (state == -1) {
            if (array_equal_str(tok, ";")) {
                state = 0;
            }
        }
    }

    if (state == 1 or state == 2 or state == 3) {
        if (out_err) array_printf(&out_err->str, "unexpected eof");
        if (out_err) {
            out_err->beg = source.size;
            out_err->end = source.size;
        }
        return false;
    }
    return true;
}

bool opengl_shader_parse_error(Array_t<u8> line, s64* out_lineno, Array_t<u8>* out_msg) {
    auto match = [](Array_t<u8> str, s64 off, char const* pre) {
        s64 len = strlen(pre);
        return off+len <= str.size and array_equal_str(array_subarray(str, off, off+len), pre);
    };
    
    s64 state = 0;
    s64 lineno = 0;
    for (s64 i = 0; i < line.size; ++i) {
        u8 c = line[i];
        if (state == 0) {
            if (c == ' ' or ('0' <= c and c <= '9')) continue;
            if (c == '(') {state = 1; continue;}
            return false;
        } else if (state == 1) {
            if ('0' <= c and c <= '9') {
                lineno = 10*lineno + (c - '0');
            } else if (c == ')') {
                state = 2;
            } else {
                return false;
            }
        } else if (state == 2) {
            if (c == ' ') continue;
            if (c == ':') {state = 3; continue;}
            return false;
        } else if (state == 3) {
            if (c == ' ') continue;
            if (match(line, i, "error")) {state = 4; continue;}
            return false;
        } else if (state == 4) {
            if (('A' <= c and c <= 'Z') or ('a' <= c and c <= 'z')
                or ('0' <= c and c <= '9') or c == ' ') continue;
            if (c == ':') {state = 5; continue;}
            return false;
        } else if (state == 5) {
            if (c == ' ') continue;
            if (out_lineno) *out_lineno = lineno;
            if (out_msg) *out_msg = array_subarray(line, i, line.size);
            return true;
        }
    }
    return false;
}

// Load shader from string, print an error and abort if there is an error
GLuint opengl_shader_compile(Array_t<u8> source, Array_t<u8> name, GLenum type, Code_location loc = {}) {
   GLuint shader = glCreateShader(type);

   if (not shader) {
       fprintf(stderr, "Error: Could not create shader of type %d\n", type);
       exit(2101);
   }

   GLint source_size = source.size;
   glShaderSource(shader, 1, (GLchar**)&source.data, &source_size);
   glCompileShader(shader);
   
   GLint compiled;
   glGetShaderiv(shader, GL_COMPILE_STATUS, &compiled);
   if (not compiled) {
      GLint info_size = 0;
      glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &info_size);

      Array_t<u8> info = array_create<u8>(info_size); // @Leak
      glGetShaderInfoLog(shader, info.size, NULL, (char*)info.data);
      fprintf(stderr, "Error: while compiling shader '");
      array_fwrite(stderr, name);
      fprintf(stderr, "':\n");

      s64 last = 0;
      s64 lineno = -1;
      Array_t<u8> err_msg;
      for (s64 i = 0; i < info_size; ++i) {
          if (info[i] == '\n' or info[i] == 0) {
              auto line = array_subarray(info, last, i);
              fprintf(stderr, "    ");
              array_fwrite(stderr, line);
              fprintf(stderr, "\n");

              if (lineno == -1) {
                  opengl_shader_parse_error(line, &lineno, &err_msg);
              }
              last = i+1;
          }
      }

      if (lineno != -1) {
          fprintf(stderr, "Error: ");
          array_fwrite(stderr, err_msg);
          fprintf(stderr, "\n    ");
          if (loc.path.size) {
              array_fwrite(stderr, loc.path);
              fprintf(stderr, ":%lld: ", loc.offset_lines + lineno);
          } else {
              fprintf(stderr, "%lld: ", lineno);
          }

          s64 i_line = 0;
          s64 i_last = 0;
          for (s64 i = 0; i <= source.size; ++i) {
              if (i == source.size or source[i] == '\n') {
                  if (i_line+1 == lineno) {
                      array_fwrite(stderr, array_subarray(source, i_last, i));
                      break;
                  }
                  ++i_line;
                  i_last = i+1;
              } else if (i == i_last and source[i] == ' ') {
                  ++i_last;
              }
          }
          fprintf(stderr, "\n\n");
      }
                    
      exit(2102);
   }

   return shader;
}

// Link a shader program, print an error and abort if there is an error
void opengl_shader_link(GLuint program, Array_t<u8> program_name) {
    glLinkProgram(program);
    GLint linked;
    glGetProgramiv(program, GL_LINK_STATUS, &linked);
    if (not linked) {
        GLint info_size = 0;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &info_size);
      
        char* info = (char*)calloc(1, info_size+1);
        glGetProgramInfoLog(program, info_size, NULL, info);
        fprintf(stderr, "Error: while linking program ");
        fwrite(program_name.data, 1, program_name.size, stderr);
        fprintf(stderr, ":\n\n");
        fputs(info, stderr);
        exit(2201);
    }
}

Shader opengl_shader(
    Array_t<u8> name, Array_t<u8> vertex, Array_t<u8> fragment,
    Code_location vertex_loc = {}, Code_location fragment_loc = {}
) {
    Shader result;

    array_push_back(&result.attribs, {0, 0, Shader::TYPE_INDEX});

    Array_dyn<u8> buf;
    defer { array_free(&buf); };
    
    Shader_parse_error err;
    opengl_shader_error_init(&err);
    defer { opengl_shader_error_free(&err); };

    {bool succ = opengl_shader_parse(Shader::VERTEX,   vertex,   &result, &err);
    if (not succ) {
        opengl_shader_error_print(&err, vertex);
        exit(5);
    }}
    {bool succ = opengl_shader_parse(Shader::FRAGMENT, fragment, &result, &err);
    if (not succ) {
        opengl_shader_error_print(&err, fragment);
        exit(5);
    }}

    result.id_program = glCreateProgram();
    
    buf.size = 0;
    array_append(&buf, name);
    array_printf(&buf, ":vertex");
    result.id_vertex = opengl_shader_compile(vertex, buf, GL_VERTEX_SHADER, vertex_loc);

    buf.size = 0;
    array_append(&buf, name);
    array_printf(&buf, ":fragment");
    result.id_fragment = opengl_shader_compile(fragment, buf, GL_FRAGMENT_SHADER, fragment_loc);

    glAttachShader(result.id_program, result.id_vertex);
    glAttachShader(result.id_program, result.id_fragment);
    opengl_shader_link(result.id_program, name);

    result.id_buffers = array_create<u32>(result.attribs.size);
    glGenBuffers(result.id_buffers.size, result.id_buffers.data);

    result.loc_attribs  = array_create<s32>(result.attribs .size);
    result.loc_uniforms = array_create<s32>(result.uniforms.size);

    result.buffers = array_create<Shader_buffer>(result.attribs.size);

    for (s64 i = 1; i < result.attribs.size; ++i) {
        // Remember to skip the first one, which is the index buffer
        Shader_var attr = result.attribs[i];
        char* name = (char*)&result.name_data[attr.name_beg];
        result.loc_attribs[i] = i-1;
        glBindAttribLocation(result.id_program, result.loc_attribs[i], name);
    }
    for (s64 i = 0; i < result.uniforms.size; ++i) {
        Shader_var uni = result.uniforms[i];
        char* name = (char*)&result.name_data[uni.name_beg];
        result.loc_uniforms[i] = glGetUniformLocation(result.id_program, name);
        assert(result.loc_uniforms[i] != -1);
    }
    
    result.compiled = true;
    return result;
}

Shader opengl_shader_assets(Asset_store* assets, Array_t<u8> name, Array_t<u8> name_v = {}, Array_t<u8> name_f = {}) {
    u8* p = (u8*)alloca(2 * (name.size + 3));
    if (name_v.size == 0) {
        name_v = {p, name.size + 2};
        memcpy(name_v.data, name.data, name.size);
        memcpy(name_v.data + name.size, "_v", 3);
    }
    if (name_f.size == 0) {
        name_f = {name_v.end() + 1, name.size + 2};
        memcpy(name_f.data, name.data, name.size);
        memcpy(name_f.data + name.size, "_f", 3);
    }
    Code_location loc_v, loc_f;
    Array_t<u8> data_v = asset_get(assets, name_v, &loc_v);
    Array_t<u8> data_f = asset_get(assets, name_f, &loc_f);
    return opengl_shader(name, data_v, data_f, loc_v, loc_f);
}

void _opengl_buffer_helper2(Shader* shader, s64 index, char const* name) {
#ifndef NDEBUG
    assert(shader->compiled);
    auto var = shader->attribs[index+1];
    if (index >= 0) {
        // Only check name for the real attributes, not the index array
        auto var_name = array_subarray(shader->name_data, var.name_beg, var.name_end);
        if (name and not array_equal_str(var_name, name)) {
            fprintf(stderr, "Error: index of variable '%s' specified as %lld, but that is '", name, index);
            fwrite(var_name.data, 1, var_name.size, stderr);
            fputs("'\n", stderr);
            exit(2301);
        }
    }
#endif
}

Array_dyn<u8>* _opengl_buffer_helper(Shader* shader, s64 index, char const* name, u8 btype) {
#ifndef NDEBUG
    if ((shader->check_counter & 0xffff0) == 0) {
        assert(btype < Shader_buffer::BTYPE_COUNT); // Probably invalid btype
        _opengl_buffer_helper2(shader, index, name);
        auto var_btype = shader->buffers[index+1].btype;
        if (var_btype and var_btype != btype) {
            fprintf(
                stderr, "Error: different types for variable '%s', %s (%d) and %s (%d)", name,
                Shader_buffer::btype_name[var_btype], (int)var_btype,
                Shader_buffer::btype_name[btype], (int)btype
                );
            exit(2302);
        }
    }
    ++shader->check_counter;
#endif
    shader->buffers[index+1].btype = btype;
    return &shader->buffers[index+1].data;
}

template <typename T>
Array_dyn<T>* opengl_buffer(Shader* shader, s64 index, char const* name) {
    return (Array_dyn<T>*)_opengl_buffer_helper(shader, index, name, Shader_buffer_btype_value<T>);
}

template <typename T>
void opengl_buffer_append(Shader* shader, s64 index, char const* name, std::initializer_list<T> data) {
    auto arr = opengl_buffer<T>(shader, index, name);
    array_append(arr, data);
}
template <typename T>
void opengl_buffer_append(Shader* shader, s64 index, char const* name, Array_t<T> data) {
    auto arr = opengl_buffer<T>(shader, index, name);
    array_append(arr, data);
}

template <typename T>
void opengl_buffer_append_multi(
    Shader* shader,
    std::initializer_list<s64> indices_,
    std::initializer_list<char const*> names_,
    Array_t<T> data,
    s64 times=1
) {
    Array_t<s64> indices {(s64*)indices_.begin(), indices_.end() - indices_.begin()};
    Array_t<char*> names {(char**)names_.begin(), names_.end() - names_.begin()};
    u8 btype = Shader_buffer_btype_value<T>;
    s64 len = data.size / indices.size;
    
#ifndef NDEBUG
    assert(data.size % indices.size == 0);
    if ((shader->check_counter & 0xffff0) == 0) {
        for (s64 i = 0; i < indices.size; ++i) {
            _opengl_buffer_helper(shader, indices[i], names[i], btype);
        }
    } else {
        shader->check_counter += indices.size;
    }
#endif

    for (s64 i = 0; i < indices.size; ++i) {
        shader->buffers[indices[i]+1].btype = btype;
        auto arr = (Array_dyn<T>*)&shader->buffers[indices[i]+1].data;
        auto i_data = array_subarray(data, len*i, len*(i+1));
        array_reserve(arr, arr->size + i_data.size * times);
        for (s64 j = 0; j < times; ++j) {
            memcpy(arr->end(), i_data.data, i_data.size * sizeof(T));
            arr->size += i_data.size;
        }
    }
}

template <typename T>
void opengl_buffer_append_multi(
    Shader* shader,
    std::initializer_list<s64> indices,
    std::initializer_list<char const*> names,
    std::initializer_list<T> data,
    s64 times=1
) {
    opengl_buffer_append_multi<T>(shader, indices, names,
    Array_t<T> {(T*)data.begin(), data.end() - data.begin()}, times);
}

void opengl_shader_attrib_divisor(Shader* shader, s64 index, char const* name, s64 divisor) {
    _opengl_buffer_helper2(shader, index, name);
    shader->buffers[index+1].instanced = divisor != 0;
    glVertexAttribDivisor(index, divisor);
}

s32 _opengl_uniform_set_helper(Shader* shader, char const* name) {
#ifndef NDEBUG
    int data;
    glGetIntegerv(GL_CURRENT_PROGRAM, &data);
    assert(data == shader->id_program);
    assert(shader->compiled);
#endif
    
    for (s64 i = 0; i < shader->uniforms.size; ++i) {
        Shader_var uni = shader->uniforms[i];
        Array_t<u8> uni_name = array_subarray(shader->name_data, uni.name_beg, uni.name_end);
        if (array_equal_str(uni_name, name)) return shader->loc_uniforms[i];
    }
    return -1;
}

#define OOO(...) __VA_ARGS__
#define OO(x) OOO x
#define O(pre, pars, args) \
void opengl_uniform_set(Shader* shader, char const* name, OO(pars)) { \
    glUniform##pre(_opengl_uniform_set_helper(shader, name), OO(args)); \
}

O(1f, (float f1), (f1))
O(2f, (float f1, float f2), (f1, f2))
O(3f, (float f1, float f2, float f3), (f1, f2, f3))
O(4f, (float f1, float f2, float f3, float f4), (f1, f2, f3, f4))
O(1i, (s32 f1), (f1))
O(2i, (s32 f1, s32 f2), (f1, f2))
O(3i, (s32 f1, s32 f2, s32 f3), (f1, f2, f3))
O(4i, (s32 f1, s32 f2, s32 f3, s32 f4), (f1, f2, f3, f4))
O(1i, (u32 f1), (f1))
O(2i, (u32 f1, u32 f2), (f1, f2))
O(3i, (u32 f1, u32 f2, u32 f3), (f1, f2, f3))
O(4i, (u32 f1, u32 f2, u32 f3, u32 f4), (f1, f2, f3, f4))

#undef O

#define O(n, args) \
void opengl_uniform_set##n(Shader* shader, char const* name, u8 x[n]) { \
    float f[n]; for (s64 i = 0; i < n; ++i) f[i] = (float)x[i] / 255.f; \
    glUniform##n##f(_opengl_uniform_set_helper(shader, name), OO(args)); \
}

O(1, (f[0]));
O(2, (f[0], f[1]));
O(3, (f[0], f[1], f[2]));
O(4, (f[0], f[1], f[2], f[3]));

#undef O
#undef OO
#undef OOO

s32 opengl_uniform_getindex(Shader* shader, char const* name) {
    s32 index = _opengl_uniform_set_helper(shader, name);
    assert(index != -1);
    return index;
}

void opengl_shader_clear(Shader* shader) {
    for (s64 i = 0; i < shader->buffers.size; ++i) {
        shader->buffers[i].data.size = 0;
    }
}

void opengl_shader_use(Shader* shader) {
    assert(shader->compiled);
    glUseProgram(shader->id_program);
}

void opengl_shader_use_and_set_origin(Shader* shader, s64 screen_w, s64 screen_h) {
    opengl_shader_use(shader);
    float ox = screen_w / 2.f;
    float oy = screen_h / 2.f;
    float sx =  2.f / screen_w;
    float sy = -2.f / screen_h;
    opengl_uniform_set(shader, "origin", ox, oy);
    opengl_uniform_set(shader, "scale",  sx, sy);
}

void opengl_shader_buffers_copy(Shader* shader, bool use_indices=false) {
#ifndef NDEBUG
    int data;
    glGetIntegerv(GL_CURRENT_PROGRAM, &data);
    assert(data == shader->id_program);
    assert(shader->compiled);
#endif

    assert(use_indices or shader->buffers[0].data.size == 0); // Forgot to pass the flag?
    for (s64 i = use_indices ? 0 : 1; i < shader->buffers.size; ++i) {
        auto buf = shader->buffers[i];
        if (buf.btype == Shader_buffer::BTYPE_NONE) continue;
        s64 siz = Shader_buffer::btype_size[buf.btype];
        auto bp = i == 0 ? GL_ELEMENT_ARRAY_BUFFER : GL_ARRAY_BUFFER;
        glBindBuffer(bp, shader->id_buffers[i]);

        s64 size_ram = siz * buf.data.size;
        s64 size_gpu;
        glGetBufferParameteri64v(bp, GL_BUFFER_SIZE, (long*)&size_gpu);
        if (size_gpu < size_ram) {
            s64 size_gpu_new = max(size_ram, size_gpu + size_gpu/5);
            glBufferData(bp, size_gpu_new, nullptr, GL_STREAM_DRAW);
        }
        
        glBufferSubData(bp, 0, size_ram, buf.data.data);
    }
}

void opengl_shader_buffers_enable(Shader* shader, s64 vertex_offset=0, s64 inst_offset=0) {
    for (s64 i = 1; i < shader->buffers.size; ++i) {
        auto buf = shader->buffers[i];
        if (buf.btype == Shader_buffer::BTYPE_NONE) continue;
        s64 siz = Shader_buffer::btype_size[buf.btype];
        u32 typ = Shader_buffer::btype_type[buf.btype];
        s64 nor = Shader_buffer::btype_norm[buf.btype];
        s64 mult = Shader::type_mult[shader->attribs[i].type];
        s64 off = siz * (buf.instanced ? inst_offset : vertex_offset);
        glBindBuffer(GL_ARRAY_BUFFER, shader->id_buffers[i]);
        u8 conv = Shader::type_conv[shader->attribs[i].type];
        if (conv == 0) {
            glVertexAttribPointer(shader->loc_attribs[i], mult, typ, nor, 0, (void*)off);
        } else if (conv == 1) {
            glVertexAttribIPointer(shader->loc_attribs[i], mult, typ, 0, (void*)off);
        } else if (conv == 2) {
            glVertexAttribLPointer(shader->loc_attribs[i], mult, typ, 0, (void*)off);
        } else {
            assert(false);
        }
        glEnableVertexAttribArray(shader->loc_attribs[i]);
    }
}

void opengl_shader_draw(Shader* shader, u32 primitive, bool use_indices=false, bool debug_individual=false) {
    opengl_shader_buffers_copy(shader, use_indices);
    opengl_shader_buffers_enable(shader);
    
    if (use_indices) {
        assert(not debug_individual); // Not implemented (yet?)
        u8 btype = shader->buffers[0].btype;
        u32 typ = Shader_buffer::btype_type[btype];
        s64 bmult = Shader_buffer::btype_mult[btype];
        s64 count = shader->buffers[0].data.size * bmult;
        glDrawElements(primitive, count, typ, 0);
    } else {
        s64 mult = Shader::type_mult[shader->attribs[1].type];
        s64 bmult = Shader_buffer::btype_mult[shader->buffers[1].btype];
        s64 count = shader->buffers[1].data.size * bmult / mult;
    
        if (not debug_individual) {
            glDrawArrays(primitive, 0, count);
        } else {
            if (primitive == GL_TRIANGLE_STRIP) {
                for (s64 i = 0; i+2 < count; ++i)  glDrawArrays(primitive, i, 3);
            } else if (primitive == GL_TRIANGLES) {
                for (s64 i = 0; i+2 < count; i+=3) glDrawArrays(primitive, i, 3);
            } else {
                assert(false);
            }
        }
    }
}

void opengl_shader_drawinst(Shader* shader, u32 primitive, s64 inst_count) {
    assert((s32)inst_count == inst_count);
    opengl_shader_buffers_copy(shader, false);
    opengl_shader_buffers_enable(shader);

    s64 mult = Shader::type_mult[shader->attribs[1].type];
    s64 bmult = Shader_buffer::btype_mult[shader->buffers[1].btype];
    s64 count = shader->buffers[1].data.size * bmult / mult;
    glDrawArraysInstanced(primitive, 0, count, (s32)inst_count);
}

void opengl_shader_draw_and_clear(Shader* shader, u32 primitive, bool use_indices=false, bool debug_individual=false) {
    opengl_shader_draw(shader, primitive, use_indices, debug_individual);
    opengl_shader_clear(shader);
}

void opengl_shader_drawinst_and_clear(Shader* shader, u32 primitive, s64 inst_count) {
    opengl_shader_drawinst(shader, primitive, inst_count);
    opengl_shader_clear(shader);
}

void opengl_clear_color(Color col) {
    glClearColor(col.r/255.f, col.g/255.f, col.b/255.f, col.a/255.f);
}

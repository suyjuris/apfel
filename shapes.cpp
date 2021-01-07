


struct Shape_drawer {
    Shader circle, triangle, rectangle;
    float z_level_add = 0.f;
};

#ifdef ASSET_shape_circle_v
    #version 150 core
    in vec2 pos;
    in float z;
    in vec2 c;
    in float r;
    in vec4 fill;
    out vec2 v_pos;
    out float v_r;
    out vec4 v_fill;
    uniform vec2 origin;
    uniform vec2 scale;
    void main() {
        gl_Position = vec4((pos - origin)*scale, z, 1);
        v_pos = pos - c;
        v_r = r;
        v_fill = fill;
    }
#endif
    
#ifdef ASSET_shape_circle_f
    #version 150 core
    in vec2 v_pos;
    in float v_r;
    in vec4 v_fill;
    out vec4 color;
    void main() {
        vec4 col = v_fill;
        col.a *= clamp(v_r - length(v_pos) + 0.5, 0, 1);
        color = col;
    }
#endif

void shape_circle(Shape_drawer* shapes, Vec2 c, float r, Color fill, float z = 0.f) {
    z += shapes->z_level_add;
    float p = r + .5f;
    opengl_buffer_append<Vec2>(&shapes->circle, 0, "pos", {
        {c.x-p, c.y-p}, {c.x+p, c.y+p}, {c.x-p, c.y+p},
        {c.x-p, c.y-p}, {c.x+p, c.y-p}, {c.x+p, c.y+p}
    });
    
    for (s64 i = 0; i < 6; ++i) {
        opengl_buffer_append(&shapes->circle, 1, "z", {z});
        opengl_buffer_append(&shapes->circle, 2, "c", {c});
        opengl_buffer_append(&shapes->circle, 3, "r", {r});
        opengl_buffer_append(&shapes->circle, 4, "fill", {fill});
    }
}

#ifdef ASSET_shape_triangle_v
    #version 150 core
    in vec2 pos;
    in float alpha;
    in float z;
    in vec4 fill;
    out float v_alpha;
    out vec4 v_fill;
    uniform vec2 origin;
    uniform vec2 scale;
    void main() {
        gl_Position = vec4((pos - origin)*scale, vec2(z, 1));
        v_alpha = alpha;
        v_fill = fill;
    }
#endif

#ifdef ASSET_shape_triangle_f
    #version 150 core
    in float v_alpha;
    in vec4 v_fill;
    out vec4 color;
    void main() {
        vec4 col = v_fill;
        col.a *= min(v_alpha, 1);
        color = col;
    }
#endif

#ifdef ASSET_shape_rectangle_v
    #version 150 core
    in vec2 off;
    in vec2 p;
    in vec2 size;
    in float z;
    in vec4 fill;
    out vec2 v_off;
    out vec2 v_size;
    out vec4 v_fill;
    uniform vec2 origin;
    uniform vec2 scale;
    void main() {
        gl_Position = vec4((off + (p - origin))*scale, vec2(z, 1));
        v_off = off;
        v_size = size;
        v_fill = fill;
    }
#endif

#ifdef ASSET_shape_rectangle_f
    #version 150 core
    in vec2 v_off;
    in vec2 v_size;
    in vec4 v_fill;
    out vec4 color;
    void main() {
        vec4 col = v_fill;
        vec2 off2 = v_size - v_off;
        col.a *= min(min(min(v_off.x, off2.x), min(v_off.y, off2.y)), 1);
        color = col;
    }
#endif

void shape_triangle(Shape_drawer* shapes, Vec2 p0, Vec2 p1, Vec2 p2, Color fill, float z = 0.f) {
    z += shapes->z_level_add;

    // The way we enlarge the triangle is only mostly correct.
    Vec2 m = (p0 + p1 + p2) / 3.f;
    Vec2 arr[3] = {p0 - m, p1 - m, p2 - m};
    float alpha[9] = {};
    
    for (s64 i = 0; i < 3; ++i) {
        Vec2 q0 = arr[i], q1 = arr[(i+1)%3];
        Vec2 v = (q0 - q1).ortho().normalized();
        float f0 = max(std::abs(dot(q0, v)), 0.5f);
        float f = 1.f + .5f / f0;
        
        opengl_buffer_append<Vec2>(&shapes->triangle, 0, "pos", {m, m + q0*f, m + q1*f});
        opengl_buffer_append(&shapes->triangle, 1, "alpha", {f0, 0.f, 0.f});
    }
    
    for (s64 i = 0; i < 9; ++i) {
        opengl_buffer_append(&shapes->triangle, 2, "z", {z});
        opengl_buffer_append(&shapes->triangle, 3, "fill", {fill});
    }
}

void shape_rectangle(Shape_drawer* shapes, Vec2 p, Vec2 size, Color fill, float z = 0.f) {
    z += shapes->z_level_add;
    
    opengl_buffer_append<Vec2>(&shapes->rectangle, 0, "off", {
        {0.f, 0.f}, {size.x + 1.f, 0}, size + 1.f, {0.f, 0.f}, size + 1.f, {0, size.y + 1.f}
    });
    
    for (s64 i = 0; i < 6; ++i) {
        opengl_buffer_append(&shapes->rectangle, 1, "p", {p - .5f});
        opengl_buffer_append(&shapes->rectangle, 2, "size", {size + 1.f});
        opengl_buffer_append(&shapes->rectangle, 3, "z", {z});
        opengl_buffer_append(&shapes->rectangle, 4, "fill", {fill});
    }
}

void shape_arrowhead(Shape_drawer* shapes, Vec2 p, Vec2 tip_rel, Color fill, float z = 0.f) {
    //{c, -s, s, c}, where c = cos(2pi/3), s = sin(2pi/3)
    float q = 0.8660254037844387; // sqrt(3) / 2
    Mat2 m1 = {{-.5f, -q}, { q, -.5}};
    Mat2 m2 = {{-.5f,  q}, {-q, -.5}};
    shape_triangle(shapes, p+tip_rel, p + m1*tip_rel, p + m2*tip_rel, fill, z);
}

void shape_init(Shape_drawer* shapes, Asset_store* assets) {
    shapes->circle    = opengl_shader_assets(assets, "shape_circle"_arr);
    shapes->triangle  = opengl_shader_assets(assets, "shape_triangle"_arr);
    shapes->rectangle = opengl_shader_assets(assets, "shape_rectangle"_arr);
}

void shape_frame_draw(Shape_drawer* shapes, s64 screen_w, s64 screen_h) {
    opengl_shader_use_and_set_origin(&shapes->circle, screen_w, screen_h);
    opengl_shader_draw_and_clear(&shapes->circle, GL_TRIANGLES);

    opengl_shader_use_and_set_origin(&shapes->rectangle, screen_w, screen_h);
    opengl_shader_draw_and_clear(&shapes->rectangle, GL_TRIANGLES);
    
    opengl_shader_use_and_set_origin(&shapes->triangle, screen_w, screen_h);
    opengl_shader_draw_and_clear(&shapes->triangle, GL_TRIANGLES);
}


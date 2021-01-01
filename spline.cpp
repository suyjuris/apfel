
    
#ifdef ASSET_spline_v
    #version 150 core
    in vec2 pos;
    in vec2 p0;
    in vec2 p1;
    in vec2 p2;
    in float size;
    in vec4 stroke;
    uniform vec2 origin;
    uniform vec2 scale;
    
    out vec2 v_pos;
    out vec2 v_p1;
    out vec2 v_p2;
    out float v_size;
    out vec4 v_stroke;
    out vec3 v_v;
    out vec2 v_dg;
    
    void main() {
        gl_Position = vec4((pos - origin)*scale, vec2(0.05, 1));
        v_pos = pos-p0;
        v_p1 = p1-p0;
        v_p2 = p2-p0;
        v_size = size;
        v_stroke = stroke;
        
        // For comments on the awesome math that happens here, please see the note on spline 
        // rendering below. (In the file obst.cpp, in case you are reading this in your browser.)
        mat2 A = mat2(0.5*v_p2.y, -v_p1.y, -0.5*v_p2.x, v_p1.x) / (v_p1.x*v_p2.y-v_p1.y*v_p2.x);
        mat2 At = mat2(A[0][0], A[1][0], A[0][1], A[1][1]);
        vec2 Apos = A*v_pos;
        v_v.y = Apos.y;
        v_v.z = Apos.x + Apos.y;
        v_dg = At*vec2(-2.0*v_v.z, -2.0*v_v.z+1.0);
        v_v.x = dot(At[0]+At[1], v_dg);
    }
#endif

#ifdef ASSET_spline_f
    #version 150 core
    in vec2 v_pos;
    in vec2 v_p1;
    in vec2 v_p2;
    in float v_size;
    in vec4 v_stroke;
    in vec3 v_v;
    in vec2 v_dg;
    uniform float scale_p;
    out vec4 color;
    
    void main() {
        // For comments on the awesome math that happens here, please see the note on spline 
        // rendering below. (In the file obst.cpp, in case you are reading this in your browser.)
        float t = clamp(v_v.z-(v_v.y-v_v.z*v_v.z)*v_v.x/dot(v_dg, v_dg), 0.0, 1.0);
        float d = distance(mix(v_p1*t,mix(v_p1,v_p2,t),t), v_pos);
        float d2 = clamp((d - v_size) * scale_p + 0.5, 0.0, 1.0);
        vec4 col = v_stroke;
        col.a *= 1.0 - d2;
        
        if (d2 >= 1.0) {
            discard;
        } else {
            color = col;
        }
        // For debugging: This shows the control points. Comment the discards above out. 
        //if (length(v_pos)         < 3.1 * v_size) { color = vec4(1,0,0,1); };
        //if (distance(v_p1, v_pos) < 3.1 * v_size) { color = vec4(0,1,0,1); };
        //if (distance(v_p2, v_pos) < 3.1 * v_size) { color = vec4(0,0,1,1); };
    }
#endif

// Evaluates the spline.
Vec2 spline_get(Vec2 p0, Vec2 p1, Vec2 p2, float t) {
    return lerp(lerp(p0, p1, t), lerp(p1, p2, t), t);
}

// Any subcurve of a quadratic spline is once again a quadratic spline. (Not entirely obvious,
// but very useful.) This splits the spline at a specific t.
//  The three values returned replace the control point, so the two resulting splines are
// {p0, out[0], out[1]} and {out[1], out[2], p3}.
void spline_split_t(Vec2 p0, Vec2 p1, Vec2 p2, float t, Vec2 out[3]) {
    assert(0.f <= t and t <= 1.f);
    out[0] = lerp(p0, p1, t);
    out[2] = lerp(p1, p2, t);
    out[1] = lerp(out[0], out[2], t);
};

// Similar to spline_split_t, we can split a spline to get the subsegment from t0 to t1.
void spline_split_range(Vec2 p0, Vec2 p1, Vec2 p2, float t0, float t1, Vec2 out[3]) {
    if (t0 == 0.f and t1 == 1.f) {
        out[0] = p0; out[1] = p1; out[2] = p2;
    } else {
        out[0] = spline_get(p0, p1, p2, t0);
        out[2] = spline_get(p0, p1, p2, t1);
        out[1] = .5f * (out[0] + out[2] - (t1-t0)*(t1-t0) * (p0 - 2*p1 + p2));
    }
}

// Calculates the length of the quadratic spline with control points p0, p1 and p2.
float spline_length(Vec2 p0, Vec2 p1, Vec2 p2) {
    // In _spline_length_quadrature I mentioned that the term was difficult to simplify. Still, you
    // can solve it analytically and derive a closed form solution, see e.g. [1] for the formula, or
    // [2] for a derivation.
    //
    // Doing some basic measurements, this is about 6x faster than the above, but there is a large
    // margin of error due to running in the browser. So really it just is a matter of preference.
    //
    // [1] Sablonnière, Paul. "Some approximate methods for computing arc lengths based on quadratic
    //  and cubic spline interpolation or quasi-interpolation." Rend. Sem. Mat. Univ. Politec. Torino
    //  69.1 (2011): 1-20.
    //  https://pdfs.semanticscholar.org/5c80/812405993a9f01c762baf92c893917146201.pdf
    // [2] http://web.archive.org/web/20180831125226/http://www.malczak.linuxpl.com/blog/quadratic-bezier-curve-length/
    
    Vec2 d0 = {p1.x - p0.x, p1.y - p0.y};
    Vec2 d1 = {p2.x - p1.x, p2.y - p1.y};

    if (std::abs(d0.x*d1.y - d0.y*d1.x) < 0.001) {
        // Points colinear, the formula below does not work there
        Vec2 d2 = {p2.x - p0.x, p2.y - p0.y};
        return std::sqrt(d2.x*d2.x + d2.y*d2.y);
    }
    
    Vec2 dd = {d1.x - d0.x, d1.y - d0.y};
    float a0 = d0.x*d0.x + d0.y*d0.y;
    float a1 = d0.x*d1.x + d0.y*d1.y;
    float a2 = d1.x*d1.x + d1.y*d1.y;
    float da0 = d0.x*dd.x + d0.y*dd.y;
    float da1 = d1.x*dd.x + d1.y*dd.y;
    float dda = dd.x*dd.x + dd.y*dd.y;
    return (a0*a2-a1*a1) / std::sqrt(dda*dda*dda) * std::log(std::abs((da1 + std::sqrt(a2*dda)) / (da0 + std::sqrt(a0*dda))))
        + (da1*std::sqrt(a2) - da0*std::sqrt(a0)) / dda;
}

// Calculates the length of a spline in the interval [0, t]
float spline_length_at(Vec2 p0, Vec2 p1, Vec2 p2, float t) {
    // Split the spline and calculate the length of the subspline
    Vec2 p1_ = lerp(p0, p1, t);
    Vec2 p2_ = lerp(p1_, lerp(p1, p2, t), t);
    return spline_length(p0, p1_, p2_);
}

// Calculates coefficients for an approximation of the t -> length mapping used in the edge fragment
// shader to draw gaps of the same size.
void spline_length_approx(Vec2 p0, Vec2 p1, Vec2 p2, float* f1, float* f2) {
    // Note on the spline length approximation: The speed we traverse the spline with varies
    // depending on t and the control points, so the length is not linear in terms of t. But we need
    // an estimate for the length depending on t to draw accurate gaps in the spline. The solution I
    // chose was to approximate the t -> length function with a third-degree interpolation
    // polynomial. This thing is normalised, so that the total length of the curve is 1 (hence both
    // functions map [0,1] -> [0,1]).
    //  I choose four equidistant points to interpolate, t = 0, 1/3, 2/3, 1. The lengths are
    // calculated by numerical integration, as in spline_length. Let f be out interpolation polynomial:
    //     f(x) = f3 * x**3 + f2 * x**2 + f1 * x + f0
    //     f(0) = 0  =>  f0 = 0
    //     f(1) = 1  =>  f3 = 1 - f1 - f2
    //     f(1/3) = l1
    //     f(2/3) = l2
    // where l1, l2 are the lengths at t = 1/3 and t = 2/3.
    //  The only thing left to do is to calculate f1, f2 as some affine function of l1, l2, the
    // constants of which you can derive on paper without too much difficulty.

    float l = spline_length(p0, p1, p2);
    float l1 = spline_length_at(p0, p1, p2, 1.f/3.f) / l;
    float l2 = spline_length_at(p0, p1, p2, 2.f/3.f) / l;
    *f1 =    9.f*l1 -  4.5f*l2 + 1.f;
    *f2 = -22.5f*l1 + 18.f *l2 - 4.5f;
}

// Determine how ofter the spline intersects the horizontal line at height y. Return the number of
// intersections (can be 0-2), and write the values into t_out.
// Warning: t_out can always be written to, regardless of result! It must contain valid memory.
s64 spline_hit_y(Vec2 p0, Vec2 p1, Vec2 p2, float y, float t_out[2]) {
    float a = p0.y - 2.f*p1.y + p2.y;
    float b = 2*p1.y - 2*p0.y;
    float c = p0.y - y;
    float r = b*b - 4*a*c;
    if (r < 0) {
        return 0;
    } else if (r == 0) {
        t_out[0] = -.5f * b/a;
        return 0.f <= t_out[0] and t_out[0] <= 1.f;
    } else {
        // see https://people.csail.mit.edu/bkph/articles/Quadratics.pdf
        float rs = std::sqrt(r);
        if (b >= 0) {
            t_out[0] = -.5f * (b+rs) / a;
            t_out[1] = -2.f * c / (b+rs);
        } else {
            t_out[0] = -.5f * (b-rs) / a;
            t_out[1] = -2.f * c / (b-rs);
        }
        if (0.f <= t_out[0] and t_out[0] <= 1.f) {
            return 1 + (0.f <= t_out[1] and t_out[1] <= 1.f);
        } else {
            t_out[0] = t_out[1];
            return 0.f <= t_out[0] and t_out[0] <= 1.f;
        }
    }
}

// Draw a quadratic spline. Units are in pixels.
void spline_draw(Shader* shader, Vec2 p0, Vec2 p1, Vec2 p2, float size, u8* stroke) {
    
    // Note on spline rendering: This is the mathematically most involved part of the
    // program. Basically, the problem is finding the t s.t. the corresponding point on the spline
    // (p0, p1, p2) is closest to p. The technique I use is based on [1, section 4.4], although I
    // have made some significant adjustments to generally simplify the formula and put as much work
    // as possible into the vertex shader. See also the references in [1].
    //  I will not derive the detailed formulae here, but just give a rough overview on how it works
    // and what steps one has to do to end up at my solution.    
    //  Solving the problem exactly requires you to solve third-degree polynomials, which uses cubic
    // roots and all sorts of nastiness. Instead, we proceed by inversion, taking a formula that
    // yields t exactly for a point p _on the curve_. However, the formula is continuous everywhere,
    // and we can get a good enough approximation to t as long as we are close to the curve. For
    // rendering thin lines that is all we need, as points farther away are not drawn anyway.
    //    
    //  The basic steps are as follows:
    //     1. Take a function f that has f(x) = 0 iff x is on the curve
    //     2. Do a first-order approximation of f based on p: f1(x) := f(p) + Df(p)x
    //     3. The set f1 = 0 forms a line
    //     4. Find the closest point to p on that line, p' := p - f(p) Df(p) / dot(Df(p), Df(p))
    //        (D is the total derivative, or, in this case, the transposed gradient.)
    //     5. Apply the inversion formula to p' to get t
    //
    // Regarding f: Look at the paper to get the precise definition, but basically
    //     f(x) = b(x) d(x) + a(x)**2
    // with a, b, d being affine functions mapping into the real numbers.
    //  The inversion formula in step 5 is also in the paper, but it ends up being much simpler in
    // my version, so I will not give the general form here.
    //
    //  Mainly, I did one thing to simplify the problem: Solve it for the spline ((0,0), (1/2,0),
    // (0,1)) (which I will call 'base spline' in the following) by hand and then do a linear
    // transformation to apply that result to the general case. f becomes the function
    //     f(x,y) = y - (x+y)**2
    // and
    //     Df(x,y) = (-2(x+y), 1-2(x+y))
    // Let h be the function mapping t to the position of the point on the curve, i.e.
    //     h(t) = (t(1-t), t**2)
    // It is not difficult to see that, for any point (x,y) on the curve, x+y = t. As a matter of
    // fact, the inversion formula in the paper reduces to precisely this.
    //
    //  Great, so we now know how to solve the problem for the base spline. To map any general
    // spline, first apply a translation to get p0 = 0 (this does not affect any calculations, so
    // just assume this was already the case). Then, choose a matrix A s.t. A p1 = (1/2, 0) and A p2
    // = (0, 1). Let fA be the function f for the base spline, and fG the general one from above, for
    // the spline we are interested in.) Conveniently,
    //     det(A)**2 fA(A x) = fG(x)
    // (This holds due to the special structure of a, b and d.) To get p', the constant does not
    // matter. Additionally, it follows that
    //     DfG(x) = det(A)**2 DfA(A x) A 
    // Finally, to get t we calculate A p' via
    //     A p' = A p - A fA(A p) (DfA(A x) A) / dot(DfA(A x) A, DfA(A x) A)
    // and sum its coordinates.
    //     t = (1 1) A p'
    //       = (1 1) A p - (1 1) A fA(A p) (DfA(A x) A) / dot(DfA(A x) A, DfA(A x) A)
    //       = (1 1) A p - fA(A p) ((1 1) A (DfA(A x) A)) / dot(DfA(A x) A, DfA(A x) A)
    // Letting
    //     (x,y) := A p
    //     z := x + y
    //     dg := transpose(DfA(A x) A) = transpose(A) (-2z, 1-2z)
    //     w := (1 1) A dg
    // the above term becomes
    //       = (x+y) - (y - (x+y)**2) ((1 1) A dg) / dot(dg, dg)
    //       = z - (y - z**2) w / dot(dg, dg)
    // Note that all the constants I introduced are linear functions of p and can thus be calculated
    // in the vertex shader and then linearly interpolated. So the last line really is the only
    // thing the fragment shader has to do to get t.
    //
    //  One small wrinkle of this technique (also present in the original version): If p0, p1, p2 are
    // colinear, things are not well-defined. As a small exercise, you may want to look at the
    // description above and find out where exactly things break down, as it is a bit implicit. I'll
    // wait.
    //  Back again? Great. As you no doubt noticed, the A is not well-defined in that case. Fixing
    // the problem is easy, however, it suffices to add a small perturbation to p1 so that the
    // points are not colinear anymore. (Also, move it to the middle of p0, p2 beforehand, the
    // approximation gets worse the closer p1 is to one of them.)
    //
    // [1] http://research.microsoft.com/en-us/um/people/hoppe/ravg.pdf
    // Nehab, Diego, and Hugues Hoppe. "Random-access rendering of general vector graphics."
    // ACM Transactions on Graphics (TOG) 27.5 (2008): 135.
    //
    // After finding a t, the rest is quite simple. We get the distance from the point at t, decide
    // whether to render or discard (points near the edges are antialiased). Then we do some scaling
    // and use the approximation to get the length of the curve until t and use that to decide
    // whether to make a gap or not.

    Vec2 v = (p2 - p0).normalized();
    Vec2 u = v.ortho();
    
    float d = dot(p1 - p0, u);
    if (std::abs(d) < 0.001) {
        // The approximation for the bezier curve is not well defined if the points are colinear. So
        // perturb them a bit. Also put p1 into the middle, as the approximation work better
        // then. (It is bad mostly if p1 is near p0 or p2.)
        p1.x = 0.5f*(p0.x + p2.x) + u.x * 0.004;
        p1.y = 0.5f*(p0.y + p2.y) + u.y * 0.004;
    } else if (d < 0) {
        u = -u;
    }

    float f = 2.f*size + 2.f;
    u *= f; v *= f;

    // We want a triangle enclosing the curve on all sides. I am going to be lazy here and assume
    // the curve is 'nice', i.e. p1 is 'between' p0 and p2.

    opengl_buffer_append(shader, 0, "pos", {
        p1 + u,
        p1 + u,
        p0 + u - v,
        p2 + u + v,
        p0 - u - v,
        p2 - u + v,
        p2 - u + v,
    });
    for (s64 i = 0; i < 7; ++i) {
        opengl_buffer_append(shader, 1, "p0", {p0});
        opengl_buffer_append(shader, 2, "p1", {p1});
        opengl_buffer_append(shader, 3, "p2", {p2});
        opengl_buffer_append(shader, 4, "size", {size});
        opengl_buffer_append<u8>(shader, 5, "stroke", {stroke, 4});
    }
}

void spline_frame_draw(Shader* shader, s64 screen_w, s64 screen_h) {
    opengl_shader_use_and_set_origin(shader, screen_w, screen_h);
    opengl_uniform_set(shader, "scale_p", 1.f);
    opengl_shader_draw_and_clear(shader, GL_TRIANGLE_STRIP);
}

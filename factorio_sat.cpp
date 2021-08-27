
namespace Factorio {

enum Types_factorio: u64 {
    VAR_FACTORIO        = 0x18ull << 56,
    GROUP_FACTORIO      = 0x28ull << 56,
    CONSTRAINT_FACTORIO = 0x38ull << 56,
    PARAM_FACTORIO      = 0x48ull << 56,
};

enum Groups_factorio: u64 {
    line_empty  = GROUP_FACTORIO | 0x20ull << 16,
    line_full   = GROUP_FACTORIO | 0x20ull << 16 | 1,
    lines_empty = GROUP_FACTORIO | 0x30ull << 16,
};

enum Constraints_factorio: u64 {
    CONSTRAINT_FACTORIO_BEGIN = CONSTRAINT_FACTORIO,
    field_basic,
    field_underground,
    field_border,
    field_unary,
    field_border_unary,
    line_at_most,
    line_equal_half,
    line_equal_const,
    line_equal,
    lines_equal,
    lines_equal_half,
    line_cyclic_bipart,
    line_is_fraction,
    lines_are_fraction,
};

enum Params_factorio: u64 {
    PARAM_FACTORIO_BEGIN = PARAM_FACTORIO,
    fpar_nx, fpar_ny, fpar_n_under,
    fpar_n_lines, fpar_n_linelen, fpar_n_linedim,
    fpar_do_blocking,
    fpar_s_input, fpar_s_output,
    PARAM_FACTORIO_END
};

}


struct Dir {
    enum Dir_values: u8 {
        INVALID = 63, TOP, RIGHT, BOTTOM, LEFT, ALL, DIR_VALUES_SIZE,
        BEG = TOP, END = ALL,
    };
    enum Field_var_type: u64 {
        FVAR_DIR_MASK = 0xffull << 16,
    };
    
    s64 x, y; u8 dir;
    u64 inp, out, sid, und;
    u64 sum_lines_all, sum_lines_item, sum_line_sum, sum_line_block, sum_line_first, sum_lines_most;

    Dir() = default;
    Dir(s64 x, s64 y, u8 dir);
    static Dir from_lit(u64 lit);

    Dir move(s64 dx = 0, s64 dy = 1) {
        assert(BEG <= dir and dir < END);
        s64 mapx[] = {dx, dy, -dx, -dy};
        s64 mapy[] = {dy, -dx, -dy, dx};
        return {x+mapx[dir-BEG], y+mapy[dir-BEG], dir};
    }
    Dir turnl() {
        u8 map[] = {LEFT, TOP, RIGHT, BOTTOM, ALL};
        return {x, y, map[dir - BEG]};
    }
    Dir turnr() {
        u8 map[] = {RIGHT, BOTTOM, LEFT, TOP, ALL};
        return {x, y, map[dir - BEG]};
    }
    Dir back() {
        u8 map[] = {BOTTOM, LEFT, TOP, RIGHT, ALL};
        return {x, y, map[dir - BEG]};
    }
};

struct Field {
    static constexpr s64 COORD_MASK = 0xffffffffull << 24;
    static constexpr s64 COORD_OFFSET = 32786;
    enum Field_var_type: u64 {
        // Adjust line constants and dirs if you change these
        FVAR_TYPE_MASK = 0xf0ull << 16,
        FVAR_NORMAL    = 0x10ull << 16,
        FVAR_LINE      = 0x20ull << 16,
        FVAR_LINEGROUP = 0x30ull << 16,
        FVAR_DIR       = 0x40ull << 16,
        FVAR_LINEEL    = 0x50ull << 16,
    };
    enum Field_modifier_mask: u64 {
        FVAR_MOD_MASK = 0xfull << 16,
        FVAR_SUMX     = 0x1ull << 16,
        FVAR_SUMY     = 0x2ull << 16,
        FVAR_CYCLE    = 0x3ull << 16,
    };
    enum Field_border_type: u8 {
        BORDER_EMPTY, BORDER_OUT, BORDER_INP
    };
    
    s64 x, y;
    u64 empty, belt;
    u64 split, splitl, splitr;
    u64 under, underh, underv;
    u64 lines_all, lines_item, line_first, line_sum, line_block;
    u64 lines_most; // everything but line_block
    u64 line_cyclic;

    Dir dirs[4];
    Dir dir_all;

    Field(s64 x, s64 y);
    Field(Dir d): Field::Field(d.x, d.y) {};
    static Field from_lit(u64 lit);

    bool inbounds(Sat_instance* inst) {
        s64 nx = hashmap_get(&inst->params, Factorio::fpar_nx);
        s64 ny = hashmap_get(&inst->params, Factorio::fpar_ny);
        return 0 <= x and x < nx and 0 <= y and y < ny;
    }
};

void factorio_field_init_base(Field* f) {
    s64 add = Field::COORD_OFFSET;
    assert(0 <= f->x+add and f->x < 65536);
    assert(0 <= f->y+add and f->y < 65536);

    u64 base_orig = (u64)(f->x + add) << 40 | (u64)(f->y + add) << 24;
    u64 i = 0;
    {u64 base = base_orig | Factorio::VAR_FACTORIO | Field::FVAR_NORMAL;
    f->empty  = base+ ++i; f->belt   = base+ ++i;
    f->split  = base+ ++i; f->splitl = base+ ++i; f->splitr = base+ ++i;
    f->under  = base+ ++i; f->underh = base+ ++i; f->underv = base+ ++i;}
    
    {u64 base = base_orig | Factorio::GROUP_FACTORIO | Field::FVAR_LINEGROUP;
    f->lines_all  = base+ ++i; f->lines_item = base+ ++i; f->lines_most = base+ ++i;}
    
    {u64 base = base_orig | Factorio::GROUP_FACTORIO | Field::FVAR_LINE;
    f->line_first  = base;
    f->line_sum    = base | 0xfe00;
    f->line_block  = base | 0xff00;
    f->line_cyclic = base | 0xfd00 | Field::FVAR_CYCLE;}
}

Field::Field(s64 x, s64 y): x{x}, y{y} {
    factorio_field_init_base(this);
        
    for (u8 d = Dir::BEG; d < Dir::END; ++d) {
        dirs[d - Dir::BEG] = Dir {x, y, d};
    }
    dir_all = Dir {x, y, Dir::ALL};
}

Field Field::from_lit(u64 lit) {
    u64 subtype, suffix;
    sat_decompose(lit, nullptr, &subtype, &suffix);
    assert(subtype == Factorio::VAR_FACTORIO or subtype == Factorio::GROUP_FACTORIO);
    s64 x = (suffix >> 40 & 0xffff) - COORD_OFFSET;
    s64 y = (suffix >> 24 & 0xffff) - COORD_OFFSET;
    return {x, y};
}

Dir::Dir(s64 x, s64 y, u8 dir): x{x}, y{y}, dir{dir} {
    assert(Dir::BEG <= dir and dir < Dir::DIR_VALUES_SIZE);
    s64 add = Field::COORD_OFFSET;
    
    u64 base_orig = (u64)(x + add) << 40 | (u64)(y + add) << 24 | (u64)dir << 16;
    u64 base = base_orig | (dir == Dir::ALL ? Factorio::GROUP_FACTORIO : Factorio::VAR_FACTORIO);
    inp = ++base; out = ++base; sid = ++base;

    Field* f  = (Field*)alloca(sizeof(Field)); // Avoid initialisation
    Field* f2 = (Field*)alloca(sizeof(Field));
    
    f->x = x; f->y = y;
    factorio_field_init_base(f);
    
    s64 d = dir == Dir::TOP or dir == Dir::RIGHT ? 0 : -1;
    if (dir == Dir::TOP or dir == Dir::BOTTOM) {
        und = f->underv;
        f2->x = x; f2->y = y+d;
        factorio_field_init_base(f2);
        sum_lines_all  = f2->lines_all  | Field::FVAR_SUMY;
        sum_lines_item = f2->lines_item | Field::FVAR_SUMY;
        sum_line_sum   = f2->line_sum   | Field::FVAR_SUMY;
        sum_line_block = f2->line_block | Field::FVAR_SUMY;
        sum_line_first = f2->line_first | Field::FVAR_SUMY;
        sum_lines_most = f2->lines_most | Field::FVAR_SUMY;
    } else if (dir == Dir::LEFT or dir == Dir::RIGHT) {
        und = f->underh;
        f2->x = x+d; f2->y = y;
        factorio_field_init_base(f2);
        sum_lines_all  = f2->lines_all  | Field::FVAR_SUMX;
        sum_lines_item = f2->lines_item | Field::FVAR_SUMX;
        sum_line_sum   = f2->line_sum   | Field::FVAR_SUMX;
        sum_line_block = f2->line_block | Field::FVAR_SUMX;
        sum_line_first = f2->line_first | Field::FVAR_SUMX;
        sum_lines_most = f2->lines_most | Field::FVAR_SUMX;
    } else {
        und = 0;
        sum_lines_all  = 0;
        sum_lines_item = 0;
        sum_line_sum   = 0;
        sum_line_block = 0;
        sum_line_first = 0;
        sum_lines_most = 0;
    }

}

Dir Dir::from_lit(u64 lit) {
    u64 subtype, suffix;
    sat_decompose(lit, nullptr, &subtype, &suffix);
    assert(subtype == Factorio::VAR_FACTORIO or subtype == Factorio::GROUP_FACTORIO);
    s64 x = (suffix >> 40 & 0xffff) - Field::COORD_OFFSET;
    s64 y = (suffix >> 24 & 0xffff) - Field::COORD_OFFSET;
    u8 dir = suffix >> 16 & 0xff;
    return {x, y, dir};
}

void factorio_expand(Sat_instance* inst, u64 ovar, Array_dyn<u64>* out_lits) {
    assert((ovar & Sat::MASK_SUBTYPE) == Factorio::GROUP_FACTORIO);

    u64 ftype = ovar &  Field::FVAR_TYPE_MASK;
    u64 var   = ovar & ~Field::FVAR_TYPE_MASK;
    if (ftype == Field::FVAR_LINEGROUP) {
        s64 n_linedim = hashmap_get(&inst->params, Factorio::fpar_n_linedim);
        bool do_blocking = hashmap_get(&inst->params, Factorio::fpar_do_blocking);
        
        u64 mod = var & Field::FVAR_MOD_MASK;
        assert(mod == 0 or mod == Field::FVAR_SUMX or mod == Field::FVAR_SUMY);
            
        if (var & Field::COORD_MASK) {
            // A normal linegroup, either lines_all or lines_item
            Field f = Field::from_lit(ovar);
            for (s64 i = 0; i < n_linedim; ++i) {
                array_push_back(out_lits, f.line_first | mod | i << 8);
            }

            if ((ovar^mod) == f.lines_all) {
                array_push_back(out_lits, f.line_sum | mod);
                if (do_blocking) array_push_back(out_lits, f.line_block | mod);
            } else if ((ovar^mod) == f.lines_most) {
                array_push_back(out_lits, f.line_sum | mod);
            } else {
                assert((ovar^mod) == f.lines_item);
            }
        } else {
            // This is the special empty linegroup (called lines_empty)
            for (s64 i = 0; i < n_linedim; ++i) {
                array_push_back(out_lits, Factorio::line_empty);
            }
            array_push_back(out_lits, Factorio::line_empty);
            if (do_blocking) array_push_back(out_lits, Factorio::line_full);
        }
    } else if (ftype == Field::FVAR_LINE) {
        s64 len = hashmap_get(&inst->params, Factorio::fpar_n_linelen);
        switch (var & Field::FVAR_MOD_MASK) {
        case 0: break;
        case Field::FVAR_CYCLE:
        case Field::FVAR_SUMY:
        case Field::FVAR_SUMX: len *= 2; break;
        default: assert(false);
        }

        if (var & Field::COORD_MASK) {
            u64 vvar = (var & ~Sat::MASK_SUBTYPE) | Factorio::VAR_FACTORIO | Field::FVAR_LINEEL;
            for (s64 i = 0; i < len; ++i) {
                array_push_back(out_lits, vvar | (i+1));
            }
        } else {
            // This is either line_empty or line_full
            u64 lit;
            if      (ovar == Factorio::line_empty) lit = Sat::var_false;
            else if (ovar == Factorio::line_full ) lit = Sat::var_true;
            else assert(false);
            
            for (s64 i = 0; i < len; ++i) {
                array_push_back(out_lits, lit);
            }
        }
    } else if (ftype == Field::FVAR_DIR) {
        assert((ovar & Dir::FVAR_DIR_MASK) == (Dir::ALL << 16ull));
        u64 vvar = (ovar & ~Sat::MASK_SUBTYPE & ~Dir::FVAR_DIR_MASK) | Factorio::VAR_FACTORIO;
        for (s64 d = Dir::BEG; d < Dir::END; ++d) {
            array_push_back(out_lits, vvar | d << 16);
        }
    } else {
        assert(false);
    }
    
}

void factorio_rewrite(Sat_instance* inst, u64 op, Array_t<u64> args) {
    using namespace Sat;
    using namespace Factorio;
    switch (op) {
        
    case field_basic: {
        assert(args.size == 2);
        Field f {(s64)args[0], (s64)args[1]};
        
        // *** Basics *** /

        // Exactly one type
        sat_add(inst, exactly_one, {f.empty, f.belt, f.split, f.under});
                
        // If splitter, either left or right, ...
        sat_push_add_pop(inst, {f.split},  exactly_one, { f.splitl,  f.splitr});
        // ... else neither.
        sat_push_add_pop(inst, {~f.split}, logical_and, {~f.splitl, ~f.splitr});
        
        // If underground, exactly one direction
        sat_push_add_pop(inst, {f.under},  exactly_one, { f.underh,  f.underv});
        // ... else none.
        sat_push_add_pop(inst, {~f.under}, logical_and, {~f.underh, ~f.underv});

        for (Dir fd: f.dirs) {
            // Input and output not on same side (sideflag as well)
            sat_add(inst, at_most_one, {fd.inp, fd.out, fd.sid});

            // Sideflags
            // A belt has sides everywhere it does not have inputs or outputs
            sat_push(inst, {f.belt});
            sat_addg(inst, implies, {~fd.inp, ~fd.out}, {fd.sid});
            sat_push_add_pop(inst, {fd.sid}, logical_and, {~fd.inp, ~fd.out});
            sat_pop(inst);
            // An underground belt has sides either horizontally or vertically
            sat_push_add_pop(inst, {f.under}, equivalent, {fd.turnl().und, fd.sid});
            // Empty tiles and splitters have no sides
            sat_add(inst, implies, {f.empty, ~fd.sid});
            sat_add(inst, implies, {f.split, ~fd.sid});
            
            // Splitter flag causes straight flow
            sat_push_add_pop(inst, {f.split}, equivalent, {fd.back().inp, fd.out});


            // If outputting in this direction ...
            sat_push(inst, {fd.out}); {
                
                // ... left side of splitter means right side to the right, and vice versa
                sat_add(inst, implies, {f.splitl, Field {fd.move( 1, 0)}.splitr});
                sat_add(inst, implies, {f.splitr, Field {fd.move(-1, 0)}.splitl});
                
                // ... outputs have to match as well
                sat_add(inst, implies, {f.splitl, fd.move( 1, 0).out});
                sat_add(inst, implies, {f.splitr, fd.move(-1, 0).out});
                
                // ... a splitter must have at least one input and at least one output connected
                sat_push(inst, {f.splitl});
                sat_add(inst, clause, {fd.move().back().inp, fd.move(1, 1).back().inp});
                sat_add(inst, clause, {fd.move(0, -1).out,   fd.move(1, -1).out}); 
                // ... a splitter must have at least three (input or output) connected
                sat_add(inst, at_most_one, {~fd.move().back().inp, ~fd.move(1, 1).back().inp,
                                            ~fd.move(0, -1).out,   ~fd.move(1, -1).out}); 
                sat_pop(inst);

                // ... there cannot be two subsequent splitters
                sat_addg(inst, implies, {f.splitl, fd.move().back().inp}, {~Field {fd.move()}.splitl});

            }; sat_pop(inst);

            // Non-splitters must have input and output connected
            // This is not an equivalence, as the other tile may be a splitter
            sat_push_add_pop(inst, {~f.split, ~f.empty}, implies, {fd.inp, fd.move().back().out});
            sat_push_add_pop(inst, {~f.split, ~f.empty}, implies, {fd.out, fd.move().back().inp});

            // For underground lines, input/output direction determines underground direction
            sat_push(inst, {f.under});
            sat_add(inst, implies, {fd.inp, fd.und});
            sat_add(inst, implies, {fd.out, fd.und});
            if (fd.dir == Dir::TOP or fd.dir == Dir::RIGHT)
                sat_addg(inst, implies, {fd.und}, {fd.inp, fd.out, fd.back().inp, fd.back().out});
            sat_pop(inst);
        }        

        // There is alwas at most one input and at most one output
        sat_add(inst, at_most_one, {f.dir_all.inp});
        sat_add(inst, at_most_one, {f.dir_all.out});
        
        // Belts and Splitters must have at least one input and at least one output
        sat_add(inst, implies, {f.belt, f.dir_all.inp});
        sat_add(inst, implies, {f.belt, f.dir_all.out});
        sat_add(inst, implies, {f.split, f.dir_all.inp});
        sat_add(inst, implies, {f.split, f.dir_all.out});

        // Underground belts have exactly one input or output
        sat_push_add_pop(inst, {f.under}, exactly_one, {f.dir_all.inp, f.dir_all.out});

        // Empty lines have neither input nor output
        sat_push_add_pop(inst, {f.empty}, logical_and, {~f.dir_all.inp, ~f.dir_all.out});
        
        // *** Line control ***

        // Empty tiles have empty lines
        sat_push_add_pop(inst, {f.empty}, lines_equal, {f.lines_all, lines_empty});

        for (Dir fd: f.dirs) {
            // Input determines the line, except from splitter
            sat_push_add_pop(inst, 
                {fd.inp, fd.move().back().out, ~Field {fd.move()}.split},
                lines_equal, {f.lines_all, Field {fd.move()}.lines_all}
            );
            // No matching output means empty line
            sat_push_add_pop(inst, 
                {fd.inp, ~fd.move().back().out},
                lines_equal, {f.lines_all, lines_empty}
            );
        }
    } break;

    case field_underground: {
        assert(args.size == 2);
        Field f {(s64)args[0], (s64)args[1]};
        s64 nx = hashmap_get(&inst->params, Factorio::fpar_nx);
        s64 ny = hashmap_get(&inst->params, Factorio::fpar_ny);
        s64 n_under = hashmap_get(&inst->params, Factorio::fpar_n_under);

        // TODO Underground belt quadratic variables
        
        // Handle outgoing underground (incoming handled via standard input handling above)
        for (Dir fd: f.dirs) {
            // Search for matching underground
            for (s64 w = 2; w < n_under+2; ++w) {
                // w is the width of the whole underground segment, so move w-1 tiles
                Dir fd2 = fd.move(0, w-1);
                Field f2 {fd2};

                // Condition on
                // (a) f being underground in direction fd
                sat_push(inst, {f.under, fd.back().out});
                // (b) traversed tiles not having underground with the same orientation
                for (s64 i = 1; i < w-1; ++i) {
                    sat_push_amend(inst, ~fd.move(0, i).und);
                }

                bool dobreak = false;
                if (f2.inbounds(inst) and w < n_under+1) {
                    // A connection can exist here, so we also condition on
                    // (c) target tile (i.e. f2) being underground with same orientation
                    sat_push_amend(inst, fd2.und);
                    
                    if (w > 2) {
                        // There must be a connection (no useless tiles)
                        sat_add(inst, clause, {fd2.inp});

                        // The lines are now connected
                        sat_add(inst, lines_equal, {f.lines_all, f2.lines_all});
                    } else {
                        // @Redundant Length 2 would be possible, but not sensible, forbid it 
                        sat_add(inst, clause, {});
                    }
                } else {
                    // This connection is not possible, forbid it
                    sat_add(inst, clause, {});

                    dobreak = true; // early break to avoid borders
                }
                
                sat_pop(inst);
                if (dobreak) break;
            }

            // The above is only for outputs, for inputs we only force a matching output tile to
            // exist. (Else unconnected inputs would be possible.)
            s64 index = inst->rewrite_temp.size;
            for (s64 w = 2; w < n_under+1; ++w) {
                Dir fd2 = fd.move(0, w-1);
                Field f2 {fd2};
                if (not f2.inbounds(inst)) break;
                array_push_back(&inst->rewrite_temp, fd2.und);
            }
            sat_addg(inst, implies, {f.under, fd.back().inp}, array_subarray(inst->rewrite_temp, index));

            // Still, it is possible that two input underground belts face each other. Forbid this as well.
            // Search for matching underground
            for (s64 w = 2; w < n_under+1; ++w) {
                // see above
                Dir fd2 = fd.move(0, w-1);
                Field f2 {fd2};
                if (not f2.inbounds(inst)) break;

                sat_push(inst, {f.under, fd.back().inp});
                for (s64 i = 1; i < w-1; ++i) {
                    sat_push_amend(inst, ~fd.move(0, i).und);
                }
                sat_push_amend(inst, fd2.und);
                sat_add(inst, clause, {fd2.out});
                sat_pop(inst);
            }
            
            // @Redundant Forbid unused space after underground
            sat_addg(inst, implies, {f.under, fd.back().out, Field {fd.move()}.empty},
                {fd.move(-1, 1).turnr().out, fd.move(1, 1).turnl().out});
        }
    } break;

    case field_border: {
        assert(args.size == 2);
        Field f {(s64)args[0], (s64)args[1]};
        s64 nx = hashmap_get(&inst->params, Factorio::fpar_nx);
        s64 ny = hashmap_get(&inst->params, Factorio::fpar_ny);
        
        // No splitters, no underground
        sat_add(inst, logical_and, {~f.split, ~f.splitl, ~f.splitr, ~f.under, ~f.underh, ~f.underv});

        // Line control for normal belts
        for (Dir fd: f.dirs) {
            if (not Field {fd.move()}.inbounds(inst)) continue;

            sat_push_add_pop(inst, 
                {fd.inp, fd.move().back().out, ~Field {fd.move()}.split},
                lines_equal, {f.lines_all, Field {fd.move()}.lines_all}
            );
            sat_push_add_pop(inst, 
                {f.belt, fd.inp, ~fd.move().back().out},
                lines_equal, {f.lines_all, lines_empty}
            );
        }
    } break;
        
    case lines_equal:
    case lines_equal_half: {
        assert((args[0] & Field::FVAR_TYPE_MASK) == Field::FVAR_LINEGROUP);
        assert((args[1] & Field::FVAR_TYPE_MASK) == Field::FVAR_LINEGROUP);
        u64 sub_op = op == lines_equal ? line_equal : line_equal_half;
        
        Array_t<u64> arr0 = sat_expand(inst, {args[0]}, &inst->rewrite_temp);
        Array_t<u64> arr1 = sat_expand(inst, {args[1]}, &inst->rewrite_temp);
        assert(arr0.size == arr1.size);
        
        for (s64 i = 0; i < arr0.size; ++i) {
            sat_add(inst, sub_op, {arr0[i], arr1[i]});
        }
    } break;

    case line_equal:
    case line_at_most:
    case line_equal_half: {
        assert(args.size == 2);
        assert((args[0] & Field::FVAR_TYPE_MASK) == Field::FVAR_LINE);
        assert((args[1] & Field::FVAR_TYPE_MASK) == Field::FVAR_LINE);
        Array_t<u64> arr0 = sat_expand(inst, {args[0]}, &inst->rewrite_temp);
        Array_t<u64> arr1 = sat_expand(inst, {args[1]}, &inst->rewrite_temp);

        if (op == line_equal) {
            s64 len = max(arr0.size, arr1.size);
            for (s64 i = 0; i < len; ++i) {
                // @copypaste 8w4uf98shd
                u64 lit0 = i < arr0.size ? arr0[i] : var_false;
                u64 lit1 = i < arr1.size ? arr1[i] : var_false;
                sat_add(inst, equivalent, {lit0, lit1});
            }
        } else if (op == line_at_most) {
            for (s64 i = 0; i < arr0.size; ++i) {
                u64 lit1 = i < arr1.size ? arr1[i] : var_false;
                sat_add(inst, implies, {arr0[i], lit1});
            }
        } else if (op == line_equal_half) {
            s64 len = max(arr0.size, arr1.size/2);
            for (s64 i = 0; i < len; ++i) {
                u64 lit0 = i < arr0.size ? arr0[i] : var_false;
                u64 lit1 = 2*i+1 < arr1.size ? arr1[2*i+1] : var_false;
                sat_add(inst, equivalent, {lit0, lit1});
            }
            for (s64 i = 0; i < arr1.size; i += 2) {
                u64 lit = i+1 < arr1.size ? arr1[i+1] : var_false;
                sat_add(inst, implies, {arr1[i], lit});
            }
        } else {
            assert(false);
        }
    } break;

    case line_equal_const: {
        assert(args.size == 2);
        assert((args[0] & Field::FVAR_TYPE_MASK) == Field::FVAR_LINE);
        Array_t<u64> arr = sat_expand(inst, {args[0]}, &inst->rewrite_temp);
        s64 val = args[1];
        assert(0 <= val and val <= arr.size);

        for (s64 i = 0; i < arr.size; ++i) {
            sat_add(inst, clause, {i < val ? arr[i] : ~arr[i]});
        }
    } break;

    case field_unary: {
        assert(args.size == 2);
        Field f {(s64)args[0], (s64)args[1]};
        bool do_blocking = hashmap_get(&inst->params, Factorio::fpar_do_blocking);
        
        // TODO do not fix sum for each field, just carry it around
        
        // Lines are a vector in {0, ..., n_linelen}^n_linedim where each component is encoded in
        // unary. Additionally, there is line_sum, which contains the sum of all lines, and
        // line_block, which contains blocking information. The latter may be -1, in which case we
        // do not deal with blocking.

        auto lines_all = sat_expand(inst, {f.lines_all}, &inst->rewrite_temp);
        for (u64 line_var: lines_all) {
            s64 rewrite_temp_size = inst->rewrite_temp.size;
            defer { inst->rewrite_temp.size = rewrite_temp_size; };

            auto line = sat_expand(inst, {line_var}, &inst->rewrite_temp);
            
            // The components have to be monotonic
            for (s64 i = 1; i < line.size; ++i) {
                sat_add(inst, implies, {line[i], line[i-1]});
            }
        }

        // line_sum is the sum
        sat_add(inst, merge_multi, {f.lines_item, f.line_sum});

        // sums between adjacent fields
        assert(Dir::TOP - Dir::BEG == 0 and Dir::RIGHT - Dir::BEG == 1);
        for (s64 fd_i = 0; fd_i < 2; ++fd_i) {
            Dir fd = f.dirs[fd_i];
            Field f1 {fd.move()};
            if (not f1.inbounds(inst)) continue;

            s64 rewrite_temp_size = inst->rewrite_temp.size;
            defer { inst->rewrite_temp.size = rewrite_temp_size; };

            auto line0 = sat_expand(inst, f .lines_all,     &inst->rewrite_temp);
            auto line1 = sat_expand(inst, f1.lines_all,     &inst->rewrite_temp);
            auto lined = sat_expand(inst, fd.sum_lines_all, &inst->rewrite_temp);

            assert(line0.size == line1.size and line1.size == lined.size);
            for (s64 i = 0; i < line0.size; ++i) {
                sat_add(inst, merge, {line0[i], line1[i], lined[i]});
            }
        }

        if (do_blocking) {
            // Total count determined also determined by blocking info
            sat_add(inst, line_at_most, {f.line_sum, f.line_block});

            // line_cyclic should be a cyclic bipartition
            // IDEA: make this conditional on the field being a splitter
            sat_add(inst, line_cyclic_bipart, {f.line_cyclic});
        }

        // Line control for splitters
        for (Dir fd: f.dirs) {
            Field f0  = f;
            Field f1  = fd.move(1, 0);
            Field fo0 = fd.move(0, 1);
            Field fo1 = fd.move(1, 1);
            Dir fdr  = fd.turnr();
            Dir fdro = fd.move().turnr();

            // These state which output is occupied
            u64 eq_0    = sat_temp_create(inst, 1);
            u64 eq_1    = sat_temp_create(inst, 1);
            u64 eq_both = sat_temp_create(inst, 1);

            // Determine whether the sum of inputs equals ...
            // ... the first output
            sat_push_add_pop(inst, {eq_0}, lines_equal, {fdr.sum_lines_most, fo0.lines_most});
            sat_push_add_pop(inst, {eq_0}, line_equal_half, {f0.line_block, fo0.line_block});
            sat_push_add_pop(inst, {eq_0}, line_equal_half, {f1.line_block, fo0.line_block});
            // ... the second output
            sat_push_add_pop(inst, {eq_1}, lines_equal, {fdr.sum_lines_most, fo1.lines_most});
            sat_push_add_pop(inst, {eq_1}, line_equal_half, {f0.line_block, fo1.line_block});
            sat_push_add_pop(inst, {eq_1}, line_equal_half, {f1.line_block, fo1.line_block});

            sat_push(inst, {eq_both});
            if (not do_blocking) {
                // ... and whether each output is the average of the inputs
                sat_add(inst, lines_equal_half, {fo0.lines_all, fdr.sum_lines_all});
                sat_add(inst, lines_equal_half, {fo1.lines_all, fdr.sum_lines_all});
            } else {
                // ... or that both outputs are a fraction of the input.
                sat_add(inst, lines_are_fraction, {fdr.sum_lines_item, f0.line_cyclic, fo0.lines_item});
                sat_add(inst, lines_are_fraction, {fdr.sum_lines_item, ~f0.line_cyclic, fo1.lines_item});
                
                // The outputs are blocked or equal
                u64 block0 = sat_temp_create(inst, 1);
                u64 block1 = sat_temp_create(inst, 1);
                sat_push_add_pop(inst, {~block0, ~block1}, lines_equal, {fo0.lines_most, fo1.lines_most});
                sat_push_add_pop(inst, {block0}, line_equal, {fo0.line_sum, fo0.line_block});
                sat_push_add_pop(inst, {block1}, line_equal, {fo1.line_sum, fo1.line_block});

                // Inputs are blocked at half the average rate the outputs
                sat_add(inst, line_equal_half, {f0.line_block, fdro.sum_line_block});
                sat_add(inst, line_equal_half, {f1.line_block, fdro.sum_line_block});
            }
            sat_pop(inst);
            
            // If the other side would be out-of-bounds, no need to emit anything
            if (not f1.inbounds(inst)) continue;
            
            sat_push(inst, {f0.splitl, fd.out});

            if (do_blocking) {
                // Check that both inputs block at the same rate
                sat_add(inst, line_equal, {f0.line_block, f1.line_block});
            }

            // Assert eq_0, eq_1 or eq_both depending on the combination of outputs that are occupied
            sat_addg(inst, implies, { fd.move().back().inp, ~fd.move(1, 1).back().inp}, {eq_0});
            sat_addg(inst, implies, {~fd.move().back().inp,  fd.move(1, 1).back().inp}, {eq_1});
            sat_addg(inst, implies, { fd.move().back().inp,  fd.move(1, 1).back().inp}, {eq_both});

            // Do not output onto the side of a belt
            sat_add(inst, clause, {~fd.move().back().sid});
            sat_add(inst, clause, {~fd.move(1, 1).back().sid});
            
            sat_pop(inst);
        }
    } break;

    case field_border_unary: {
        Field f {(s64)args[0], (s64)args[1]};
        u8 border_type = args[2];
        bool do_blocking = hashmap_get(&inst->params, Factorio::fpar_do_blocking);
        s64 nx        = hashmap_get(&inst->params, Factorio::fpar_nx);
        s64 ny        = hashmap_get(&inst->params, Factorio::fpar_ny);
        s64 s_input   = hashmap_get(&inst->params, Factorio::fpar_s_input);
        s64 s_output  = hashmap_get(&inst->params, Factorio::fpar_s_output);

        s64 notin = 0;
        for (Dir fd: f.dirs) {
            if (not Field {fd.move()}.inbounds(inst)) {
                sat_add(inst, logical_and, {~fd.out, ~fd.inp, ~fd.sid});
                ++notin;
                
            } else if (border_type == Field::BORDER_EMPTY) {
                // Empty border tiles have neither input nor output
                assert(args.size == 3);
                sat_add(inst, logical_and, {~fd.out, ~fd.inp, ~fd.sid, f.empty, ~f.belt});
                sat_add(inst, lines_equal, {f.lines_all, lines_empty});
                
            } else if (border_type == Field::BORDER_OUT) {
                // Output border tiles have their items concentrated in a single line
                assert(args.size == 4);
                sat_add(inst, logical_and, {fd.out, ~fd.inp, ~fd.sid, ~f.empty, f.belt});
                s64 index = args[3];
                Array_t<u64> lines_item = sat_expand(inst, f.lines_item, &inst->rewrite_temp);
                for (u64 line: lines_item) {
                    sat_add(inst, line_equal_const, {line, line == lines_item[index] ? s_output : 0ull});
                }
                sat_add(inst, line_equal_const, {f.line_sum, (u64)s_output});
                
            } else if (border_type == Field::BORDER_INP) {
                // Input border lines have items spread out
                assert(args.size == 3);
                sat_add(inst, logical_and, {~fd.out, fd.inp, ~fd.sid, ~f.empty, f.belt});
                Array_t<u64> lines = sat_expand(inst, f.lines_item, &inst->rewrite_temp);
                for (u64 line: lines) {
                    sat_add(inst, line_equal_const, {line, (u64)s_input});
                }
                sat_add(inst, line_equal_const, {f.line_sum, (u64)(s_input * lines.size)});
                sat_add(inst, line_equal, {f.line_block, line_full});
            } else {
                assert(false);
            }
        }

        if (notin == 4) {
            // Corners
            sat_add(inst, logical_and, {f.empty, ~f.belt});            
            sat_add(inst, lines_equal, {f.lines_all, lines_empty});
        } else {
            assert(notin == 3);
        }

    } break;

    case line_cyclic_bipart: {
        assert(args.size == 1);
        assert((args[0] & Field::FVAR_TYPE_MASK) == Field::FVAR_LINE);
        Array_t<u64> arr = sat_expand(inst, {args[0]}, &inst->rewrite_temp);
        assert(arr.size);

        sat_add(inst, clause, {arr[0]});
        for (s64 i = 2; i < arr.size; ++i) {
            sat_push(inst, {~arr[i-1], arr[i]});
            for (s64 j = i+1; j < arr.size; ++j) {
                sat_add(inst, equivalent, {arr[j-i], arr[j]});
            }
            sat_pop(inst);
        }
    } break;

    case lines_are_fraction: {
        assert((args[0] & Field::FVAR_TYPE_MASK) == Field::FVAR_LINEGROUP);
        assert(((args[1] ^ -(args[1] >> 63)) & Field::FVAR_TYPE_MASK) == Field::FVAR_LINE);
        assert((args[2] & Field::FVAR_TYPE_MASK) == Field::FVAR_LINEGROUP);
        
        Array_t<u64> arr0 = sat_expand(inst, {args[0]}, &inst->rewrite_temp);
        Array_t<u64> arr2 = sat_expand(inst, {args[2]}, &inst->rewrite_temp);
        assert(arr0.size == arr2.size);
        
        for (s64 i = 0; i < arr0.size; ++i) {
            sat_add(inst, line_is_fraction, {arr0[i], args[1], arr2[i]});
        }
    } break;

    case line_is_fraction: {
        assert(args.size == 3);
        assert((args[0] & Field::FVAR_TYPE_MASK) == Field::FVAR_LINE);
        u64 mask = -(args[1] >> 63);
        assert(((args[1] ^ mask) & Field::FVAR_TYPE_MASK) == Field::FVAR_LINE);
        assert((args[2] & Field::FVAR_TYPE_MASK) == Field::FVAR_LINE);

        Array_t<u64> source = sat_expand(inst, {args[0]}, &inst->rewrite_temp);
        Array_t<u64> cycle  = sat_expand(inst, {args[1]}, &inst->rewrite_temp);
        u64 target_group = args[2];

        s64 size = source.size;
        assert(size == cycle.size);
        
        u64 temp = sat_temp_group_create(inst, size);
        Array_t<u64> temp_arr  = sat_expand(inst, {temp}, &inst->rewrite_temp);

        // temp contains the filtered items from source
        for (s64 i = 0; i < size; ++i) {
            sat_addg(inst, implies, {source[i], cycle[i]}, {temp_arr[i]});
            sat_add(inst, implies, {temp_arr[i], source[i]});
            sat_add(inst, implies, {temp_arr[i], cycle[i]});
        }

        // source is a multiple of the cycle length of cycle
        for (s64 i = 0; i+1 < size; ++i) {
            sat_addg(inst, implies, {source[i], ~source[i+1]}, {~cycle[i]^mask});
            sat_addg(inst, implies, {source[i], ~source[i+1]}, {cycle[i+1]^mask});
        }

        // target contains the sorted bits from temp
        sat_add(inst, merge_multi, {temp, target_group});
    } break;
        
        
    default:
        assert(false);
        
    }

}

bool factorio_explain(Sat_instance* inst, u64 id, Array_t<u64> args, Array_dyn<u8>* into) {
    using namespace Factorio;
    
    u64 type, subtype, suffix;
    sat_decompose(id, &type, &subtype, &suffix);
    
    if (type == Sat::VAR or type == Sat::GROUP) {
        assert(subtype == VAR_FACTORIO or subtype == GROUP_FACTORIO);
        u64 fvar_type = suffix & Field::FVAR_TYPE_MASK;
        u64 fvar_mod  = suffix & Field::FVAR_MOD_MASK;
        bool has_coords = suffix >> 24 & 0xffffffffull;
        s64 x = (suffix >> 40 & 0xffff) - Field::COORD_OFFSET;
        s64 y = (suffix >> 24 & 0xffff) - Field::COORD_OFFSET;
        u64 val = suffix & 0xff;
        u64 val2 = suffix >> 8 & 0xff;

        char const* names[] = {
            nullptr, "empty", "belt", "split", "splitl", "splitr", "under", "underh", "underv"
        };
        s64 names_size = sizeof(names) / sizeof(names[0]);
        
        char const* mod_suf = "";
        if (fvar_mod == Field::FVAR_SUMX) {
            mod_suf = "_sumx";
        } else if (fvar_mod == Field::FVAR_SUMY) {
            mod_suf = "_sumy";
        }
        
        if (fvar_type == Field::FVAR_NORMAL) {
            assert(0 < val and val < names_size);
            assert(val2 == 0);
            assert(fvar_mod == 0);
            assert(has_coords);
            array_printf(into, "(%lld,%lld).%s", x, y, names[val]);
        } else if (fvar_type == Field::FVAR_LINE) {
            if (id == Factorio::line_empty) {
                array_printf(into, "0_line");
            } else if (id == Factorio::line_full) {
                array_printf(into, "1_line");
            } else {
                assert(has_coords);
                assert(val == 0);
                
                array_printf(into, "(%lld,%lld).lines%s[", x, y, mod_suf);
                if (val2 == 0xfe) {
                    array_printf(into, "sum");
                } else if (val2 == 0xff) {
                    array_printf(into, "block");
                } else if (val2 == 0xfd) {
                    array_printf(into, "cyclic");
                } else {
                    array_printf(into, "%lld", val2);
                }
                array_printf(into, "]");
            }
        } else if (fvar_type == Field::FVAR_LINEGROUP) {
            if (id == Factorio::lines_empty) {
                array_printf(into, "0_lines");
            } else {
                char const* lnames[] = {"lines_all", "lines_item"};
                s64 lnames_size = sizeof(lnames) / sizeof(lnames[0]);

                s64 val_i = val - names_size;
                
                assert(has_coords);
                assert(0 <= val_i and val_i < lnames_size);
                assert(val2 == 0);
                assert(has_coords);
                array_printf(into, "(%lld,%lld).%s%s", x, y, lnames[val_i], mod_suf);
            }
        } else if (fvar_type == Field::FVAR_LINEEL) {
            assert(has_coords);
            array_printf(into, "(%lld,%lld).lines%s[", x, y, mod_suf);
            if (val2 == 0xfe) {
                array_printf(into, "sum");
            } else if (val2 == 0xff) {
                array_printf(into, "block");
            } else if (val2 == 0xfd) {
                array_printf(into, "cyclic");
            } else {
                array_printf(into, "%lld", val2);
            }
            array_printf(into, "][%lld]", val-1);
        } else if (fvar_type == Field::FVAR_DIR) {
            u8 dir = (id & Dir::FVAR_DIR_MASK) >> 16;

            char const* dnames[] = {"top", "rig", "bot", "lef", "all"};
            s64 dnames_size = sizeof(dnames) / sizeof(dnames[0]);
            char const* ddnames[] = {nullptr, "inp", "out", "sid"};
            s64 ddnames_size = sizeof(ddnames) / sizeof(ddnames[0]);
            
            assert(Dir::BEG <= dir and dir <= Dir::ALL);
            assert(dir - Dir::BEG < dnames_size);
            assert(0 < val and val < ddnames_size);
            array_printf(into, "(%lld,%lld).%s.%s", x, y, dnames[dir - Dir::BEG], ddnames[val]);
        } else {
            assert(false);
        }
        
        return true;
    } else if (type == Sat::CONSTRAINT) {
        assert(subtype == CONSTRAINT_FACTORIO);
        char const* names[] = {
            nullptr, "field_basic", "field_underground", "field_border", "field_unary",
            "field_border_unary", "line_at_most", "line_equal_half", "line_equal_const",
            "line_equal", "lines_equal", "lines_equal_half",
            "line_cyclic_bipart", "line_is_fraction", "lines_are_fraction",
        };
        s64 names_size = sizeof(names) / sizeof(names[0]);
        u64 argtype[] = {
            0, 11, 11, 11, 11,
            1131, 22, 22, 21,
            22, 22, 22,
            2, 222, 222
        };
        s64 argtype_size = sizeof(argtype) / sizeof(argtype[0]);
        assert(argtype_size == names_size);

        char const* bnames_[] = {"empty", "out", "inp"};
        Array_t<char const*> bnames = {bnames_, sizeof(bnames_) / sizeof(bnames_[0])};

        assert(0 < suffix and suffix < names_size);
        array_printf(into, "%s", names[suffix]);
        u64 type_rev = argtype[suffix];
        u64 type = 0;
        while (type_rev) {
            type = 10*type + type_rev%10; type_rev /= 10;
        }
        
        array_printf(into, "(");
        
        bool first = true;
        for (u64 arg: args) {
            if (first) first = false;
            else array_printf(into, ", ");
            
            switch (type % 10) {
            case 1: array_printf(into, "%lld", arg); break;
            case 2: sat_explain(inst, arg, into);    break;
            case 3: array_printf(into, "%s", bnames[arg]); break;
            default: assert(false);
            };
            type /= 10;
        }
        
        array_printf(into, ")");
        return true;
    } else {
        assert(false);
    }
    return false;
}

void factorio_add_fields(Sat_instance* inst, Array_t<s64> params, Array_t<s64> yoff_output, Array_t<s64> yoff_input) {
    using namespace Sat;
    using namespace Factorio;

    for (s64 i = 0; i < params.size; ++i) {
        hashmap_set(&inst->params, PARAM_FACTORIO_BEGIN + i, params[i]);
    }

    s64 nx = hashmap_get(&inst->params, Factorio::fpar_nx);
    s64 ny = hashmap_get(&inst->params, Factorio::fpar_ny);

    for (s64 x = -1; x < nx+1; ++x) {
        for (s64 y = -1; y < ny+1; ++y) {
            Field f {x, y};
            if (f.inbounds(inst)) {
                sat_add(inst, field_basic, {(u64)x, (u64)y});
                sat_add(inst, field_underground, {(u64)x, (u64)y});
                sat_add(inst, field_unary, {(u64)x, (u64)y});
            } else {
                sat_add(inst, field_border, {(u64)x, (u64)y});

                bool flag_empty = true;
                if (y == -1 or y == ny) {
                    // nothing
                } else if (x == -1) {
                    for (s64 yi: yoff_output) flag_empty &= y != yi;
                } else if (x == nx) {
                    for (s64 yi: yoff_input)  flag_empty &= y != yi;
                } else {
                    // nothing
                }
                if (flag_empty) {
                    sat_add(inst, field_border_unary, {(u64)x, (u64)y, Field::BORDER_EMPTY});
                }
            }
        }
    }

    for (s64 i = 0; i < yoff_output.size; ++i) {
        sat_add(inst, field_border_unary, {(u64)-1, (u64)yoff_output[i], Field::BORDER_OUT, (u64)i});
    }
    for (s64 y: yoff_input) {
        sat_add(inst, field_border_unary, {(u64)nx, (u64)y, Field::BORDER_INP});        
    }
}

struct Factorio_params {
    s64 nx = 0, ny = 0, n_under = 10, scale_fac = 1;
    bool do_blocking = true;
    Array_t<s64> yoff_output, yoff_input;
};

void factorio_balancer(Sat_instance* inst, Factorio_params params) {
    using namespace Sat;
    using namespace Factorio;
    Array_t<s64> arr = array_create<s64>(PARAM_FACTORIO_END - PARAM_FACTORIO_BEGIN);
    s64 o = PARAM_FACTORIO_BEGIN;
    
    arr[fpar_nx - o] = params.nx;
    arr[fpar_ny - o] = params.ny;
    arr[fpar_n_under   - o] = params.n_under;
    arr[fpar_n_linedim - o] = params.yoff_output.size;
    arr[fpar_n_linelen - o] = max(params.yoff_input.size, params.yoff_output.size) * params.scale_fac;
    arr[fpar_n_lines   - o] = arr[fpar_n_linedim - o] + 1 + params.do_blocking;
    arr[fpar_s_input   - o] = params.scale_fac;
    arr[fpar_s_output  - o] = params.yoff_input.size * params.scale_fac;
    arr[fpar_do_blocking - o] = params.do_blocking;

    sat_register_expand_func(inst, GROUP_FACTORIO, &factorio_expand);
    sat_register_rewrite_func(inst, CONSTRAINT_FACTORIO, &factorio_rewrite);
    for (u64 i: {VAR_FACTORIO, GROUP_FACTORIO, CONSTRAINT_FACTORIO}) {
        sat_register_explain_func(inst, i, &factorio_explain);
    }

    factorio_add_fields(inst, arr, params.yoff_output, params.yoff_input);
}

void factorio_clauses_from_diagram(Sat_instance* inst, Array_t<u8> text) {
    using namespace Sat;

    s64 nx = hashmap_get(&inst->params, Factorio::fpar_nx);
    s64 ny = hashmap_get(&inst->params, Factorio::fpar_ny);
    
    s64 x = 0, y = 0;
    for (s64 i = 0; i < text.size; i += 2) {
        Field f {x, y};
        
        u8 c0 = text[i];
        u8 c1 = i+1 < text.size ? text[i+1] : 0;

        if (c0 == '\n') {
            x = 0; ++y; --i; continue;
        }

        if (x >= nx or y >= ny) {
            fprintf(stderr, "Error: index (%lld,%lld) out of bounds (nx=%lld,ny=%lld)\n", x, y, nx, ny);
            fprintf(stderr, "Error: while parsing diagram\n");
            exit(7);
        }
        
        u64 type = f.belt;
        
        switch (c0) {
        case 'v': sat_add(inst, clause, {f.dirs[2].inp}); break;
        case '>': sat_add(inst, clause, {f.dirs[3].inp}); break;
        case '^': sat_add(inst, clause, {f.dirs[0].inp}); break;
        case '<': sat_add(inst, clause, {f.dirs[1].inp}); break;
        case 'u': type = f.under; break;
        case 'S': type = f.split; break;
        case '.': break;
        default: assert(false);
        }
        switch (c1) {
        case 'v': sat_add(inst, clause, {f.dirs[0].out}); break;
        case '>': sat_add(inst, clause, {f.dirs[1].out}); break;
        case '^': sat_add(inst, clause, {f.dirs[2].out}); break;
        case '<': sat_add(inst, clause, {f.dirs[3].out}); break;
        case 'u': type = f.under; break;
        case '.': type = f.empty; break;
        default: assert(false);
        }

        assert(type != f.empty or c0 == '.');

        sat_add(inst, clause, {type});

        x += 1;
    }
}

struct Factorio_instance {
    Array_t<u8> name;
    Factorio_params params;
};

struct Factorio_solution {
    struct Line_info {
        s64 x, y, line, value;
    };
    
    Array_t<u8> name;
    Array_t<u8> instance_name;
    Array_t<u8> ascii_diagram;
    Array_t<Line_info> line_infos;
};

void factorio_clauses_from_solution(Sat_instance* inst, Factorio_solution* sol) {
    factorio_clauses_from_diagram(inst, sol->ascii_diagram);

    for (auto info: sol->line_infos) {
        Field f {info.x, info.y};
        u64 line;
        if (info.line == 0xfe) {
            line = f.line_sum;
        } else if (info.line == 0xfe) {
            line = f.line_block;
        } else {
            line = f.line_first + info.line * 256;
        }
        sat_add(inst, Factorio::line_equal_const, {line, (u64)info.value});
    }
}
    
struct Factorio_db {
    // This modifies the memory inside!
    Array_t<u8> _parsed_text;
    Array_dyn<Factorio_instance> instances;
    Array_dyn<Factorio_solution> solutions;
};

void factorio_db_clear(Factorio_db* fdb) {
    // @leak
    fdb->instances.size = 0;
    fdb->solutions.size = 0;
}

void factorio_db_parse(Factorio_db* fdb, Array_t<u8> data) {
    fdb->_parsed_text = data;

    struct Token {
        enum Token_type: u8 {
            // singe char operators are themselves
            IDENTIFIER = 128, NUMBER
        };
        u8 type;
        s64 beg, end, line;
        u64 number = 0;
    };

    Array_dyn<Token> tokens;
    defer { array_free(&tokens); };

    {s64 last = -1;
    bool alldigit = true;
    s64 line_cur = 0;
    for (s64 i = 0; i <= data.size; ++i) {
        u8 c = i < data.size ? data[i] : 0;
        bool istok = false, isspace = false;
        
        for (u8 cc: ",=[]{}()*"_arr) istok   |= c == cc;
        for (u8 cc: " \t\n"_arr)  isspace |= c == cc;
        isspace |= c == 0;

        if (istok or isspace) {
            if (last != -1) {
                if (alldigit) {
                    u64 val = 0;
                    for (s64 j = last; j < i; ++j) {
                        val = 10*val + (data[j] - '0');
                    }
                    array_push_back(&tokens, {Token::NUMBER, last, i, line_cur, val});
                } else {
                    array_push_back(&tokens, {Token::IDENTIFIER, last, i, line_cur});
                }
                last = -1;
                alldigit = true;
            }
            if (istok) array_push_back(&tokens, {c, i, i+1, line_cur});                
        } else {
            if (last == -1) last = i;
            alldigit &= '0' <= c and c <= '9';
        }
        line_cur += c == '\n';
    }}

    auto print_type = [](u8 type) {
        if (type == Token::IDENTIFIER) fprintf(stderr, "<identifier>");
        else if (type == Token::NUMBER) fprintf(stderr, "<number>");
        else if (type == 0) fprintf(stderr, "<eof>");
        else fprintf(stderr, "'%c'", type);
    };
    auto print_val = [data](Token tok) {
        if (tok.type == Token::IDENTIFIER) {
            auto str = array_subarray(data, tok.beg, tok.end);
            fprintf(stderr, " (value: '");
            fwrite(str.data, 1, str.size, stderr);
            fprintf(stderr, "')");
        } else if (tok.type == Token::NUMBER) {
            fprintf(stderr, " (value: %llu)'", tok.number);
        }
    };

    //for (Token i: tokens) { print_type(i.type); print_val(i); puts(""); }

    auto match = [&](s64* i, u8 type, Array_t<u8>* into_str=nullptr) {
        Token tok = tokens[*i];
        auto str = array_subarray(data, tok.beg, tok.end);
        
        if (tok.type != type) {
            fprintf(stderr, "Error (line %lld): expected token with type ", tok.line);
            print_type(type);
            fprintf(stderr, ", got type ");
            print_type(tok.type);
            print_val(tok);
            fprintf(stderr, "\n");
            exit(5);
        }
        
        if (into_str) *into_str = str;
        ++*i;
    };
    
    for (s64 i = 0; i < tokens.size;) {
        Array_t<u8> heading;
        match(&i, Token::IDENTIFIER, &heading);
        
        if (array_equal_str(heading, "instance")) {
            Factorio_instance fi;
            match(&i, Token::IDENTIFIER, &fi.name);

            for (auto inst: fdb->instances) {
                if (array_equal(inst.name, fi.name)) {
                    fprintf(stderr, "Warning (line %lld): Multiple instances with name '", tokens[i-1].line);
                    fwrite(fi.name.data, 1, fi.name.size, stderr);
                    fprintf(stderr, "'. This will lead to weird behaviour.\n");
                }
            }
            
            match(&i, '{');
            while (tokens[i].type != '}') {
                Array_t<u8> par_name;
                match(&i, Token::IDENTIFIER, &par_name);

                static constexpr s64 count = 7;
                Factorio_params* p = &fi.params;
                char const* pars[count] = { "nx", "ny", "n_under", "scale_fac", "do_blocking", "yoff_output", "yoff_input" };
                void* ptrs[count] = { &p->nx,&p->ny,&p->n_under,&p->scale_fac,&p->do_blocking,&p->yoff_output,&p->yoff_input };
                s64 types[count] = {1, 1, 1, 1, 2, 3, 3};

                s64 found = -1;
                for (s64 j = 0; j < count; ++j) {
                    if (array_equal_str(par_name, pars[j])) {
                        found = j; break;
                    }
                }
                if (found == -1) {
                    fprintf(stderr, "Error (line %lld): Unrecognised parameter '", tokens[i-1].line);
                    fwrite(par_name.data, 1, par_name.size, stderr);
                    fprintf(stderr, "'\n");
                    exit(5);
                }

                match(&i, '=');

                if (types[found] == 1) {
                    match(&i, Token::NUMBER);
                    *(u64*)ptrs[found] = tokens[i-1].number;
                } else if (types[found] == 2) {
                    match(&i, Token::NUMBER);
                    *(bool*)ptrs[found] = tokens[i-1].number;
                } else if (types[found] == 3) {
                    match(&i, '[');
                    s64 orig_i = i;
                    s64 count = 0;
                    while (tokens[i].type != ']') {
                        match(&i, Token::NUMBER);
                        ++count;
                        if (tokens[i].type == ',') ++i;
                    }
                    Array_t<u64> numbers = array_create<u64>(count);
                    count = 0;
                    i = orig_i;
                    while (tokens[i].type != ']') {
                        match(&i, Token::NUMBER);
                        numbers[count++] = tokens[i-1].number;
                        if (tokens[i].type == ',') ++i;
                    }
                    assert(numbers.size == count);
                    *(Array_t<u64>*)ptrs[found] = numbers;
                    match(&i, ']');
                } else {
                    assert(false);
                }
                
                if (tokens[i].type == ',') ++i;
            }
            
            array_push_back(&fdb->instances, fi);
            match(&i, '}');
        } else if (array_equal_str(heading, "solution")) {
            Factorio_solution sol;
            match(&i, Token::IDENTIFIER, &sol.instance_name);
            match(&i, Token::IDENTIFIER, &sol.name);
            match(&i, '{');

            s64 total_size = 0;
            {s64 orig_i = i;
            while (tokens[i].type != '}') {
                Array_t<u8> line;
                match(&i, Token::IDENTIFIER, &line);
                total_size += line.size + 1;
            }
            i = orig_i;}

            {Array_dyn<u8> diagram;
            array_reserve(&diagram, total_size);
            diagram.do_not_reallocate = true;

            while (tokens[i].type != '}') {
                Array_t<u8> line;
                match(&i, Token::IDENTIFIER, &line);
                array_append(&diagram, line);
                array_push_back(&diagram, '\n');
            }
            assert(diagram.size == total_size);

            sol.ascii_diagram = diagram;}
            
            match(&i, '}');
            
            if (i < tokens.size and tokens[i].type == '{') {
                Array_dyn<Factorio_solution::Line_info> line_infos;
                match(&i, '{');
                while (i < tokens.size and tokens[i].type != '}') {
                    Factorio_solution::Line_info info;
                    match(&i, '(');
                    match(&i, Token::NUMBER);
                    info.x = tokens[i-1].number;
                    match(&i, ',');
                    match(&i, Token::NUMBER);
                    info.y = tokens[i-1].number;
                    match(&i, ')'); 
                    Array_t<u8> field;
                    match(&i, Token::IDENTIFIER, &field);
                    
                    if (not array_equal_str(field, ".lines")) {
                        fprintf(stderr, "Error (line %lld): Expected '.lines', got '", tokens[i-1].line);
                        fwrite(field.data, 1, field.size, stderr);
                        fprintf(stderr, "'\n");
                        exit(5);
                    }
                    match(&i, '='); 

                    match(&i, '(');
                    s64 offset = 0;
                    while (i < tokens.size and tokens[i].type != ')') {
                        if (tokens[i].type == '*') {
                            // We could check whether the star makes sense at this point
                            match(&i, '*');
                            offset = 0xfe;
                            
                            if (tokens[i].type == ')') break;
                            match(&i, ',');
                            continue;
                        }
                        
                        match(&i, Token::NUMBER);
                        info.line = offset;
                        info.value = tokens[i-1].number;
                        array_push_back(&line_infos, info);
                        
                        if (tokens[i].type == ')') break;
                        match(&i, ',');
                        
                        ++offset;
                    }
                    match(&i, ')');
                    
                    if (i >= tokens.size or tokens[i].type == '}') break;
                    match(&i, ',');
                }
                match(&i, '}');

                sol.line_infos = line_infos;
            }
            
            array_push_back(&fdb->solutions, sol);
        } else {
            fprintf(stderr, "Error (line %lld): Expected either 'instance' or 'solution', got '", tokens[i-1].line);
            fwrite(heading.data, 1, heading.size, stderr);
            fprintf(stderr, "'\n");
            exit(5);
        }
    }
}


#include <unistd.h>
#include <poll.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

#include "global.hpp"
#include "hashmap.cpp"
#include "array_linux.cpp"
#include "sat.cpp"



namespace Construct {

enum Types_construct: u64 {
    VAR_CONSTRUCT        = 0x19ull << 56,
    GROUP_CONSTRUCT      = 0x29ull << 56,
    CONSTRAINT_CONSTRUCT = 0x39ull << 56,
    PARAM_CONSTRUCT      = 0x49ull << 56,
};

enum Constraints_construct: u64 {
    CONSTRAINT_CONSTRUCT_BEGIN = CONSTRAINT_CONSTRUCT,
    field_basic,
    field_border,
    field_global,
    field_global2,
    field_incremental,
    field_incremental_border,
    use_all_recipes,
    partition_field,
    partition_border,
    partition_border_nooutput,
};

enum Params_construct: u64 {
    PARAM_CONSTRUCT_BEGIN = PARAM_CONSTRUCT,
    param_recipes, param_nx, param_ny, param_n_items,
    param_n_transport_max, param_n_border_max, param_n_partition_max,
    param_n_sparsecut_maxsize, param_n_sparsecut_groups,
    PARAM_CONSTRUCT_END
};

}

struct Field_var {
    enum Masks: u64 {
        MASK_COORD = 0xffff0000000000ull,
        MASK_X     = 0xff000000000000ull,
        MASK_Y     =   0xff0000000000ull,
        MASK_TYPE  =     0xff00000000ull,
        MASK_DIR   =       0xff000000ull,
        MASK_KIND  =         0xff0000ull,
        MASK_EL    =           0xffffull,
    };
    enum Type: u8 {
        VAR, GROUP_DIR, GROUP_REC, GROUP_ADJ
    };
    enum Kind: u8 {
        PROD, BELT, BEACON, PBORDER, REC, INP, OUT, INP_BORDER, KIND_SIZE
    };
    static constexpr char const* kind_names[KIND_SIZE] = {
        "prod", "belt", "beacon", "pborder", "rec", "inp", "out", "inp_border"
    };
    enum Dirs: u8 {
        UNDIRECTED, RIGHT, TOP, LEFT, BOTTOM, DIR_ALL, DIR_SIZE,
        BEG = RIGHT, END = BOTTOM+1
    };
    static constexpr char const* dir_names[DIR_SIZE] = {
        "none", "rig", "top", "lef", "bot", "all"
    };

    static constexpr u64 OFFSET = 2;
    
    s8 x, y;
    u8 type, dir, kind;
    u16 el = 0;

    Field_var move(u8 dir) {
        Field_var r = *this;
        if      (dir == RIGHT ) ++r.x;
        else if (dir == TOP   ) ++r.y;
        else if (dir == LEFT  ) --r.x;
        else if (dir == BOTTOM) --r.y;
        else assert(false);
        return r;
    }
    bool inbounds(s64 nx, s64 ny) { return 0 <= x and x < nx and 0 <= y and y < ny; }
    bool inbounds_almost(s64 nx, s64 ny) { return -1 <= x and x <= nx and -1 <= y and y <= ny; }

    static u8 dir_back(u8 d) {
        assert(BEG <= d and d < END);
        u8 r[4] = {LEFT, BOTTOM, RIGHT, TOP};
        return r[d - BEG];
    }
};

constexpr char const* Field_var::kind_names[Field_var::KIND_SIZE];
constexpr char const* Field_var::dir_names[Field_var::DIR_SIZE];

Field_var construct_decompose(u64 lit) {
    u64 subtype, suffix;
    sat_decompose(lit, nullptr, &subtype, &suffix);

    Field_var r;

    #define GETMASK(lit, mask) ((lit & (mask)) >> __builtin_ctzll((mask)))

    r.x    = GETMASK(suffix, Field_var::MASK_X) - Field_var::OFFSET;
    r.y    = GETMASK(suffix, Field_var::MASK_Y) - Field_var::OFFSET;
    r.type = GETMASK(suffix, Field_var::MASK_TYPE);
    r.dir  = GETMASK(suffix, Field_var::MASK_DIR);
    r.kind = GETMASK(suffix, Field_var::MASK_KIND);
    r.el   = GETMASK(suffix, Field_var::MASK_EL);

    #undef GETMASK

    if (subtype == Construct::VAR_CONSTRUCT) assert(r.type == Field_var::VAR);

    return r;
}

u64 construct_compose(Field_var l) {
    #define SETMASK(val, mask) ((u64)(val) << __builtin_ctzll((mask)))

    if (l.type == Field_var::GROUP_REC) assert(l.kind == Field_var::REC);
    
    u64 base = l.type == Field_var::VAR ? Construct::VAR_CONSTRUCT : Construct::GROUP_CONSTRUCT;
    s64 off = Field_var::OFFSET;

    assert(0 <= l.x+off and l.x+off <= 0xff);
    assert(0 <= l.y+off and l.y+off <= 0xff);
    
    return base
         | SETMASK(l.x+off, Field_var::MASK_X)
         | SETMASK(l.y+off, Field_var::MASK_Y)
         | SETMASK(l.type,  Field_var::MASK_TYPE)
         | SETMASK(l.dir,   Field_var::MASK_DIR)
         | SETMASK(l.kind,  Field_var::MASK_KIND)
         | SETMASK(l.el,    Field_var::MASK_EL);

    #undef SETMASK
}

struct Field {    
    s8 x, y;

    u64 prod, belt, beacon, pborder;
    u64 rec_all;
    
    u64 inp(s64 dim) {
        u16 el = dim; assert(el == dim);
        return construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::INP, el});
    }
    u64 adjout(s64 dim) {
        u16 el = dim; assert(el == dim);
        return construct_compose({x, y, Field_var::GROUP_ADJ, Field_var::UNDIRECTED, Field_var::OUT, el});
    }
    u64 out(s64 dim, u8 d) {
        u16 el = dim; assert(el == dim);
        assert((Field_var::BEG <= d and d < Field_var::END) or d == Field_var::DIR_ALL or d == Field_var::UNDIRECTED);
        u8 type = d == Field_var::DIR_ALL ? Field_var::GROUP_DIR : Field_var::VAR;
        return construct_compose({x, y, type, d, Field_var::OUT, el});
    }
    u64 inp_border(s64 dim) {
        u16 el = dim; assert(el == dim);
        return construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::INP_BORDER, el});
    }
    u64 rec(s64 dim) {
        u16 el = dim; assert(el == dim);
        return construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::REC, el});
    }        

    Field(s64 x_, s64 y_): x{(s8)x_}, y{(s8)y_} {
        assert(x == x_ and y == y_);
        
        prod    = construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::PROD});
        belt    = construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::BELT});
        beacon  = construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::BEACON});
        pborder = construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::PBORDER});
        rec_all = construct_compose({x, y, Field_var::GROUP_REC, Field_var::UNDIRECTED, Field_var::REC});
    }

    static Field global() { return {-2,  0}; }
    static Field border() { return {-1, -1}; }

    Field move(u8 dir) {
        Field r = *this;
        if      (dir == Field_var::RIGHT ) ++r.x;
        else if (dir == Field_var::TOP   ) ++r.y;
        else if (dir == Field_var::LEFT  ) --r.x;
        else if (dir == Field_var::BOTTOM) --r.y;
        else assert(false);
        return r;
    }
    bool inbounds(s64 nx, s64 ny) {
        return 0 <= x and x < nx and 0 <= y and y < ny;
    }

};

struct Recipe {
    s64 name;
    s64 beacons;
    s64 entity;
    Array_t<u64> mask_input, mask_output; // These are bitmasks
};

struct Recipe_db {
    struct Recipe_rec {
        s64 name, beacons, entity;
    };
    Array_dyn<Recipe_rec> recs;
    Array_dyn<u64> data;
    s64 recipe_count = 0; // This excludes the artificial recipe describing the whole production
    s64 n_itemmask = 0;
    s64 _n_items = 0;
    
    Recipe get(s64 recipe) {
        s64 i = 2 * n_itemmask * recipe;
        return {
            recs[recipe].name,
            recs[recipe].beacons,
            recs[recipe].entity,
            array_subarray(data, i, i+n_itemmask),
            array_subarray(data, i+n_itemmask, i+2*n_itemmask)
        };
    }
};


void construct_expand(Sat_instance* inst, u64 ovar, Array_dyn<u64>* out_lits) {
    assert((ovar & Sat::MASK_SUBTYPE) == Construct::GROUP_CONSTRUCT);

    Field_var v = construct_decompose(ovar);
    s8 nx = (s8)hashmap_get(&inst->params, Construct::param_nx);
    s8 ny = (s8)hashmap_get(&inst->params, Construct::param_ny);
        

    if (v.type == Field_var::GROUP_DIR) {
        if (v.dir == Field_var::DIR_ALL) {
            if (v.x == -1 and v.y == -1) {
                for (s8 x = 0; x < nx; ++x) {
                    array_push_back(out_lits, construct_compose({x, -1, Field_var::VAR, Field_var::TOP,    v.kind, v.el}));
                    array_push_back(out_lits, construct_compose({x, ny, Field_var::VAR, Field_var::BOTTOM, v.kind, v.el}));
                }
                for (s8 y = 0; y < ny; ++y) {
                    array_push_back(out_lits, construct_compose({nx, y, Field_var::VAR, Field_var::LEFT,  v.kind, v.el}));
                    array_push_back(out_lits, construct_compose({-1, y, Field_var::VAR, Field_var::RIGHT, v.kind, v.el}));
                }
            } else /*if (v.inbounds(nx, ny) or (v.x == -1 and v.y == 0))*/ {
                for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                    Field_var r = v;
                    r.type = Field_var::VAR;
                    r.dir = d;
                    array_push_back(out_lits, construct_compose(r));
                }
            //} else {
            //    assert(false);
            }
        } else {
            assert(false);
        }
    } else if (v.type == Field_var::GROUP_REC) {
        Recipe_db* rdb = (Recipe_db*)hashmap_get(&inst->params, Construct::param_recipes);

        for (s64 i = 0; i < rdb->recipe_count; ++i) {
            Field_var r = v;
            r.type = Field_var::VAR;
            r.el = i;
            array_push_back(out_lits, construct_compose(r));
        }
    } else if (v.type == Field_var::GROUP_ADJ) {
        if (v.x == -1 and v.y == -1) {
            for (s8 x = 0; x < nx; ++x) {
                array_push_back(out_lits, construct_compose({x, 0,          Field_var::VAR, Field_var::BOTTOM, v.kind, v.el}));
                array_push_back(out_lits, construct_compose({x, (s8)(ny-1), Field_var::VAR, Field_var::TOP,    v.kind, v.el}));
            }
            for (s8 y = 0; y < ny; ++y) {
                array_push_back(out_lits, construct_compose({0,          y, Field_var::VAR, Field_var::LEFT,  v.kind, v.el}));
                array_push_back(out_lits, construct_compose({(s8)(nx-1), y, Field_var::VAR, Field_var::RIGHT, v.kind, v.el}));
            }
        } else if (v.inbounds(nx, ny)) {
            for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                Field_var v1 = v.move(d);
                if (not v1.inbounds_almost(nx, ny)) continue;
                array_push_back(out_lits, construct_compose(
                    {v1.x, v1.y, Field_var::VAR, Field_var::dir_back(d), v.kind, v.el}
                ));
            }
        } else {
            assert(v.inbounds_almost(nx, ny));
            
            for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                Field_var v1 = v.move(d);
                if (not v1.inbounds(nx, ny)) continue;
                array_push_back(out_lits, construct_compose(
                    {v1.x, v1.y, Field_var::VAR, Field_var::dir_back(d), v.kind, v.el}
                ));
            }
        }
    } else {
        assert(false);
    }
}

bool bitset_logical_and(Array_t<u64> a, Array_t<u64> b) {
    assert(a.size == b.size);
    for (s64 i = 0; i < a.size; ++i) {
        if (a[i] & b[i]) return true;
    }
    return false;
}

bool bitset_vget(Array_t<u64> bitset, u64 bit) {
    u64 index  = bit / 64;
    u64 offset = bit % 64;
    return index < bitset.size ? bitset.data[index] >> offset & 1 : false;
}

void construct_rewrite(Sat_instance* inst, u64 op, Array_t<u64> args) {
    using namespace Sat;
    using namespace Construct;

    Recipe_db* rdb = (Recipe_db*)hashmap_get(&inst->params, param_recipes);
    s64 n_items = hashmap_get(&inst->params, param_n_items);
    s64 nx = hashmap_get(&inst->params, param_nx);
    s64 ny = hashmap_get(&inst->params, param_ny);
    
    switch (op) {

    case field_basic: {
        assert(args.size == 2);
        Field f {(s64)args[0], (s64)args[1]};

        // Exactly one type
        sat_add(inst, exactly_one, {f.prod, f.belt, f.beacon});

        // Exactly one recipe if prod, else zero
        sat_add(inst, at_most_one, {f.rec_all});
        sat_push_add_pop(inst, {f.prod}, clause, {f.rec_all});
        sat_push_add_pop(inst, {~f.prod}, logical_and, {~f.rec_all});

        // For each type of recipe, do the correct inputs and outputs
        for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
            Recipe r = rdb->get(r_it);
            sat_push(inst, {f.rec(r_it)});

            for (s64 i = 0; i < n_items/2; ++i) {
                if (bitset_get(r.mask_input,  i)) {
                    // If it is an input of the recipe, we need to have it or the tainted version
                    sat_add(inst, clause, {f.inp(i), f.inp(i+n_items/2)});
                } else {
                    // Else we do not want it @Redundant
                    sat_add(inst, logical_and, {~f.inp(i), ~f.inp(i+n_items/2)});
                }

                // If it is NOT an output, we must not output it
                if (not bitset_get(r.mask_output, i)) {
                    sat_add(inst, logical_and, {~f.out(i, Field_var::DIR_ALL)});
                }
            }
            
            sat_pop(inst);
        }

        // A production facility never outputs tainted items
        for (s64 i = n_items/2; i < n_items; ++i) {
            sat_push_add_pop(inst, {f.prod}, logical_and, {~f.out(i, Field_var::DIR_ALL)});
        }

        // Belts need to obey item conservation
        sat_push(inst, {f.belt});
        for (s64 i = 0; i < n_items; ++i) {
            // A belt only outputs items which it has
            for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                sat_add(inst, implies, {f.out(i, d), f.inp(i)});
            }
        }
        
        sat_pop(inst);

        // There can be at most 6 items on a belt
        s64 index = inst->rewrite_temp.size;
        for (s64 i = 0; i < n_items; ++i) {
            array_push_back(&inst->rewrite_temp, f.inp(i));
        }
        sat_push_add_pop(inst, {f.belt}, at_most_k, {sat_group(inst, array_subarray(inst->rewrite_temp, index)), 6});
        
        //Field fg = Field::global();

        for (s64 i = 0; i < n_items; ++i) {
            // Having an item as input means that an adjacent tile gave it to us
            if (i < n_items/2) {
                sat_add(inst, implies, {f.inp(i), f.adjout(i)});
            } else {
                sat_addg(inst, implies, {f.inp(i)}, {f.inp(i-n_items/2), f.adjout(i)});
            }

            for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                // If a belt outputs into a direction, the corresponding global flag must be set
                //sat_addg(inst, implies, {f.belt, f.out(i, d)}, {fg.out(i, d)});
                
                // If we output in a direction, that field must not output back
                Field f2 = f.move(d);
                if (f2.inbounds(nx, ny)) {
                    //DEBUGsat_add(inst, implies, {f.out(i, d), ~f2.out(i, Field_var::dir_back(d))});
                    sat_addg(inst, implies, {f.belt, f2.belt, f.out(i, d)}, {~f2.out(i, Field_var::dir_back(d))});
                }
            }
        }
        
        // No beacons (yet)
        sat_add(inst, clause, {~f.beacon});
    } break;

    case field_border: {
        assert(args.size == 0);
        Field f = Field::border();
        sat_add(inst, logical_and, {~f.prod, f.belt, ~f.beacon});

        // The global recipe
        Recipe rg = rdb->get(rdb->recipe_count);
        
        for (s64 i = 0; i < n_items; ++i) {
            // Having an item as input means that an adjacent tile gave it to us
            sat_add(inst, implies, {f.inp(i), f.adjout(i)});

            // Tainted items cannot be inputs
            if (i >= n_items/2) {
                sat_add(inst, clause, {~f.inp(i)});
            }

            // Output an (untainted) item only if it is an input of the global recipe
            if (i < n_items/2 and not bitset_get(rg.mask_input, i)) {
                sat_add(inst, logical_and, {~f.out(i, Field_var::DIR_ALL)});
            }
            // Output a tainted item only if we receive the untainted version as input
            if (i >= n_items/2) {
                sat_addg(inst, implies, {f.inp(i-n_items/2)}, {f.out(i, Field_var::DIR_ALL)});
            }

            // If it is an output of the global recipe, we must have it
            if (bitset_vget(rg.mask_output, i)) {
                sat_add(inst, clause, {f.inp(i)});
            }
        }
    } break;
    
    case field_global: {
        assert(args.size == 0);
        Field f = Field::global();

        for (s64 i = 0; i < n_items; ++i) {
            // At most three output flags are set
            sat_add(inst, clause, {~f.out(i, Field_var::DIR_ALL)});
        }
    } break;

    case field_global2: {
        assert(args.size == 4);
        s64 px = args[0];
        s64 py = args[1];
        s64 sx = args[2];
        s64 sy = args[3];
        Field f = {px+nx+2, py};

        for (s64 i = 0; i < n_items; ++i) {
            for (s64 y = sy*py; y < sy*(py+1); ++y) {
                for (s64 x = sx*px; x < sx*(px+1); ++x) {
                    for (u8 d = Field_var::BEG; d < Field_var::END; ++d) {
                        // If the belt outputs in this direction, the corresponding flag must be set
                        Field fb {x, y};
                        sat_addg(inst, implies, {fb.belt, fb.out(i, d)}, {f.out(i, d)});
                    }
                }
            }
            
            // At most three output flags are set
            sat_add(inst, clause, {~f.out(i, Field_var::DIR_ALL)});
        }
    } break;

    case partition_field: {
        assert(args.size == 2);
        s64 n_transport_max = hashmap_get(&inst->params, param_n_transport_max);
        s64 n_border_max    = hashmap_get(&inst->params, param_n_border_max);
        s64 n_partition_max = hashmap_get(&inst->params, param_n_partition_max);
        Field f {(s64)args[0], (s64)args[1]};

        for (s64 i = 0; i < n_items; ++i) {
            // Set the types to be correct (does not affect the instance, but is useful for the output)
            sat_add(inst, logical_and, {~f.belt, f.prod, ~f.beacon});
            
            // If an item is output, there must be a recipe actually outputting it
            {s64 index = inst->rewrite_temp.size;
            for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
                if (bitset_get(rdb->get(r_it).mask_output, i)) {
                    array_push_back(&inst->rewrite_temp, f.rec(r_it));
                }
            }
            auto arr = array_subarray(inst->rewrite_temp, index);
            sat_addg(inst, implies, {f.out(i, Field_var::UNDIRECTED)}, arr);}

            // If we have a recipe, the inputs of the recipe must be inputs of the field
            for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
                if (bitset_get(rdb->get(r_it).mask_input, i)) {
                    sat_add(inst, implies, {f.rec(r_it), f.inp(i)});
                }
            }

            // If the item is an input of the field and not an output of the field, it must be transported
            sat_addg(inst, implies, {f.inp(i), ~f.out(i, Field_var::UNDIRECTED)}, {f.adjout(i)});
        
            // If we transport an item, we must output it
            for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                sat_add(inst, implies, {f.out(i, d), f.out(i, Field_var::UNDIRECTED)});
            }
        }

        for (u8 d = Field_var::BEG; d < Field_var::END; ++d) {
            Field f2 = f.move(d);
            if (f2.inbounds(nx, ny) and (d == Field_var::LEFT or d == Field_var::BOTTOM)) continue;
            s64 limit = n_transport_max;

            // Transport only a limited number of items between fields
            s64 index = inst->rewrite_temp.size;
            for (s64 i = 0; i < n_items; ++i) {
                array_push_back(&inst->rewrite_temp, f .out(i, d));
                array_push_back(&inst->rewrite_temp, f2.out(i, Field_var::dir_back(d)));
            }
            u64 group_transport = sat_group(inst, array_subarray(inst->rewrite_temp, index));
            sat_add(inst, at_most_k, {group_transport, (u64)limit});

            // An item cannot be transported into both directions @Redundant
            for (s64 i = 0; i < n_items; ++i) {
                sat_add(inst, implies, {f.out(i, d), ~f2.out(i, Field_var::dir_back(d))});
            }
        }

        // A field must have a limited number of production facilities
        sat_add(inst, at_most_k, {f.rec_all, (u64)n_partition_max});
    } break;
    
    case partition_border: {
        assert(args.size == 0);
        s64 n_transport_max = hashmap_get(&inst->params, param_n_transport_max);
        s64 n_border_max    = hashmap_get(&inst->params, param_n_border_max);
        s64 n_partition_max = hashmap_get(&inst->params, param_n_partition_max);
        Field f = Field::border();
        Recipe rg = rdb->get(rdb->recipe_count);
        
        for (s64 i = 0; i < n_items; ++i) {
            // Having an item as input means that an adjacent tile gave it to us
            sat_add(inst, implies, {f.inp(i), f.adjout(i)});

            if (not bitset_get(rg.mask_input, i)) {
                // If the item is not an input of the global recipe, output it only if it is an input of the field
                sat_push_add_pop(inst, {~f.inp(i)}, logical_and, {~f.out(i, Field_var::DIR_ALL)});
            }

            // If it is an output of the global recipe, we must have it
            if (bitset_get(rg.mask_output, i)) {
                sat_add(inst, clause, {f.inp(i)});
            }
        }

        // Only have a limited number of items which are not involved in the global recipe as inputs of the border
        s64 index = inst->rewrite_temp.size;
        for (s64 i = 0; i < n_items; ++i) {
            if (not bitset_get(rg.mask_input, i) and not bitset_get(rg.mask_output, i)) {
                array_push_back(&inst->rewrite_temp, f.inp(i));
            }
        }
        auto arr = array_subarray(inst->rewrite_temp, index);
        sat_add(inst, at_most_k, {sat_group(inst, arr), (u64)n_border_max});
    } break;

    case partition_border_nooutput: {
        assert(args.size == 0);
        s64 n_transport_max = hashmap_get(&inst->params, param_n_transport_max);
        s64 n_partition_max = hashmap_get(&inst->params, param_n_partition_max);
        Field f = Field::border();
        Recipe rg = rdb->get(rdb->recipe_count);
        
        for (s64 i = 0; i < n_items; ++i) {
            // Having an item as input means that an adjacent tile gave it to us
            sat_add(inst, implies, {f.inp(i), f.adjout(i)});

            if (not bitset_get(rg.mask_input, i)) {
                // If the item is not an input of the global recipe, do not output it
                sat_add(inst, logical_and, {~f.out(i, Field_var::DIR_ALL)});
            }

            // If it is an output of the global recipe, we must have it
            if (bitset_get(rg.mask_output, i)) {
                sat_add(inst, clause, {f.inp(i)});
            }
        }
    } break;
    
    case use_all_recipes: {
        assert(args.size == 0);
        for (s64 i = 0; i < n_items; ++i) {
            // Every recipe must appear somewhere @Redundant
            s64 index = inst->rewrite_temp.size;
            for (s64 y = 0; y < ny; ++y) {
                for (s64 x = 0; x < nx; ++x) {
                    Field f {x, y};
                    array_push_back(&inst->rewrite_temp, f.rec(i));
                }
            }
            sat_add(inst, clause, array_subarray(inst->rewrite_temp, index));
            inst->rewrite_temp.size = index;
        }
        
    } break;
        
    case field_incremental: {
        assert(args.size == 2);
        Field f {(s64)args[0], (s64)args[1]};

        // Exactly one type
        sat_add(inst, exactly_one, {f.prod, f.belt, f.beacon, f.pborder});

        // Exactly one recipe if prod, else zero
        sat_add(inst, at_most_one, {f.rec_all});
        sat_push_add_pop(inst, {f.prod}, clause, {f.rec_all});
        sat_push_add_pop(inst, {~f.prod}, logical_and, {~f.rec_all});

        // For each type of recipe, do the correct inputs and outputs
        for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
            Recipe r = rdb->get(r_it);
            sat_push(inst, {f.rec(r_it)});

            for (s64 i = 0; i < n_items; ++i) {
                if (bitset_get(r.mask_input,  i)) {
                    // If it is an input of the recipe, we need to have it
                    sat_add(inst, clause, {f.inp(i)});
                } else {
                    // Else we do not want it @Redundant
                    sat_add(inst, logical_and, {~f.inp(i)});
                }

                // If it is NOT an output, we must not output it
                if (not bitset_get(r.mask_output, i)) {
                    sat_add(inst, logical_and, {~f.out(i, Field_var::DIR_ALL)});
                }
            }
            
            sat_pop(inst);
        }

        // Belts need to obey item conservation
        sat_push(inst, {f.belt});
        for (s64 i = 0; i < n_items; ++i) {
            // A belt only outputs items which it has
            for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                sat_add(inst, implies, {f.out(i, d), f.inp(i)});
            }
        }
        
        sat_pop(inst);

        // There can be at most 6 items on a belt
        s64 index = inst->rewrite_temp.size;
        for (s64 i = 0; i < n_items; ++i) {
            array_push_back(&inst->rewrite_temp, f.inp(i));
        }
        sat_push_add_pop(inst, {f.belt}, at_most_k, {sat_group(inst, array_subarray(inst->rewrite_temp, index)), 6});
        
        Field fg = Field::global();
        for (s64 i = 0; i < n_items; ++i) {
            // Having an item as input means that an adjacent tile gave it to us
            sat_add(inst, implies, {f.inp(i), f.adjout(i)});

            for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                // If a belt outputs into a direction, the corresponding global flag must be set
                sat_addg(inst, implies, {f.belt, f.out(i, d)}, {fg.out(i, d)});
                
                // If we output in a direction, that field must not output back
                Field f2 = f.move(d);
                if (f2.inbounds(nx, ny)) {
                    //DEBUGsat_add(inst, implies, {f.out(i, d), ~f2.out(i, Field_var::dir_back(d))});
                    sat_addg(inst, implies, {f.belt, f2.belt, f.out(i, d)}, {~f2.out(i, Field_var::dir_back(d))});
                }
            }
        }
        
        // No beacons (yet)
        sat_add(inst, clause, {~f.beacon});
    } break;

    case field_incremental_border: {
        assert(args.size == 2);
        Field f {(s64)args[0], (s64)args[1]};
        sat_push(inst, {f.pborder});

        // A border field is precisely a border
        sat_add(inst, logical_and, {~f.prod, ~f.belt, ~f.beacon});

        // The global recipe
        Recipe rg = rdb->get(rdb->recipe_count);
        
        for (s64 i = 0; i < n_items; ++i) {
            // Having an item as input to the border means that an adjacent tile gave it to us
            sat_add(inst, implies, {f.inp(i), f.adjout(i)});
    
            // Output an item only if it is an input of the global recipe
            if (not bitset_get(rg.mask_input, i)) {
                sat_add(inst, logical_and, {~f.out(i, Field_var::DIR_ALL)});
            }
        }

        sat_pop(inst);

        for (s64 i = 0; i < n_items; ++i) {
            sat_push_add_pop(inst, {f.inp_border(i)}, logical_and, {f.pborder, f.inp(i)});
        }
    } break;
        
    }

}

bool construct_explain(Sat_instance* inst, u64 id, Array_t<u64> args, Array_dyn<u8>* into) {
    u64 type, subtype, suffix;
    sat_decompose(id, &type, &subtype, &suffix);

    if (type == Sat::VAR or type == Sat::GROUP) {
        Field_var l = construct_decompose(id);

        assert(l.kind < Field_var::KIND_SIZE);
        
        array_printf(into, "(%d,%d).%s", (int)l.x, (int)l.y, Field_var::kind_names[l.kind]);

        switch (l.kind) {
        case Field_var::PROD:
        case Field_var::BELT:
        case Field_var::BEACON: break;
        case Field_var::REC:
            if (l.type == Field_var::GROUP_REC) {} // nothing
            else if (l.type == Field_var::VAR)  array_printf(into, "[%d]", (int)l.el);
            else assert(false);
            break;
        case Field_var::INP: array_printf(into, "[%d]", (int)l.el); break;
        case Field_var::OUT:
            if (l.type == Field_var::GROUP_ADJ) {
                assert(l.dir == Field_var::UNDIRECTED);
                into->size -= 3;
                array_printf(into, "adjout[%d]", (int)l.el);
            } else {
                assert(l.dir < Field_var::DIR_SIZE);
                array_printf(into, "[%d,%s]", (int)l.el, Field_var::dir_names[l.dir]);
            }
            break;
        default: assert(false);
        }
        
        return true;
    } else if (type == Sat::CONSTRAINT) {
        switch (id) {
        case Construct::field_basic:  array_printf(into, "field_basic");  break;
        case Construct::field_border: array_printf(into, "field_border"); break;
        case Construct::field_global: array_printf(into, "field_global"); break;
        case Construct::field_global2: array_printf(into, "field_global2"); break;
        default: assert(false); break;
        }
        return false;
    } else {
        assert(false);
    }
    return false;
}

struct Construct_params {
    s64 nx = 4, ny = 4, n_items = 1000;
    s64 n_transport_max = 6, n_border_max = 6, n_partition_max = -1;
    s64 n_sparsecut_maxsize = 12, n_sparsecut_groups = 3;
    s64 partition_nx = 2, partition_ny = 2;
    Recipe_db rdb;

    Array_dyn<s64> item_names;
    Array_dyn<u8>  name_data;
    Hashmap<s64> item_idmap;
};

template <typename T>
void array_push_bytes(Array_dyn<u8>* into, T obj) {
    array_reserve(into, into->size + sizeof(T));
    into->size += sizeof(T);
    *(T*)&into->data[into->size - sizeof(T)] = obj;
}

template <typename T>
T array_get_bytes(Array_t<u8> arr, s64 offset) {
    assert(0 <= offset and offset + sizeof(T) <= arr.size);
    return *(T*)&arr.data[offset];
}


s64 construct_string_store(Construct_params* cpar, Array_t<u8> str) {
    s64 offset = cpar->name_data.size;
    array_push_bytes<s64>(&cpar->name_data, str.size);
    array_append(&cpar->name_data, str);
    return offset;
}

Array_t<u8> construct_string_load(Construct_params* cpar, s64 offset) {
    auto size = array_get_bytes<s64>(cpar->name_data, offset);
    return array_subarray(cpar->name_data, offset+sizeof(size), offset+sizeof(size) + size);
}

u64 _parse_number(Array_t<u8> digits) {
    u64 result = 0;
    for (u8 c: digits) {
        assert('0' <= c and c <= '9');
        result = result*10 + (c - '0');
    }
    return result;
}


void _digit(s64 item, FILE* f = stdout) {
    char const* colors[] = { nullptr, "\x1b[34m", "\x1b[31m", "\x1b[32m", "\x1b[33m", "\x1b[35m", "\x1b[36m" };
    char const* color = colors[item / 62 % 7];
    if (color) fputs(color, f);

    item %= 62;
    if (item < 10) {
        fputc('0' + item, f);
    } else if (item < 36) {
        fputc('a' + (item - 10), f);
    } else {
        fputc('A' + (item - 36), f);
    }
        
    if (color) fputs("\x1b[0m", f);
}

void construct_params_parse(Construct_params* cpar, Array_t<u8> data) {
    bool n_items_should_not_change = false;

    bool recipe_cur_set = false;
    Recipe recipe_cur;
    Recipe_db::Recipe_rec* recipe_cur_rec = nullptr;

    bool has_total_recipe = false;

    for (s64 i = 0; i < data.size; ++i) {
        while (i < data.size and data[i] == ' ') ++i;
        s64 word_i = i;
        while (i < data.size and data[i] != ' ' and data[i] != '\n') ++i;

        Array_t<u8> word = array_subarray(data, word_i, i);
        while (i < data.size and data[i] == ' ') ++i;
        s64 rest_i = i;
        
        while (i < data.size and data[i] != '\n') ++i;
        s64 rest_last = i;
        while (rest_i < rest_last and data[rest_last-1] == ' ') --rest_last;
        Array_t<u8> rest = array_subarray(data, rest_i, i);

        if (word.size == 0) {
            // nothing
        } else if (array_equal_str(word, "size") or array_equal_str(word, "partition_size")) {
            bool is_size = array_equal_str(word, "size");
            
            s64 j = 0;
            while (j < rest.size and rest[j] != ' ') ++j;
            assert(j < rest.size);

            Array_t<u8> num1 = array_subarray(rest, 0, j);
            while (j < rest.size and rest[j] == ' ') ++j;
            Array_t<u8> num2 = array_subarray(rest, j);

            s64 nx = _parse_number(num1);
            s64 ny = _parse_number(num2);
            assert(0 <= nx and nx <= 0xff and 0 <= ny and ny <= 0xff);

            if (is_size) {
                cpar->nx = nx; cpar->ny = ny;
            } else {
                cpar->partition_nx = nx; cpar->partition_ny = ny;
            }
        } else if (array_equal_str(word, "items")) {
            assert(not n_items_should_not_change);
            cpar->n_items = _parse_number(rest);
            cpar->rdb.n_itemmask = (cpar->n_items + 63) / 64;
            
        } else if (array_equal_str(word, "transport_max")) {
            cpar->n_transport_max = _parse_number(rest);
        } else if (array_equal_str(word, "border_max")) {
            cpar->n_border_max = _parse_number(rest);
        } else if (array_equal_str(word, "partition_max")) {
            cpar->n_partition_max = _parse_number(rest);

        } else if (array_equal_str(word, "sparsecut_maxsize")) {
            cpar->n_sparsecut_maxsize = _parse_number(rest);
        } else if (array_equal_str(word, "sparsecut_groups")) {
            cpar->n_sparsecut_groups = _parse_number(rest);
            
        } else if (array_equal_str(word, "recipe")) {
            assert(not has_total_recipe);
            n_items_should_not_change = true;
            
            auto* rdb = &cpar->rdb;
            
            array_push_back(&rdb->recs, {});
            array_append_zero(&rdb->data, 2*rdb->n_itemmask);

            recipe_cur_rec = &rdb->recs.back();
            recipe_cur_rec->name = construct_string_store(cpar, rest);
            recipe_cur_set = true;
            recipe_cur = rdb->get(rdb->recipe_count);

            if (array_equal_str(rest, "total")) {
                has_total_recipe = true;
            } else {
                ++rdb->recipe_count;
            }
        } else if (array_equal_str(word, "in") or array_equal_str(word, "out")) {
            bool isin = word[0] == 'i';
            assert(recipe_cur_set);

            u64 hash = hash_str(rest);
            s64* item_id = hashmap_getcreate(&cpar->item_idmap, hash, (s64)-1);
            if (*item_id == -1) {
                *item_id = cpar->item_names.size;
                s64 name_offset = construct_string_store(cpar, rest);
                array_push_back(&cpar->item_names, name_offset);
            }

            assert(*item_id < cpar->n_items);

            if (isin) {
                bitset_set(&recipe_cur.mask_input,  *item_id, true);
            } else {
                bitset_set(&recipe_cur.mask_output, *item_id, true);
            }
            if (bitset_get(recipe_cur.mask_input, *item_id) and bitset_get(recipe_cur.mask_output, *item_id)) {
                fprintf(stderr, "Warning: recipe ");
                auto name = construct_string_load(cpar, recipe_cur.name);
                fwrite(name.data, 1, name.size, stderr);
                fprintf(stderr, " has item ");
                fwrite(rest.data, 1, rest.size, stderr);
                fprintf(stderr, " as both input and output. Output will be ignored.\n");
                bitset_set(&recipe_cur.mask_output, *item_id, false);
            }
        } else if (array_equal_str(word, "beacon")) {
            recipe_cur_rec->beacons = _parse_number(rest);
        } else if (array_equal_str(word, "entity")) {
            recipe_cur_rec->entity = construct_string_store(cpar, rest);
        } else if (array_equal_str(word, "entity_speed")
                or array_equal_str(word, "time") or array_equal_str(word, "amount")) {
            // nothing
        } else {
            assert(false);
        }
    }

    if (cpar->item_names.size != cpar->n_items) {
        fprintf(stderr, "Warning: Instance specifies a wrong number of items (given: %lld, actual: %lld)\n",
             cpar->n_items, cpar->item_names.size);
    }

    FILE* f = fopen("names.out", "w");
    fputs("Items:\n", f);
    for (s64 i = 0; i < cpar->item_names.size; ++i) {
        fputs("  ", f);
        fprintf(f, "%3lld,%3lld ", i, i+cpar->n_items);
        _digit(i, f);
        fputc(',', f);
        _digit(i + cpar->n_items, f);
        fputs(": ", f);
        auto name = construct_string_load(cpar, cpar->item_names[i]);
        fwrite(name.data, 1, name.size, f);
        fputc('\n', f);
    }
    fputs("Recipes:\n", f);
    for (s64 i = 0; i <= cpar->rdb.recipe_count; ++i) {
        fputs("  ", f);
        fprintf(f, "%3lld ", i);
        _digit(i, f);
        fputs(": ", f);
        auto r = cpar->rdb.get(i);
        auto name = construct_string_load(cpar, r.name);
        fwrite(name.data, 1, name.size, f);
        fputs(",  ", f);
        for (s64 j = 0; j < cpar->n_items; ++j) {
            if (bitset_get(r.mask_input, j)) _digit(j, f);
        }
        fputs(" -> ", f);
        for (s64 j = 0; j < cpar->n_items; ++j) {
            if (bitset_get(r.mask_output, j)) _digit(j, f);
        }
        fputc('\n', f);
    }
    fclose(f);
    
}

void construct_params_instance(Construct_params* params, Sat_instance* inst) {
    using namespace Construct;

    hashmap_set(&inst->params, param_nx, params->nx);
    hashmap_set(&inst->params, param_ny, params->ny);
    hashmap_set(&inst->params, param_n_items, params->n_items * 2);
    hashmap_set(&inst->params, param_recipes, (s64)&params->rdb);
    
    sat_register_expand_func(inst, GROUP_CONSTRUCT, &construct_expand);
    sat_register_rewrite_func(inst, CONSTRAINT_CONSTRUCT, &construct_rewrite);
    for (u64 i: {VAR_CONSTRUCT, GROUP_CONSTRUCT, CONSTRAINT_CONSTRUCT}) {
        sat_register_explain_func(inst, i, &construct_explain);
    }
    
    for (s64 y = 0; y < params->ny; ++y) {
        for (s64 x = 0; x < params->nx; ++x) {
            sat_add(inst, field_basic, {(u64)x, (u64)y});
        }
    }
    sat_add(inst, field_border, {});
    sat_add(inst, field_global, {});
    sat_add(inst, use_all_recipes, {});
}

void construct_partition_instance(Construct_params* params, Sat_instance* inst) {
    using namespace Construct;

    hashmap_set(&inst->params, param_nx, params->partition_nx);
    hashmap_set(&inst->params, param_ny, params->partition_ny);
    hashmap_set(&inst->params, param_n_items, params->n_items);
    hashmap_set(&inst->params, param_n_transport_max, params->n_transport_max);
    hashmap_set(&inst->params, param_n_border_max,    params->n_border_max);
    hashmap_set(&inst->params, param_n_partition_max, params->n_partition_max);
    
    hashmap_set(&inst->params, param_recipes, (s64)&params->rdb);
    
    sat_register_expand_func(inst, GROUP_CONSTRUCT, &construct_expand);
    sat_register_rewrite_func(inst, CONSTRAINT_CONSTRUCT, &construct_rewrite);
    for (u64 i: {VAR_CONSTRUCT, GROUP_CONSTRUCT, CONSTRAINT_CONSTRUCT}) {
        sat_register_explain_func(inst, i, &construct_explain);
    }
    
    for (s64 y = 0; y < params->partition_ny; ++y) {
        for (s64 x = 0; x < params->partition_nx; ++x) {
            sat_add(inst, partition_field, {(u64)x, (u64)y});
        }
    }
    sat_add(inst, partition_border, {});
    sat_add(inst, use_all_recipes, {});    
}

void construct_refine_instance(Construct_params* params, Sat_instance* inst, Sat_solution* sol) {
    using namespace Construct;

    s64 psize_x = (params->nx + params->partition_nx-1) / params->partition_nx;
    s64 psize_y = (params->ny + params->partition_ny-1) / params->partition_ny;
    s64 nx = psize_x * params->partition_nx;
    s64 ny = psize_y * params->partition_ny;
    
    hashmap_set(&inst->params, param_nx, nx);
    hashmap_set(&inst->params, param_ny, ny);
    hashmap_set(&inst->params, param_n_items, 2*params->n_items);
    hashmap_set(&inst->params, param_recipes, (s64)&params->rdb);
    
    sat_register_expand_func(inst, GROUP_CONSTRUCT, &construct_expand);
    sat_register_rewrite_func(inst, CONSTRAINT_CONSTRUCT, &construct_rewrite);
    for (u64 i: {VAR_CONSTRUCT, GROUP_CONSTRUCT, CONSTRAINT_CONSTRUCT}) {
        sat_register_explain_func(inst, i, &construct_explain);
    }

    for (s64 y = 0; y < params->partition_ny; ++y) {
        for (s64 x = 0; x < params->partition_nx; ++x) {
            sat_add(inst, field_global2, {(u64)x, (u64)y, (u64)psize_x, (u64)psize_y});
        }
    }
    
    for (s64 y = 0; y < ny; ++y) {
        for (s64 x = 0; x < nx; ++x) {
            sat_add(inst, field_basic, {(u64)x, (u64)y});

            Field f {x, y};
            Field fp {(x+psize_x) / psize_x - 1, (y+psize_y) / psize_y - 1};

            for (s64 i = 0; i < params->rdb.recipe_count; ++i) {
                // If the recipe is not present in the partition at this place, do not use it
                if (not (*sol)[fp.rec(i)]) sat_add(inst, Sat::clause, {~f.rec(i)});
            }

            for (u8 d = Field_var::BEG; d < Field_var::END; ++d) {
                Field f2 = f.move(d);
                Field fp2 {(f2.x+psize_x) / psize_x - 1, (f2.y+psize_y) / psize_y - 1};
                if (fp.x == fp2.x and fp.y == fp2.y) continue;

                for (s64 i = 0; i < params->n_items; ++i) {
                    // Do not transport an item if it was not transported in the partition
                    if (not (*sol)[fp.out(i, d)]) {
                        sat_add(inst, Sat::clause, {~f.out(i, d)});
                    }

                    // Do not transport tainted items
                    sat_add(inst, Sat::clause, {~f.out(i + params->n_items, d)});
                }
            }
        }
    }


    //s64 size = inst->rewrite_temp.size;
    //defer { inst->rewrite_temp.size = size; };
    //
    //for (s64 item = 0; item < params->n_items; ++item) {
    //    for (u64 lit: sat_expand(inst, Field::border().out(item+params->n_items, Field_var::DIR_ALL), &inst->rewrite_temp)) {
    //        Field_var v = construct_decompose(lit);
    //        Field_var vp = v;
    //        vp.x /= psize_x; vp.y /= psize_y;
    //        vp.el -= params->n_items;
    //
    //        // If the item is not output by the border here, we do not output it (the tainted version) either
    //        if (not (*sol)[construct_compose(vp)]) sat_add(inst, Sat::clause, {~lit});
    //    }
    //
    //    for (u64 lit: sat_expand(inst, Field::border().adjout(item), &inst->rewrite_temp)) {
    //        Field_var v = construct_decompose(lit);
    //        Field_var vp = v; vp.x /= psize_x; vp.y /= psize_y;
    //
    //        // If the item is not output to the border here, we do not output it either
    //        if (not (*sol)[construct_compose(vp)]) sat_add(inst, Sat::clause, {~lit});
    //    }
    //}
             
    sat_add(inst, field_border, {});
    sat_add(inst, use_all_recipes, {});    
}


void construct_partition_different(Construct_params* params, Sat_instance* inst, Sat_solution* sol) {
    s64 index = inst->rewrite_temp.size;
    defer { inst->rewrite_temp.size = index; };

    auto mirror_x = [params](Array_t<u64> lits) {
        for (u64& lit: lits) {
            Field_var v = construct_decompose(lit);
            v.x = params->partition_nx-1 - v.x;
            lit = construct_compose(v);
        }
    };
    auto mirror_y = [params](Array_t<u64> lits) {
        for (u64& lit: lits) {
            Field_var v = construct_decompose(lit);
            v.y = params->partition_ny-1 - v.y;
            lit = construct_compose(v);
        }
    };
    auto flip = [params](Array_t<u64> lits) {
        for (u64& lit: lits) {
            Field_var v = construct_decompose(lit);
            simple_swap(&v.x, &v.y);
            lit = construct_compose(v);
        }
    };
    
    for (s64 y = 0; y < params->partition_ny; ++y) {
        for (s64 x = 0; x < params->partition_nx; ++x) {
            for (s64 i = 0; i < params->rdb.recipe_count; ++i) {
                Field f {x, y};
                array_push_back(&inst->rewrite_temp, f.rec(i) ^ (u64)-(*sol)[f.rec(i)]);
            }
        }
    }

    auto arr = array_subarray(inst->rewrite_temp, index);
    sat_add(inst, Sat::clause, arr);
    mirror_x(arr);
    sat_add(inst, Sat::clause, arr);
    mirror_y(arr);
    sat_add(inst, Sat::clause, arr);
    mirror_x(arr);
    sat_add(inst, Sat::clause, arr);

    if (params->partition_nx == params->partition_ny) {
        flip(arr);
        sat_add(inst, Sat::clause, arr);
        mirror_x(arr);
        sat_add(inst, Sat::clause, arr);
        mirror_y(arr);
        sat_add(inst, Sat::clause, arr);
        mirror_x(arr);
        sat_add(inst, Sat::clause, arr);
    }
}


void sat_solver_parse(
    Sat_dimacs* dimacs, Array_t<u8> output,
    Sat_solution* out_sol = nullptr, s8* out_status = nullptr, s64* out_size_parsed = nullptr
) {
    s64 last = 0;
    for (s64 i = 0; i < output.size; ++i) {
        if (output[i] != '\n') continue;
        
        if (output[last] == 'v') {
            s64 val = 0;
            bool isneg = false;
            for (s64 j = last+2; j <= i; ++j) {
                u8 c = j < i ? output[j] : ' ';
                if (c == '-') {
                    isneg ^= true;
                } else if ('0' <= c and c <= '9') {
                    val = 10 * val + (c - '0');
                } else if (c == ' ') {
                    if (val) {
                        u64 var = dimacs->map_back[val];
                        if (out_sol) out_sol->set(isneg ? ~var : var);
                        val = 0; isneg = false;
                    }
                }
            }
        } else {
            auto str = array_subarray(output, last, i+1);
            fwrite(str.data, 1, str.size, stdout);
            
            if (str.size > 0 and str[0] == 's') {
                auto result = array_subarray(str, 2, 5);
                if (array_equal_str(result, "SAT")) {
                    if (out_status) *out_status = 1;
                } else if (array_equal_str(result, "UNS")) {
                    if (out_status) *out_status = 0;
                } else {
                    fputs("  ^~~ not recognised\n", stdout);
                }
            }
        }
        last = i+1;
    }

    if (out_size_parsed) *out_size_parsed = last;
}

void sat_debug_variables(Sat_instance* inst) {
    Sat_dimacs dimacs;
    sat_write_dimacs(inst, &dimacs);
    defer { sat_dimacs_free(&dimacs); };

    FILE* f = fopen("variables.out", "w");
    Array_dyn<u8> temp;
    for (u64 lit: dimacs.map_back) {
        if (not lit) continue;
        temp.size = 0;
        sat_explain(inst, lit, &temp);
        fwrite(temp.data, 1, temp.size, f);
        fputs("\n", f);
    }
    fclose(f);
}

template <typename T>
Array_t<T> array_load_from_file_type(Array_t<u8> path) {
    auto arr = array_load_from_file(path);
    assert(arr.size % sizeof(T) == 0);
    return {(T*)arr.data, (s64)(arr.size / sizeof(T))};
}

bool sat_solver_run(Sat_instance* inst, Array_t<u8> solver, Sat_solution* out_sol, Sat_proof* out_proof=nullptr, bool use_temp_file=false) {
    assert(solver.data[solver.size] == 0);
    assert(out_sol);
    
    Sat_dimacs dimacs;
    sat_write_dimacs(inst, &dimacs);
    defer { sat_dimacs_free(&dimacs); };

    FILE* f = fopen("dimacs.out", "w");
    fwrite(dimacs.text.data, 1, dimacs.text.size, f);
    fclose(f);

    u64 hash = hash_str(dimacs.text) ^ (bool)out_proof;
    char buf[64] = {};
    assert(snprintf(buf, sizeof(buf), "cache/inst-%016llx.sol", hash)+1 < sizeof(buf));
    {if (mkdir("cache", 0775)) {
        if (errno != EEXIST) goto error;
    }
    if (access(buf, R_OK) == 0) {
        //fprintf(stdout, "Load from cache at '%s'...\n", buf);
        Array_t<u64> temp = array_load_from_file_type<u64>(array_create_str(buf));
        defer { array_free(&temp); };

        if (temp.size == 0) {
            fprintf(stdout, "Error: cache file is invalid. To delete the file\n  rm %s\n", buf);
            exit(14);
        } else if (temp[0]) {
            // SAT
            for (u64 lit: array_subarray(temp, 1)) out_sol->set(lit);
        } else {
            // UNSAT
            if (out_proof) {
                s64 size0 = temp[1];
                s64 size1 = temp[2];
                array_append((Array_dyn<u64>*)&out_proof->offsets, array_subarray(temp, 3, 3+size0));
                array_append(&out_proof->data, array_subarray(temp, 3+size0, 3+size0+size1));
            }
        }
        return temp[0];
    }

    int fd_cache = open(buf, O_CREAT | O_EXCL | O_WRONLY, 0664);
    assert(fd_cache != -1);
    
    if (out_proof and out_proof->offsets.size == 0) {
        array_push_back(&out_proof->offsets, out_proof->data.size);
    }
    
    {int pipe_solver_to  [2] = {-1, -1};
    int pipe_solver_from[2] = {};
    int pipe_proof[2] = {-1, -1};
    int temp_file = -1;
    if (use_temp_file) {
        temp_file = open("sat_instance.out", O_CREAT | O_RDWR | O_TRUNC, 0664);
        //temp_file = open("/tmp", O_TMPFILE | O_RDWR);
        if (temp_file == -1) goto error;
    } else {
        if (pipe(pipe_solver_to)) goto error;
    }
    if (pipe(pipe_solver_from)) goto error;
    if (out_proof and pipe(pipe_proof)) goto error;
    
    {pid_t pid = fork();
    if (pid == 0) {
        if (use_temp_file) {
            if (dup2(temp_file, STDIN_FILENO)  == -1) goto error;
        } else {
            if (dup2(pipe_solver_to[0], STDIN_FILENO)  == -1) goto error;
            if (close(pipe_solver_to[1])) goto error;
        }
        if (dup2(pipe_solver_from[1], STDOUT_FILENO) == -1) goto error;
        if (dup2(pipe_solver_from[1], STDERR_FILENO) == -1) goto error;
        if (close(pipe_solver_from[0])) goto error;
        
        //char* argv[] = {(char*)solver.data, "-max-memory=20", "-c=11", "/dev/stdin", nullptr, nullptr};
        char* argv[] = {(char*)solver.data, "/dev/stdin", nullptr, nullptr};
        int argc = 2;
        if (pipe_proof[0] != -1) {
            if (close(pipe_proof[0])) goto error;
            
            char buf[32] = {};
            assert(snprintf(buf, sizeof(buf), "/dev/fd/%d", pipe_proof[1]) <= sizeof(buf));

            argv[argc] = buf;
            argv[argc+1] = nullptr;
        }
        execvp((char*)solver.data, argv);
        goto error;
    } else if (pid == -1) goto error;

    if (close(pipe_solver_from[1])) goto error;
    if (not use_temp_file and close(pipe_solver_to[0])) goto error;
    if (pipe_proof[0] != -1 and close(pipe_proof[1])) goto error;
    
    {int solver_read_fd = pipe_solver_from[0];
    int solver_write_fd = use_temp_file ? temp_file : pipe_solver_to[1];
    int proof_read_fd = pipe_proof[0];

    Array_dyn<u8> line_buf;
    defer { array_free(&line_buf); };
    Array_t<u8> to_write = dimacs.text;
    Array_dyn<u8> proof_buf;
    defer { array_free(&proof_buf); };
    
    s8 status = -1;

    while (solver_read_fd != -1 or solver_write_fd != -1 or proof_read_fd != -1) {
        struct pollfd pfds[3];
        memset(pfds, 0, sizeof(pfds));

        pfds[0].fd = solver_read_fd ; pfds[0].events = POLLIN;
        pfds[1].fd = solver_write_fd; pfds[1].events = POLLOUT;
        pfds[2].fd = proof_read_fd  ; pfds[2].events = POLLIN;

        int code = poll(pfds, sizeof(pfds) / sizeof(pfds[0]), -1);
        if (code == -1) goto error;
        else if (code == 0) {
            fprintf(stderr, "Error: poll() thinks there is a timeout\n");
            goto error2;
        }

        if ((pfds[0].revents | pfds[1].revents | pfds[2].revents) & POLLERR) {
            fprintf(stderr, "Error: I cannot handle POLLERR\n");
            goto error2;
        } else if ((pfds[0].revents | pfds[1].revents | pfds[2].revents) & POLLNVAL) {
            fprintf(stderr, "Error: I cannot handle POLLNVAL\n");
            goto error2;
        }

        if (pfds[0].revents & (POLLIN | POLLHUP)) {
            array_reserve(&line_buf, line_buf.size + 256);
            ssize_t n = read(pfds[0].fd, line_buf.data + line_buf.size, line_buf.capacity - line_buf.size);
            if (n < 0) goto error;
            else if (n == 0) {
                if (close(pfds[0].fd)) goto error;
                solver_read_fd = -1;
            } else line_buf.size += n;

            s64 last;
            sat_solver_parse(&dimacs, line_buf, out_sol, &status, &last);
            if (0 < last and last < line_buf.size) {
                memmove(&line_buf[0], &line_buf[last], line_buf.size - last);
            }
            line_buf.size -= last;
        }

        if (pfds[1].revents & POLLOUT) {
            s64 wn = to_write.size;
            if (wn > 4096) wn = 4096;
            ssize_t n = write(pfds[1].fd, to_write.data, wn);
            if (n < 0) goto error;
            else if (n == 0) {}
            else to_write = array_subarray(to_write, n);

            if (to_write.size == 0) {
                if (close(pfds[1].fd)) goto error;
                solver_write_fd = -1;
            }
        }
        
        if (pfds[2].revents & (POLLIN | POLLHUP)) {
            array_reserve(&proof_buf, proof_buf.size + 256);
            ssize_t n = read(pfds[2].fd, proof_buf.data + proof_buf.size, proof_buf.capacity - proof_buf.size);
            if (n < 0) goto error;
            else if (n == 0) {
                if (close(pfds[2].fd)) goto error;
                proof_read_fd = -1;
            } else proof_buf.size += n;

            // see https://github.com/marijnheule/drat-trim for a description of the DRAT format
            s64 last = 0;
            for (s64 i = 0; i < proof_buf.size; ++i) {
                if (proof_buf[i] != 0) continue;

                if (proof_buf[last] == 'a') {
                    auto clause = array_subarray(proof_buf, last+1, i);
                    u64 val = 0;
                    s64 val_i = 0;
                    for (u8 c: clause) {
                        val |= (c & 0x7f) << (val_i*7);
                        if (c & 0x80) {
                            ++val_i;
                        } else {
                            u64 var = dimacs.map_back[val >> 1];
                            array_push_back(&out_proof->data, var ^ -(val&1));
                            val = 0;
                            val_i = 0;
                        }
                    }
                    array_push_back(&out_proof->offsets, out_proof->data.size);
                } else if (proof_buf[last] == 'd') {
                    // ignore
                } else {
                    assert(false);
                }

                last = i+1;
            }
            if (0 < last and last < proof_buf.size) {
                memmove(proof_buf.data, proof_buf.data+last, proof_buf.size - last);
            }
            proof_buf.size -= last;
        }
    }

    assert(status != -1);
    s64 status_s64 = status;
    if (write(fd_cache, &status_s64, sizeof(status_s64)) < sizeof(status_s64)) goto error;
    
    if (status) {
        Array_dyn<u64> temp;
        defer { array_free(&temp); };
        sat_solution_write(out_sol, &temp);
        if (write(fd_cache, temp.data, temp.size * sizeof(temp[0])) < temp.size * sizeof(temp[0])) goto error;
    } else {
        if (out_proof) {
            if (write(fd_cache, &out_proof->offsets.size, sizeof(s64)) < sizeof(status_s64)) goto error;
            if (write(fd_cache, &out_proof->data   .size, sizeof(s64)) < sizeof(status_s64)) goto error;
            if (write(fd_cache, out_proof->offsets.data, out_proof->offsets.size*sizeof(s64)) < out_proof->offsets.size*sizeof(s64)) goto error;
            if (write(fd_cache, out_proof->data.data, out_proof->data.size*sizeof(u64)) < out_proof->data.size*sizeof(u64)) goto error;
        }
    }
    close(fd_cache);
    
    return status;}}}}
    
  error:
    perror("Error");
  error2:
    exit(3);
}

void construct_solution_print(Construct_params* cpar, Sat_instance* inst, Sat_solution* sol, bool do_global=true, bool do_border=true, Array_t<s64> item_map_back = {}, Array_t<s64> recipe_map_back = {}) {
    s64 n_items = hashmap_get(&inst->params, Construct::param_n_items);
    s64 recipe_count = recipe_map_back.size == 0 ? cpar->rdb.recipe_count : recipe_map_back.size;
    
    auto count_inp = [cpar, sol, n_items](Field f) {
        s64 count = 0;
        for (s64 item = 0; item < n_items; ++item) count += (*sol)[f.inp(item)];
        return count;
    };
    auto count_out = [cpar, sol, n_items](Field f, u8 d) {
        s64 count = 0;
        for (s64 item = 0; item < n_items; ++item) count += (*sol)[f.out(item, d)];
        return count;
    };
    auto count_rec = [cpar, sol, n_items, recipe_count](Field f) {
        s64 count = 0;
        for (s64 i = 0; i < recipe_count; ++i) count += (*sol)[f.rec(i)];
        return count;
    };
    auto pad = [](s64 n) {
        for (s64 i = 0; i < n; ++i) fputc(' ', stdout);
    };
    auto _digit_i = [&item_map_back](s64 n) {
        _digit(item_map_back.size == 0 ? n : item_map_back[n]);
    };
    auto _digit_r = [&recipe_map_back](s64 n) {
        _digit(recipe_map_back.size == 0 ? n : recipe_map_back[n]);
    };

    s8 nx = (s8)hashmap_get(&inst->params, Construct::param_nx);
    s8 ny = (s8)hashmap_get(&inst->params, Construct::param_ny);
    
    s64 max_outputs = 0;
    for (s64 y = ny-1; y >= 0; --y) {
        for (s64 x = 0; x < nx; ++x) {
            Field f {x, y};

            {s64 count = count_inp(f);
            if (max_outputs < count) max_outputs = count;}
            
            {s64 count = count_rec(f);
            if (max_outputs < count) max_outputs = count;}
            
            for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                s64 count = count_out(f, d);
                if (max_outputs < count) max_outputs = count;
            }

        }
    }

    s64 w = max_outputs + 2;

    fputs("Fields:\n", stdout);
    for (s64 y = ny-1; y >= 0; --y) {
        if (y < ny-1) {
            fputs("  ", stdout);
            for (s64 x = 0; x < nx; ++x) {
                if (x) fputs(u8"???", stdout);
                for (s64 i = 0; i < w; ++i) fputs(u8"???", stdout);
            }
            fputs("\n", stdout);
        }
        
        fputs("  ", stdout);
        for (s64 x = 0; x < nx; ++x) {
            if (x) fputs(u8"???", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::TOP);
            pad((w-count) / 2);
            for (s64 item = 0; item < n_items; ++item) {
                if ((*sol)[f.out(item, Field_var::TOP)]) _digit_i(item);
            }
            pad((w-count + 1) / 2);
        }
        fputs("\n", stdout);
        
        fputs("  ", stdout);
        for (s64 x = 0; x < nx; ++x) {
            if (x) fputs(u8"???", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::LEFT);
            fputs("< ", stdout);
            for (s64 item = 0; item < n_items; ++item) {
                if ((*sol)[f.out(item, Field_var::LEFT)]) _digit_i(item);
            }
            pad(w-count-2);
        }
        fputs("\n", stdout);
        
        fputs("  ", stdout);
        for (s64 x = 0; x < nx; ++x) {
            if (x) fputs(u8"???", stdout);
            Field f {x, y};
            s64 count = count_inp(f);
            pad((w-count)/2);
            for (s64 item = 0; item < n_items; ++item) {
                if ((*sol)[f.inp(item)]) _digit_i(item);
            }
            pad((w-count+1)/2);
        }
        fputs("\n", stdout);

        fputs("  ", stdout);
        for (s64 x = 0; x < nx; ++x) {
            if (x) fputs(u8"???", stdout);
            Field f {x, y};
            if ((*sol)[f.belt]) {
                pad((w-1) / 2);
                fputs("#", stdout);
                pad((w-1+1) / 2);
            }
            if ((*sol)[f.prod]) {
                s64 count = 0;
                for (s64 r = 0; r < recipe_count; ++r) count += (*sol)[f.rec(r)];

                pad((w-count) / 2);
                for (s64 r = 0; r < recipe_count; ++r) {
                    if ((*sol)[f.rec(r)]) _digit_r(r);
                }
                pad((w-count+1) / 2);
            }
            if ((*sol)[f.beacon]) {
                pad((w-1) / 2);
                fputs("*", stdout);
                pad((w-1+1) / 2);
            }
            if ((*sol)[f.pborder]) {
                pad((w-1) / 2);
                fputs("+", stdout);
                pad((w-1+1) / 2);
            } 
        }
        fputs("\n", stdout);
        
        fputs("  ", stdout);
        for (s64 x = 0; x < nx; ++x) {
            if (x) fputs(u8"???", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::RIGHT);
            pad(w-count-2);
            for (s64 item = 0; item < n_items; ++item) {
                if ((*sol)[f.out(item, Field_var::RIGHT)]) _digit_i(item);
            }
            fputs(" >", stdout);
        }
        fputs("\n", stdout);

        fputs("  ", stdout);
        for (s64 x = 0; x < nx; ++x) {
            if (x) fputs(u8"???", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::BOTTOM);
            pad((w-count) / 2);
            for (s64 item = 0; item < n_items; ++item) {
                if ((*sol)[f.out(item, Field_var::BOTTOM)]) _digit_i(item);
            }
            pad((w-count + 1) / 2);
        }
        fputs("\n", stdout);        
    }

    if (do_global and false) {
        fputs("Global:\n  ", stdout);
        {Field f = Field::global();
            for (s64 i = 0; i < n_items; ++i) {
                if (i) fputs(", ", stdout);
                _digit_i(i);
                fputc(' ', stdout);
                char const dn[] = "rtlb";
                for (u8 d = Field_var::BEG; d < Field_var::END; ++d) {
                    fputc((*sol)[f.out(i, d)] ? dn[d-Field_var::BEG] : '-', stdout);
                }
            }}
        fputs("\n", stdout);
    }

    if (do_border) {
        fputs("Border:", stdout);
        {Field f = Field::border();
        fputs("\n  inputs:  ", stdout);
        for (s64 i = 0; i < n_items; ++i) {
            if ((*sol)[f.inp(i)]) _digit(i);
        }
        fputs("\n  outputs:\n", stdout);
        for (u64 v1_lit: sat_expand(inst, f.out(0, Field_var::DIR_ALL), &inst->explain_temp)) {
            Field_var v1 = construct_decompose(v1_lit);
            fprintf(stdout, "  (%2d,%2d): ", (int)v1.x, (int)v1.y);
            for (s64 i = 0; i < n_items; ++i) {
                Field_var v2 = v1;
                v2.el = i;
                if ((*sol)[construct_compose(v2)]) _digit(i);
            }
            fputs("\n", stdout);
        }
        }
        fputs("\n", stdout);
    }
}

void construct_solution_partition_print(Construct_params* cpar, Sat_instance* inst, Sat_solution* sol) {
    for (s64 y = 0; y < cpar->partition_ny; ++y) {
        for (s64 x = 0; x < cpar->partition_nx; ++x) {
            printf("Field (%lld,%lld)\n", x, y);
            Field f {x, y};
            printf("  recipes\n");
            for (s64 i = 0; i < cpar->rdb.recipe_count; ++i) {
                Recipe rec = cpar->rdb.get(i);
                if ((*sol)[f.rec(i)]) {
                    Array_t<u8> name = construct_string_load(cpar, rec.name);
                    printf("    ");
                    fwrite(name.data, 1, name.size, stdout);
                    printf("\n");
                }
            }
            for (u8 d = Field_var::BEG; d < Field_var::END; ++d) {
                printf("  input %d\n", (int)d);
                Field f2 = f.move(d);
                for (s64 i = 0; i < cpar->n_items; ++i) {
                    if ((*sol)[f2.out(i, Field_var::dir_back(d))]) {
                        Array_t<u8> name = construct_string_load(cpar, cpar->item_names[i]);
                        printf("    ");
                        fwrite(name.data, 1, name.size, stdout);
                        printf("\n");
                    }
                }
            }
            for (u8 d = Field_var::BEG; d < Field_var::END; ++d) {
                printf("  output %d\n", (int)d);
                for (s64 i = 0; i < cpar->n_items; ++i) {
                    if ((*sol)[f.out(i, d)]) {
                        Array_t<u8> name = construct_string_load(cpar, cpar->item_names[i]);
                        printf("    ");
                        fwrite(name.data, 1, name.size, stdout);
                        printf("\n");
                    }
                }
            }
        }
    }
}


void construct_print_stacktrace(Sat_instance* inst, Array_dyn<u8>* into) {
    Array_dyn<s64> offsets;
    Array_dyn<Sat_propagation> props;
    sat_solution_from_instance(inst, &offsets, &props);

    if (not sat_solution_propagations_conflict_only(inst, &props)) {
        array_printf(into, "Sadly, there was no conflict to print. Full propagations:\n");
    }
    sat_solution_propagations_print(inst, props, into);
}

void construct_do_stupid_solution(Construct_params* cpar, Sat_instance* inst) {
    assert(cpar->nx == 2 and cpar->ny >= cpar->rdb.recipe_count);
    using namespace Sat;
    
    for (s64 r_it = 0; r_it < cpar->rdb.recipe_count; ++r_it) {
        Recipe r = cpar->rdb.get(r_it);
        Field f {0, r_it}, f2 {1, r_it};
        sat_add(inst, logical_and, {f.belt, f2.prod, f2.rec(r_it)});
        
        for (s64 i = 0; i < cpar->n_items; ++i) {
            sat_add(inst, clause, {f.inp(i)});
            if (bitset_get(r.mask_input,  i)) {
                sat_add(inst, clause, {f.out(i, Field_var::RIGHT)});
            }
        }
    }
}

void construct_do_stupid_refinement(Construct_params* cpar, Sat_instance* inst, Sat_solution* sol) {
    using namespace Sat;
    s64 size_y = (1+cpar->n_partition_max);
    assert(cpar->nx == 2*cpar->partition_nx);
    assert(cpar->ny == cpar->partition_ny*size_y);

    for (s64 py = 0; py < cpar->partition_ny; ++py) {
        for (s64 px = 0; px < cpar->partition_nx; ++px) {
            Field fp {px, py};
            s64 count = 0;
            for (s64 r_it = 0; r_it < cpar->rdb.recipe_count; ++r_it) {
                if (not (*sol)[fp.rec(r_it)]) continue;
                
                Recipe r = cpar->rdb.get(r_it);
                Field f  {2*px,   size_y*py + count};
                Field f2 {2*px+1, size_y*py + count};
                sat_add(inst, clause, {f.rec(r_it)});
                sat_add(inst, clause, {f2.belt});
                ++count;

                for (s64 i = 0; i < cpar->n_items; ++i) {
                    sat_add(inst, clause, {~f.out(i, Field_var::TOP)});
                    sat_add(inst, clause, {~f.out(i, Field_var::LEFT)});
                    sat_add(inst, clause, {~f.out(i, Field_var::BOTTOM)});
                    sat_add(inst, clause, {~f.out(i + cpar->n_items, Field_var::TOP)});
                    sat_add(inst, clause, {~f.out(i + cpar->n_items, Field_var::LEFT)});
                    sat_add(inst, clause, {~f.out(i + cpar->n_items, Field_var::BOTTOM)});
                    if (bitset_get(r.mask_output, i)) {
                        sat_add(inst, clause, {f.out(i, Field_var::RIGHT)});
                    }
                    if (bitset_get(r.mask_input, i)) {
                        sat_add(inst, clause, {f2.out(i, Field_var::LEFT), f2.out(i+cpar->n_items, Field_var::LEFT)});
                    }
                }
            }
            assert(count < size_y);
            for (; count+1 < size_y; ++count) {
                Field f  {2*px,   size_y*py + count};
                Field f2 {2*px+1, size_y*py + count};
                sat_add(inst, clause, {f.belt});
                sat_add(inst, clause, {f2.belt});
                for (s64 i = 0; i < 2*cpar->n_items; ++i) {
                    sat_add(inst, clause, {~f.inp(i)});
                }
            }

            Field f  {2*px,   size_y*py + count};
            Field f2 {2*px+1, size_y*py + count};
            Field f3 {2*px+1, size_y*py};
            sat_add(inst, clause, {f.belt});
            sat_add(inst, clause, {f2.belt});
            for (s64 i = 0; i < cpar->n_items; ++i) {
                if ((*sol)[fp.out(i, Field_var::RIGHT)]) {
                    sat_add(inst, clause, {f2.out(i, Field_var::RIGHT)});
                } else {
                    sat_add(inst, clause, {~f2.out(i, Field_var::RIGHT)});
                }
                if ((*sol)[fp.out(i, Field_var::BOTTOM)]) {
                    sat_add(inst, clause, {f3.out(i, Field_var::BOTTOM)});
                } else {
                    sat_add(inst, clause, {~f3.out(i, Field_var::BOTTOM)});
                }
                if ((*sol)[fp.out(i, Field_var::LEFT)]) {
                    sat_add(inst, clause, {f.out(i, Field_var::LEFT)});
                } else {
                    sat_add(inst, clause, {~f.out(i, Field_var::LEFT)});
                }
                if ((*sol)[fp.out(i, Field_var::TOP)]) {
                    sat_add(inst, clause, {f2.out(i, Field_var::TOP)});
                } else {
                    sat_add(inst, clause, {~f2.out(i, Field_var::TOP)});
                }

                sat_add(inst, clause, {~f3.out(i+cpar->n_items, Field_var::BOTTOM)});
                sat_add(inst, clause, {~f .out(i+cpar->n_items, Field_var::LEFT)});
                sat_add(inst, clause, {~f .out(i, Field_var::TOP)});
                sat_add(inst, clause, {~f .out(i+cpar->n_items, Field_var::TOP)});
                sat_add(inst, clause, {~f2.out(i+cpar->n_items, Field_var::TOP)});
                sat_add(inst, clause, {~f2.out(i+cpar->n_items, Field_var::RIGHT)});

                for (s64 y = 0; y+1 < size_y; ++y) {
                    Field ff {2*px+1, size_y*py + y};
                    sat_add(inst, clause, {~ff.out(i, Field_var::RIGHT)});
                    sat_add(inst, clause, {~ff.out(i+cpar->n_items, Field_var::RIGHT)});
                }
            }
            
        }
    }
}


void construct_make_blueprint(Construct_params* cpar, Sat_instance* inst, Sat_solution* ssol) {
    Sat_solution& sol = *ssol;

    FILE* f = fopen("blueprint.out", "w");
    fputs("{\"blueprint\":{\"item\": \"blueprint\",\"version\": 281479273316352,\"entities\": [\n", f);

    s64 entity_count = 0;
    for (s64 y = 0; y < cpar->ny; ++y) {
        for (s64 x = 0; x < cpar->nx; ++x) {
            Field fi {x, y};
            if (sol[fi.prod]) {
                s64 recipe = -1;
                for (s64 r = 0; r < cpar->rdb.recipe_count; ++r) {
                    if (sol[fi.rec(r)]) {
                        recipe = r; break;
                    }
                }
                assert(recipe != -1);

                Recipe rec = cpar->rdb.get(recipe);
                Array_t<u8> name = construct_string_load(cpar, rec.name);
                Array_t<u8> entity = construct_string_load(cpar, rec.entity);
                if (entity_count) fputs(",", f);
                fputs("{\"name\":\"", f);
                fwrite(entity.data, 1, entity.size, f);
                fputs("\",\"recipe\":\"", f);
                fwrite(name.data, 1, name.size, f);
                fprintf(f, "\",\"position\":{\"x\":%lld,\"y\":%lld},\"entity_number\":%lld", 5*x, 5*y, entity_count+1);
                fputs("}\n", f);
                ++entity_count;
            }
        }
    }
    
    fputs("]}}\n", f);
}

void construct_partition_make_blueprint(Construct_params* cpar, Sat_instance* inst, Sat_solution* ssol) {
    Sat_solution& sol = *ssol;

    FILE* f = fopen("blueprint.out", "w");
    fputs("{\"blueprint\":{\"item\": \"blueprint\",\"version\": 281479273316352,\"entities\": [\n", f);

    s64 w = 4;
    s64 h = (cpar->n_partition_max + 3) / 4;
    
    s64 entity_count = 0;
    for (s64 y = 0; y < cpar->partition_ny; ++y) {
        for (s64 x = 0; x < cpar->partition_nx; ++x) {
            Field fi {x, y};
            s64 ox = 0, oy = 0;
            
            for (s64 r = 0; r < cpar->rdb.recipe_count; ++r) {
                if (not sol[fi.rec(r)]) continue;

                s64 oox = 5*((w+1)*x + ox);
                s64 ooy = 5*((h+1)*y + oy);
                
                Recipe rec = cpar->rdb.get(r);
                Array_t<u8> name = construct_string_load(cpar, rec.name);
                Array_t<u8> entity = construct_string_load(cpar, rec.entity);
                if (entity_count) fputs(",", f);
                fputs("{\"name\":\"", f);
                fwrite(entity.data, 1, entity.size, f);
                fputs("\",\"recipe\":\"", f);
                fwrite(name.data, 1, name.size, f);
                fprintf(f, "\",\"position\":{\"x\":%lld,\"y\":%lld},\"entity_number\":%lld", oox, ooy, entity_count+1);
                fputs("}\n", f);
                ++entity_count;

                ++ox;
                if (ox == w) { ++oy; ox = 0; }
            }
        }
    }
    
    fputs("]}}\n", f);
}

struct Sparsestcut_opt_param {
    float fac_max = -1.f;
    float fac_left = -1.f;
    s64 count_left = -1;
};

void construct_sparsestcut_instance(Construct_params* cpar, Sat_instance* inst, Array_t<u64> mask_recipes, Sparsestcut_opt_param spar) {
    using namespace Sat;
    using namespace Construct;

    hashmap_set(&inst->params, param_nx, 2);
    hashmap_set(&inst->params, param_ny, 1);
    hashmap_set(&inst->params, param_n_items, cpar->n_items);
    hashmap_set(&inst->params, param_n_sparsecut_maxsize, cpar->n_sparsecut_maxsize);
    hashmap_set(&inst->params, param_n_sparsecut_groups, cpar->n_sparsecut_groups);
    hashmap_set(&inst->params, param_recipes, (s64)&cpar->rdb);
    
    sat_register_expand_func(inst, GROUP_CONSTRUCT, &construct_expand);
    sat_register_rewrite_func(inst, CONSTRAINT_CONSTRUCT, &construct_rewrite);
    for (u64 i: {VAR_CONSTRUCT, GROUP_CONSTRUCT, CONSTRAINT_CONSTRUCT}) {
        sat_register_explain_func(inst, i, &construct_explain);
    }

    
    auto* rdb = &cpar->rdb;
    s64 n_sparsecut_maxsize = cpar->n_sparsecut_maxsize;
    s64 n_sparsecut_groups = cpar->n_sparsecut_groups;
    Recipe rg = rdb->get(rdb->recipe_count);

    for (s64 x = 0; x < n_sparsecut_groups; ++x) {
        Field f {x, 0};

        for (s64 i = 0; i < cpar->n_items; ++i) {
            // If an item is output, there must be a recipe actually outputting it
            {s64 index = inst->rewrite_temp.size;
            for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
                if (not bitset_get(mask_recipes, r_it)) continue;
                
                if (bitset_get(rdb->get(r_it).mask_output, i)) {
                    array_push_back(&inst->rewrite_temp, f.rec(r_it));
                }
            }
            auto arr = array_subarray(inst->rewrite_temp, index);
            sat_addg(inst, implies, {f.out(i, Field_var::UNDIRECTED)}, arr);}
            
            // If we have a recipe, the inputs of the recipe must be inputs or outputs of the field
            for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
                if (not bitset_get(mask_recipes, r_it)) continue;
                
                if (bitset_get(rdb->get(r_it).mask_input, i)) {
                    sat_addg(inst, implies, {f.rec(r_it)}, {f.inp(i), f.out(i, Field_var::UNDIRECTED)});
                }
            }

            // Disable unused recipes
            for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
                if (not bitset_get(mask_recipes, r_it)) {
                    sat_add(inst, clause, {~f.rec(r_it)});
                }
            }

            // If the item is not an input of the global recipe, and it is an input of the field,
            // then it must be transported from the left
            if (not bitset_get(rg.mask_input, i)) {
                s64 index = inst->rewrite_temp.size;
                defer { inst->rewrite_temp.size = index; };
                for (s64 ax = 0; ax < x; ++ax) {
                    array_push_back(&inst->rewrite_temp, Field {ax, 0}.out(i, Field_var::RIGHT));
                }
                sat_addg(inst, implies, {f.inp(i)}, {array_subarray(inst->rewrite_temp, index)});
            }

            // If we are transporting the item, then we must output it
            sat_add(inst, implies, {f.out(i, Field_var::RIGHT), f.out(i, Field_var::UNDIRECTED)});
        }

        // If the field is production facility, then it must have at least one recipe
        sat_add(inst, implies, {f.prod, f.rec_all});
        // else it has no recipes at all
        sat_push_add_pop(inst, {~f.prod}, logical_and, {~f.rec_all});
    }
    

    for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
        if (not bitset_get(mask_recipes, r_it)) continue;
        
        // The recipe has to be on exactly one of the fields
        s64 index = inst->rewrite_temp.size;
        defer { inst->rewrite_temp.size = index; };
        for (s64 x = 0; x < n_sparsecut_groups; ++x) {
            Field f {x, 0};
            array_push_back(&inst->rewrite_temp, f.rec(r_it));
        }
        sat_add(inst, exactly_one, array_subarray(inst->rewrite_temp, index));
    }

    
    for (s64 i = 0; i < cpar->n_items; ++i) {
        if (not bitset_get(rg.mask_output, i)) continue;
        
        // Global items must be transported
        s64 index = inst->rewrite_temp.size;
        defer { inst->rewrite_temp.size = index; };
        for (s64 x = 0; x < n_sparsecut_groups; ++x) {
            Field f {x, 0};
            array_push_back(&inst->rewrite_temp, f.out(i, Field_var::RIGHT));
        }
        sat_add(inst, clause, array_subarray(inst->rewrite_temp, index));
    }
    

    // The leftmost field must be a production facility
    sat_add(inst, clause, {Field {0, 0}.prod});
    
    // At least one recipe
    for (s64 x = 1; x+1 < n_sparsecut_groups; ++x) {
        Field f {x, 0}, f2 {x+1, 0};
        sat_add(inst, implies, {~f.prod, ~f2.prod});
    }

    for (s64 x = 0; x+1 < n_sparsecut_groups; ++x) {
        float fac = x == 0 and spar.fac_left >= 0.f ? spar.fac_left : spar.fac_max;
        if (fac < 0.f) continue;
        fac += 0.001f;
        
        Field f {x, 0};
        
        // Sparsity
        s64 index = inst->rewrite_temp.size;
        for (s64 i = 0; i < cpar->n_items; ++i) {
            array_push_back(&inst->rewrite_temp, f.inp(i));
            array_push_back(&inst->rewrite_temp, f.out(i, Field_var::RIGHT));
        }
        auto arr_inp = array_subarray(inst->rewrite_temp, index);
        index = inst->rewrite_temp.size;
        for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
            if (not bitset_get(mask_recipes, r_it)) continue;
            array_push_back(&inst->rewrite_temp, f.rec(r_it));
        }
        auto arr_rec = array_subarray(inst->rewrite_temp, index);
        s64 max_rec = arr_rec.size >= n_sparsecut_maxsize ? n_sparsecut_maxsize : arr_rec.size;
        u64 group_rec = sat_temp_group_create(inst, max_rec);
        u64 group_inp = sat_temp_group_create(inst, (s64)(max_rec * fac));
            
        sat_add(inst, merge_multi, {sat_group(inst, arr_inp), group_inp});
        sat_add(inst, merge_multi, {sat_group(inst, arr_rec), group_rec});
            
        auto sort_inp = sat_expand(inst, group_inp, &inst->rewrite_temp);
        auto sort_rec = sat_expand(inst, group_rec, &inst->rewrite_temp);
        for (s64 i = 0; i < sort_rec.size; ++i) {
            sat_add(inst, implies, {~sort_rec[i], ~sat_vget(sort_inp, (s64)(fac*i))});
        }
            
        // Count
        if (x == 0 and spar.count_left > 0) {
            sat_add(inst, clause, {~sat_vget(sort_rec, spar.count_left)});
        }
    }
}

Sparsestcut_opt_param construct_sparsecut_value(Construct_params* cpar, Sat_instance* inst, Sat_solution* sol) {
    Sparsestcut_opt_param result;    
    s64 n_sparsecut_groups = cpar->n_sparsecut_groups;
    float max_value = -1.f;
    for (s64 x = 0; x+1 < n_sparsecut_groups; ++x) {
        Field f {x, 0};
        s64 n_inp = 0, n_rec = 0;
        for (s64 r_it = 0; r_it < cpar->rdb.recipe_count; ++r_it) {
            if (not (*sol)[f.rec(r_it)]) continue;
            ++n_rec;
        }
        for (s64 i = 0; i < cpar->n_items; ++i) {
            n_inp += sol->istrue(f.inp(i));
            n_inp += sol->istrue(f.out(i, Field_var::RIGHT));
        }

        float value = n_inp / (float)n_rec;
        if (max_value < value) max_value = value;
        
        if (x == 0) {
            result.fac_left = value;
            result.count_left = n_rec;
        }
    }

    result.fac_max = max_value;
    return result;
}

void construct_sparsecut_solution_print(Construct_params* cpar, Sat_instance* inst, Sat_solution* sol) {
    Field f {0, 0};

    s64 n_inp = 0, n_rec = 0;
    puts("Recipes:");
    for (s64 r_it = 0; r_it < cpar->rdb.recipe_count; ++r_it) {
        if (not (*sol)[f.rec(r_it)]) continue;
        Recipe rec = cpar->rdb.get(r_it);
        
        Array_t<u8> name = construct_string_load(cpar, rec.name);
        printf("    ");
        fwrite(name.data, 1, name.size, stdout);
        printf("\n");

        ++n_rec;
    }
    puts("Inputs:");
    for (s64 i = 0; i < cpar->n_items; ++i) {
        if (not sol->istrue(f.inp(i))) continue;

        Array_t<u8> name = construct_string_load(cpar, cpar->item_names[i]);
        printf("    ");
        fwrite(name.data, 1, name.size, stdout);
        printf("\n");

        ++n_inp;
    }
    puts("Outputs:");
    for (s64 i = 0; i < cpar->n_items; ++i) {
        if (not sol->istrue(f.out(i, Field_var::RIGHT))) continue;

        Array_t<u8> name = construct_string_load(cpar, cpar->item_names[i]);
        printf("    ");
        fwrite(name.data, 1, name.size, stdout);
        printf("\n");

        ++n_inp;
    }

    auto spar = construct_sparsecut_value(cpar, inst, sol);
    assert(n_rec == spar.count_left);
    
    puts("");
    printf("Inputs:  %lld / %lld\n", n_inp, cpar->n_items);
    printf("Recipes: %lld / %lld\n", n_rec, cpar->rdb.recipe_count);
    printf("Value:   %.2f (max: %.2f)\n", spar.fac_left, spar.fac_max);
}

void construct_sparsecut_minimal(Construct_params* cpar, Sat_instance* inst, Sat_solution* sol, Array_t<u64> mask_recipes) {
    sat_init(inst);
    sat_solution_init(sol);
    construct_sparsestcut_instance(cpar, inst, mask_recipes, {});
    assert(sat_solver_run(inst, "kissat"_arr, sol));

    float lower = 0.f;
    auto best_known = construct_sparsecut_value(cpar, inst, sol);
    printf("Bounds fac_max %.3f - %.3f, fac_left %.3f, count %lld\n", lower,
        best_known.fac_max, best_known.fac_left, best_known.count_left);

    bool is_sat = true;
    for (s64 it = 0; it < 7; ++it) {
        float mid = it == 0 ? 4.f : (lower + best_known.fac_max) / 2.f;
        sat_init(inst);
        sat_solution_init(sol);
        construct_sparsestcut_instance(cpar, inst, mask_recipes, Sparsestcut_opt_param {mid});
        is_sat = sat_solver_run(inst, "kissat"_arr, sol);
        if (is_sat) {
            best_known = construct_sparsecut_value(cpar, inst, sol);
            assert(best_known.fac_max <= mid + 0.001f);
        } else {
            lower = mid;
        }

        printf("Bounds fac_max %.3f - %.3f, fac_left %.3f, count %lld\n", lower,
            best_known.fac_max, best_known.fac_left, best_known.count_left);
    }

    puts("Binary search 1 finished. Starting binary search 2.");

    lower = 0.f;
    for (s64 it = 0; it < 7; ++it) {
        float mid = (lower + best_known.fac_left) / 2.f;
        sat_init(inst);
        sat_solution_init(sol);
        construct_sparsestcut_instance(cpar, inst, mask_recipes, {best_known.fac_max, mid});
        is_sat = sat_solver_run(inst, "kissat"_arr, sol);
        if (is_sat) {
            best_known = construct_sparsecut_value(cpar, inst, sol);
            assert(best_known.fac_left <= mid + 0.001f);
        } else {
            lower = mid;
        }

        printf("Bounds fac_max %.3f, fac_left %.3f - %.3f, count %lld\n", best_known.fac_max,
            lower, best_known.fac_left, best_known.count_left);
    }
    puts("Binary search 2 finished. Starting to optimise count.");

    
    while (best_known.count_left > 1) {
        sat_init(inst);
        sat_solution_init(sol);
        construct_sparsestcut_instance(cpar, inst, mask_recipes, {best_known.fac_max, best_known.fac_left, best_known.count_left-1});
        
        is_sat = sat_solver_run(inst, "kissat"_arr, sol);
        if (is_sat) {
            best_known = construct_sparsecut_value(cpar, inst, sol);
            printf("Bounds fac_max %.3f, fac_left %.3f, count %lld\n", best_known.fac_max, best_known.fac_left, best_known.count_left);
        } else {
            break;
        }
    } 
    printf("Final value fac_max %.3f, fac_left %.3f, count %lld\n", best_known.fac_max, best_known.fac_left, best_known.count_left);
    
    sat_init(inst);
    sat_solution_init(sol);
    construct_sparsestcut_instance(cpar, inst, mask_recipes, best_known);
    assert(sat_solver_run(inst, "kissat"_arr, sol));
}

void construct_sparsecut_partition(Construct_params* cpar, Sat_instance* inst, Sat_solution* sol) {
    s64 left = cpar->rdb.recipe_count;
    Array_t<u64> mask_recipes = array_create<u64>((left + 63) / 64);
    memset(mask_recipes.data, -1, mask_recipes.size * sizeof(mask_recipes.data[0]));

    Field f1 {0, 0};
    Recipe rg = cpar->rdb.get(cpar->rdb.recipe_count);

    FILE* f = fopen("partition.out", "w");
    
    while (left > 0) {
        construct_sparsecut_minimal(cpar, inst, sol, mask_recipes);
        construct_sparsecut_solution_print(cpar, inst, sol);

        for (s64 r_it = 0; r_it < cpar->rdb.recipe_count; ++r_it) {
            if (not (*sol)[f1.rec(r_it)]) continue;
            Recipe rec = cpar->rdb.get(r_it);
            
            Array_t<u8> name = construct_string_load(cpar, rec.name);
            fwrite(name.data, 1, name.size, f);
            fputc(' ', f);

            
            bitset_set(&mask_recipes, r_it, false);
            for (s64 i = 0; i < cpar->n_items; ++i) {
                if (bitset_get(rec.mask_output, i)) {
                    bitset_set(&rg.mask_input, i, true);
                    bitset_set(&rg.mask_output, i, false);
                }
            }
            --left;
        }
        
        fputc('\n', f);
    }
    
    fclose(f);
}

void bitset_or(Array_t<u64>* target, Array_t<u64> other) {
    assert(target->size == other.size);
    for (s64 i = 0; i < target->size; ++i) {
        (*target)[i] |= other[i];
    }
}

Recipe_db construct_recipedb_subset(Construct_params* cpar, Array_t<u64> mask_recipes, Array_dyn<s64>* out_items_map_back) {
    auto* rdb = &cpar->rdb;
    Array_t<u64> items_needed {(u64*)alloca(cpar->rdb.n_itemmask * sizeof(u64)), cpar->rdb.n_itemmask};
    array_memset(&items_needed);

    for (s64 i = 0; i < rdb->recipe_count; ++i) {
        if (not bitset_get(mask_recipes, i)) continue;
        Recipe r = rdb->get(i);
        bitset_or(&items_needed, r.mask_input);
        bitset_or(&items_needed, r.mask_output);
    }

    Array_dyn<s64> items_map_forth; // map old item index -> new item index
    defer { array_free(&items_map_forth); };

    s64 n_items_new = 0;
    for (s64 i = 0; i < cpar->n_items; ++i) {
        if (bitset_get(items_needed, i)) {
            array_push_back(&items_map_forth, n_items_new);
            if (out_items_map_back) {
                array_push_back(out_items_map_back, i);
            }
            ++n_items_new;
        } else {
            array_push_back(&items_map_forth, -1);
        }
    }

    Recipe_db rdb_new;
    rdb_new.n_itemmask = (n_items_new + 63) / 64;
    
    for (s64 i = 0; i < rdb->recipe_count; ++i) {
        if (not bitset_get(mask_recipes, i)) continue;
        Recipe r = rdb->get(i);
        
        array_push_back(&rdb_new.recs, {r.name, r.beacons, r.entity});
        array_append_zero(&rdb_new.data, 2*rdb_new.n_itemmask);
        ++rdb_new.recipe_count;

        Recipe r_new = rdb_new.get(rdb_new.recipe_count-1);
        for (s64 i = 0; i < cpar->n_items; ++i) {
            if (bitset_get(r.mask_input, i)) {
                bitset_set(&r_new.mask_input, items_map_forth[i], true);
            }
            if (bitset_get(r.mask_output, i)) {
                bitset_set(&r_new.mask_output, items_map_forth[i], true);
            }
        }
    }

    Recipe rg = rdb->get(rdb->recipe_count);
    array_push_back(&rdb_new.recs, {rg.name, rg.beacons, rg.entity});
    array_append_zero(&rdb_new.data, 2*rdb_new.n_itemmask);
    
    Recipe rg_new = rdb_new.get(rdb_new.recipe_count);

    for (s64 i = 0; i < rdb_new.recipe_count; ++i) {
        Recipe r_new = rdb_new.get(i);
        bitset_or(&rg_new.mask_input,  r_new.mask_input);
        bitset_or(&rg_new.mask_output, r_new.mask_output);
    }

    for (s64 i = 0; i < cpar->n_items; ++i) {
        s64 i_new = items_map_forth[i];
        if (i_new == -1) continue;

        bitset_set(&rg_new.mask_input, i_new, bitset_get(rg.mask_input, i) and bitset_get(rg_new.mask_input, i_new));

        bool is_used_by_old = false;
        for (s64 r = 0; r < rdb->recipe_count; ++r) {
            if (bitset_get(mask_recipes, r)) continue;
            is_used_by_old |= bitset_get(rdb->get(r).mask_input, i);
        }
        
        bitset_set(&rg_new.mask_output, i_new, (is_used_by_old or bitset_get(rg.mask_output, i)) and bitset_get(rg_new.mask_output, i_new));
    }

    rdb_new._n_items = n_items_new;
    return rdb_new;
}

s64 construct_recipe_lookup(Construct_params* cpar, Array_t<u8> name) {
    for (s64 i = 0; i < cpar->rdb.recipe_count; ++i) {
        Array_t<u8> i_name = construct_string_load(cpar, cpar->rdb.get(i).name);
        if (array_equal(name, i_name)) return i;
    }
    return -1;
}

void construct_partition_parse(Construct_params* cpar, Array_t<u8> fname, Array_dyn<s64>* out_partition_offsets, Array_dyn<s64>* out_partition_data) {
    assert(out_partition_offsets and out_partition_data);
    assert(out_partition_offsets->size == 0 and out_partition_data->size == 0);

    Array_t<u8> text = array_load_from_file(fname);
    defer { array_free(&text); };

    if (out_partition_offsets->size == 0) {
        array_push_back(out_partition_offsets, out_partition_data->size);
    }
    
    s64 last = 0;
    for (s64 i = 0; i <= text.size; ++i) {
        if (i == text.size or text[i] == ' ' or text[i] == '\n') {
            auto word = array_subarray(text, last, i);
            if (word.size > 0) {
                s64 word_r = construct_recipe_lookup(cpar, word);
                if (word_r == -1) {
                    fputs("Error: could not resolve recipe name '", stderr);
                    fwrite(word.data, 1, word.size, stderr);
                    fputs("'\n", stderr);
                    exit(6);
                }

                array_push_back(out_partition_data, word_r);
            }
            last = i+1;
        }

        if (i == text.size or text[i] == '\n') {
            if (out_partition_offsets->back() != out_partition_data->size) {
                array_push_back(out_partition_offsets, out_partition_data->size);
            }
        }
    }
}

void construct_recipedb_print(Construct_params* cpar, Recipe_db* rdb, Array_t<s64> item_map_back, FILE* f) {
    for (s64 rec_i = 0; rec_i <= rdb->recipe_count; ++rec_i) {
        auto rec = rdb->get(rec_i);
        Array_t<u8> name = construct_string_load(cpar, rec.name);
        
        fputs("recipe ", f);
        fwrite(name.data, 1, name.size, f);
        fputs(" \n", f);
        for (s64 i = 0; i < cpar->n_items; ++i) {
            if (not bitset_get(rec.mask_input, i)) continue;
            fputs("  in ", f);
            
            Array_t<u8> item = construct_string_load(cpar, cpar->item_names[item_map_back[i]]);
            fwrite(item.data, 1, item.size, f);
            fputc('\n', f);
        }
        for (s64 i = 0; i < cpar->n_items; ++i) {
            if (not bitset_get(rec.mask_output, i)) continue;
            fputs("  out ", f);
            
            Array_t<u8> item = construct_string_load(cpar, cpar->item_names[item_map_back[i]]);
            fwrite(item.data, 1, item.size, f);
            fputc('\n', f);
        }
    }
}

void construct_incremental_instance(Construct_params* cpar, Sat_instance* inst, Array_t<u64> mask_recipes, Array_dyn<s64>* out_items_map_back) {
    using namespace Sat;
    using namespace Construct;

    //@Leak
    Recipe_db rdb = construct_recipedb_subset(cpar, mask_recipes, out_items_map_back);
    construct_recipedb_print(cpar, &rdb, *out_items_map_back, stdout);
    
    hashmap_set(&inst->params, param_nx, cpar->nx);
    hashmap_set(&inst->params, param_ny, cpar->ny);
    hashmap_set(&inst->params, param_n_items, rdb._n_items);
    hashmap_set(&inst->params, param_recipes, (s64)&rdb);
        
    sat_register_expand_func(inst, GROUP_CONSTRUCT, &construct_expand);
    sat_register_rewrite_func(inst, CONSTRAINT_CONSTRUCT, &construct_rewrite);
    for (u64 i: {VAR_CONSTRUCT, GROUP_CONSTRUCT, CONSTRAINT_CONSTRUCT}) {
        sat_register_explain_func(inst, i, &construct_explain);
    }

    Recipe rg = rdb.get(rdb.recipe_count);

    for (s64 y = 0; y < cpar->ny; ++y) {
        for (s64 x = 0; x < cpar->nx; ++x) {
            sat_add(inst, field_incremental, {(u64)x, (u64)y});
        }
    }
    for (s64 y = -1; y <= cpar->ny; ++y) {
        for (s64 x = -1; x <= cpar->nx; ++x) {
            if ((x == -1 or x == cpar->nx) and (y == -1 or y == cpar->ny)) continue;
            sat_add(inst, field_incremental_border, {(u64)x, (u64)y});

            Field f {x, y};
            if (not f.inbounds(cpar->nx, cpar->ny)) {
                sat_add(inst, clause, {f.pborder});
            }
        }
    }

    sat_add(inst, field_global, {});

    for (s64 i = 0; i < rdb._n_items; ++i) {
        // If it is an output of the global recipe, some border field must take it as input
        if (not bitset_get(rg.mask_output, i)) continue;
        s64 index = inst->rewrite_temp.size;
        for (s64 y = -1; y <= cpar->ny; ++y) {
            for (s64 x = -1; x <= cpar->nx; ++x) {
                if ((x == -1 or x == cpar->nx) and (y == -1 or y == cpar->ny)) continue;
                Field f {x, y};
                array_push_back(&inst->rewrite_temp, f.inp_border(i));
            }
        }
        sat_add(inst, clause, array_subarray(inst->rewrite_temp, index));

       inst->rewrite_temp.size = index;
    }
    
}


void construct_incremental_solution_print(Construct_params* cpar, Sat_instance* inst, Sat_solution* sol, Array_t<s64> item_map_back = {}, Array_t<s64> recipe_map_back = {}) {
    s64 n_items = hashmap_get(&inst->params, Construct::param_n_items);
    s64 recipe_count = recipe_map_back.size == 0 ? cpar->rdb.recipe_count : recipe_map_back.size;
    
    auto count_inp = [cpar, sol, n_items](Field f) {
        s64 count = 0;
        for (s64 item = 0; item < n_items; ++item) count += sol->istrue(f.inp(item));
        return count;
    };
    auto count_out = [cpar, sol, n_items](Field f, u8 d) {
        s64 count = 0;
        for (s64 item = 0; item < n_items; ++item) count += sol->istrue(f.out(item, d));
        return count;
    };
    auto count_rec = [cpar, sol, n_items, recipe_count](Field f) {
        s64 count = 0;
        for (s64 i = 0; i < recipe_count; ++i) count += sol->istrue(f.rec(i));
        return count;
    };
    auto pad = [](s64 n) {
        for (s64 i = 0; i < n; ++i) fputc(' ', stdout);
    };
    auto _digit_i = [&item_map_back](s64 n) {
        _digit(item_map_back.size == 0 ? n : item_map_back[n]);
    };
    auto _digit_r = [&recipe_map_back](s64 n) {
        _digit(recipe_map_back.size == 0 ? n : recipe_map_back[n]);
    };

    s8 nx = (s8)hashmap_get(&inst->params, Construct::param_nx);
    s8 ny = (s8)hashmap_get(&inst->params, Construct::param_ny);
    s64 x0 = -1, x1 = ny;
    s64 y0 = -1, y1 = ny;
    
    s64 max_outputs = 0;
    for (s64 y = y1; y >= y0; --y) {
        for (s64 x = x0; x <= x1; ++x) {
            Field f {x, y};

            {s64 count = count_inp(f);
            if (max_outputs < count) max_outputs = count;}
            
            {s64 count = count_rec(f);
            if (max_outputs < count) max_outputs = count;}
            
            for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                s64 count = count_out(f, d);
                if (max_outputs < count) max_outputs = count;
            }

        }
    }

    s64 w = max_outputs + 2;
    
    fputs("Fields:\n", stdout);
    for (s64 y = y1; y >= y0; --y) {
        if (y < y1) {
            fputs("  ", stdout);
            for (s64 x = x0; x <= x1; ++x) {
                if (x != x0) fputs(u8"???", stdout);
                for (s64 i = 0; i < w; ++i) fputs(u8"???", stdout);
            }
            fputs("\n", stdout);
        }
        
        fputs("  ", stdout);
        for (s64 x = x0; x <= x1; ++x) {
            if (x != x0) fputs(u8"???", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::TOP);
            pad((w-count) / 2);
            for (s64 item = 0; item < n_items; ++item) {
                if (sol->istrue(f.out(item, Field_var::TOP))) _digit_i(item);
            }
            pad((w-count + 1) / 2);
        }
        fputs("\n", stdout);
        
        fputs("  ", stdout);
        for (s64 x = x0; x <= x1; ++x) {
            if (x != x0) fputs(u8"???", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::LEFT);
            fputs("< ", stdout);
            for (s64 item = 0; item < n_items; ++item) {
                if (sol->istrue(f.out(item, Field_var::LEFT))) _digit_i(item);
            }
            pad(w-count-2);
        }
        fputs("\n", stdout);
        
        fputs("  ", stdout);
        for (s64 x = x0; x <= x1; ++x) {
            if (x != x0) fputs(u8"???", stdout);
            Field f {x, y};
            s64 count = count_inp(f);
            pad((w-count)/2);
            for (s64 item = 0; item < n_items; ++item) {
                if (sol->istrue(f.inp(item))) _digit_i(item);
            }
            pad((w-count+1)/2);
        }
        fputs("\n", stdout);

        fputs("  ", stdout);
        for (s64 x = x0; x <= x1; ++x) {
            if (x != x0) fputs(u8"???", stdout);
            Field f {x, y};
            if (sol->istrue(f.belt)) {
                pad((w-1) / 2);
                fputs("#", stdout);
                pad((w-1+1) / 2);
            } else if (sol->istrue(f.prod)) {
                s64 count = 0;
                for (s64 r = 0; r < recipe_count; ++r) count += sol->istrue(f.rec(r));

                pad((w-count) / 2);
                for (s64 r = 0; r < recipe_count; ++r) {
                    if (sol->istrue(f.rec(r))) _digit_r(r);
                }
                pad((w-count+1) / 2);
            } else if (sol->istrue(f.beacon)) {
                pad((w-1) / 2);
                fputs("*", stdout);
                pad((w-1+1) / 2);
            } else if (sol->istrue(f.pborder)) {
                pad((w-1) / 2);
                fputs("+", stdout);
                pad((w-1+1) / 2);
            } else {
                pad((w-1) / 2);
                fputs("?", stdout);
                pad((w-1+1) / 2);
            }
        }
        fputs("\n", stdout);
        
        fputs("  ", stdout);
        for (s64 x = x0; x <= x1; ++x) {
            if (x != x0) fputs(u8"???", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::RIGHT);
            pad(w-count-2);
            for (s64 item = 0; item < n_items; ++item) {
                if (sol->istrue(f.out(item, Field_var::RIGHT))) _digit_i(item);
            }
            fputs(" >", stdout);
        }
        fputs("\n", stdout);

        fputs("  ", stdout);
        for (s64 x = x0; x <= x1; ++x) {
            if (x != x0) fputs(u8"???", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::BOTTOM);
            pad((w-count) / 2);
            for (s64 item = 0; item < n_items; ++item) {
                if (sol->istrue(f.out(item, Field_var::BOTTOM))) _digit_i(item);
            }
            pad((w-count + 1) / 2);
        }
        fputs("\n", stdout);        
    }

    fputs("Global:\n  ", stdout);
    {Field f = Field::global();
        for (s64 i = 0; i < n_items; ++i) {
            if (i) fputs(", ", stdout);
            _digit_i(i);
            fputc(' ', stdout);
            char const dn[] = "rtlb";
            for (u8 d = Field_var::BEG; d < Field_var::END; ++d) {
                fputc(sol->istrue(f.out(i, d)) ? dn[d-Field_var::BEG] : '-', stdout);
            }
        }}
    fputs("\n", stdout);
}


int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "Usage:\n  %s <mode> <file> [args...]\n", argv[0]);
        return 1;
    }

    auto inst_str = array_load_from_file(array_create_str(argv[2]));
    Construct_params params;
    construct_params_parse(&params, inst_str);

    Sat_instance inst;
    sat_init(&inst);

    if (strcmp(argv[1], "generate_instance") == 0) {
        if (argc != 4) {
            fprintf(stderr, "Usage:\n  %s %s <file> <instance>\n", argv[0], argv[1]);
            return 1;
        }
        
        construct_params_instance(&params, &inst);
        
        Sat_dimacs dimacs;
        sat_write_dimacs(&inst, &dimacs);
        FILE* f = fopen(argv[3], "w");
        fwrite(dimacs.text.data, 1, dimacs.text.size, f);
        fclose(f);
        return 0;
    } else if (strcmp(argv[1], "partition") == 0) {
        if (argc != 3) {
            fprintf(stderr, "Usage:\n  %s %s <file>\n", argv[0], argv[1]);
            return 1;
        }

        construct_partition_instance(&params, &inst);
        
        Sat_solution sol;
        bool is_sat = sat_solver_run(&inst, "kissat"_arr, &sol);

        if (is_sat) {
            construct_solution_print(&params, &inst, &sol, false);
            construct_partition_make_blueprint(&params, &inst, &sol);
        } else {
            fputs("\nInstance is not satisfiable. Sorry.\n", stdout);
        }
        
        return 0;
    } else if (strcmp(argv[1], "sparsestcut") == 0) {
        if (argc != 3) {
            fprintf(stderr, "Usage:\n  %s %s <file>\n", argv[0], argv[1]);
            return 1;
        }

        Sat_solution sol;
        construct_sparsecut_partition(&params, &inst, &sol);
        
        //Sat_solution sol;
        //bool is_sat = sat_solver_run(&inst, "kissat"_arr, &sol);
        //
        //if (is_sat) {
        //    construct_sparsecut_solution_print(&params, &inst, &sol);
        //} else {
        //    fputs("\nInstance is not satisfiable. Sorry.\n", stdout);
        //}
        
        return 0;
    } else if (strcmp(argv[1], "partition_and_refine") == 0) {
        if (argc != 3) {
            fprintf(stderr, "Usage:\n  %s %s <file>\n", argv[0], argv[1]);
            return 1;
        }

        construct_partition_instance(&params, &inst);


        while (true) {
            Sat_solution sol;
            bool is_sat = sat_solver_run(&inst, "kissat"_arr, &sol);
            
            if (is_sat) {
                construct_solution_print(&params, &inst, &sol, false);
                
                //construct_solution_partition_print(&params, &inst, &sol);
                //return 0;

                Sat_instance inst2;
                sat_init(&inst2);
                //construct_do_stupid_refinement(&params, &inst2, &sol);
                construct_refine_instance(&params, &inst2, &sol);
            
                Sat_solution sol2;
                //Sat_proof proof;
                bool is_sat = sat_solver_run(&inst2, "kissat"_arr, &sol2 /*,&proof*/);

                if (is_sat) {
                    construct_solution_print(&params, &inst2, &sol2);
                    break;
                } else {
                    //Array_dyn<u8> temp_u8;
                    //
                    //temp_u8.size = 0;
                    //construct_print_stacktrace(&inst2, &temp_u8);
                    //{FILE* f = fopen("stacktrace.out", "w");
                    //fwrite(temp_u8.data, 1, temp_u8.size, f);
                    //fclose(f);}
                    //
                    //temp_u8.size = 0;
                    //sat_proof_print(&inst2, &proof, &temp_u8);
                    //{FILE* f = fopen("proof.out", "w");
                    //fwrite(temp_u8.data, 1, temp_u8.size, f);
                    //fclose(f);}

                    fputs("\nInstance (refined) is not satisfiable. Trying a different partition....\n", stdout);
                    construct_partition_different(&params, &inst, &sol);
                }
            } else {
                fputs("\nInstance (partition) is not satisfiable. Sorry.\n", stdout);
                break;
            }

            sat_solution_free(&sol);
        }
        
        return 0;
    } else if (strcmp(argv[1], "parse_solution") == 0) {
        if (argc != 4) {
            fprintf(stderr, "Usage:\n  %s %s <file> <output>\n", argv[0], argv[1]);
            return 1;
        }
        
        construct_params_instance(&params, &inst);
        
        Sat_dimacs dimacs;
        sat_write_dimacs(&inst, &dimacs);
        auto output = array_load_from_file("out"_arr);
        
        Sat_solution sol;
        s8 status;
        sat_solver_parse(&dimacs, output, &sol, &status);
        assert(status != -1);

        if (status == 1) {
            construct_solution_print(&params, &inst, &sol);
        } else {
            fputs("\nInstance is not satisfiable. Sorry.\n", stdout);
        }
        
        
        return 1;
    } else if (strcmp(argv[1], "incremental") == 0) {
        if (argc != 4) {
            fprintf(stderr, "Usage:\n  %s %s <file> <partition>\n", argv[0], argv[1]);
            return 1;
        }

        Array_dyn<s64> partition_offsets;
        Array_dyn<s64> partition_data;
        construct_partition_parse(&params, array_create_str(argv[3]), &partition_offsets, &partition_data);

        Array_t<u64> mask_recipes = array_create<u64>((params.rdb.recipe_count + 63) / 64);
        for (s64 i: array_subindex(partition_offsets, partition_data, 0)) {
            bitset_set(&mask_recipes, i, true);
        }

        Array_dyn<s64> item_map_back;
        construct_incremental_instance(&params, &inst, mask_recipes, &item_map_back);

        Array_dyn<s64> recipe_map_back;
        for (s64 i = 0; i < params.rdb.recipe_count; ++i) {
            if (bitset_get(mask_recipes, i)) {
                array_push_back(&recipe_map_back, i);
            }
        }
        
        Sat_solution sol;
        bool is_sat = sat_solver_run(&inst, "kissat"_arr, &sol);

        if (is_sat) {
            construct_incremental_solution_print(&params, &inst, &sol, item_map_back, recipe_map_back);
        } else {
            fputs("\nInstance is not satisfiable. Sorry.\n", stdout);
        }
        
        return 0;
    }
    

    //auto output = array_load_from_file("out"_arr);
    //construct_make_blueprint(&params, &inst, output);
    return 1;
    
    
    Array_dyn<u8> temp_u8;
    //construct_do_stupid_solution(&params, &inst);
    
    
    //Array_dyn<u8> human;
    //sat_write_human(&inst, &human, true);
    //{FILE* f = fopen("human.out", "w");
    //fwrite(human.data, 1, human.size, f);
    //fclose(f);}

    Sat_solution sol;
    Sat_proof proof;
    bool is_sat = sat_solver_run(&inst, "kissat"_arr, &sol, nullptr, false);
    if (is_sat) {
        construct_solution_print(&params, &inst, &sol);

        Array_dyn<u64> sol_arr;
        sat_solution_write(&sol, &sol_arr);
        {FILE* f = fopen("solution.out", "w");
        fwrite(sol_arr.data, 8, sol_arr.size, f);
        fclose(f);}
    } else {
        //temp_u8.size = 0;
        //sat_proof_print(&inst, &proof, &temp_u8);
        //{FILE* f = fopen("proof.out", "w");
        //fwrite(temp_u8.data, 1, temp_u8.size, f);
        //fclose(f);}

        //temp_u8.size = 0;
        //construct_print_stacktrace(&inst, &temp_u8);
        //{FILE* f = fopen("stacktrace.out", "w");
        //fwrite(temp_u8.data, 1, temp_u8.size, f);
        //fclose(f);}
    }
    
    //human.size = 0;
    //sat_solution_write_human(&inst, &sol, &human);
    //{FILE* f = fopen("shuman.out", "w");
    //fwrite(human.data, 1, human.size, f);
    //fclose(f);}
    
    return 0;
}

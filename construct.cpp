
#include <unistd.h>
#include <poll.h>
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
    bipartition,
};

enum Params_construct: u64 {
    PARAM_CONSTRUCT_BEGIN = PARAM_CONSTRUCT,
    param_recipes, param_nx, param_ny, param_n_items,
    PARAM_CONSTRUCT_END
};

}

struct Field_var {
    enum Masks: u64 {
        MASK_COORD = 0xffff00000000ull << 8,
        MASK_X     = 0xff0000000000ull << 8,
        MASK_Y     =   0xff00000000ull << 8,
        MASK_TYPE  =            0xff << 8,
        MASK_DIR   =     0xff000000ull << 8,
        MASK_KIND  =       0xff0000ull << 8,
        MASK_EL    =         0xff00ull << 8,
    };
    enum Type: u8 {
        VAR, GROUP_DIR, GROUP_REC, GROUP_ADJ
    };
    enum Kind: u8 {
        PROD, BELT, BEACON, REC, INP, OUT, KIND_SIZE
    };
    static constexpr char const* kind_names[KIND_SIZE] = {
        "prod", "belt", "beacon", "rec", "inp", "out"
    };
    enum Dirs: u8 {
        UNDIRECTED, RIGHT, TOP, LEFT, BOTTOM, DIR_ALL, DIR_SIZE,
        BEG = RIGHT, END = BOTTOM+1
    };
    static constexpr char const* dir_names[DIR_SIZE] = {
        "none", "rig", "top", "lef", "bot", "all"
    };

    static constexpr u64 OFFSET = 1;
    
    s8 x, y;
    u8 type, dir, kind;
    u8 el = 0;

    Field_var move(u8 dir) {
        Field_var r = *this;
        if      (dir == RIGHT ) ++r.x;
        else if (dir == TOP   ) ++r.y;
        else if (dir == LEFT  ) --r.x;
        else if (dir == BOTTOM) --r.y;
        else assert(false);
        return r;
    }
    bool inbounds(s64 nx, s64 ny) {
        return 0 <= x and x < nx and 0 <= y and y < ny;
    }

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

    u64 prod, belt, beacon;
    u64 rec_all;
    
    u64 inp(s64 dim) {
        u8 el = dim; assert(el == dim);
        return construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::INP, el});
    }
    u64 adjout(s64 dim) {
        u8 el = dim; assert(el == dim);
        return construct_compose({x, y, Field_var::GROUP_ADJ, Field_var::UNDIRECTED, Field_var::OUT, el});
    }
    u64 out(s64 dim, u8 d) {
        u8 el = dim; assert(el == dim);
        assert((Field_var::BEG <= d and d < Field_var::END) or d == Field_var::DIR_ALL or d == Field_var::UNDIRECTED);
        u8 type = d == Field_var::DIR_ALL ? Field_var::GROUP_DIR : Field_var::VAR;
        return construct_compose({x, y, type, d, Field_var::OUT, el});
    }
    u64 rec(s64 dim) {
        u8 el = dim; assert(el == dim);
        return construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::REC, el});
    }        

    Field(s64 x_, s64 y_): x{(s8)x_}, y{(s8)y_} {
        assert(x == x_ and y == y_);
        
        prod    = construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::PROD});
        belt    = construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::BELT});
        beacon  = construct_compose({x, y, Field_var::VAR, Field_var::UNDIRECTED, Field_var::BEACON});
        rec_all = construct_compose({x, y, Field_var::GROUP_REC, Field_var::UNDIRECTED, Field_var::REC});
    }

    static Field global() { return {-1,  0}; }
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
    s64 recipe_count; // This excludes the artificial recipe describing the whole production
    s64 n_itemmask;
    
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

    if (v.type == Field_var::GROUP_DIR) {
        if (v.dir == Field_var::DIR_ALL) {
            for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                Field_var r = v;
                r.type = Field_var::VAR;
                r.dir = d;
                array_push_back(out_lits, construct_compose(r));
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
        s8 nx = (s8)hashmap_get(&inst->params, Construct::param_nx);
        s8 ny = (s8)hashmap_get(&inst->params, Construct::param_ny);
        
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
            u8 dirs[4] = {Field_var::RIGHT, Field_var::TOP, Field_var::LEFT, Field_var::BOTTOM};
            u8 back[4] = {Field_var::LEFT, Field_var::BOTTOM, Field_var::RIGHT, Field_var::TOP};
            for (s64 d_i = 0; d_i < sizeof(dirs) / sizeof(dirs[0]); ++d_i) {
                Field_var v1 = v.move(dirs[d_i]);
                Field_var v2 = v1.inbounds(nx, ny)
                    ? Field_var {v1.x, v1.y, Field_var::VAR, back[d_i],             v.kind, v.el}
                    : Field_var {-1,   -1,   Field_var::VAR, Field_var::UNDIRECTED, v.kind, v.el};
                array_push_back(out_lits, construct_compose(v2));
            }
        } else {
            assert(false);
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

            for (s64 i = 0; i < n_items; ++i) {
                // If it is an input of the recipe, we need to have it
                if (bitset_get(r.mask_input,  i)) {
                    sat_add(inst, clause, {f.inp(i)});
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

        // There can be at most 6 items on a belt, unless the belt is a border belt
        if (not (f.x == 0 or f.y == 0 or f.x == nx-1 or f.y == ny-1)) {
            s64 index = inst->rewrite_temp.size;
            for (s64 i = 0; i < n_items; ++i) {
                array_push_back(&inst->rewrite_temp, f.inp(i));
            }
            sat_add(inst, at_most_k, {sat_group(inst, array_subarray(inst->rewrite_temp, index)), 6});
        }
        
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
                    sat_add(inst, implies, {f.out(i, d), ~f2.out(i, Field_var::dir_back(d))});
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

            // Output an item iff it is an input of the global recipe, output it
            if (bitset_get(rg.mask_input, i)) {
                sat_add(inst, clause, {f.out(i, Field_var::UNDIRECTED)});
            } else {
                sat_add(inst, clause, {~f.out(i, Field_var::UNDIRECTED)});
            }

            // If it is an output of the global recipe, we must have it
            if (bitset_get(rg.mask_output, i)) {
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

    case bipartition: {
        assert(args.size == 1);
        Field f1 {1, 0}, f2 {2, 0};
        s64 n_transport_max = (s64)args[0];

        for (s64 i = 0; i < n_items; ++i) {
            // If an item is output, there must be a recipe actualy outputting it (field 1)
            {s64 index = inst->rewrite_temp.size;
            for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
                if (bitset_get(rdb->get(r_it).mask_output, i)) {
                    array_push_back(&inst->rewrite_temp, f1.rec(r_it));
                }
            }
            auto arr = array_subarray(inst->rewrite_temp, index);
            sat_addg(inst, implies, {f1.out(i, Field_var::UNDIRECTED)}, arr);}
            
            // Same for field 2
            {s64 index = inst->rewrite_temp.size;
            for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
                if (bitset_get(rdb->get(r_it).mask_output, i)) {
                    array_push_back(&inst->rewrite_temp, ~f1.rec(r_it));
                }
            }
            auto arr = array_subarray(inst->rewrite_temp, index);
            sat_addg(inst, implies, {f2.out(i, Field_var::UNDIRECTED)}, arr);}

            // If we have a recipe on field 1 or field 2, the inputs of the recipe must be inputs of the field
            for (s64 r_it = 0; r_it < rdb->recipe_count; ++r_it) {
                if (bitset_get(rdb->get(r_it).mask_input, i)) {
                    sat_add(inst, implies, { f1.rec(r_it), f1.inp(i)});
                    sat_add(inst, implies, {~f1.rec(r_it), f2.inp(i)});
                }
            }

            Recipe rg = rdb->get(rdb->recipe_count);
            if (not bitset_get(rg.mask_input, i)) {
                // If it is not an input of the total recipe, not an output of field 1, and needed by field 1, transport it
                sat_addg(inst, implies, {f1.inp(i), ~f1.out(i, Field_var::UNDIRECTED)}, {f2.out(i, Field_var::LEFT)});
                // Same for field 2
                sat_addg(inst, implies, {f2.inp(i), ~f2.out(i, Field_var::UNDIRECTED)}, {f1.out(i, Field_var::RIGHT)});
            }

            // If we transport an item, we must output it
            sat_add(inst, implies, {f1.out(i, Field_var::RIGHT), f1.out(i, Field_var::UNDIRECTED)});
            sat_add(inst, implies, {f2.out(i, Field_var::LEFT),  f2.out(i, Field_var::UNDIRECTED)});
        }

        sat_add(inst, exactly_k, {f1.rec_all, (u64)(rdb->recipe_count / 2)});

        s64 index = inst->rewrite_temp.size;
        for (s64 i = 0; i < n_items; ++i) {
            array_push_back(&inst->rewrite_temp, f1.out(i, Field_var::RIGHT));
            array_push_back(&inst->rewrite_temp, f2.out(i, Field_var::LEFT));
        }
        u64 group_transport = sat_group(inst, array_subarray(inst->rewrite_temp, index));
        sat_add(inst, at_most_k, {group_transport, (u64)n_transport_max});
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
        default: assert(false); break;
        }
        return false;
    } else {
        assert(false);
    }
    return false;
}

struct Construct_params {
    s64 nx, ny, n_items, n_transport_max;
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
        } else if (array_equal_str(word, "size")) {
            s64 j = 0;
            while (j < rest.size and rest[j] != ' ') ++j;
            assert(j < rest.size);

            Array_t<u8> num1 = array_subarray(rest, 0, j);
            while (j < rest.size and rest[j] == ' ') ++j;
            Array_t<u8> num2 = array_subarray(rest, j);

            cpar->nx = _parse_number(num1);
            cpar->ny = _parse_number(num2);
            assert(0 <= cpar->nx and cpar->nx <= 0xff);
            assert(0 <= cpar->ny and cpar->ny <= 0xff);
        } else if (array_equal_str(word, "items")) {
            assert(not n_items_should_not_change);
            cpar->n_items = _parse_number(rest);
            cpar->rdb.n_itemmask = (cpar->n_items + 63) / 64;
            
        } else if (array_equal_str(word, "transport_max")) {
            cpar->n_transport_max = _parse_number(rest);
            
        } else if (array_equal_str(word, "recipe")) {
            assert(not has_total_recipe);
            n_items_should_not_change = true;
            
            auto* rdb = &cpar->rdb;
            
            array_push_back(&rdb->recs, {});
            array_append_zero(&rdb->data, 2*rdb->n_itemmask);

            recipe_cur = rdb->get(rdb->recipe_count);
            recipe_cur_rec = &rdb->recs.back();
            recipe_cur_rec->name = construct_string_store(cpar, rest);
            recipe_cur_set = true;

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
        fprintf(f, "  %3lld: ", i);
        auto name = construct_string_load(cpar, cpar->item_names[i]);
        fwrite(name.data, 1, name.size, f);
        fputc('\n', f);
    }
    fputs("Recipes:\n", f);
    for (s64 i = 0; i < cpar->rdb.recipe_count; ++i) {
        fprintf(f, "  %3lld: ", i);
        auto name = construct_string_load(cpar, cpar->rdb.get(i).name);
        fwrite(name.data, 1, name.size, f);
        fputc('\n', f);
    }
    fclose(f);
    
}

void construct_params_instance(Construct_params* params, Sat_instance* inst) {
    using namespace Construct;

    hashmap_set(&inst->params, param_nx, params->nx);
    hashmap_set(&inst->params, param_ny, params->ny);
    hashmap_set(&inst->params, param_n_items, params->n_items);
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
}

void construct_bipartition_instance(Construct_params* params, Sat_instance* inst) {
    using namespace Construct;

    hashmap_set(&inst->params, param_nx, 2);
    hashmap_set(&inst->params, param_ny, 1);
    hashmap_set(&inst->params, param_n_items, params->n_items);
    hashmap_set(&inst->params, param_recipes, (s64)&params->rdb);
    
    sat_register_expand_func(inst, GROUP_CONSTRUCT, &construct_expand);
    sat_register_rewrite_func(inst, CONSTRAINT_CONSTRUCT, &construct_rewrite);
    for (u64 i: {VAR_CONSTRUCT, GROUP_CONSTRUCT, CONSTRAINT_CONSTRUCT}) {
        sat_register_explain_func(inst, i, &construct_explain);
    }
    
    sat_add(inst, bipartition, {(u64)params->n_transport_max});
}

void construct_bipartition_solution_print(Construct_params* cpar, Sat_solution* sol) {
    Field f1 {1, 0}, f2 {2, 0};
    
    fprintf(stdout, "Left:\n");
    for (s64 r_it = 0; r_it < cpar->rdb.recipe_count; ++r_it) {
        Recipe r = cpar->rdb.get(r_it);
        
        if ((*sol)[f1.rec(r_it)]) {
            auto name = construct_string_load(cpar, r.name);
            fputs("  ", stdout);
            fwrite(name.data, 1, name.size, stdout);
            fputs("\n", stdout);
        }
    }
    fprintf(stdout, "Right:\n");
    for (s64 r_it = 0; r_it < cpar->rdb.recipe_count; ++r_it) {
        Recipe r = cpar->rdb.get(r_it);
        
        if (not (*sol)[f1.rec(r_it)]) {
            auto name = construct_string_load(cpar, r.name);
            fputs("  ", stdout);
            fwrite(name.data, 1, name.size, stdout);
            fputs("\n", stdout);
        }
    }
    fprintf(stdout, "Transport:\n");
    for (s64 i = 0; i < cpar->n_items; ++i) {
        if ((*sol)[f1.out(i, Field_var::RIGHT)]) {
            auto name = construct_string_load(cpar, cpar->item_names[i]);
            fputs("  -> ", stdout);
            fwrite(name.data, 1, name.size, stdout);
            fputs("\n", stdout);
        }
        if ((*sol)[f2.out(i, Field_var::LEFT)]) {
            auto name = construct_string_load(cpar, cpar->item_names[i]);
            fputs("  <- ", stdout);
            fwrite(name.data, 1, name.size, stdout);
            fputs("\n", stdout);
        }
    }
}

void construct_bipartition_solution_write(Construct_params* cpar, Sat_solution* sol, Array_t<u8> fname, bool is_left) {
    Array_t<u64> items_used = array_create<u64>(cpar->rdb.n_itemmask);
    defer { array_free(&items_used); };
    Field f1 {1, 0}, ff {2 - is_left, 0};
     
    for (s64 r_it = 0; r_it < cpar->rdb.recipe_count; ++r_it) {
        if ((*sol)[f1.rec(r_it)] != is_left) continue;
        
        Recipe r = cpar->rdb.get(r_it);
        for (s64 i = 0; i < items_used.size; ++i) {
            items_used[i] |= r.mask_input[i];
            items_used[i] |= r.mask_output[i];
        }
    }

    s64 item_count = 0;
    for (u64 i: items_used) item_count += __builtin_popcountll(i);
    

    assert(*fname.end() == 0);
    FILE* f = fopen((char*)fname.data, "w");
    
    fprintf(f, "size %lld %lld\n", cpar->nx / 2, cpar->ny);
    fprintf(f, "items %lld\n", item_count);
    fprintf(f, "transport_max %lld\n", cpar->n_transport_max);

    for (s64 r_it = 0; r_it < cpar->rdb.recipe_count; ++r_it) {
        if ((*sol)[f1.rec(r_it)] != is_left) continue;
        
        Recipe r = cpar->rdb.get(r_it);
        auto name =   construct_string_load(cpar, r.name);
        auto entity = construct_string_load(cpar, r.entity);

        fputs("recipe ", f);
        fwrite(name.data, 1, name.size, f);
        fputs("\n  entity ", f);
        fwrite(entity.data, 1, entity.size, f);
        fputs("\n", f);

        for (s64 i = 0; i < cpar->n_items; ++i) {
            auto item = construct_string_load(cpar, cpar->item_names[i]);
            if (bitset_get(r.mask_input, i)) {
                fputs("  in ", f);
                fwrite(item.data, 1, item.size, f);
                fputs("\n", f);
            }
        }
        for (s64 i = 0; i < cpar->n_items; ++i) {
            auto item = construct_string_load(cpar, cpar->item_names[i]);
            if (bitset_get(r.mask_output, i)) {
                fputs("  out ", f);
                fwrite(item.data, 1, item.size, f);
                fputs("\n", f);
            }
        }
    }

    Recipe rg = cpar->rdb.get(cpar->rdb.recipe_count);
    
    fputs("recipe total\n", f);
    for (s64 i = 0; i < cpar->n_items; ++i) {
        auto item = construct_string_load(cpar, cpar->item_names[i]);
        if ((*sol)[ff.inp(i)] and not (*sol)[ff.out(i, Field_var::UNDIRECTED)] and bitset_get(items_used, i)) {
            fputs("  in ", f);
            fwrite(item.data, 1, item.size, f);
            fputs("\n", f);
        }
    }
    for (s64 i = 0; i < cpar->n_items; ++i) {
        auto item = construct_string_load(cpar, cpar->item_names[i]);
        // We write all outputs of the global recipe which are used, but they might only be used as
        // an input. This might become a problem later.
        if ((*sol)[ff.out(i, is_left ? Field_var::RIGHT : Field_var::LEFT)] or
                (bitset_get(rg.mask_output, i) and bitset_get(items_used, i)))
        {
            fputs("  out ", f);
            fwrite(item.data, 1, item.size, f);
            fputs("\n", f);
        }
    }

    fclose(f);
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

bool sat_solver_run(Sat_instance* inst, Array_t<u8> solver, Sat_solution* out_sol, Sat_proof* out_proof=nullptr, bool use_temp_file=false) {
    assert(solver.data[solver.size] == 0);
    assert(out_sol);
    
    Sat_dimacs dimacs;
    sat_write_dimacs(inst, &dimacs);
    defer { sat_dimacs_free(&dimacs); };

    if (out_proof and out_proof->offsets.size == 0) {
        array_push_back(&out_proof->offsets, out_proof->data.size);
    }
    
    int pipe_solver_to  [2] = {-1, -1};
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
        if (pipe_proof[0] != -1) {
            if (close(pipe_proof[0])) goto error;
            
            char buf[32] = {};
            assert(snprintf(buf, sizeof(buf), "/dev/fd/%d", pipe_proof[1]) <= sizeof(buf));

            argv[2] = buf;
            argv[3] = nullptr;
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
            ssize_t n = write(pfds[1].fd, to_write.data, to_write.size);
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
    return status;}}
    
  error:
    perror("Error");
  error2:
    exit(3);
}


void construct_solution_print(Construct_params* cpar, Sat_solution* sol) {
    auto count_inp = [cpar, sol](Field f) {
        s64 count = 0;
        for (s64 item = 0; item < cpar->n_items; ++item) count += (*sol)[f.inp(item)];
        return count;
    };
    auto count_out = [cpar, sol](Field f, u8 d) {
        s64 count = 0;
        for (s64 item = 0; item < cpar->n_items; ++item) count += (*sol)[f.out(item, d)];
        return count;
    };
    auto pad = [](s64 n) {
        for (s64 i = 0; i < n; ++i) fputc(' ', stdout);
    };
    auto digit = [](s64 item) {
        if (item < 10) {
            fputc('0' + item, stdout);
        } else if (item < 36) {
            fputc('a' + (item - 10), stdout);
        } else {
            fputc('A' + (item - 36), stdout);
        }
    };
    
    s64 max_outputs = 0;
    for (s64 y = cpar->nx-1; y >= 0; --y) {
        for (s64 x = 0; x < cpar->nx; ++x) {
            Field f {x, y};

            {s64 count = count_inp(f);
            if (max_outputs < count) max_outputs = count;}

            for (s64 d = Field_var::BEG; d < Field_var::END; ++d) {
                s64 count = count_out(f, d);
                if (max_outputs < count) max_outputs = count;
            }
        }
    }

    s64 w = max_outputs + 2;

    fputs("Fields:\n", stdout);
    for (s64 y = cpar->ny-1; y >= 0; --y) {
        if (y < cpar->ny-1) {
            fputs("  ", stdout);
            for (s64 x = 0; x < cpar->nx; ++x) {
                if (x) fputs(u8"┼", stdout);
                for (s64 i = 0; i < w; ++i) fputs(u8"─", stdout);
            }
            fputs("\n", stdout);
        }
        
        fputs("  ", stdout);
        for (s64 x = 0; x < cpar->nx; ++x) {
            if (x) fputs(u8"│", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::TOP);
            pad((w-count) / 2);
            for (s64 item = 0; item < cpar->n_items; ++item) {
                if ((*sol)[f.out(item, Field_var::TOP)]) digit(item);
            }
            pad((w-count + 1) / 2);
        }
        fputs("\n", stdout);
        
        fputs("  ", stdout);
        for (s64 x = 0; x < cpar->nx; ++x) {
            if (x) fputs(u8"│", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::LEFT);
            fputs("< ", stdout);
            for (s64 item = 0; item < cpar->n_items; ++item) {
                if ((*sol)[f.out(item, Field_var::LEFT)]) digit(item);
            }
            pad(w-count-2);
        }
        fputs("\n", stdout);

        fputs("  ", stdout);
        for (s64 x = 0; x < cpar->nx; ++x) {
            if (x) fputs(u8"│", stdout);
            Field f {x, y};
            pad((w-2) / 2);
            if ((*sol)[f.belt]) {
                fputs("b ", stdout);
            }
            if ((*sol)[f.prod]) {
                fputc('p', stdout);
                for (s64 r = 0; r < cpar->rdb.recipe_count; ++r) {
                    if ((*sol)[f.rec(r)]) digit(r);
                }
            }
            if ((*sol)[f.beacon]) {
                fputs("**", stdout);
            } 
            pad((w-2+1) / 2);
        }
        fputs("\n", stdout);
        
        fputs("  ", stdout);
        for (s64 x = 0; x < cpar->nx; ++x) {
            if (x) fputs(u8"│", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::RIGHT);
            pad(w-count-2);
            for (s64 item = 0; item < cpar->n_items; ++item) {
                if ((*sol)[f.out(item, Field_var::RIGHT)]) digit(item);
            }
            fputs(" >", stdout);
        }
        fputs("\n", stdout);

        fputs("  ", stdout);
        for (s64 x = 0; x < cpar->nx; ++x) {
            if (x) fputs(u8"│", stdout);
            Field f {x, y};
            s64 count = count_out(f, Field_var::BOTTOM);
            pad((w-count) / 2);
            for (s64 item = 0; item < cpar->n_items; ++item) {
                if ((*sol)[f.out(item, Field_var::BOTTOM)]) digit(item);
            }
            pad((w-count + 1) / 2);
        }
        fputs("\n", stdout);        
    }
    
    fputs("Global:\n  ", stdout);
    {Field f = Field::global();
    for (s64 i = 0; i < cpar->n_items; ++i) {
        if (i) fputs(", ", stdout);
        digit(i);
        fputc(' ', stdout);
        char const dn[] = "rtlb";
        for (u8 d = Field_var::BEG; d < Field_var::END; ++d) {
            fputc((*sol)[f.out(i, d)] ? dn[d-Field_var::BEG] : '-', stdout);
        }
    }}
    fputs("\n", stdout);
    
    fputs("Border:", stdout);
    {Field f = Field::border();
    fputs("\n  inputs:  ", stdout);
    for (s64 i = 0; i < cpar->n_items; ++i) {
        if ((*sol)[f.inp(i)]) digit(i);
    }
    fputs("\n  outputs: ", stdout);
    for (s64 i = 0; i < cpar->n_items; ++i) {
        if ((*sol)[f.out(i, Field_var::UNDIRECTED)]) digit(i);
    }}
    fputs("\n", stdout);
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

void construct_make_blueprint(Construct_params* cpar, Sat_instance* inst, Array_t<u8> data) {
    Sat_dimacs dimacs;
    sat_write_dimacs(inst, &dimacs);
    defer { sat_dimacs_free(&dimacs); };

    Sat_solution sol;
    defer { sat_solution_free(&sol); };

    s8 status;
    sat_solver_parse(&dimacs, data, &sol, &status, nullptr);
    assert(status == 1);

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

int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "Usage:\n  %s <mode> <file> [args...]\n", argv[0]);
        return 1;
    }

    auto inst_str = array_load_from_file(array_create_str(argv[2]));
    Construct_params params;
    memset(&params, 0, sizeof(params));
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
    } else if (strcmp(argv[1], "split") == 0) {
        if (argc != 5) {
            fprintf(stderr, "Usage:\n  %s %s <file> <instance1> <instance2>\n", argv[0], argv[1]);
            return 1;
        }

        construct_bipartition_instance(&params, &inst);
        
        Sat_solution sol;
        bool is_sat = sat_solver_run(&inst, "kissat"_arr, &sol);

        if (is_sat) {
            construct_bipartition_solution_print(&params, &sol);
            
            construct_bipartition_solution_write(&params, &sol, array_create_str(argv[3]), true);
            construct_bipartition_solution_write(&params, &sol, array_create_str(argv[4]), false);
            
        } else {
            fputs("\nInstance is not satifiable. Sorry.\n", stdout);
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
            construct_solution_print(&params, &sol);
        } else {
            fputs("\nInstance is not satifiable. Sorry.\n", stdout);
        }
        
        
        return 1;
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
        construct_solution_print(&params, &sol);

        Array_dyn<u64> sol_arr;
        sat_solution_write(&inst, &sol, &sol_arr);
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

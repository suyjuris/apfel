
struct Sat_instance {
    // !!! New members must be re-initialisable in sat_init !!!
    
    Array_dyn<s64> clause_offsets;
    Array_dyn<u64> clause_literals;
    Array_dyn<u64> clause_constraint;

    Array_dyn<s64> context_offsets;
    Array_dyn<u64> context_data;
    Array_dyn<s64> context_stack;
    
    Array_dyn<s64> constraint_offsets;
    Array_dyn<u64> constraint_data;
    Array_dyn<s64> constraint_parent; // -1 if no parent
    Array_dyn<s64> constraint_parent_stack;
    Array_dyn<s64> constraint_context;

    Array_dyn<s64> group_offsets;
    Array_dyn<u64> group_literals;

    Array_dyn<u64> rewrite_temp;
    Array_dyn<u64> expand_temp;
    Array_dyn<u64> explain_temp;

    s64 temp_var_count;

    using Rewrite_func = void(*)(Sat_instance*, u64, Array_t<u64>);
    Array_t<Rewrite_func> rewrite_funcs;

    using Expand_func = void(*)(Sat_instance*, u64, Array_dyn<u64>*);
    Array_t<Expand_func> expand_funcs;
    
    using Explain_func = bool(*)(Sat_instance*, u64, Array_t<u64>, Array_dyn<u8>*);
    Hashmap<Explain_func> explain_funcs;

    Hashmap<s64> params;

    u64 debug_forbidden_id;
    s64 debug_forbidden_clause;
    bool debug_explain_vars_raw;
};

namespace Sat {

enum Types: u64 {
    MASK_TYPE        = 0x70ull << 56,
    MASK_SUBTYPE     = 0x7full << 56,
    MASK_SUFFIX      = ~0ull >> 8,
    VAR              = 0x10ull << 56,
    VAR_TEMP         = 0x11ull << 56,
    VAR_CONST        = 0x12ull << 56,
    GROUP            = 0x20ull << 56,
    GROUP_STORED     = 0x21ull << 56,
    CONSTRAINT       = 0x30ull << 56,
    CONSTRAINT_BASIC = 0x31ull << 56,
    PARAM            = 0x40ull << 56,
    PARAM_BASIC      = 0x41ull << 56,
};

enum Vars_const: u64 {
    VAR_BASIC_BEGIN = VAR_CONST,
    var_false,
    var_true,
};

enum Constraints_basic: u64 {
    CONSTRAINT_BASIC_BEGIN = CONSTRAINT_BASIC,
    clause,
    clause_noexpand,
    at_most_one,
    exactly_one,
    at_most_k,
    exactly_k,
    logical_and,
    implies,
    equivalent,
    merge,
    merge_multi,    
};

} // end of namespace Sat_constants

void sat_decompose(u64 lit, u64* out_type=nullptr, u64* out_subtype=nullptr, u64* out_suffix=nullptr) {
    if (lit >> 63 & 1) lit = ~lit;
    if (out_type)    *out_type    = lit & Sat::MASK_TYPE;
    if (out_subtype) *out_subtype = lit & Sat::MASK_SUBTYPE;
    if (out_suffix)  *out_suffix  = lit & Sat::MASK_SUFFIX;
}

template <typename T>
Array_t<T> array_subindex(Array_t<s64> indices, Array_t<T> data, s64 el) {
    return array_subarray(data, indices[el], indices[el+1]);
}

void sat_register_expand_func(Sat_instance* inst, u64 subtype, Sat_instance::Expand_func func) {
    assert((subtype & Sat::MASK_TYPE) == Sat::GROUP);
    s64 index = ((subtype - Sat::GROUP) >> 56) - 1;
    assert(inst->expand_funcs[index] == nullptr);
    inst->expand_funcs[index] = func;
}
void sat_register_rewrite_func(Sat_instance* inst, u64 subtype, Sat_instance::Rewrite_func func) {
    assert((subtype & Sat::MASK_TYPE) == Sat::CONSTRAINT);
    s64 index = ((subtype - Sat::CONSTRAINT) >> 56) - 1;
    assert(inst->rewrite_funcs[index] == nullptr);
    inst->rewrite_funcs[index] = func;
}
void sat_register_explain_func(Sat_instance* inst, u64 subtype, Sat_instance::Explain_func func) {
    Sat_instance::Explain_func* f = hashmap_getcreate(&inst->explain_funcs, subtype);
    assert(*f == nullptr);
    *f = func;
}

u64 sat_group(Sat_instance* inst, Array_t<u64> lits) {
    s64 index = inst->group_offsets.size - 1;
    array_append(&inst->group_literals, lits);
    array_push_back(&inst->group_offsets, inst->group_literals.size);
    assert((index & ~Sat::GROUP_STORED) == index);
    return index | Sat::GROUP_STORED;
}

u64 sat_temp_create(Sat_instance* inst, s64 count = 1) {
    u64 result = Sat::VAR_TEMP | inst->temp_var_count;
    inst->temp_var_count += count;
    return result;
}

u64 sat_temp_group_create(Sat_instance* inst, s64 size) {
    assert(size >= 0);
    u64 lit_beg = sat_temp_create(inst, size);
    
    s64 index = inst->group_offsets.size - 1;
    for (s64 i = 0; i < size; ++i) {
        array_push_back(&inst->group_literals, lit_beg + i);
    }
    array_push_back(&inst->group_offsets, inst->group_literals.size);
    assert((index & ~Sat::GROUP_STORED) == index);
    return index | Sat::GROUP_STORED;
}

void sat_expand_stored(Sat_instance* inst, u64 var, Array_dyn<u64>* out_lits) {
    auto group = array_subindex(inst->group_offsets, inst->group_literals, var & Sat::MASK_SUFFIX);
    array_append(out_lits, group);
}

Array_t<u64> sat_expand(Sat_instance* inst, Array_t<u64> lits, Array_dyn<u64>* out_lits) {
    assert(out_lits);
    s64 off = out_lits->size;
    for (u64 lit: lits) {
        assert(lit != inst->debug_forbidden_id);
        u64 type, subtype, suffix;
        sat_decompose(lit, &type, &subtype, &suffix);

        if (type == Sat::VAR) {
            array_push_back(out_lits, lit);
        } else if (type == Sat::GROUP) {
            u64 mask = (s64)lit >> 63; // use sign-extension
            
            s64 beg = out_lits->size;
            s64 func_index = ((subtype - Sat::GROUP) >> 56) - 1;
            (*inst->expand_funcs[func_index])(inst, lit ^ mask, out_lits);
            
            for (u64& i: array_subarray(*out_lits, beg, out_lits->size)) {
                i ^= mask;
            }
        } else {
            assert(false);
        }
    }

    return array_subarray(*out_lits, off, out_lits->size);
}

Array_t<u64> sat_expand(Sat_instance* inst, u64 lit, Array_dyn<u64>* out_lits) {
    return sat_expand(inst, std::initializer_list<u64>{lit}, out_lits);
}

template <typename T>
void array_reverse(Array_t<T> arr) {
    for (s64 i = 0; 2*i + 1 < arr.size; ++i) {
        T temp = arr[i];
        arr[i] = arr[arr.size-1 - i];
        arr[arr.size-1 - i] = temp;
    }
}

Array_t<u64> sat_expand_recursive(Sat_instance* inst, Array_t<u64> lits, Array_dyn<u64>* out_lits) {
    assert(out_lits);
    s64 off = out_lits->size;

    inst->expand_temp.size = 0;
    sat_expand(inst, lits, &inst->expand_temp);
    array_reverse(inst->expand_temp);

    while (inst->expand_temp.size) {
        u64 lit = inst->expand_temp.back();
        --inst->expand_temp.size;
        
        u64 type;
        sat_decompose(lit, &type);
        if (type == Sat::VAR) {
            array_push_back(out_lits, lit);
        } else {
            auto arr = sat_expand(inst, {lit}, &inst->expand_temp);
            array_reverse(arr);
        }
    }

    return array_subarray(*out_lits, off, out_lits->size);
}

Array_t<u64> sat_expand_recursive(Sat_instance* inst, u64 lit, Array_dyn<u64>* out_lits) {
    return sat_expand_recursive(inst, std::initializer_list<u64>{lit}, out_lits);
}


u64 sat_vget(Array_t<u64> vec, s64 i) {
    return 0 <= i and i < vec.size ? vec.data[i] : (u64)Sat::var_false;
}

Array_t<u64> sat_subrange(Sat_instance* inst, Array_t<u64> arr, s64 beg, s64 end, s64 step) {
    if (step < 0) {
        simple_swap(&beg, &end);
        step = -step;
    }
    assert(0 <= beg and end <= arr.size and step > 0);
    
    s64 off = inst->rewrite_temp.size;
    for (s64 i = beg; i < end; i += step) {
        array_push_back(&inst->rewrite_temp, arr[i]);
    }
    return array_subarray(inst->rewrite_temp, off);
}

void sat_add(Sat_instance* inst, u64 op, Array_t<u64> args);

void sat_adds(Sat_instance* inst, u64 op, u64 arg) {
    sat_add(inst, op, {arg});
}
void sat_addg(Sat_instance* inst, u64 op, Array_t<u64> arg) {
    sat_add(inst, op, {sat_group(inst, arg)});
}
void sat_addg(Sat_instance* inst, u64 op, Array_t<u64> arg0, Array_t<u64> arg1) {
    sat_add(inst, op, {sat_group(inst, arg0), sat_group(inst, arg1)});
}
void sat_addg(Sat_instance* inst, u64 op, Array_t<u64> arg0, Array_t<u64> arg1, Array_t<u64> arg2) {
    sat_add(inst, op, {sat_group(inst, arg0), sat_group(inst, arg1), sat_group(inst, arg2)});
}

void sat_rewrite_basic(Sat_instance* inst, u64 op, Array_t<u64> args) {
    using namespace Sat;
    switch (op) {
        
    case clause: {
        auto context = array_subindex(inst->context_offsets, inst->context_data, inst->context_stack.back());
        for (u64 lit: context) array_push_back(&inst->clause_literals, ~lit);
        sat_expand_recursive(inst, args, &inst->clause_literals);
        assert(inst->clause_constraint.size != inst->debug_forbidden_clause);
        array_push_back(&inst->clause_constraint, inst->constraint_parent_stack.back());
        array_push_back(&inst->clause_offsets, inst->clause_literals.size);
    } break;

    case clause_noexpand: {
        for (u64 lit: args) {
            u64 type;
            sat_decompose(lit, &type);
            assert(type == VAR);
        }
        auto context = array_subindex(inst->context_offsets, inst->context_data, inst->context_stack.back());
        for (u64 lit: context) array_push_back(&inst->clause_literals, ~lit);
        array_append(&inst->clause_literals, args);
        assert(inst->clause_constraint.size != inst->debug_forbidden_clause);
        array_push_back(&inst->clause_constraint, inst->constraint_parent_stack.back());
        array_push_back(&inst->clause_offsets, inst->clause_literals.size);
    } break;

    case at_most_one: {
        auto lits = sat_expand_recursive(inst, args, &inst->rewrite_temp);
        for (s64 i = 0; i < lits.size; ++i) {
            for (s64 j = i+1; j < lits.size; ++j) {
                sat_add(inst, clause_noexpand, {~lits[i], ~lits[j]});
            }
        }
    } break;

    case exactly_one:
        sat_add(inst, clause, args);
        sat_add(inst, at_most_one, args);
        break;
        
    case at_most_k: {
        // Sequential counter
        // See: SAT Encodings of the At-Most-k Constraint, Alan Frish and Paul Giannaros
        assert(args.size == 2);
        auto arr = sat_expand(inst, args[0], &inst->rewrite_temp);
        u64 k = (s64)args[1];
        //u64 out = sat_temp_group_create(inst, k);
        //sat_add(inst, merge_multi, {args[0], out});
        //break;
        
        u64 base = sat_temp_create(inst, arr.size * k);
        
        for (s64 i = 0; i+1 < arr.size; ++i) {
            sat_add(inst, implies, {arr[i], base + i*k});
        }
        for (s64 j = 1; j < k; ++j) {
            sat_add(inst, clause, {~(base + j)});
        }
        for (s64 i = 1; i+1 < arr.size; ++i) {
            for (s64 j = 0; j < k; ++j) {
                sat_add(inst, implies, {base + (i-1)*k + j, base + i*k + j});
            }
        }
        for (s64 i = 1; i+1 < arr.size; ++i) {
            for (s64 j = 1; j < k; ++j) {
                sat_add(inst, clause, {~arr[i], ~(base + (i-1)*k + j-1), base + i*k + j});
            }
        }
        for (s64 i = 1; i < arr.size; ++i) {
            sat_add(inst, implies, {arr[i], ~(base + (i-1)*k + k-1)});
        }
    } break;

    case exactly_k: {
        assert(args.size == 2);
        u64 k = (s64)args[1];
        s64 index = inst->rewrite_temp.size;
        for (s64 i = 0; i < k; ++i) {
            array_push_back(&inst->rewrite_temp, var_true);
        }
        u64 out = sat_group(inst, array_subarray(inst->rewrite_temp, index));
        sat_add(inst, merge_multi, {args[0], out});
    } break;
        
    case logical_and: {
        auto lits = sat_expand_recursive(inst, args, &inst->rewrite_temp);
        auto context = array_subindex(inst->context_offsets, inst->context_data, inst->context_stack.back());

        s64 lits_to_write = (context.size + 1) * lits.size;
        array_reserve(&inst->clause_literals, inst->clause_literals.size + lits_to_write);
        u64* p = inst->clause_literals.data + inst->clause_literals.size;

        array_reserve(&inst->clause_constraint, inst->clause_constraint.size + lits.size);
        array_reserve(&inst->clause_offsets,    inst->clause_offsets.size    + lits.size);

        s64 parent = inst->constraint_parent_stack.back();
        
        for (u64 lit: lits) {
            memcpy(p, context.data, context.size * sizeof(context[0]));
            for (s64 i = 0; i < context.size; ++i) *p = ~*p;
            p += context.size;
            *p++ = lit;
            inst->clause_literals.size += context.size + 1;
            
            assert(inst->clause_constraint.size != inst->debug_forbidden_clause);
            inst->clause_constraint.data[inst->clause_constraint.size++] = parent;
            inst->clause_offsets.data[inst->clause_offsets.size++] = inst->clause_literals.size;
        }

        assert(p == inst->clause_literals.end());
    } break;

    case implies:
        assert(args.size == 2);
        sat_add(inst, clause, {~args[0], args[1]});
        break;

    case equivalent:
        assert(args.size == 2);
        sat_add(inst, clause_noexpand, {~args[0],  args[1]});
        sat_add(inst, clause_noexpand, { args[0], ~args[1]});
        break;

    case merge: {
        assert(args.size == 3);
        Array_t<u64> arr0 = sat_expand(inst, {args[0]}, &inst->rewrite_temp);
        Array_t<u64> arr1 = sat_expand(inst, {args[1]}, &inst->rewrite_temp);
        Array_t<u64> into = sat_expand(inst, {args[2]}, &inst->rewrite_temp);

        if (arr1.size == 0) {
            for (s64 i = 0; i < arr0.size or i < into.size; ++i) {
                sat_add(inst, equivalent, {sat_vget(arr0, i), sat_vget(into, i)});
            }
        } else if (arr0.size == 0) {
            for (s64 i = 0; i < arr1.size or i < into.size; ++i) {
                sat_add(inst, equivalent, {sat_vget(arr1, i), sat_vget(into, i)});
            }
        } else if (arr0.size == 1 and arr1.size == 1) {
            u64 a0 = arr0[0], b0 = arr1[0], c0 = sat_vget(into, 0), c1 = sat_vget(into, 1);
            sat_add(inst, implies, {a0, c0});
            sat_add(inst, implies, {b0, c0});
            sat_addg(inst, implies, {~a0, ~b0}, {~c0});
            sat_add(inst, implies, {~a0, ~c1});
            sat_add(inst, implies, {~b0, ~c1});
            sat_addg(inst, implies, {a0, b0}, {c1});
            for (s64 i = 2; i < into.size; ++i) {
                sat_add(inst, clause, {~into[i]});
            }
        } else {
            if (arr0.size < arr1.size) simple_swap(&arr0, &arr1);
            s64 len = arr0.size + arr1.size;

            s64 inter_off = inst->rewrite_temp.size;
            array_push_back(&inst->rewrite_temp, sat_vget(into, 0));
            for (s64 i = 1; i+1 < len; ++i) {
                array_push_back(&inst->rewrite_temp, sat_temp_create(inst, 1));
            }
            array_push_back(&inst->rewrite_temp,
                len % 2 == 0 ? sat_vget(into, len-1) : sat_temp_create(inst, 1)
            );
            Array_t<u64> inter = array_subarray(inst->rewrite_temp, inter_off);

            s64 even_off = inst->rewrite_temp.size;
            sat_subrange(inst, inter, 0, arr0.size, 2);
            sat_subrange(inst, inter, arr0.size, len, 2);
            Array_t<u64> even = array_subarray(inst->rewrite_temp, even_off);

            s64 odds_off = inst->rewrite_temp.size;
            sat_subrange(inst, inter, 1, arr0.size, 2);
            sat_subrange(inst, inter, arr0.size+1, len, 2);
            Array_t<u64> odds = array_subarray(inst->rewrite_temp, odds_off);

            sat_addg(inst, merge, sat_subrange(inst, arr0, 0, arr0.size, 2),
                                  sat_subrange(inst, arr1, 0, arr1.size, 2), even);
            sat_addg(inst, merge, sat_subrange(inst, arr0, 1, arr0.size, 2),
                                  sat_subrange(inst, arr1, 1, arr1.size, 2), odds );
            for (s64 i = 1; i+1 < len; i += 2) {
                sat_addg(inst, merge, {inter[i]}, {inter[i+1]}, {sat_vget(into, i), sat_vget(into, i+1)});
            }
            for (s64 i = len; i < into.size; ++i) {
                sat_add(inst, clause, {~into[i]});
            }
        }
    } break;

        
    case merge_multi: {
        assert(args.size == 2);
        Array_t<u64> arrs = sat_expand(inst, {args[0]}, &inst->rewrite_temp);
        u64 into = args[1];

        Array_t<u64> into_arr = sat_expand(inst, {into}, &inst->rewrite_temp);

        while (arrs.size > 2) {
            s64 j = 0;
            for (s64 i = 0; i < arrs.size; i += 2) {
                if (i+1 < arrs.size) {
                    u64 temp = sat_temp_group_create(inst, into_arr.size);
                    sat_add(inst, merge, {arrs[i], arrs[i+1], temp});
                    arrs[j++] = temp;
                } else {
                    arrs[j++] = arrs[i];                    
                }
            }
            arrs.size = j;
        }
        if (arrs.size == 0) {
            sat_add(inst, logical_and, {~into});
        } else if (arrs.size == 1) {
            Array_t<u64> from_arr = sat_expand(inst, {arrs[0]}, &inst->rewrite_temp);
            s64 len = max(into_arr.size, from_arr.size);
            for (s64 i = 0; i < len; ++i) {
                // @copypaste 8w4uf98shd
                u64 lit0 = i < from_arr.size ? from_arr[i] : (u64)var_false;
                u64 lit1 = i < into_arr.size ? into_arr[i] : (u64)var_false;
                sat_add(inst, equivalent, {lit0, lit1});
            }
        } else {
            assert(arrs.size == 2);
            sat_add(inst, merge, {arrs[0], arrs[1], into});
        }
    } break;

    default:
        assert(false);
        
    }
}

void sat_add(Sat_instance* inst, u64 op, Array_t<u64> args) {
    u64 type, subtype, suffix;
    sat_decompose(op, &type, &subtype, &suffix);

    assert(op != inst->debug_forbidden_id);
    for (u64 lit: args) assert(lit != inst->debug_forbidden_id);
    
    assert(type == Sat::CONSTRAINT);
    
    s64 prev = inst->rewrite_temp.size;
    defer { inst->rewrite_temp.size = prev; };
    
    s64 prev_context = inst->context_stack.size;

    array_push_back(&inst->constraint_data, op);
    array_append(&inst->constraint_data, args);
    array_push_back(&inst->constraint_offsets, inst->constraint_data.size);
    
    s64 index = inst->constraint_parent.size;
    array_push_back(&inst->constraint_parent, inst->constraint_parent_stack.back());
    array_push_back(&inst->constraint_parent_stack, index);
    defer { --inst->constraint_parent_stack.size; };

    array_push_back(&inst->constraint_context, inst->context_stack.back());

    s64 args_copy_i = inst->rewrite_temp.size;
    array_append(&inst->rewrite_temp, args);
    auto args_copy = array_subarray(inst->rewrite_temp, args_copy_i);

    s64 func_index = ((subtype - Sat::CONSTRAINT) >> 56) - 1;
    (*inst->rewrite_funcs[func_index])(inst, op, args_copy);

    assert(inst->context_stack.size == prev_context);
}

void sat_explain(Sat_instance* inst, u64 id, Array_dyn<u8>* into);
void sat_explain(Sat_instance* inst, u64 id, Array_t<u64> args, Array_dyn<u8>* into);
    
void _sat_explain_print_joined(Sat_instance* inst, Array_t<u64> ids, char const* join_str, Array_dyn<u8>* into) {
    bool first = true;
    for (u64 lit: ids) {
        if (not first) array_printf(into, join_str);
        else           first = false;
        sat_explain(inst, lit, into);
    }
}

bool sat_explain_undefined(Sat_instance* inst, u64 id, Array_t<u64> args, Array_dyn<u8>* into) {
    using namespace Sat;
    
    u64 type, subtype, suffix;
    sat_decompose(id, &type, &subtype, &suffix);

    if (type == VAR) {
        array_printf(into, "$%llx:%llx", (subtype >> 56) & 0xf, suffix);
        return true;
    } else if (type == CONSTRAINT) {
        array_printf(into, "<%llx:%llx>", (subtype >> 56) & 0xf, suffix);
        return false;
    } else if (type == GROUP) {
        return false;
    } else {
        assert(false);
    }
}
    
void sat_explain(Sat_instance* inst, u64 id, Array_t<u64> args, Array_dyn<u8>* into) {
    using namespace Sat;

    u64 type, subtype;
    sat_decompose(id, &type, &subtype);

    u64 id_mod = id;
    if (type == VAR or type == GROUP) {
        u64 mask = -(id >> 63);
        if (mask) array_printf(into, u8"¬");    
        id_mod = id ^ mask;
    }


    s64 off = inst->explain_temp.size;
    defer { inst->explain_temp.size = off; };
        
    Sat_instance::Explain_func* func = hashmap_getptr(&inst->explain_funcs, subtype);

    if (type == VAR and inst->debug_explain_vars_raw) {
        func = nullptr;
    }

    bool done = func
        ? (*func)(inst, id_mod, args, into)
        : sat_explain_undefined(inst, id_mod, args, into);

    if (not done) {
        if (type == VAR) {
            // This does not really make sense. Why would you want to not explain a variable?  There
            // is not really anything recursive. For now, let's just panic.
            assert(false);
        } else if (type == CONSTRAINT) {
            // Print the arguments in standard function form
            array_printf(into, "(");
            _sat_explain_print_joined(inst, args, ", ", into);
            array_printf(into, ")");
        } else if (type == GROUP) {
            array_printf(into, "[");
            _sat_explain_print_joined(inst, args, ", ", into);
            array_printf(into, "]");
        } else {
            assert(false);
        }
    }
}

void sat_explain(Sat_instance* inst, u64 id, Array_dyn<u8>* into) {
    using namespace Sat;
    
    u64 type, subtype, suffix;
    sat_decompose(id, &type, &subtype, &suffix);

    if (type == VAR) {
        return sat_explain(inst, id, {}, into);
    } else if (type == GROUP) {
        s64 off = inst->explain_temp.size;
        defer { inst->explain_temp.size = off; };
        
        Array_t<u64> arr = sat_expand(inst, id, &inst->explain_temp);
        sat_explain(inst, id, arr, into);
    } else if (type == 0) {
        array_printf(into, "%lld", id);
    } else {
        assert(false);
    }
}

void sat_explain_constraint(Sat_instance* inst, s64 index, Array_dyn<u8>* into) {
    auto context = array_subindex(inst->context_offsets, inst->context_data, inst->constraint_context[index]);

    if (context.size) {
        array_printf(into, "{");
        _sat_explain_print_joined(inst, context, ", ", into);
        array_printf(into, "} ");
    }

    auto data = array_subindex(inst->constraint_offsets, inst->constraint_data, index);
    sat_explain(inst, data[0], array_subarray(data, 1), into);
}

bool sat_explain_basic(Sat_instance* inst, u64 id, Array_t<u64> args, Array_dyn<u8>* into) {
    using namespace Sat;
    
    u64 type, subtype, suffix;
    sat_decompose(id, &type, &subtype, &suffix);
    
    if (type == VAR) {
        if (subtype == VAR_TEMP) {
            array_printf(into, "$%lld", suffix);
        } else if (subtype == VAR_CONST) {
            if (id == var_false) {
                array_printf(into, u8"⊥");
            } else if (id == var_true) {
                array_printf(into, u8"⊤");
            } else {
                assert(false);
            }
        } else {
            assert(false);
        }
    } else if (type == GROUP) {
        assert(subtype == GROUP_STORED);
        array_printf(into, "[");
        _sat_explain_print_joined(inst, args, ", ", into);
        array_printf(into, "]");
    } else if (type == CONSTRAINT) {
        assert(subtype == CONSTRAINT_BASIC);
        
        if (id == clause or id == clause_noexpand) {
            Array_t<u64> arr = sat_expand_recursive(inst, args, &inst->explain_temp);
            _sat_explain_print_joined(inst, arr, u8" ∨ ", into);
        } else if (id == at_most_one) {
            _sat_explain_print_joined(inst, args, " + ", into);
            array_printf(into, u8" ≤ 1");
        } else if (id == exactly_one) {
            _sat_explain_print_joined(inst, args, " + ", into);
            array_printf(into, " = 1");
        } else if (id == logical_and) {
            _sat_explain_print_joined(inst, args, u8" ∧ ", into);
        } else if (id == implies) {
            assert(args.size == 2);
            Array_t<u64> arr0 = sat_expand_recursive(inst, args[0], &inst->explain_temp);
            Array_t<u64> arr1 = sat_expand_recursive(inst, args[1], &inst->explain_temp);
            _sat_explain_print_joined(inst, arr0, u8" ∧ ", into);
            array_printf(into, u8" ⇒ ");
            _sat_explain_print_joined(inst, arr1, u8" ∨ ", into);
        } else if (id == equivalent) {
            assert(args.size == 2);
            sat_explain(inst, args[0], into);
            array_printf(into, u8" ⇔ ");
            sat_explain(inst, args[1], into);
        } else if (id == merge) {
            assert(args.size == 3);
            sat_explain(inst, args[2], into);
            array_printf(into, " = merge(");
            sat_explain(inst, args[0], into);
            array_printf(into, ", ");
            sat_explain(inst, args[1], into);
            array_printf(into, ")");
        } else if (id == merge_multi) {
            assert(args.size == 2);
            Array_t<u64> arr = sat_expand(inst, args[0], &inst->explain_temp);
            sat_explain(inst, args[1], into);
            array_printf(into, " = merge_multi(");
            _sat_explain_print_joined(inst, arr, ", ", into);
            array_printf(into, ")");
        }
    } else {
        assert(false);
    }
    return true;
}
    
void sat_push(Sat_instance* inst, Array_t<u64> condition) {
    auto cur = array_subindex(inst->context_offsets, inst->context_data, inst->context_stack.back());
    array_append(&inst->context_data, cur);
    array_append(&inst->context_data, condition);
    array_push_back(&inst->context_stack, inst->context_offsets.size-1);
    array_push_back(&inst->context_offsets, inst->context_data.size);
}
void sat_push_amend(Sat_instance* inst, u64 condition_lit) {
    array_push_back(&inst->context_data, condition_lit);
    ++inst->context_offsets.back();
}

void sat_pop(Sat_instance* inst) {
    --inst->context_stack.size;
}

void sat_push_add_pop(Sat_instance* inst, Array_t<u64> condition, u64 op, Array_t<u64> args) {
    sat_push(inst, condition);
    sat_add(inst, op, args);
    sat_pop(inst);
}

void sat_init(Sat_instance* inst) {
    inst->clause_offsets.size = 0;
    inst->clause_literals.size = 0;
    inst->clause_constraint.size = 0;
    inst->context_offsets.size = 0;
    inst->context_data.size = 0;
    inst->context_stack.size = 0;
    inst->constraint_offsets.size = 0;
    inst->constraint_data.size = 0;
    inst->constraint_parent = 0;
    inst->constraint_parent_stack.size = 0;
    inst->constraint_context.size = 0;
    inst->group_offsets.size = 0;
    inst->group_literals.size = 0;
    inst->rewrite_temp.size = 0;
    inst->expand_temp.size = 0;
    inst->explain_temp.size = 0;

    if (not inst->rewrite_temp.data) {
        inst->rewrite_temp = array_create_unreserved<u64>(128 * 1024 * 1024);
    }
    if (not inst->expand_temp.data) {
        inst->expand_temp  = array_create_unreserved<u64>(1024 * 1024);
    }
    if (not inst->explain_temp.data) {
        inst->explain_temp = array_create_unreserved<u64>(1024 * 1024);
    }
    
    array_memset(&inst->rewrite_funcs);
    array_memset(&inst->expand_funcs);
    hashmap_clear(&inst->explain_funcs);
    hashmap_clear(&inst->params);
    inst->debug_forbidden_id = 0x10ull << 56;
    inst->debug_forbidden_clause = -1;
    inst->debug_explain_vars_raw = false;
    
    array_push_back(&inst->clause_offsets, 0ll);
    array_append(&inst->context_offsets, {0ll, 0ll}); // One empty context in front
    array_push_back(&inst->context_stack, 0ll);
    array_push_back(&inst->constraint_offsets, 0ll);
    array_push_back(&inst->constraint_parent_stack, -1ll);
    array_push_back(&inst->group_offsets, 0ll);

    array_resize(&inst->rewrite_funcs, 16);
    array_resize(&inst->expand_funcs,  16);
    
    sat_register_expand_func(inst, Sat::GROUP_STORED, &sat_expand_stored);
    sat_register_rewrite_func(inst, Sat::CONSTRAINT_BASIC, &sat_rewrite_basic);
    for (u64 subtype: {Sat::VAR_TEMP, Sat::VAR_CONST, Sat::GROUP_STORED, Sat::CONSTRAINT_BASIC}) {
        sat_register_explain_func(inst, subtype, &sat_explain_basic);
    }

    inst->temp_var_count = 0;

    sat_add(inst, Sat::clause, {~Sat::var_false});
    sat_add(inst, Sat::clause, {Sat::var_true});

}

struct Sat_dimacs {
    Hashmap<u32> map_forth;  // map to dimacs
    Array_dyn<u64> map_back; // map back from dimacs
    Array_dyn<u8> text;
};

void sat_write_dimacs(Sat_instance* inst, Sat_dimacs* dimacs) {
    static const u16* lookup = (u16*)
        "0001020304050607080910111213141516171819202122232425262728293031323334353637383940414243444546474849"
        "5051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899";
    hashmap_clear(&dimacs->map_forth);
    dimacs->map_back.size = 0;
    dimacs->text.size = 0;
    
    array_push_back(&dimacs->map_back, 0ull);

    array_printf(&dimacs->text, "p cnf ");
    s64 patch_var_count_offset = dimacs->text.size;
    array_printf(&dimacs->text, "%20s %lld\n", "", inst->clause_offsets.size-1);

    Array_dyn<u64> lookups;
    defer { array_free(&lookups); };
    
    for (s64 i = 0; i+1 < inst->clause_offsets.size; ++i) {
        Array_t<u64> clause = array_subindex(inst->clause_offsets, inst->clause_literals, i);
        s64 maximum_size = dimacs->text.size + 22 * clause.size + 8;
        array_reserve(&dimacs->text, maximum_size);

        for (u64 lit: clause) {
            bool neg = lit >> 63;
            u64 var = neg ? ~lit : lit;

            array_push_back(&lookups, var);
            u32* mapped_ptr = hashmap_getcreate(&dimacs->map_forth, var, {});
            if (*mapped_ptr == 0) {
                *mapped_ptr = dimacs->map_back.size;
                array_push_back(&dimacs->map_back, var);
            }
            u32 mapped = *mapped_ptr;

            if (neg) dimacs->text.data[dimacs->text.size++] = '-';
            s64 width = (174082924800ull - __builtin_clzll(mapped) * 2585827972ull) >> 33;
            u8* p0 = dimacs->text.data + dimacs->text.size;
            u16* p = (u16*)p0;

            s64 x = width&1 ? mapped / 10 : mapped;
            switch (width/2) {
            case 5: *p++ = lookup[x / 100000000]; // fall-through
            case 4: *p++ = lookup[x / 1000000 % 100]; // fall-through
            case 3: *p++ = lookup[x / 10000   % 100]; // fall-through
            case 2: *p++ = lookup[x / 100     % 100]; // fall-through
            case 1: *p++ = lookup[x           % 100];
            }
            if (width&1) *(u8*)p = '0' + mapped % 10;
            if (*p0 == '0') {*p0 = '-'; p0[-neg] = ' ';}
            dimacs->text.size += width;
            
            dimacs->text.data[dimacs->text.size++] = ' ';
        }

        dimacs->text.data[dimacs->text.size++] = '0';
        dimacs->text.data[dimacs->text.size++] = '\n';

        assert(dimacs->text.size <= maximum_size);
    }
    
    s64 n = snprintf((char*)(dimacs->text.data + patch_var_count_offset), 20, "%lld", dimacs->map_forth.size);
    assert(n < 20);
    dimacs->text.data[patch_var_count_offset + n] = ' ';
}

void sat_dimacs_free(Sat_dimacs* dimacs) {
    hashmap_free(&dimacs->map_forth);
    array_free(&dimacs->map_back);
    array_free(&dimacs->text);
}

void sat_write_human(Sat_instance* inst, Array_dyn<u8>* into, bool skip_basic=false) {
    Array_dyn<s64> parent_stack;
    defer { array_free(&parent_stack); };

    s64 i_clause = 0;
    s64 silent_until_stack_length = -1;
    for (s64 i = 0; i < inst->constraint_parent.size; ++i) {
        s64 parent = inst->constraint_parent[i];

        while (parent_stack.size and parent_stack.back() != parent) {
            --parent_stack.size;
        }
        array_push_back(&parent_stack, i);

        if (silent_until_stack_length >= parent_stack.size) {
            silent_until_stack_length = -1;
        }
        if (silent_until_stack_length == -1) {
            for (s64 j = 0; j+2 < parent_stack.size*2; ++j) {
                array_push_back(into, (u8)' ');
            }

            sat_explain_constraint(inst, i, into);
            array_push_back(into, (u8)'\n');
        }

        while (i_clause < inst->clause_constraint.size and inst->clause_constraint[i_clause] == i) {
            if (not skip_basic) {
                for (s64 j = 0; j < parent_stack.size*2; ++j) {
                    array_push_back(into, (u8)' ');
                }

                Array_t<u64> data = array_subindex(inst->clause_offsets, inst->clause_literals, i_clause);
                sat_explain(inst, Sat::clause, data, into);
                array_push_back(into, (u8)'\n');
            }
            
            ++i_clause;
        }

        if (skip_basic and silent_until_stack_length == -1
                and (inst->constraint_data[inst->constraint_offsets[i]]>>56 & 0xf) <= 4) {
            silent_until_stack_length = parent_stack.size;
        }
    }
}

struct Sat_solution {
    enum Literal_values: u8 {
        L_UNASSIGNED, L_FALSE, L_TRUE
    };

    Hashmap<bool> values;

    u8 get(u64 lit) {
        u64 isneg = lit >> 63;
        if (bool* val = hashmap_getptr(&values, lit^-isneg)) {
            return *val ^ isneg ? L_TRUE : L_FALSE;
        } else {
            return L_UNASSIGNED;
        }
    }
    void set(u64 lit) {
        u64 isneg = lit >> 63;
        hashmap_set(&values, lit^-isneg, isneg ? false : true);
    }
    bool istrue (u64 lit) { return get(lit) == L_TRUE;  }
    bool isfalse(u64 lit) { return get(lit) == L_FALSE; }
    bool operator[] (u64 lit) {
        switch (get(lit)) {
        case L_FALSE: return false;
        case L_TRUE:  return true;
        default: assert(false); return false;
        }
    }
};

void sat_solution_write_human(Sat_instance* inst, Sat_solution* sol, Array_dyn<u8>* into) {
    for (auto slot: sol->values.slots) {
        if (slot.key == sol->values.empty) continue;
        u64 lit = slot.key;
        if (not slot.val) lit ^= (u64)-1;
        sat_explain(inst, lit, into);
        array_push_back(into, '\n');
    }
}

void sat_solution_write(Sat_solution* sol, Array_dyn<u64>* into) {
    array_push_back(into, 0);
    for (auto slot: sol->values.slots) {
        if (slot.key == sol->values.empty) continue;
        u64 lit = slot.key;
        if (not slot.val) lit ^= (u64)-1;
        array_push_back(into, lit);
    }
    (*into)[0] = into->size-1;
}

struct Sat_propagation {
    u64 lit; s64 clause;
};

Sat_solution sat_solution_from_instance(
    Sat_instance* inst, Array_dyn<s64>* out_offsets=nullptr, Array_dyn<Sat_propagation>* out_props=nullptr
) {
    if (out_offsets or out_props) {
        assert(out_offsets and out_props);
        if (out_offsets->size == 0) {
            array_push_back(out_offsets, out_props->size);
        }
    }
    
    Sat_solution sol;

    Array_dyn<s64> clauses;
    Array_dyn<Sat_propagation> mark_for_entry;
    array_reserve(&clauses, inst->clause_offsets.size-1);
    defer { array_free(&clauses); };
    defer { array_free(&mark_for_entry); };
    for (s64 i = 0; i+1 < inst->clause_offsets.size; ++i) {
        array_push_back(&clauses, i);
    }

    bool dirty = true;
    while (dirty) {
        dirty = false;
        s64 i_out = 0;
        for (s64 i: clauses) {
            auto clause = array_subindex(inst->clause_offsets, inst->clause_literals, i);

            bool retain = false;
            u64 prop = 0;
            bool flag = false;
            bool clause_is_false = true;
            for (u64 lit: clause) {
                switch (sol.get(lit)) {
                case Sat_solution::L_FALSE: break;
                case Sat_solution::L_TRUE:
                    clause_is_false = false;
                    prop = 0; flag = true;
                    break;
                case Sat_solution::L_UNASSIGNED: {
                    clause_is_false = false;
                    if (prop) {
                        retain = true;
                        prop = 0; flag = true;
                    } else {
                        prop = lit;
                    }
                    break;
                };
                default: assert(false);
                }
                if (flag) break;
            }

            if (prop) {
                array_push_back(&mark_for_entry, {prop, i});
                dirty = true;
            } else if (clause_is_false) {
                array_push_back(&mark_for_entry, {Sat::var_false, i});
                dirty = true;
            }
            
            if (retain) {
                clauses[i_out++] = i;
            }
        }
        clauses.size = i_out;

        for (auto prop: mark_for_entry) {
            if (sol.get(prop.lit) == Sat_solution::L_FALSE) {
                if (out_props) array_push_back(out_props, {Sat::var_false, prop.clause});
            } else {
                if (out_props) array_push_back(out_props, prop);
                sol.set(prop.lit);
            }
        }
        mark_for_entry.size = 0;
        
        if (out_offsets and dirty) array_push_back(out_offsets, out_props->size);
    }
    
    return sol;
}

bool sat_solution_propagations_conflict_only(
    Sat_instance* inst, Array_dyn<Sat_propagation>* inout_props=nullptr
) {
    s64 index = -1;
    for (s64 i = 0; i < inout_props->size; ++i) {
        if ((*inout_props)[i].lit == Sat::var_false) {
            index = i; break;
        }
    }
    if (index == -1) return false;
    inout_props->size = index+1;

    Hashmap<bool> lits_needed;
    defer { hashmap_free(&lits_needed); };
    {auto clause = array_subindex(inst->clause_offsets, inst->clause_literals, inout_props->back().clause);
    for (u64 lit: clause) hashmap_set(&lits_needed, ~lit, true);}

    Array_dyn<s64> props_needed;
    array_push_back(&props_needed, inout_props->size-1);
    defer { array_free(&props_needed); };
    
    for (s64 i = inout_props->size-2; i >= 0; --i) {
        auto prop = (*inout_props)[i];
        if (hashmap_getptr(&lits_needed, prop.lit)) {
            auto clause = array_subindex(inst->clause_offsets, inst->clause_literals, prop.clause);
            for (u64 lit: clause) {
                if (lit == prop.lit) continue;
                hashmap_set(&lits_needed, ~lit, true);
            }
            array_push_back(&props_needed, i);
        }
    }

    s64 size = 0;
    for (s64 i = props_needed.size-1; i >= 0; --i) {
        (*inout_props)[size++] = (*inout_props)[props_needed[i]];
    }
    inout_props->size = size;
    return true;
}


void sat_solution_propagations_print(Sat_instance* inst, Array_t<Sat_propagation> props, Array_dyn<u8>* into) {
    for (auto prop: props) {
        auto clause = array_subindex(inst->clause_offsets, inst->clause_literals, prop.clause);
        sat_explain(inst, prop.lit, into);
        array_printf(into, "\t\t\t\t[lit=%llx,clause=%lld]\n", prop.lit, prop.clause);
        if (clause.size == 1) {
            // nothing
        } else {
            s64 constraint = inst->clause_constraint[prop.clause];
            while (constraint != -1) {
                array_printf(into, u8" ↳ ");
                sat_explain_constraint(inst, constraint, into);
                constraint = inst->constraint_parent[constraint];
                array_push_back(into, '\n');
            }
        }
    }
}

void sat_solution_init(Sat_solution* sol) {
    hashmap_clear(&sol->values);
}
void sat_solution_free(Sat_solution* sol) {
    hashmap_free(&sol->values);
}

struct Sat_proof {
    Array_dyn<s64> offsets;
    Array_dyn<u64> data;
};


void sat_proof_print(Sat_instance* inst, Sat_proof* proof, Array_dyn<u8>* into) {
    for (s64 i = 0; i+1 < proof->offsets.size; ++i) {
        auto clause = array_subindex(proof->offsets, proof->data, i);
        if (clause.size == 1) array_printf(into, "* ");
        _sat_explain_print_joined(inst, clause, ", ", into);
        array_push_back(into, '\n');
    }
}

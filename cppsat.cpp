
struct Sat_instance {
    Array_dyn<s64> clause_offsets;
    Array_dyn<u64> clause_literals;
    
    Array_dyn<s64> context_offsets;
    Array_dyn<u64> context_literals;
    
    Array_dyn<s64> constraint_offsets;
    Array_dyn<u64> constraint_data;
    Array_dyn<s64> constraint_parent; // -1 if no parent
    Array_dyn<s64> constraint_parent_stack;

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

    u64 debug_forbidden_id = 0;
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
    return 0 <= i and i < vec.size ? vec[i] : (u64)Sat::var_false;
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
        
    case clause:
        array_append(&inst->clause_literals, inst->context_literals);
        sat_expand_recursive(inst, args, &inst->clause_literals);
        array_push_back(&inst->clause_offsets, inst->clause_literals.size);
        break;

    case clause_noexpand:
        for (u64 lit: args) {
            u64 type;
            sat_decompose(lit, &type);
            assert(type == VAR);
        }
        array_append(&inst->clause_literals, inst->context_literals);
        array_append(&inst->clause_literals, args);
        array_push_back(&inst->clause_offsets, inst->clause_literals.size);
        break;

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
        
    case logical_and: {
        auto lits = sat_expand_recursive(inst, args, &inst->rewrite_temp);
        for (u64 lit: lits) {
            sat_add(inst, clause_noexpand, {lit});
        }
    } break;

    case implies:
        assert(args.size == 2);
        sat_add(inst, clause, {~args[0], args[1]});
        break;

    case equivalent:
        assert(args.size == 2);
        assert((args[0] & MASK_TYPE) == VAR and (args[1] & MASK_TYPE) == VAR);
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
    
    s64 prev_context = inst->context_literals.size;

    array_push_back(&inst->constraint_data, op);
    array_append(&inst->constraint_data, args);
    array_push_back(&inst->constraint_offsets, inst->constraint_data.size);
    
    s64 index = inst->constraint_parent.size;
    array_push_back(&inst->constraint_parent, inst->constraint_parent_stack.back());
    array_push_back(&inst->constraint_parent_stack, index);
    defer { --inst->constraint_parent_stack.size; };

    auto args_copy = array_subindex(inst->constraint_offsets, inst->constraint_data, index);
    args_copy = array_subarray(args_copy, 1, args_copy.size);

    s64 func_index = ((subtype - Sat::CONSTRAINT) >> 56) - 1;
    (*inst->rewrite_funcs[func_index])(inst, op, args_copy);

    assert(inst->context_literals.size == prev_context);
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
    bool done = func
        ? (*func)(inst, id_mod, args, into)
        : sat_explain_undefined(inst, id_mod, args, into);

    if (not done) {
        if (type == VAR) {
            // This does not really make sense. Why would you want to not explain a variable?  There
            // is not really anything recursive. For now, let's just panix.
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
    } else {
        assert(false);
    }
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
            _sat_explain_print_joined(inst, args, u8" ∨ ", into);
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
    array_append(&inst->context_literals, condition);
    assert(inst->context_literals.size != 4);
    array_push_back(&inst->context_offsets, inst->context_literals.size);
}
void sat_push_amend(Sat_instance* inst, u64 condition_lit) {
    array_push_back(&inst->context_literals, condition_lit);
    ++inst->context_offsets.back();
}

void sat_pop(Sat_instance* inst) {
    --inst->context_offsets.size;
    inst->context_literals.size = inst->context_offsets.back();
}

void sat_push_add_pop(Sat_instance* inst, Array_t<u64> condition, u64 op, Array_t<u64> args) {
    sat_push(inst, condition);
    sat_add(inst, op, args);
    sat_pop(inst);
}

void sat_init(Sat_instance* inst) {
    array_push_back(&inst->clause_offsets, 0ll);
    array_push_back(&inst->context_offsets, 0ll);
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

    inst->rewrite_temp = array_create_unreserved<u64>(128 * 1024 * 1024);
    inst->expand_temp  = array_create_unreserved<u64>(1024 * 1024);
    inst->explain_temp = array_create_unreserved<u64>(1024 * 1024);
}

struct Sat_dimacs {
    Hashmap<s64> map_forth;  // map to dimacs
    Array_dyn<u64> map_back; // map back from dimacs
    Array_dyn<u8> text;
};

void sat_write_dimacs(Sat_instance* inst, Sat_dimacs* dimacs) {
    array_push_back(&dimacs->map_back, 0ull);

    for (s64 i = 0; i+1 < inst->clause_offsets.size; ++i) {
        for (u64 lit: array_subindex(inst->clause_offsets, inst->clause_literals, i)) {
            u64 var = lit >> 63 ? ~lit : lit;

            s64* mapped = hashmap_getcreate(&dimacs->map_forth, var, 0ll);
            if (not *mapped) {
                *mapped = dimacs->map_back.size;
                array_push_back(&dimacs->map_back, var);
            }
        }
    }

    array_printf(&dimacs->text, "p cnf %d %d\n", dimacs->map_forth.size, inst->clause_offsets.size-1);
    
    for (s64 i = 0; i+1 < inst->clause_offsets.size; ++i) {
        Array_t<u64> clause = array_subindex(inst->clause_offsets, inst->clause_literals, i);

        for (u64 lit: clause) {
            bool neg = lit >> 63;
            u64 var = neg ? ~lit : lit;

            s64* mapped = hashmap_getcreate(&dimacs->map_forth, var, 0ll);
            if (not *mapped) {
                *mapped = dimacs->map_back.size;
                array_push_back(&dimacs->map_back, var);
            }

            s64 dilit = neg ? -*mapped : *mapped;
            array_printf(&dimacs->text, "%d ", (int)dilit);
        }
        array_printf(&dimacs->text, "0\n");
    }
}

void sat_write_human(Sat_instance* inst, Array_dyn<u8>* into) {
    Array_dyn<s64> parent_stack;
    defer { array_free(&parent_stack); };
    
    for (s64 i = 0; i < inst->constraint_parent.size; ++i) {
        Array_t<u64> data = array_subindex(inst->constraint_offsets, inst->constraint_data, i);
        s64 parent = inst->constraint_parent[i];

        while (parent_stack.size and parent_stack.back() != parent) {
            --parent_stack.size;
        }
        array_push_back(&parent_stack, i);

        for (s64 j = 0; j+2 < parent_stack.size*2; ++j) {
            array_push_back(into, (u8)' ');
        }
        
        sat_explain(inst, data[0], array_subarray(data, 1), into);
        array_push_back(into, (u8)'\n');
    }
}

namespace Sat {

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
};

enum Params_factorio: u64 {
    PARAM_FACTORIO_BEGIN = PARAM_FACTORIO,
    fpar_nx, fpar_ny, fpar_n_under,
    fpar_n_lines, fpar_n_linelen, fpar_n_linedim,
    fpar_do_block,
    fpar_s_input, fpar_s_output,
    PARAM_FACTORIO_END
};

} // end of namespace Sat_constants


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
    u64 sum_lines_all, sum_lines_item, sum_line_sum, sum_line_block;

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
    };
    enum Field_border_type: u8 {
        BORDER_EMPTY, BORDER_OUT, BORDER_INP
    };
    
    s64 x, y;
    u64 empty, belt;
    u64 split, splitl, splitr;
    u64 under, underh, underv;
    u64 lines_all, lines_item, line_first, line_sum, line_block;

    Dir dirs[4];
    Dir dir_all;

    Field(s64 x, s64 y);
    Field(Dir d): Field::Field(d.x, d.y) {};
    static Field from_lit(u64 lit);

    bool inbounds(Sat_instance* inst) {
        s64 nx = hashmap_get(&inst->params, Sat::fpar_nx);
        s64 ny = hashmap_get(&inst->params, Sat::fpar_ny);
        return 0 <= x and x < nx and 0 <= y and y < ny;
    }
};

void factorio_field_init_base(Field* f) {
    s64 add = Field::COORD_OFFSET;
    assert(0 <= f->x+add and f->x < 65536);
    assert(0 <= f->y+add and f->y < 65536);

    u64 base_orig = (u64)(f->x + add) << 40 | (u64)(f->y + add) << 24;
    u64 i = 0;
    {u64 base = base_orig | Sat::VAR_FACTORIO | Field::FVAR_NORMAL;
    f->empty  = base+ ++i; f->belt   = base+ ++i;
    f->split  = base+ ++i; f->splitl = base+ ++i; f->splitr = base+ ++i;
    f->under  = base+ ++i; f->underh = base+ ++i; f->underv = base+ ++i;}
    
    {u64 base = base_orig | Sat::GROUP_FACTORIO | Field::FVAR_LINEGROUP;
    f->lines_all  = base+ ++i; f->lines_item = base+ ++i;}
    
    {u64 base = base_orig | Sat::GROUP_FACTORIO | Field::FVAR_LINE;
    f->line_first = base;
    f->line_sum   = base | 0xfe00; f->line_block = base | 0xff00;}
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
    assert(subtype == Sat::VAR_FACTORIO or subtype == Sat::GROUP_FACTORIO);
    s64 x = (suffix >> 40 & 0xffff) - COORD_OFFSET;
    s64 y = (suffix >> 24 & 0xffff) - COORD_OFFSET;
    return {x, y};
}

Dir::Dir(s64 x, s64 y, u8 dir): x{x}, y{y}, dir{dir} {
    assert(Dir::BEG <= dir and dir < Dir::DIR_VALUES_SIZE);
    s64 add = Field::COORD_OFFSET;
    
    u64 base_orig = (u64)(x + add) << 40 | (u64)(y + add) << 24 | (u64)dir << 16;
    u64 base = base_orig | (dir == Dir::ALL ? Sat::GROUP_FACTORIO : Sat::VAR_FACTORIO);
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
    } else if (dir == Dir::LEFT or dir == Dir::RIGHT) {
        und = f->underh;
        f2->x = x+d; f2->y = y;
        factorio_field_init_base(f2);
        sum_lines_all  = f2->lines_all  | Field::FVAR_SUMX;
        sum_lines_item = f2->lines_item | Field::FVAR_SUMX;
        sum_line_sum   = f2->line_sum   | Field::FVAR_SUMX;
        sum_line_block = f2->line_block | Field::FVAR_SUMX;
    } else {
        und = 0;
        sum_lines_all  = 0;
        sum_lines_item = 0;
        sum_line_sum   = 0;
        sum_line_block = 0;
    }

}

Dir Dir::from_lit(u64 lit) {
    u64 subtype, suffix;
    sat_decompose(lit, nullptr, &subtype, &suffix);
    assert(subtype == Sat::VAR_FACTORIO or subtype == Sat::GROUP_FACTORIO);
    s64 x = (suffix >> 40 & 0xffff) - Field::COORD_OFFSET;
    s64 y = (suffix >> 24 & 0xffff) - Field::COORD_OFFSET;
    u8 dir = suffix >> 16 & 0xff;
    return {x, y, dir};
}

void factorio_expand(Sat_instance* inst, u64 ovar, Array_dyn<u64>* out_lits) {
    assert((ovar & Sat::MASK_SUBTYPE) == Sat::GROUP_FACTORIO);

    u64 ftype = ovar &  Field::FVAR_TYPE_MASK;
    u64 var   = ovar & ~Field::FVAR_TYPE_MASK;
    if (ftype == Field::FVAR_LINEGROUP) {
        s64 n_linedim = hashmap_get(&inst->params, Sat::fpar_n_linedim);
        bool do_block = hashmap_get(&inst->params, Sat::fpar_do_block);
        
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
                if (do_block) array_push_back(out_lits, f.line_block | mod);
            } else {
                assert((ovar^mod) == f.lines_item);
            }
        } else {
            // This is the special empty linegroup (called lines_empty)
            for (s64 i = 0; i < n_linedim; ++i) {
                array_push_back(out_lits, Sat::line_empty);
            }
            array_push_back(out_lits, Sat::line_empty);
            if (do_block) array_push_back(out_lits, Sat::line_full);
        }
    } else if (ftype == Field::FVAR_LINE) {
        s64 len = hashmap_get(&inst->params, Sat::fpar_n_linelen);
        switch (var & Field::FVAR_MOD_MASK) {
        case 0: break;
        case Field::FVAR_SUMY:
        case Field::FVAR_SUMX: len *= 2; break;
        default: assert(false);
        }
        
        if (var & Field::COORD_MASK) {
            u64 vvar = (var & ~Sat::MASK_SUBTYPE) | Sat::VAR_FACTORIO | Field::FVAR_LINEEL;
            for (s64 i = 0; i < len; ++i) {
                array_push_back(out_lits, vvar | (i+1));
            }
        } else {
            // This is either line_empty or line_full
            u64 lit;
            if      (ovar == Sat::line_empty) lit = Sat::var_false;
            else if (ovar == Sat::line_full ) lit = Sat::var_true;
            else assert(false);
            
            for (s64 i = 0; i < len; ++i) {
                array_push_back(out_lits, lit);
            }
        }
    } else if (ftype == Field::FVAR_DIR) {
        assert((ovar & Dir::FVAR_DIR_MASK) == (Dir::ALL << 16ull));
        u64 vvar = (ovar & ~Sat::MASK_SUBTYPE & ~Dir::FVAR_DIR_MASK) | Sat::VAR_FACTORIO;
        for (s64 d = Dir::BEG; d < Dir::END; ++d) {
            array_push_back(out_lits, vvar | d << 16);
        }
    } else {
        assert(false);
    }
    
}

void factorio_rewrite(Sat_instance* inst, u64 op, Array_t<u64> args) {
    using namespace Sat;
    switch (op) {
        
    case field_basic: {
        assert(args.size == 2);
        Field f {(s64)args[0], (s64)args[1]};
        
        // *** Basics *** /

        // If splitter, either left or right, ...
        sat_push_add_pop(inst, {f.split},  exactly_one, { f.splitl,  f.splitr});
        // ... else neither.
        sat_push_add_pop(inst, {~f.split}, logical_and, {~f.splitl, ~f.splitr});
        
        // If underground, exactly one direction
        sat_push_add_pop(inst, {f.under},  exactly_one, { f.underh,  f.underv});
        // ... else none.
        sat_push_add_pop(inst, {~f.under}, logical_and, {~f.underh, ~f.underv});

        for (Dir fd: f.dirs) {
            // Input and output not on same side
            sat_add(inst, at_most_one, {fd.inp, fd.out});

            // Sideflags
            sat_addg(inst, implies, {f.belt,  ~fd.inp, ~fd.out}, {fd.sid});
            sat_push_add_pop(inst, {f.under}, equivalent, {fd.turnl().und, fd.sid});

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
                sat_pop(inst);

                // ... there cannot be two subsequent splitters
                sat_addg(inst, implies, {f.splitl, fd.move().back().inp}, {~Field {fd.move()}.splitl});

            }; sat_pop(inst);

            // Non-splitters must have input and output connected
            sat_push_add_pop(inst, {~f.split}, equivalent, {fd.inp, fd.move().back().out});
            sat_push_add_pop(inst, {~f.split}, equivalent, {fd.out, fd.move().back().inp});

            // For underground lines, input/output direction determines underground direction
            sat_push(inst, {f.under});
            sat_add(inst, implies, {fd.inp, fd.und});
            sat_add(inst, implies, {fd.out, fd.und});
            sat_addg(inst, implies, {fd.und}, {fd.inp, fd.out});
            sat_pop(inst);
        }        

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
        s64 nx = hashmap_get(&inst->params, Sat::fpar_nx);
        s64 ny = hashmap_get(&inst->params, Sat::fpar_ny);
        s64 n_under = hashmap_get(&inst->params, Sat::fpar_n_under);

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
                    sat_push_amend(inst, fd.move(0, i).und);
                }

                bool dobreak = false;
                if (f2.inbounds(inst) and w < n_under+1) {
                    if (w > 2) {
                        // A connection can exist here, so we also condition on
                        // (c) target tile (i.e. f2) being underground with same orientation
                        sat_push_amend(inst, fd2.und);

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

            // @Redundant Forbid unused space after underground
            sat_addg(inst, implies, {f.under, fd.back().out, Field {fd.move()}.empty},
                {fd.move(-1, 1).turnr().out, fd.move(1, 1).turnl().out});
        }
    } break;

    case field_border: {
        assert(args.size == 2);
        Field f {(s64)args[0], (s64)args[1]};
        s64 nx = hashmap_get(&inst->params, Sat::fpar_nx);
        s64 ny = hashmap_get(&inst->params, Sat::fpar_ny);
        
        // No splitters, no underground
        sat_add(inst, logical_and, {~f.split, ~f.splitl, ~f.splitr, ~f.under, ~f.underh, ~f.underv});

        // Line control for normal belts
        for (Dir fd: f.dirs) {
            if (not Field {fd.move()}.inbounds(inst)) continue;

            sat_push_add_pop(inst, 
                {f.belt, fd.inp, fd.move().back().out},
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
        bool do_block = hashmap_get(&inst->params, Sat::fpar_do_block);
        
        // TODO do not fix sum for each field, just carry it around
        
        // Lines are a vector in {0, ..., n_linelen}^n_linedim where each component is encoded in
        // unary. Additionally, there is line_sum, which contains the sum of all lines, and
        // line_block, which contains blocking information. The latter may be -1, in which case we
        // do not deal with blocking.

        auto lines_all = sat_expand(inst, {f.lines_all}, &inst->rewrite_temp);
        for (u64 line_var: lines_all) {
            s64 rewrite_temp_size = inst->rewrite_temp.size;
            defer { inst->rewrite_temp.size = rewrite_temp_size; };

            auto line = sat_expand(inst, {f.lines_all}, &inst->rewrite_temp);
            
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

        if (do_block) {
            // Total count determined also determined by blocking info
            sat_add(inst, line_at_most, {f.line_sum, f.line_block});
        }

        // Line control for splitters
        for (Dir fd: f.dirs) {
            Field f0  = f;
            Field f1  = fd.move(1, 0);
            Field fo0 = fd.move(0, 1);
            Field fo1 = fd.move(1, 1);
            Dir fdr  = fd.turnr();
            Dir fdro = fd.move().turnr();

            // Determine whether the sum of inputs equals ...
            u64 eq_0    = sat_temp_create(inst, 1);
            u64 eq_1    = sat_temp_create(inst, 1);
            u64 eq_both = sat_temp_create(inst, 1);

            // ... the first output
            sat_push_add_pop(inst, {eq_0}, lines_equal, {fdr.sum_lines_all, fo0.lines_all});
            // ... the second output
            sat_push_add_pop(inst, {eq_1}, lines_equal, {fdr.sum_lines_all, fo1.lines_all});

            if (do_block) {
                // ... the sum of both outputs
                sat_push_add_pop(inst, {eq_both}, lines_equal, {fdr.sum_lines_all, fdro.sum_lines_all});
            } else {
                // Or whether each output is the average of the inputs
                sat_push_add_pop(inst, {eq_both}, lines_equal_half, {fo0.lines_all, fdr.sum_lines_all});
                sat_push_add_pop(inst, {eq_both}, lines_equal_half, {fo1.lines_all, fdr.sum_lines_all});
            }
            
            // If the other side would be out-of-bounds, no need to emit anything
            if (not f1.inbounds(inst)) continue;
            
            sat_push(inst, {f0.splitl, fd.out});

            if (do_block) {
                // Check that both inputs block at the same rate
                sat_add(inst, lines_equal, {f0.line_block, f1.line_block});

                // TODO Check that the outputs are either equal, or blocked
            }

            // Assert eq_0, eq_1 or eq_both depending on the combination of outputs that are occupied
            sat_addg(inst, implies, { fd.move().back().inp, ~fd.move(1, 1).back().inp}, {eq_0});
            sat_addg(inst, implies, {~fd.move().back().inp,  fd.move(1, 1).back().inp}, {eq_1});
            sat_addg(inst, implies, { fd.move().back().inp,  fd.move(1, 1).back().inp}, {eq_both});

            
            sat_pop(inst);
        }
    } break;

    case field_border_unary: {
        Field f {(s64)args[0], (s64)args[1]};
        u8 border_type = args[2];
        bool do_block = hashmap_get(&inst->params, Sat::fpar_do_block);
        s64 nx        = hashmap_get(&inst->params, Sat::fpar_nx);
        s64 ny        = hashmap_get(&inst->params, Sat::fpar_ny);
        s64 s_input   = hashmap_get(&inst->params, Sat::fpar_s_input);
        s64 s_output  = hashmap_get(&inst->params, Sat::fpar_s_output);
        
        for (Dir fd: f.dirs) {
            if (not Field {fd.move()}.inbounds(inst)) continue;

            if (border_type == Field::BORDER_EMPTY) {
                // Empty border tiles have neither input nor output
                assert(args.size == 3);
                sat_add(inst, logical_and, {~fd.out, ~fd.inp});
                
            } else if (border_type == Field::BORDER_OUT) {
                // Output border tiles have their items concentrated in a single line
                assert(args.size == 4);
                sat_add(inst, logical_and, {fd.out, ~fd.inp});
                s64 index = args[3];
                Array_t<u64> lines_item = sat_expand(inst, f.lines_item, &inst->rewrite_temp);
                for (u64 line: lines_item) {
                    sat_add(inst, line_equal_const, {line, line == lines_item[index] ? s_output : 0ull});
                }
                sat_add(inst, line_equal_const, {f.line_sum, (u64)s_output});
                
            } else if (border_type == Field::BORDER_INP) {
                // Input border lines have items spread out
                assert(args.size == 3);
                sat_add(inst, logical_and, {~fd.out, fd.inp});
                Array_t<u64> lines = sat_expand(inst, f.lines_item, &inst->rewrite_temp);
                for (u64 line: lines) {
                    sat_add(inst, line_equal_const, {line, (u64)s_input});
                }
                sat_add(inst, line_equal_const, {f.line_sum, (u64)(s_input * lines.size)});
                sat_add(inst, line_equal, {f.line_block, line_full});
            }
        }

    } break;
        
    default:
        assert(false);
        
    }

}

bool factorio_explain(Sat_instance* inst, u64 id, Array_t<u64> args, Array_dyn<u8>* into) {
    using namespace Sat;
    
    u64 type, subtype, suffix;
    sat_decompose(id, &type, &subtype, &suffix);
    
    if (type == VAR or type == GROUP) {
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
            if (id == Sat::line_empty) {
                array_printf(into, "0_line");
            } else if (id == Sat::line_full) {
                array_printf(into, "1_line");
            } else {
                assert(has_coords);
                assert(val == 0);
                
                array_printf(into, "(%lld,%lld).lines%s[", x, y, mod_suf);
                if (val2 == 0xfe) {
                    array_printf(into, "sum");
                } else if (val2 == 0xff) {
                    array_printf(into, "block");
                } else {
                    array_printf(into, "%lld", val2);
                }
                array_printf(into, "]");
            }
        } else if (fvar_type == Field::FVAR_LINEGROUP) {
            if (id == Sat::lines_empty) {
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
            array_printf(into, "(%lld,%lld).lines%s[%lld][%lld]", x, y, mod_suf, val, val2);
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
    } else if (type == CONSTRAINT) {
        assert(subtype == CONSTRAINT_FACTORIO);
        char const* names[] = {
            nullptr, "field_basic", "field_underground", "field_border", "field_unary",
            "field_border_unary", "line_at_most", "line_equal_half", "line_equal_const",
            "line_equal", "lines_equal", "lines_equal_half"
        };
        s64 names_size = sizeof(names) / sizeof(names[0]);
        u64 argtype[] = {
            0, 11, 11, 11, 11,
            1131, 22, 22, 21,
            22, 22, 22
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

    for (s64 i = 0; i < params.size; ++i) {
        hashmap_set(&inst->params, PARAM_FACTORIO_BEGIN + i, params[i]);
    }

    s64 nx = hashmap_get(&inst->params, Sat::fpar_nx);
    s64 ny = hashmap_get(&inst->params, Sat::fpar_ny);

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
        sat_add(inst, field_border_unary, {(u64)-1, (u64)y, Field::BORDER_INP});        
    }
}

struct Factorio_params {
    s64 nx, ny, n_under, scale_fac;
    bool do_blocking;
    Array_t<s64> yoff_output, yoff_input;
};

void factorio_balancer(Sat_instance* inst, Factorio_params params) {
    using namespace Sat;
    Array_t<s64> arr = array_create<s64>(PARAM_FACTORIO_END - PARAM_FACTORIO_BEGIN);
    s64 o = PARAM_FACTORIO_BEGIN;
    
    arr[fpar_nx - o] = params.nx;
    arr[fpar_ny - o] = params.ny;
    arr[fpar_n_under   - o] = params.n_under;
    arr[fpar_n_linedim - o] = params.yoff_output.size;
    arr[fpar_n_linelen - o] = params.yoff_input.size * params.scale_fac;
    arr[fpar_n_lines   - o] = arr[fpar_n_linedim - o] + 1 + params.do_blocking;
    arr[fpar_s_input   - o] = params.scale_fac;
    arr[fpar_s_output  - o] = arr[fpar_n_linelen - o];

    sat_register_expand_func(inst, GROUP_FACTORIO, &factorio_expand);
    sat_register_rewrite_func(inst, CONSTRAINT_FACTORIO, &factorio_rewrite);
    for (u64 i: {VAR_FACTORIO, GROUP_FACTORIO, CONSTRAINT_FACTORIO}) {
        sat_register_explain_func(inst, i, &factorio_explain);
    }

    factorio_add_fields(inst, arr, params.yoff_output, params.yoff_input);
}

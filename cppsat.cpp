

struct Sat_instance {
    Array_dyn<s64> clause_offsets;
    Array_dyn<u64> clause_literals;
    
    Array_dyn<s64> context_stack_offsets;
    Array_dyn<u64> context_stack_literals;

    Array_dyn<u64> context_temp;
};

struct Clause_accumulator {
    Array_dyn<s64>* clause_offsets;
    Array_dyn<u64>* clause_literals;
    Array_t<u64> context_literals; // These will be added to each clause
};

namespace sat {

Clause_accumulator default_accumulator(Sat_instance* inst) {
    return {&inst->clause_offsets, &inst->clause_literals, {}};
}

Clause_accumulator condition(Sat_instance* inst, Array_t<u64> lits) {
    inst->context_temp.size = 0;
    array_append(&inst->context_temp, lits);
    return {&inst->clause_offsets, &inst->clause_literals, inst->context_temp};
}

void add_clause(Clause_accumulator acc, Array_t<u64> lits) {
    array_append(acc.clause_literals, acc.context_literals);
    array_append(acc.clause_literals, lits);
    array_push_back(acc.clause_offsets, acc.clause_literals->size);
}

void add_at_most_one(Clause_accumulator acc, Array_t<u64> lits) {
    for (s64 i = 0; i < lits.size; ++i) {
        for (s64 j = i+1; j < lits.size; ++i) {
            add_clause(acc, {~lits[i], ~lits[j]});
        }
    }
}

void add_exactly_one(Clause_accumulator acc, Array_t<u64> lits) {
    add_clause(acc, lits);
    add_at_most_one(acc, lits);
}

void add_implies(Clause_accumulator acc, Array_t<u64> if_all, Array_t<u64> then_any) {
    array_append(acc.clause_literals, acc.context_literals);
    for (u64 lit: if_all) {
        array_push_back(acc.clause_literals, ~lit);
    }
    array_append(acc.clause_literals, then_any);
    array_push_back(acc.clause_offsets, acc.clause_literals->size);    
}

} // end of namespace sat

void factorio_instance_init(Factorio_instance* inst) {
    using namespace sat;

    for (Field f: inst->fields) {
        // *** Basics *** /

        // If splitter, either left or right, ...
        condition(inst, {f.split}); exactly_one(factorio->temp, {f.splitl, f.splitr});
        
        inst->push_add_pop({condition, f.split},  {exactly_one,  f.splitl,  f.splitr});
        // ... else neither.
        inst->push_add_pop({condition, ~f.split}, {logical_and, ~f.splitl, ~f.splitr});
        
        // If underground, exactly one direction
        inst->push_add_pop({condition,  f.under}, {exactly_one,  f.underh,  f.underv});
        // ... else none.
        inst->push_add_pop({condition, ~f.under}, {logical_and, ~f.underh, ~f.underv});

        for (Dir fd: f.dirs()) {
            // Input and output not on same side
            inst->add({at_most_one, fd.inp, fd.out});

            // Sideflags
            inst->add({implies, f.belt,  ~fd.inp, ~fd.out, fd.sid});
            inst->add({implies, f.under, fd.turnl().inp, fd.sid});
            inst->add({implies, f.under, fd.turnr().inp, fd.sid});
            inst->add({implies, f.under, fd.turnl().out, fd.sid});
            inst->add({implies, f.under, fd.turnr().out, fd.sid});

            // Do not output onto side
            inst->add({implies, fd.out, ~fd.move().back().sid});

            // Splitter flag causes straight flow
            inst->add({implies, f.split, fd.inp, fd.back().out});
            inst->add({implies, f.split, fd.out, fd.back().inp});

            // If outputting in this direction ...
            inst->push({condition, fd.out}); {
            
                // ... splitter flags for left and right have to match, and
                inst->add({equivalent, f.splitl, fd.turnr().move().field().splitr});
                // ... splitter directions have to match for both sides, and
                inst->add({equivalent, f.splitl, fd.turnr().move().turnl().out});
                
                // ... a splitter must have at least one input and at least one output connected
                inst->add({clause, ~f.splitl, fd.move().back().inp, fd.move(1, 1).back().inp});
                inst->add({clause, ~f.splitl, fd.move(0, -1).out, fd.move(1, -1).out});

                // ... there cannot be two subsequent splitters
                inst->add({implies, f.splitl, ~fd.move().field().splitl});

            }; inst->pop();

            // Non-splitters must have valid input and output
            inst->push_one({condition, ~f.split});
            inst->add({equivalent, fd.inp, fd.move().back().out});

            // For underground lines, input/output direction determines underground direction
            inst->add({implies, f.under, fd.inp, fd.und});
            inst->add({implies, f.under, fd.out, fd.und});
        }
        
        // At most one input and output
        inst->add({at_most_one, f.dir_all.inp});
        inst->add({at_most_one, f.dir_all.out});

        // Belts and Splitters must have at least one input and at least one output
        inst->add({implies, ~f.empty, ~f.under, f.dir_all.inp});
        inst->add({implies, ~f.empty, ~f.under, f.dir_all.out});

        // Underground belts have exactly one input or output
        inst->push_add_pop({condition, f.under}, {exactly_one, f.dir_all.inp, f.dir_all.out});

        // Empty lines have neither input nor output
        inst->push_add_pop({condition, f.empty}, {logical_and, ~f.dir_all.inp, ~f.dir_all.out});
        inst->add({implies, ~f.dir_all.inp, ~f.dir_all.out, f.empty});

        
        // *** Line control ***

        // Empty tiles have empty lines
        inst->push({condition, f.empty});
        inst->add({line_equal, f.line_all, line_empty});
        inst->pop();

        for (Dir fd: f.dirs()) {
            // Input determines the line, except from splitter
            inst->push_add_pop(
                {condition, fd.inp, fd.move().back().out, ~fd.move().field().split},
                {line_equal, f.line_all, fd.move().field().line_all}
            );
            // No matching output means empty line
            inst->push_add_pop(
                {condition, fd.inp, ~fd.move().back().out},
                {line_equal, f.line_all, line_empty}
            );
        }

        // Handle outgoing underground (incoming handled via standard input handling above)
        for (Dir fd: f.dirs()) {
            // Search for matching underground
            for (s64 w = 2; w < inst->n_under+2; ++w) {
                // w is the width of the whole underground segment, so move w-1 tiles
                Dir fd2 = fd.move(0, w-1);
                Field f2 = fd2.field();

                // Condition on
                // (a) f being underground in direction fd
                // (b) traversed tiles not having underground with the same orientation
                inst->curgroup_clear();
                for (s64 i = 1; i < w-1; ++i) inst->curgroup_push(fd.move(0, i).und);
                inst->push({condition, f.under, fd.back().out, ~curgroup});

                bool dobreak = false;
                if (inst->is_inbounds(f2) and w < inst->n_under+1) {
                    // A connection can exist here, so we also condition on
                    // (c) target tile (i.e. f2) being underground with same orientation
                    inst->push({condition, fd2.under});

                    // There must be a connection (no useless tiles)
                    inst->add({clause, fd2.in});

                    // The lines are now connected
                    inst->add({line_equal, f.line_all, f2.line_all});
                    
                    inst->pop();
                } else if (inst->is_inbounds(f2) and w == 2) {
                    // @Redundant Length 2 would be possible, but not sensible, forbid it 
                    inst->add({clause});
                } else {
                    // This connection is not possible, forbid it
                    inst->add({clause});

                    dobreak = true;
                }
                
                inst->pop();
                if (dobreak) break;
            }

            // @Redundant Forbid unused space after underground
            inst->add({clause, ~f.under, ~fd.back().out, ~fd.move().field.empty,
                       fd.move(-1, 1).turnr().out, fd.move(1, 1).turnl().out});
        }
    }

    // *** Border conditions ***
    for (Field f: inst->borders()) {
        // No splitters, no underground
        inst->add({logical_and, ~f.split, ~f.splitl, ~f.splitr, ~f.under, ~f.underh, ~f.underv});

        // Line control for normal belts
        for (Dir fd: f.dirs()) {
            if (not inst->is_inbounds(fd.move().field())) continue;

            inst->push_add_pop(
                {condition, f.belt, fd.inp, fd.move().back().out},
                {line_equal, f.line_all, fd.move().field().line_all}
            );
            inst->push_add_pop(
                {condition, f.belt, fd.inp, ~fd.move().back().out},
                {line_equal, f.line_all, line_empty}
            );
        }
    }
}

void factorio_instance_init(Factorio_instance* inst, s64 nx, s64 ny, s64 n_lines) {


struct Sat_solver_state {
    enum State: u8 {
        INVALID, RUNNING, SAT, UNSAT
    };
    
    u8 state = INVALID;
    
    Sat_dimacs dimacs;
    pid_t solver_pid;
    int output_fd;
    
    Array_dyn<u8> output_data;
    Array_dyn<s64> output_lines;
    u8 output_state = UNSAT;
    Sat_solution sol;
};

void sat_solver_init(Sat_solver_state* solver, Sat_instance* inst) {
    solver->state = Sat_solver_state::INVALID;
    solver->output_data.size = 0;
    solver->output_lines.size = 0;
    array_push_back(&solver->output_lines, 0);

    sat_write_dimacs(inst, &solver->dimacs);
    sat_solution_init(&solver->sol);

    FILE* f = fopen("dimacs.out", "wb");
    fwrite(solver->dimacs.text.data, 1, solver->dimacs.text.size, f);
    
    pid_t parent_pid = getpid();
    
    int pipe_solver_to  [2] = {};
    int pipe_solver_from[2] = {};
    if (pipe(pipe_solver_to  )) goto error;
    if (pipe(pipe_solver_from)) goto error;
    
    {pid_t pid = fork();
    if (pid == 0) {
        // Die when the parent dies (the getppid() check avoids a race condition)
        if (prctl(PR_SET_PDEATHSIG, SIGTERM) == -1) goto error;
        if (getppid() != parent_pid) goto error;
        
        if (dup2(pipe_solver_to[0],   STDIN_FILENO)  == -1) goto error;
        if (dup2(pipe_solver_from[1], STDOUT_FILENO) == -1) goto error;
        if (dup2(pipe_solver_from[1], STDERR_FILENO) == -1) goto error;
        if (platform_close_try(pipe_solver_to[1])) goto error;
        if (platform_close_try(pipe_solver_from[0])) goto error;
        
        {char* argv[] = {"kissat", nullptr};
        execvp("kissat", argv);
        platform_error_printf("$ while trying to execute solver in child");
        platform_error_print();
        exit(12);}
    } else if (pid != -1) {
        solver->solver_pid = pid;
        solver->output_fd = pipe_solver_from[0];
        
        if (platform_read_all_try(solver->output_fd, &solver->output_data)) goto error2;
        if (platform_write_try(pipe_solver_to[1], solver->dimacs.text)) goto error2;
        if (platform_close_try(pipe_solver_to[1])) goto error2;
        if (platform_close_try(pipe_solver_from[1])) goto error2;
        
        solver->state = Sat_solver_state::RUNNING;
    } else {
        goto error3;
    }}

    return;

  error:
    platform_error_printf("$ while initialising child");
    platform_error_print();
    exit(8);
  error2:
    platform_error_printf("$ while talking to the child");
    platform_error_print();
    exit(9);
  error3:
    platform_error_printf("$ while trying to fork");
    platform_error_print();
    exit(15);

}

void sat_solver_process(Sat_solver_state* solver, pollfd pfd) {
    if (solver->state != Sat_solver_state::RUNNING) return;

    bool dirty = false;
    if (pfd.revents & POLLIN) {
        if (platform_read_all_try(solver->output_fd, &solver->output_data)) goto error;
        dirty = true;
    }
    if (pfd.revents & POLLHUP) {
        if (platform_close_try(solver->output_fd)) goto error;
        if (solver->output_data.size and solver->output_data.back() != '\n') {
            array_push_back(&solver->output_data, '\n');
        }
        solver->output_fd = -1;
        dirty = true;
    }
    if (pfd.revents & POLLERR) {
        fprintf(stderr, "Warning: polling returned POLLERR, I do not know how to handle this\n");
    }

    if (dirty) {
        s64 lines_prev = solver->output_lines.size - 1;
        for (s64 i = solver->output_lines.back(); i < solver->output_data.size; ++i) {
            if (solver->output_data[i] == '\n') {
                array_push_back(&solver->output_lines, i+1);
            }
        }

        s64 del = -1, del_count = 0;
        for (s64 i = lines_prev; i+1 < solver->output_lines.size; ++i) {
            auto line = array_subindex(solver->output_lines, solver->output_data, i);
            if (line[0] == 'v') {
                if (del + del_count == i) {
                    ++del_count;
                } else if (del_count == 0) {
                    del = i;
                    del_count = 1;
                } else {
                    assert(false);
                }
                
                s64 val = 0;
                bool isneg = false;
                for (s64 j = 2; j < line.size; ++j) {
                    u8 c = line[j];
                    if (c == '-') {
                        isneg ^= true;
                    } else if ('0' <= c and c <= '9') {
                        val = 10 * val + (c - '0');
                    } else if (c == ' ' or c == '\n') {
                        if (val) {
                            u64 var = solver->dimacs.map_back[val];
                            solver->sol.set(isneg ? ~var : var);
                            val = 0; isneg = false;
                        }
                    }
                }
            } else if (line[0] == 's') {
                if (2 < line.size and line[2] == 'S') {
                    solver->output_state = Sat_solver_state::SAT;
                } else if (2 < line.size and line[2] == 'U') {
                    solver->output_state = Sat_solver_state::UNSAT;
                }
            }
        }

        if (del_count) {
            s64 i = solver->output_lines[del];
            s64 j = solver->output_lines[del+del_count];
            s64 size = solver->output_data.size - j;
            if (size) {
                memmove(&solver->output_data[i], &solver->output_data[j], size);
            }
            solver->output_data.size = i + size;

            s64 size_lines = solver->output_lines.size - (del+1 + del_count);
            if (size_lines) {
                memmove(&solver->output_lines[del+1], &solver->output_lines[del+1 + del_count],
                    size_lines * sizeof(solver->output_lines[0]));
            }
            solver->output_lines.size -= del_count;
            for (s64& off: array_subarray(solver->output_lines, del+1)) {
                off -= j - i;
            }
        }
    }

    if (solver->output_fd == -1) {
        solver->state = solver->output_state;
    }
    
    return;
  error:
    platform_error_print();
    exit(11);
}

void sat_solver_stop(Sat_solver_state* solver) {
    if (solver->output_fd != -1) {
        if (platform_close_try(solver->output_fd)) {
            platform_error_print();
            exit(11);
        }
        solver->output_fd = -1;
    }
}


#include <stdio.h>
#include <time.h>

#include "global.hpp"


#define HASHMAP_CUM_PROBES_SIZE 128
enum Counter_types: u64 {
    HASHMAP0_CUM_PROBES,
    HASHMAP0_CUM_PROBES_MAX = HASHMAP0_CUM_PROBES + HASHMAP_CUM_PROBES_SIZE-1,
    HASHMAP1_CUM_PROBES,
    HASHMAP1_CUM_PROBES_MAX = HASHMAP1_CUM_PROBES + HASHMAP_CUM_PROBES_SIZE-1,
    HASHMAP0_QUERIES,
    HASHMAP1_QUERIES,
    HASHMAP0_FOUND,
    HASHMAP1_FOUND,
    HASHMAP1_SLOWPATH,
    COUNTER_SIZE
};
u64 global_counters[COUNTER_SIZE];

#include "hashmap.cpp"

struct timespec global_start_tp;
u64 global_last;
u64 _global_last_temp;
u64 now() {
    struct timespec tp;
    clock_gettime(CLOCK_MONOTONIC_RAW, &tp);
    u64 val = (tp.tv_sec - global_start_tp.tv_sec) * 1000000000ull + tp.tv_nsec - global_start_tp.tv_nsec;
    global_last = _global_last_temp;
    _global_last_temp = val;
    return val;
}




int main(int argc, char** argv) {
    bool only0 = false;
    bool only1 = false;
    if (argc >= 2 and strcmp(argv[1], "only0") == 0) {
        only0 = true;
    } else if (argc >= 2 and strcmp(argv[1], "only1") == 0) {
        only1 = true;
    } else if (argc >= 2) {
        fprintf(stderr, "Error: Unrecognised argument\n");
        return 1;
    }
    
    clock_gettime(CLOCK_MONOTONIC_RAW, &global_start_tp);
    
    FILE* f = fopen("lookups.out", "r");
    Array_t<u64> lookups;
    assert(fread(&lookups.size, sizeof(lookups.size), 1, f) > 0);
    lookups = array_create<u64>(lookups.size);
    assert(fread(lookups.data, sizeof(*lookups.data), lookups.size, f) > 0);
    fclose(f);

    const int mul = 1;

    printf(u8"%8lldµs loaded data, length %lld\n", (now() - global_last) / 1000, lookups.size);

    u64 hash = 0;
    for (u64 i: lookups) {
        hash += hash_u64(i);
    }
    printf(u8"%8lldµs hashed data\n", (now() - global_last) / 1000);

    Hashmap<u32> hashmap1;
    for (s64 j = 0; j < mul and not only1; ++j) {
        for (u64 i: lookups) {
            hash += not hashmap_getcreate(&hashmap1, i);
        }
    }
    printf(u8"%8lldµs Hashmap<u32>\n", (now() - global_last) / 1000);
    
    Hashmap_u32 hashmap2;
    for (s64 j = 0; j < mul and not only0; ++j) {
        for (u64 i: lookups) {
            hash += not hashmap_getcreate(&hashmap2, i);
        }
    }
    printf(u8"%8lldµs Hashmap_u32\n", (now() - global_last) / 1000);
    float f0 = 1.f / max(global_counters[HASHMAP0_QUERIES], 1ull);
    float f1 = 1.f / max(global_counters[HASHMAP1_QUERIES], 1ull);

    printf("\nQueries   %8lld %8lld  %6.4f %6.4f\n",
        global_counters[HASHMAP0_QUERIES],
        global_counters[HASHMAP1_QUERIES],
        global_counters[HASHMAP0_QUERIES] * f0,
        global_counters[HASHMAP1_QUERIES] * f1
    );
    printf("Size      %8lld %8lld\n", hashmap1.size, hashmap2.size);
    printf("Found     %8lld %8lld  %6.4f %6.4f\n",
        global_counters[HASHMAP0_FOUND],
        global_counters[HASHMAP1_FOUND],
        global_counters[HASHMAP0_FOUND] * f0,
        global_counters[HASHMAP1_FOUND] * f1
    );
    printf("Slowpath  %8s %8lld  %6s %6.4f\n",
        "-",
        global_counters[HASHMAP1_SLOWPATH],
        "-",
        global_counters[HASHMAP1_SLOWPATH] * f1
    );
    printf("Probes:\n");
    for (s64 i = 0; i < HASHMAP_CUM_PROBES_SIZE and i < 20; ++i) {
        printf("  %3lld  %8lld %8lld  %6.4f %6.4f\n", i,
            global_counters[HASHMAP0_CUM_PROBES + i],
            global_counters[HASHMAP1_CUM_PROBES + i],
            global_counters[HASHMAP0_CUM_PROBES + i] * f0,
            global_counters[HASHMAP1_CUM_PROBES + i] * f1
        );
    }
    
    return hash != 29837482;
}

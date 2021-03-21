
#ifdef HASHMAP_TEST
#include <unistd.h>
#include "global.hpp"
#endif

#ifdef HASHMAP_TEST
#define assert_nofuzz(x) (void)(x)
#else
#define assert_nofuzz(x) assert((x))
#endif

template <typename T>
struct Hashmap {
    static constexpr u64 _SLOT_FIND_MASK = 1ull << 63;
    struct Slot {
        u64 key;
        T val;
    };

    u64 empty = 0;
    Array_t<Slot> slots;
    s64 size = 0;

#ifdef HASHMAP_TEST
    Array_dyn<Slot> check;
#endif
};

s64 _hashmap_slot_base(u64 map_slots_size, u64 key) {
#ifndef HASHMAP_TEST
	u64 x = key + 0x9e3779b97f4a7c15;
	x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9;
	x = (x ^ (x >> 27)) * 0x94d049bb133111eb;
	x =  x ^ (x >> 31);
    return x & (map_slots_size-1);
#else
    return key & (map_slots_size-1); // no hashing for the fuzzer
#endif
}

template <typename T>
s64 _hashmap_slot_find(Hashmap<T>* map, u64 key, bool* out_empty) {
    assert(key != map->empty);
    assert(out_empty);
    s64 slot_base = _hashmap_slot_base(map->slots.size, key);
    for (s64 i = 0; i < map->slots.size; ++i) {
        s64 slot = (slot_base + i) & (map->slots.size-1);
        s64 slot_key = map->slots.data[slot].key;
        if (slot_key == key) {
            *out_empty = false;
            return slot;
        } else if (slot_key == map->empty) {
            *out_empty = true;
            return slot;
        }
    }
    
    assert(map->size == 0); // map too full
    *out_empty = true;
    return -1;
}

template <typename T>
void _hashmap_enlarge(Hashmap<T>* map) {
    if (map->slots.size == 0) {
        // Initialise to size 16
        map->slots = array_create<typename Hashmap<T>::Slot>(16);
        assert(map->size == 0);
    } else {
        array_resize(&map->slots, map->slots.size * 2);
        
        //for (s64 i = 0; i < map->slots.size/2; ++i) {
        //    printf("%02x ", (u8)map->slots[i].key);
        //} puts("");
        
        for (s64 i = 0; i < map->slots.size; ++i) {
            typename Hashmap<T>::Slot slot = map->slots[i];
            if (slot.key == map->empty) {
                if (i >= map->slots.size/2) break;
                continue;
            }
            map->slots[i].key = map->empty;
            bool index_empty;
            s64 index = _hashmap_slot_find(map, slot.key, &index_empty);
            assert(index_empty);
            map->slots[index] = slot;
        }
        

    }
}

#ifdef HASHMAP_TEST
template <typename T>
void hashmap_selfcheck(Hashmap<T>* map) {}

template <>
void hashmap_selfcheck(Hashmap<u64>* map) {
    bool seen[256] = {};
    for (s64 i = 0; i < map->slots.size; ++i) {
        u64 key = map->slots[i].key;
        if (key <= 0 or key >= 256) continue;
        assert(not seen[key]);
        seen[key] = true;
    }
}
#endif

template <typename T, typename V>
void hashmap_set(Hashmap<T>* map, u64 key, V val_) {
    assert_nofuzz(key != map->empty);
    if (key == map->empty) return;

    T val = val_;
    if (map->size * 5 >= map->slots.size * 3) _hashmap_enlarge(map);
    bool slot_empty;
    s64 slot = _hashmap_slot_find(map, key, &slot_empty);
    map->slots[slot].key = key;
    map->slots[slot].val = val;
    map->size += slot_empty;
    
#ifdef HASHMAP_TEST
    bool flag = false;
    for (typename Hashmap<T>::Slot& i: map->check) {
        if (i.key == key) { i.val = val; flag = true; break; }
    }
    if (not flag) array_push_back(&map->check, {key, val});
#endif
}

template <typename T>
T* hashmap_getptr(Hashmap<T>* map, u64 key) {
    assert_nofuzz(key != map->empty);
    if (key == map->empty) return nullptr;

    bool slot_empty;
    s64 slot = _hashmap_slot_find(map, key, &slot_empty);
    if (slot_empty) return nullptr;

#ifdef HASHMAP_TEST
    for (typename Hashmap<T>::Slot i: map->check) {
        if (i.key == key) assert(i.val == map->slots[slot].val);
    }
#endif
    return &map->slots.data[slot].val;
}

template <typename T>
T hashmap_get(Hashmap<T>* map, u64 key) {
    T* ptr = hashmap_getptr(map, key);
    assert(ptr);
    return *ptr;
}

template <typename T>
T* hashmap_getcreate(Hashmap<T>* map, u64 key, T init={}) {
    assert_nofuzz(key != map->empty);
    if (key == map->empty) return nullptr;
    if (map->size * 5 >= map->slots.size * 3) _hashmap_enlarge(map);

    bool slot_empty;
    s64 slot = _hashmap_slot_find(map, key, &slot_empty);
    if (slot_empty) {
        map->slots[slot].key = key;
        map->slots[slot].val = init;
        ++map->size;
    }
    
    return &map->slots[slot].val;
}

template <typename T>
T hashmap_delete(Hashmap<T>* map, u64 key) {
    assert_nofuzz(key != map->empty);
    if (key == map->empty) return {};
    
    bool slot_empty;
    s64 slot = _hashmap_slot_find(map, key, &slot_empty);
    assert_nofuzz(not slot_empty);
    if (slot_empty) return {};
    T result = map->slots[slot].val;

    s64 empty = slot;
    for (s64 i = 1; i < map->slots.size; ++i) {
        s64 slot_i = (slot + i) & (map->slots.size-1);
        s64 key_i = map->slots[slot_i].key;
        if (key_i == map->empty) break;
        s64 slot_base = _hashmap_slot_base(map->slots.size, key_i) & (map->slots.size-1);
        if (((empty - slot_base) & (map->slots.size-1)) <= ((slot_i - slot_base) & (map->slots.size-1))) {
            map->slots[empty] = map->slots[slot_i];
            empty = slot_i;
        }
    }
    map->slots[empty].key = map->empty;
    --map->size;

#ifdef HASHMAP_TEST
    for (s64 i = 0; i < map->check.size; ++i) {
        if (map->check[i].key == key) {
            assert(map->check[i].val == result);
            map->check[i] = map->check[map->check.size-1];
            --map->check.size;
        }
    }
#endif
    
    return result;
}

template <typename T>
void hashmap_clear(Hashmap<T>* map) {
    for (s64 i = 0; i < map->slots.size; ++i) {
        map->slots[i].key = map->empty;
    }
    map->size = 0;
}

template <typename T>
void hashmap_free(Hashmap<T>* map) {
    array_free(&map->slots);
    map->size = 0;
}



static inline u64 _hash_rotate_left(u64 x, int k) {
	return (x << k) | (x >> (64 - k));
}

u64 hash_u64(u64 x) {
    // see http://mostlymangling.blogspot.com/2020/01/nasam-not-another-strange-acronym-mixer.html
    x ^= _hash_rotate_left(x, 39) ^ _hash_rotate_left(x, 17);
    x *= 0x9e6c63d0676a9a99ull;
    x ^= (x >> 23) ^ (x >> 51);
    x *= 0x9e6d62d06f6a9a9bull;
    return x ^ (x >> 23) ^ (x >> 51);
} 

u64 hash_u64_pair(u64 a, u64 b) {
    return hash_u64(hash_u64(a) ^ b);
}

u64 hash_str(Array_t<u8> str) {
    u64 x = 0xffdf38dd3e69bd91ull ^ str.size;
    s64 i = 0;
    for (; i+8 <= str.size; i += 8) {
        u64 a = *(u64*)(str.data + i);
        x = hash_u64(a ^ x);
    }
    for (s64 j = i; j < str.size; ++j) {
        x ^= str.data[j] << 8*j;
    }
    return hash_u64(x);
}

u64 hash_arr(Array_t<u64> arr) {
    u64 x = 0xefdf38dd3e69bd91ull ^ arr.size;
    for (u64 a: arr) x = hash_u64(a ^ x);
    return x;
}


#if 0
struct Hashmap_u32 {
    struct Slot {
        u64 hashes[2]; // slots: 0 -> 02468a, 1 -> 13579b
        u32 values[12];
    };
    struct Slot_keys {
        u64 keys[12];
    };

    Array_t<Slot> slots;
    Array_t<Slot_keys> slot_keys;
    s64 size = 0;
};

u64 _hashmap_slot_find(Hashmap_u32* map, u64 key) {
#ifndef HASHMAP_TEST    
    u64 hash = hash_u64(key);
#else
    u64 hash = key;
#endif
    
    u64 subhash = hash & 0x3ff;
    s64 map_slots_size = map->slots.size;
    if (map_slots_size == 0) return -1;
    s64 slot_it = (hash >> 10) ;
    if (subhash == 0) subhash = hash >> 54;
    subhash += not subhash;

    for (;; ++slot_it) {
        Slot* slot = &map->slots[slot_it & (map_slots_size-1)];

        u64 subhash_bc = 0x4010040100401ull * subhash;
        u64 slot_hashes_0 = slot->hashes[0];
        u64 cmp0 = slot_hashes_0   ^ subhash_bc;
        u64 cmp1 = slot->hashes[1] ^ subhash_bc;
        cmp0 &= cmp0 >> 5;
        cmp1 &= cmp1 >> 5;
        cmp0 &= 0x7c1f07c1f07c1full;
        cmp1 &= 0x7c1f07c1f07c1full;
        cmp0 += 0x4010040100401ull;
        cmp1 += 0x4010040100401ull;
        cmp0 &= 0x80200802008020ull;
        cmp1 &= 0x80200802008020ull;
        u64 eq = cmp0 >> 1 | cmp1;
        s64 count = __builtin_popcountll(eq);

        // fast-path
        if (count == 1) {
            s64 index = __builtin_ctzll(eq) / 5;
            if (map->slots_keys[slot_it].keys[index] == key) {
                return slot_it << 4 | index;
            } 
        } else if (count > 1) {
            for (s64 i = 0; i < count; ++i) {
                s64 index = __builtin_ctzll(eq) / 5;
                if (map->slots_keys[slot_it].keys[index] == key) {
                    return slot_it << 4 | index;
                }
                eq >>= 5;
            }
        }

        s64 filled = slot_hashes_0 >> 60;
        if (filled < 12) {
            return slot_it << 4 | filled | (1ull << 63);
        }
    }

    assert(false);
    return -1;
}
#endif

#ifdef HASHMAP_TEST
int main(int argc, char** argv) {
    if (argc > 1) {
        Hashmap<u64> map;
        char buf[4096] = {};
        int len = read(STDIN_FILENO, buf, sizeof(buf));
        for (int i = 0; i < len; i += 2) {
#ifdef HASHMAP_VERBOSE
            if      (buf[i] % 2 == 0) puts("--------------- set");
            else if (buf[i] % 4 == 1) puts("--------------- delete");
            else                      puts("--------------- getptr");
            for (s64 i = 0; i < map.slots.size; ++i) {
                printf("%02x,%02x ", (u8)map.slots[i].key, (u8)map.slots[i].val);
            } puts("");
#endif
            if (buf[i] % 2 == 0) {
                hashmap_set(&map, buf[i+1], buf[i]);
            } else if (buf[i] % 4 == 1) {
                hashmap_delete(&map, buf[i+1]);
            } else {
                hashmap_getptr(&map, buf[i+1]);
            }
#ifdef HASHMAP_VERBOSE
            for (s64 i = 0; i < map.slots.size; ++i) {
                printf("%02x,%02x ", (u8)map.slots[i].key, (u8)map.slots[i].val);
            } puts("");
#endif
            hashmap_selfcheck(&map);
        }
    } else {
        Hashmap<u64> map;

        for (s64 i = 1; i < 123; ++i) hashmap_set<u64>(&map, i, i*i*i);
        for (s64 i = 1; i < 123; ++i) {
            assert(hashmap_get(&map, i) == i*i*i);
        }
        for (s64 i = 7; i < 123; i += 7) {
            assert(hashmap_delete(&map, i) == i*i*i);
        }
        for (s64 i = 1; i < 123; ++i) {
            u64* p = hashmap_getptr(&map, i);
            assert(i%7 ? *p == i*i*i : p == nullptr);
        }
    }    
}
#endif

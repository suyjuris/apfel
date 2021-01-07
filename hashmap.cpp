
#ifdef HASHMAP_TEST
#include <unistd.h>
#include "global.hpp"
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
    return key; // no hashing for the fuzzer
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
#ifndef HASHMAP_TEST
    assert(key != map->empty);
#else
    if (key == map->empty) return;
#endif

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
    hashmap_selfcheck(map);
#endif
}

template <typename T>
T* hashmap_getptr(Hashmap<T>* map, u64 key) {
#ifndef HASHMAP_TEST
    assert(key != map->empty);
#endif
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
#ifndef HASHMAP_TEST
    assert(key != map->empty);
#endif
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
void hashmap_clear(Hashmap<T>* map) {
    for (s64 i = 0; i < map->slots.size; ++i) {
        map->slots[i].key = map->empty;
    }
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
    u64 x = 0xffdf38dd3e69bd91ull ^ arr.size;
    for (u64 a: arr) x = hash_u64(a ^ x);
    return x;
}

#ifdef HASHMAP_TEST
int main(int argc, char** argv) {
    if (argc > 1) {
        Hashmap<u64> map;
        char buf[4096] = {};
        int len = read(STDIN_FILENO, buf, sizeof(buf));
        for (int i = 0; i < len; i += 2) {
            if (buf[i] % 2 == 0) {
                hashmap_set(&map, buf[i+1], buf[i]);
            } else if (buf[i] % 4 == 1) {
                hashmap_get(&map, buf[i+1]);
            } else {
                hashmap_getptr(&map, buf[i+1]);
            }
        }
    } else {
        Hashmap<u64> map;

        for (s64 i = 1; i < 123; ++i) hashmap_set<u64>(&map, i, i*i*i);
        for (s64 i = 1; i < 123; ++i) {
            assert(hashmap_get(&map, i) == i*i*i);
        }
    }    
}
#endif

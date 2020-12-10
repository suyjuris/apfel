
#include <sys/mman.h>


void* array_alloc_pages(s64 size) {
    assert(size >= 0);
    void* p = mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_NORESERVE | MAP_ANONYMOUS, -1, 0);
    assert(p != MAP_FAILED);
    return p;
}

template <typename T>
Array_dyn<T> array_create_unreserved(s64 size) {
    return {(T*)array_alloc_pages(size * sizeof(T)), 0, size, true};
}

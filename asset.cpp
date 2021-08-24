
struct Code_location {
    Array_t<u8> path;
    s64 offset_lines = 0;
};

struct Asset_item {
    enum Source_type: u8 {
        INVALID, FILE, CODE, EXECUTABLE
    };
    Array_t<u8> name, data;
    u8 origin;
    Code_location location;
};

struct Asset_store {
    static constexpr u64 MAGIC = 0xf2c65ce529bbc8dfll;
    
    Array_dyn<Asset_item> assets;
    bool finalized = false;
};

bool asset_init(Asset_store* store) {
    char const* path = "/proc/self/exe";

    auto error = [path] (bool check = true, char const* str = nullptr, bool do_errno = true) {
        if (not check) return;
        if (do_errno) {
            error_errno(str);
        } else if (str) {
            array_printf(&global_err, str);
        }
        array_printf(&global_err, "while opening file '%s' for reading\n", path);
        error_print();
        exit(1100);
    };

    FILE* f = fopen(path, "rb");
    error(f == nullptr, "while calling fopen()");
    
    error(fseek(f, 0, SEEK_END) == -1, "while calling fseek()");
    s64 f_size = ftell(f);
    
    error(f_size == -1, "while calling ftell()");
    
    struct Trailer {
        u64 magic;
        s64 data_offset;
    };

    auto read = [f, path, &error](u8* into, s64 bytes) {
        if (fread(into, 1, bytes, f) < bytes) {
            error(ferror(f), "while calling fread()");
            error(true, "unexpected eof (concurrent modification?)\nwhile calling fread()\n", false);
        }
    };
    auto read_s64 = [&read]() {
        s64 into;
        read((u8*)&into, sizeof(into));
        return into;
    };
    
    Trailer trailer;

    if (f_size < sizeof(trailer)) {
        array_printf(&global_err, "Error: file is less than %d bytes long\n", (int)sizeof(trailer));
        error();
    }
    error(fseek(f, -sizeof(trailer), SEEK_CUR) == -1, "while calling fseek() (2)");
    read((u8*)&trailer, sizeof(trailer));

    if (trailer.magic != Asset_store::MAGIC) {
        // No magic means that the assets are not stored in the binary
        return false;
    }

    // Get the assets out

    error(fseek(f, trailer.data_offset, SEEK_SET) == -1, "while calling fseek()");

    array_resize(&store->assets, read_s64());
    for (auto& i: store->assets) {
        i.origin = Asset_item::EXECUTABLE;
        i.name = array_create<u8>(read_s64());
        read(i.name.data, i.name.size);
        i.data = array_create<u8>(read_s64());
        read(i.data.data, i.data.size);
    }
    
    if (fclose(f)) {
        // Something weird is happening here, but hey, we already have our data.
        fprintf(stderr, "Warning: Could not close file '%s'\n", path);
        perror("Warning:");
    }

    return true;
}

void asset_load_source(Asset_store* store, Array_t<u8> file) {
    assert(not store->finalized /* no loading after finalised */);

    Array_t<u8> source = array_load_from_file(file);
    s64 i;
    s64 lineno = 0;

    auto nextline = [&i, &lineno, &source]() {
        while (i < source.size and source[i] != '\n') ++i;
        ++i;
        ++lineno;
    };
    auto match = [&i, source](Array_t<u8> str) -> bool {
        Array_t<u8> arr = array_subarray(source, i, min(i + str.size, source.size));
        if (not array_equal(str, arr)) return false;
        i += str.size;
        return true;
    };
    auto isident = [](u8 c) {
        return ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or ('0' <= c and c <= '9') or c == '_';
    };

    s64 state = 0;
    for (i = 0; i < source.size; nextline()) {
        if (not match("#ifdef ASSET_"_arr)) continue;

        s64 name_beg = i;
        while (i < source.size and isident(source[i])) ++i;
        Array_t<u8> name = array_subarray(source, name_beg, i);
        for (auto j: store->assets) {
            assert(not array_equal(name, j.name) /* Asset already exists */);
        }
        
        nextline();
        s64 data_beg = i;
        s64 data_end = i;
        s64 lineno_orig = lineno;
        
        for (; i < source.size; nextline()) {
            data_end = i;
            if (match("#endif"_arr)) break;
        }
        
        auto data = array_subarray(source, data_beg, data_end);
        array_push_back(&store->assets, {name, data, Asset_item::CODE, {file, lineno_orig}});
    }
}

Array_t<u8> asset_load_file(Asset_store* store, Array_t<u8> name, Array_t<u8> path) {
    assert(not store->finalized); // no loading after finalised 
    for (auto i: store->assets) {
        assert(not array_equal(i.name, name)); // Asset already exists
    }

    Array_t<u8> data = array_load_from_file(path);
    array_push_back(&store->assets, {name, data, Asset_item::FILE, {path, 0}});
    return data;
}

Array_t<u8> asset_get(Asset_store* store, Array_t<u8> name, Code_location* out_loc = nullptr) {
    for (auto i: store->assets) {
        if (array_equal(i.name, name)) {
            if (out_loc) *out_loc = i.location;
            return i.data;
        }
    }
    assert(false); // asset not found
    return {};
}

bool asset_try_reload(Asset_store* store, Array_t<u8> name) {
    Asset_item* item = nullptr;
    for (auto& i: store->assets) {
        if (array_equal(i.name, name)) {
            item = &i;
            break;
        }
    }
    assert(item);

    if (item->origin != Asset_item::FILE) return false;

    u64 hash = hash_str(item->data);
    auto data_new = array_load_from_file(item->location.path);
    if (hash_str(data_new) != hash) {
        array_free(&item->data);
        item->data = data_new;
        return true;
    }
    return false;
}

void asset_finalize(Asset_store* store, bool do_pack) {
    assert(not store->finalized);
    store->finalized = true;

    if (not do_pack) return;

    char const* path1 = "/proc/self/exe";
    char const* path2 = PROGRAM_NAME "_packed";
    
    FILE* f1 = fopen(path1, "rb");

    if (f1 == nullptr) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (12)\n", path1);
        perror("Error"); exit(1201);
    }

    FILE* f2 = fopen(path2, "wb");

    if (f2 == nullptr) {
        fprintf(stderr, "Error: Could not open file '%s' for writing (1)\n", path2);
        perror("Error"); exit(1202);
    }

    if (platform_chmod_try(fileno(f2), 0755)) {
        fprintf(stderr, "Warning: Could not mark file '%s' as executable\n", path2);
        perror("Warning");
        platform_error_clear();
    }
    
    {char buf[4096];
    bool done = false;
    while (not done) {
        s64 read = fread(buf, 1, sizeof(buf), f1);
        if (read < sizeof(buf)) {
            if (ferror(f1)) {
                fprintf(stderr, "Error: Could not open file '%s' for reading (13)\n", path1);
                perror("Error"); exit(1203);
            }
            done = true;
        }
        
        if (fwrite(buf, 1, read, f2) < read) {
            fprintf(stderr, "Error: Could not open file '%s' for writing (2)\n", path2);
            perror("Error"); exit(1204);
        }
    }}
    
    s64 f_size = ftell(f2);
    if (f_size == -1) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (14)\n", path2);
        perror("Error"); exit(1205);
    }

    auto f2_write = [f2, path2](u8* data, s64 size) {
        if (fwrite(data, 1, size, f2) < size) {
            fprintf(stderr, "Error: Could not open file '%s' for writing (3)\n", path2);
            perror("Error"); exit(1206);
        }
    };
    auto f2_write_s64 = [&f2_write](s64 data) {
        f2_write((u8*)&data, sizeof(data));
    };

    f2_write_s64(store->assets.size);
    for (auto i: store->assets) {
        f2_write_s64(i.name.size);
        f2_write(i.name.data, i.name.size);
        f2_write_s64(i.data.size);
        f2_write(i.data.data, i.data.size);
    }
    f2_write_s64(Asset_store::MAGIC);
    f2_write_s64(f_size);

    if (fclose(f2)) {
        fprintf(stderr, "Error: Could not close file '%s'\n", path2);
        perror("Error:"); exit(1207);
    }
    
    if (fclose(f1)) {
        // Something weird is happening here, but hey, we already have our data.
        fprintf(stderr, "Warning: Could not close file '%s'\n", path1);
        perror("Warning:");
    }
}

#pragma once

// General interface for input events and states
struct Key {
    enum Key_type: u8 {
        NONE, TEXT, SPECIAL, MOUSE, GENERAL
    };
    enum Key_special: u8 {
        INVALID, ESCAPE, ARROW_L, ARROW_R, ARROW_D, ARROW_U,
        HOME, END, PAGE_U, PAGE_D, TAB, SHIFT_TAB, DELETE, BACKSPACE, RETURN,
        C_COPY, C_PASTE, C_CUT, C_SELECTALL, C_UNDO, C_REDO, C_QUIT, C_SAVE,
        C_ZOOM_IN, C_ZOOM_OUT, C_ZOOM_ZERO,
        F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
        // The following will not generate events, but can still be queried
        SHIFT, CONTROL, ALT,
        
        SPECIAL_COUNT
    };
    enum Key_flags: u8 {
        MOD_SHIFT = 1, MOD_CTRL = 2,
    };
    static constexpr char const* key_special_names[] = {
        "invalid", "escape", "arrow_l", "arrow_r", "arrow_d", "arrow_u",
        "home", "end", "page_u", "page_d", "tab", "shift_tab", "delete", "backspace", "return",
        "c_copy", "c_paste", "c_cut", "c_selectall", "c_undo", "c_redo", "c_quit", "c_save",
        "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12",
        "shift", "control", "alt"
    };
    enum Mouse_action: u8 {
        // Only the *_DOWN values can be queried
        LEFT_DOWN, LEFT_UP, RIGHT_DOWN, RIGHT_UP, MIDDLE_DOWN, MIDDLE_UP, MOTION,
        SCROLL_DOWNWARDS, SCROLL_UPWARDS
    };

    enum General_type: u8 {
        FOCUS_IN, FOCUS_OUT, GENERAL_COUNT
    };

    u8 type = Key::NONE;
    union {
        u8 text[15];
        struct { u8 special; u8 flags; s64 data; };
        s32 mouse[3];
        u8 general;
    };

    static Key create_text(Array_t<u8> text_) {
        assert(text_.size+1 < (s64)sizeof(Key::text));
        Key result;
        result.type = Key::TEXT;
        memcpy(result.text, text_.data, text_.size);
        result.text[text_.size] = 0;
        return result;
    }
    static Key create_special(u8 special, u8 flags = 0, s32 data = -1) {
        assert(special != INVALID and special < SPECIAL_COUNT);
        Key result;
        result.type = Key::SPECIAL;
        result.special = special;
        result.flags = flags;
        result.data = data;
        return result;
    }
    static Key create_mouse(u8 action, s64 x = -1, s64 y = -1) {
        assert((s32)x == x and (s32)y == y);
        Key result;
        result.type = Key::MOUSE;
        result.mouse[0] = (s32)action;
        result.mouse[1] = (s32)x;
        result.mouse[2] = (s32)y;
        return result;
    }
    static Key create_general(u8 general) {
        assert(general < GENERAL_COUNT);
        Key result;
        result.type = Key::GENERAL;
        result.general = general;
        return result;
    }

    void get_mouse_param(u8* action, s64* x = nullptr, s64* y = nullptr) {
        assert(type == Key::MOUSE);
        if (action) *action = (u8)mouse[0];
        if (x) *x = mouse[1];
        if (y) *y = mouse[2];
    }
};

constexpr char const* Key::key_special_names[];

void key_print(Key key) {
    if (key.type == Key::NONE) {
        printf("{NONE}");
    } else if (key.type == Key::TEXT) {
        printf("{\"%s\" %02x}", key.text, (u8)key.text[0]);
    } else if (key.type == Key::SPECIAL) {
        assert(key.special < Key::SPECIAL_COUNT);
        printf("{%s}", Key::key_special_names[key.special]);
    } else {
        assert(false);
    }
}

// When to draw the next frame. Argument is in nanoseconds since program start, same as
// platform_now. Call this with 0 to redraw at the next opportunity.
void platform_redraw(u64 t);

// Return whether the key is currently pressed.
bool platform_key_get(Key key);

// Return the number of nanoseconds since program start, approximately (does not actually query
// timer, this is time at frame start)
u64 platform_now();
// Same, but this does query the timer.
u64 platform_now_real();

// Return the number of frames since program start
u64 platform_frame_count();

// Change the mode of the underlying file descriptor
int platform_chmod(FILE* f, u32 mode);

// The following are callbacks to be implemented by the application

struct Application;
void application_init_assets(Application* context, bool flag_pack_assets);
void application_init(Application* context);
void application_render(Application* context);
void application_handle_resize(Application* context, s64 width = -1, s64 height = -1);
void application_print_help(Application* context, char* argv0, bool is_packed);

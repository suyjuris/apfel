
#include "global.hpp"
#include "platform.hpp"
#include <GL/glx.h>
#include "platform_linux_autogen.cpp"
#define PLATFORM_INCLUDES

#include "factorio_gui.cpp"

#include <alloca.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/extensions/Xrandr.h>
#include <time.h>
#include <locale.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <unistd.h>

typedef GLXContext (*glXCreateContextAttribsARB_t) (
    Display *dpy, GLXFBConfig config, GLXContext share_context, Bool direct, const int *attrib_list
);

struct Platform_state {
    Display* display = nullptr;
    Window window;
    GLXWindow window_glx;
    Atom sel_primary, sel_clipboard, sel_target, sel_utf8str, sel_string, sel_incr;
    Atom net_wm_state, net_wm_state_fullscreen, type_atom;

    Application app_context;

    timespec t_start; // Time of program start
    u64 now = 0; // ns since t_start
    u64 redraw_next = -1; // next redraw, in ns since t_start
    u64 frame_count = 0; // number of frames since program start
    
    s64 rate = -1;

    Array_dyn<u8> clipboard_recv;
    Array_dyn<u8> clipboard_send[Platform_clipboard::COUNT] = {};

    bool keys_dirty = true;
    char keys_map[32];
    u32 keys_mask;
};
Platform_state global_platform;

void linux_get_event_key(Array_dyn<Key>* keys, XKeyEvent e) {
    KeySym keysym;

    // You would think that we could do some dynamic resizing here if the buffer is too
    // small. However, the API does not seem to support it.
    u8 buffer_[64];
    Array_t<u8> buffer {buffer_, sizeof(buffer)};
    buffer.size = XLookupString(&e, (char*)buffer.data, buffer.size, &keysym, NULL);
    
    s64 special = Key::INVALID;
    u64 mod = 0;
    u64 shiftctrl = ShiftMask | ControlMask;
    switch (keysym) {
    case XK_Escape:       special = Key::ESCAPE; break;
    case XK_Page_Up:      special = Key::PAGE_U; break;
    case XK_Page_Down:    special = Key::PAGE_D; break;
    case XK_Tab:          special = Key::TAB;    break;
    case XK_F1:           special = Key::F1;     break;
    case XK_F2:           special = Key::F2;     break;
    case XK_F3:           special = Key::F3;     break;
    case XK_F4:           special = Key::F4;     break;
    case XK_F5:           special = Key::F5;     break;
    case XK_F6:           special = Key::F6;     break;
    case XK_F7:           special = Key::F7;     break;
    case XK_F8:           special = Key::F8;     break;
    case XK_F9:           special = Key::F9;     break;
    case XK_F10:          special = Key::F10;    break;
    case XK_F11:          special = Key::F11;    break;
    case XK_F12:          special = Key::F12;    break;
    case XK_Return:       special = Key::RETURN; break;
    case XK_Left:         special = Key::ARROW_L;   mod = e.state & shiftctrl; break;
    case XK_Right:        special = Key::ARROW_R;   mod = e.state & shiftctrl; break;
    case XK_Down:         special = Key::ARROW_D;   mod = e.state & shiftctrl; break;
    case XK_Up:           special = Key::ARROW_U;   mod = e.state & shiftctrl; break;
    case XK_Home:         special = Key::HOME;      mod = e.state & shiftctrl; break;
    case XK_End:          special = Key::END;       mod = e.state & shiftctrl; break;
    case XK_Delete:       special = Key::DELETE;    mod = e.state & shiftctrl; break;
    case XK_BackSpace:    special = Key::BACKSPACE; mod = e.state & shiftctrl; break;
    case XK_ISO_Left_Tab: special = Key::SHIFT_TAB;   mod = ShiftMask;   break;
    case XK_c:            special = Key::C_COPY;      mod = ControlMask; break;
    case XK_v:            special = Key::C_PASTE;     mod = ControlMask; break;
    case XK_x:            special = Key::C_CUT;       mod = ControlMask; break;
    case XK_a:            special = Key::C_SELECTALL; mod = ControlMask; break;
    case XK_q:            special = Key::C_QUIT;      mod = ControlMask; break;
    case XK_s:            special = Key::C_SAVE;      mod = ControlMask; break;
    case XK_z:            special = Key::C_UNDO;      mod = ControlMask; break;
    case XK_plus:         special = Key::C_ZOOM_IN;   mod = ControlMask; break;
    case XK_minus:        special = Key::C_ZOOM_OUT;  mod = ControlMask; break;
    case XK_0:            special = Key::C_ZOOM_ZERO; mod = ControlMask; break;
    case XK_Z:            special = Key::C_REDO;      mod = ControlMask | ShiftMask; break;
    }

    // Exclude NumLock (Mod2) and CapsLock (Lock) from the modifier list, as our shortcuts should
    // still work if they are pressed.
    u64 mod_mask = ShiftMask | ControlMask | Mod1Mask | Mod3Mask | Mod4Mask | Mod5Mask;
    if (special != Key::INVALID and (e.state & mod_mask) == mod) {
        u8 flags = 0;
        if (e.state & ShiftMask) flags |= Key::MOD_SHIFT;
        if (e.state & ControlMask) flags |= Key::MOD_CTRL;
        array_push_back(keys, Key::create_special(special, flags));
    } else {
        s64 chunk = sizeof(Key::text);
        for (s64 i = 0; i < buffer.size; i += chunk) {
            s64 end = i + chunk < buffer.size ? i + chunk : buffer.size;
            array_push_back(keys, Key::create_text(array_subarray(buffer, i, end)));
        }
    }
}


void linux_set_wm_prop(Display* display, Window window, char const* property, char const* data) {
    Atom prop = XInternAtom(display, property, true);
    assert(prop != None);

    Atom type_string = XInternAtom(display, "STRING", true);
    assert(type_string != None);
    
    XChangeProperty(display, window, prop, type_string, 8, PropModeReplace, (u8*)data, strlen(data));
}

void linux_set_wm_class(Display* display, Window window, int argc, char** argv) {
    char* instance = nullptr;
    for (s64 i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "-name") == 0 and i+1 < argc) {
            instance = argv[i+1];
        }
    }
    if (not instance) {
        instance = getenv("RESOURCE_NAME");
    }
    if (not instance) {
        s64 last_sep = 0;
        for (s64 i = 0; argv[0][i]; ++i) {
            if (argv[0][i] == '/') last_sep = i;
        }
        if (argv[0][last_sep] and argv[0][last_sep+1]) {
            instance = &argv[0][last_sep + 1];
        }
    }
    if (not instance) {
        instance = (char*)"obst";
    }

    char* cls = (char*)"obst";
    s64 buf_size = strlen(instance) + strlen(cls) + 2;
    char* buf = (char*)alloca(buf_size);
    assert( snprintf(buf, buf_size, "%s %s", instance, cls) < buf_size );
    
    linux_set_wm_prop(display, window, "WM_CLASS", buf);
}

Array_t<u8> platform_clipboard_get(s64 index) {
    s64 size = *(s64*)&global_platform.clipboard_recv[index];
    index += sizeof(s64);
    return array_subarray(global_platform.clipboard_recv, index, index+size);
}
void platform_clipboard_free(s64 index) {
    auto arr = platform_clipboard_get(index);
    if (arr.end() == global_platform.clipboard_recv.end()) {
        global_platform.clipboard_recv.size = 0;
    }
}
void platform_clipboard_set(u8 type, Array_t<u8> data) {
    assert(type <= Platform_clipboard::COUNT);
    global_platform.clipboard_send[type].size = 0;
    array_append(&global_platform.clipboard_send[type], data);
    auto type_x = type == Platform_clipboard::CONTROL_C ? global_platform.sel_clipboard : global_platform.sel_primary;
    XSetSelectionOwner(global_platform.display, type_x, global_platform.window, CurrentTime);
}
void linux_handle_selection_request(Platform_state* platform, XSelectionRequestEvent* ev) {
    XSelectionEvent ev_out;
    memset(&ev_out, 0, sizeof(ev_out));
    ev_out.type = SelectionNotify;
    ev_out.requestor = ev->requestor;
    ev_out.selection = ev->selection;
    ev_out.target    = ev->target;
    ev_out.time      = ev->time;

    if (ev->target == platform->sel_utf8str or ev->target == platform->sel_string) {
        Atom property = ev->property;
        if (property == None) {
            property = ev->target;
        }

        u8 sel_type = ev->selection == platform->sel_primary ? Platform_clipboard::MIDDLE_BUTTON : Platform_clipboard::CONTROL_C;
        auto buf = platform->clipboard_send[sel_type];
        XChangeProperty(platform->display, ev->requestor, property, ev->target, 8, PropModeReplace, buf.data, buf.size);
        
        ev_out.property = property;
    } else {
        ev_out.property = None;
    }

    XSendEvent(platform->display, ev_out.requestor, false, 0, (XEvent*)&ev_out);
}
    
void linux_handle_selection_response(Platform_state* platform, XSelectionEvent* ev) {
    if (ev->property == None) {
        // Try to fall back to STRING type
        XConvertSelection(platform->display, ev->selection, platform->sel_target, platform->sel_string, platform->window, ev->time);
    } else {
        Atom actual_type;
        int actual_format;
        unsigned long n_items, bytes_after;
        u8* prop;
        XGetWindowProperty(platform->display, platform->window, platform->sel_target, 0, -1, 0,
            AnyPropertyType, &actual_type, &actual_format, &n_items, &bytes_after, &prop);

        if (actual_type == platform->sel_incr) {
            fprintf(stderr, "Warning: Received clipboard of type INCR, which obst does not implement.\n");
            return;
        } else if (actual_type == None or actual_format != 8) {
            return;
        }
        
        s64 index = global_platform.clipboard_recv.size;
        s64 n_items_ = n_items;
        array_append(&global_platform.clipboard_recv, {(u8*)&n_items_, sizeof(s64)});
        array_append(&global_platform.clipboard_recv, {prop, (s64)n_items});

        XFree(prop);
        XDeleteProperty(platform->display, platform->window, platform->sel_target);

        // TODO this is broken in this version of platform_linux
        /*if (ev->selection == platform->sel_primary) {
            // TODO This should not press buttons
            Key key1 = Key::create_mouse(Key::LEFT_DOWN, platform->app_context.pointer_x, platform->app_context.pointer_y);
            Key key2 = Key::create_mouse(Key::LEFT_UP,   platform->app_context.pointer_x, platform->app_context.pointer_y);
            array_append(&global_platform.app_context.input_queue, {key1, key2});
        }*/
        
        Key key = Key::create_special(Key::C_PASTE, 0, index);
        array_push_back(&global_platform.app_context.input_queue, key);
    }
}

void linux_fullscreen(Platform_state* platform) {
    // This tries to go to fullscreen via NET_WM_STATE, which might not work for some window managers

    if (platform->net_wm_state != None and platform->net_wm_state_fullscreen != None) {
        XEvent ev;
        memset(&ev, 0, sizeof(ev));
        ev.type = ClientMessage;
        ev.xclient.window = platform->window;
        ev.xclient.format = 32;
        ev.xclient.message_type = platform->net_wm_state;
        ev.xclient.data.l[0] = 2; // _NET_WM_STATE_TOGGLE
        ev.xclient.data.l[1] = platform->net_wm_state_fullscreen;
        ev.xclient.data.l[2] = 0;
        ev.xclient.data.l[3] = 1;
        ev.xclient.data.l[4] = 0;
        XSendEvent(platform->display, DefaultRootWindow(platform->display), false,
            SubstructureNotifyMask | SubstructureRedirectMask, &ev);
    }
}

void platform_redraw(u64 t) {
    global_platform.redraw_next = min(global_platform.redraw_next, t);
}

bool platform_key_get(Key key) {
    KeySym sym = NoSymbol;
    s32 mask = 0;
    
    if (key.type == Key::TEXT) {
        sym = XStringToKeysym((char*)key.text);        
    } else if (key.type == Key::SPECIAL) {
        switch (key.special) {
        case Key::ESCAPE:    sym = XK_Escape;    break;
        case Key::PAGE_U:    sym = XK_Page_Up;   break;
        case Key::PAGE_D:    sym = XK_Page_Down; break;
        case Key::TAB:       sym = XK_Tab;       break;
        case Key::F1:        sym = XK_F1;        break;
        case Key::F2:        sym = XK_F2;        break;
        case Key::F3:        sym = XK_F3;        break;
        case Key::F4:        sym = XK_F4;        break;
        case Key::F5:        sym = XK_F5;        break;
        case Key::F6:        sym = XK_F6;        break;
        case Key::F7:        sym = XK_F7;        break;
        case Key::F8:        sym = XK_F8;        break;
        case Key::F9:        sym = XK_F9;        break;
        case Key::F10:       sym = XK_F10;       break;
        case Key::F11:       sym = XK_F11;       break;
        case Key::F12:       sym = XK_F12;       break;
        case Key::RETURN:    sym = XK_Return;    break;
        case Key::ARROW_L:   sym = XK_Left;      break;
        case Key::ARROW_R:   sym = XK_Right;     break;
        case Key::ARROW_D:   sym = XK_Down;      break;
        case Key::ARROW_U:   sym = XK_Up;        break;
        case Key::HOME:      sym = XK_Home;      break;
        case Key::END:       sym = XK_End;       break;
        case Key::DELETE:    sym = XK_Delete;    break;
        case Key::BACKSPACE: sym = XK_BackSpace; break;
        case Key::SHIFT:     mask = ShiftMask;   break;
        case Key::CONTROL:   mask = ControlMask; break;
        case Key::ALT:       mask = Mod1Mask;    break;
        default: assert(false); return false;
        }
    } else if (key.type == Key::MOUSE) {
        u8 action;
        key.get_mouse_param(&action, nullptr, nullptr);
        switch (action) {
        case Key::LEFT_DOWN:   mask = Button1Mask; break;   
        case Key::MIDDLE_DOWN: mask = Button2Mask; break;   
        case Key::RIGHT_DOWN:  mask = Button3Mask; break;
        default: assert(false); return false;
        }
    } else {
        assert(false);
        return false;
    }

    if (global_platform.keys_dirty) {
        XQueryKeymap(global_platform.display, global_platform.keys_map);
        Window a0, a1; int a2, a3, a4, a5;
        XQueryPointer(global_platform.display, global_platform.window,
            &a0, &a1, &a2, &a3, &a4, &a5, &global_platform.keys_mask);
        global_platform.keys_dirty = false;
    }
    
    if (sym != NoSymbol) {
        KeyCode code = XKeysymToKeycode(global_platform.display, sym);
        if (code == 0) {
            assert(false);
            return false;
        }
        assert(code < sizeof(global_platform.keys_map) * 8);

        return global_platform.keys_map[code/8] >> code%8 & 1;
    } else if (mask != 0) {
        return global_platform.keys_mask & mask;
    } else {
        assert(false);
        return false;
    }
}

u64 platform_now() {
    return global_platform.now;
}

u64 platform_now_real() {
    timespec ts;
    clock_gettime(CLOCK_MONOTONIC_RAW, &ts);
    return (ts.tv_sec - global_platform.t_start.tv_sec) * 1000000000ull
        + (ts.tv_nsec - global_platform.t_start.tv_nsec);
}

u64 platform_frame_count() {
    return global_platform.frame_count;
}

Array_dyn<u8> platform_error_buf;

void platform_error_print(Array_t<u8> prefix) {
    s64 last = 0;
    for (s64 i = 0; i < platform_error_buf.size; ++i) {
        if (platform_error_buf[i] == '\n') {
            fwrite(prefix.data, 1, prefix.size, stderr);
            fputs(": ", stderr);
            fwrite(&platform_error_buf[last], 1, i - last, stderr);
            fputs("\n", stderr);
            last = i+1;
        }
    }
    platform_error_buf.size = 0;
}

void platform_error_clear() {
    platform_error_buf.size = 0;
}

int platform_open_try(Array_t<u8> path, u64 flags, u32 mode, int* out_fd) {
    using namespace Platform;
    assert(not (flags & ~(OPEN_READ | OPEN_WRITE | OPEN_CREATE | OPEN_APPEND | OPEN_TRUNCATE)));
    assert(out_fd);
    
    char* tmp = (char*)alloca(path.size + 1);
    memcpy(tmp, path.data, path.size);
    tmp[path.size] = 0;

    int f = 0;
    u64 flagsrw = flags & (OPEN_READ | OPEN_WRITE);
    f |= flagsrw == (OPEN_READ | OPEN_WRITE) ? O_RDWR : 0;
    f |= flagsrw == OPEN_READ ? O_RDONLY : 0;
    f |= flagsrw == OPEN_WRITE ? O_WRONLY : 0;
    assert(flagsrw);
    
    f |= flags & OPEN_CREATE ? O_CREAT : 0;
    f |= flags & OPEN_APPEND ? O_APPEND : 0;
    f |= flags & OPEN_TRUNCATE ? O_TRUNC : 0;
    
    int fd = open(tmp, f, (mode_t)mode);

    if (fd == -1) {
        platform_error_printf("$ while opening file '%s'\n", tmp);
        return 1;
    }

    if (out_fd) *out_fd = fd;
    return 0;
}

int platform_request_lock_try(int fd, bool* out_success) {
    int code = flock(fd, LOCK_EX | LOCK_NB);

    bool succ = true;
    if (code) {
        if (errno == EWOULDBLOCK) {
            succ = false;
        } else {
            platform_error_printf("$ while trying to acquire lock\n");
            return 1;
        }
    }
    
    if (out_success) *out_success = succ;
    return 0;
}

int platform_write_try(int fd, Array_t<u8> buf) {
    while (buf.size > 0) {
        s64 bytes_written = write(fd, buf.data, buf.size);
        if (bytes_written == -1) {
            if (errno == EPIPE) {
                array_printf(&platform_error_buf, "eof while writing bytes (%ld left to write)\n", (long)buf.size);
                return Platform::WRITE_EOF;
            } else {
                bool wouldblock = errno == EWOULDBLOCK || errno == EAGAIN;
                platform_error_printf("$ while calling write()");
                return wouldblock ? Platform::WRITE_WOULDBLOCK : Platform::WRITE_ERROR;
            }
        }
        assert(bytes_written > 0);
        buf = array_subarray(buf, bytes_written, buf.size);
    }

    assert(buf.size == 0);
    return 0;
}

int platform_read_try(int fd, Array_t<u8> buf) {
    while (buf.size > 0) {
        s64 bytes_read = read(fd, buf.data, buf.size);
        if (bytes_read == -1) {
            bool wouldblock = errno == EWOULDBLOCK || errno == EAGAIN;
            platform_error_printf("$ while calling read()");
            return wouldblock ? Platform::READ_WOULDBLOCK : Platform::READ_ERROR;
        }
        if (bytes_read == 0) {
            /* Note that buf.size > 0 due to loop condition */
            array_printf(&platform_error_buf, "unexpected eof (%ld bytes left to read)\n", (long)buf.size);
            return Platform::READ_EOF;
        }
        buf = array_subarray(buf, bytes_read, buf.size);
    }
    
    assert(buf.size == 0);
    return 0;
}

int platform_read_all_try(int fd, Array_dyn<u8>* buf) {
    while (true) {
        if (buf->capacity < buf->size + 256) {
            array_reserve(buf, buf->size + 256);
        }
        
        s64 bytes_read = read(fd, buf->data + buf->size, buf->capacity - buf->size);
        if (bytes_read == -1) {
            bool wouldblock = errno == EWOULDBLOCK || errno == EAGAIN;
            platform_error_printf("$ while calling read()");
            return wouldblock ? Platform::READ_WOULDBLOCK : Platform::READ_ERROR;
        }
        buf->size += bytes_read;

        if (buf->size < buf->capacity) break;
    }
    
    return 0;
}

int platform_seek_try(int fd, u64 offset, u8 whence, u64* out_offset) {
    int w = SEEK_SET;
    switch (whence) {
    case Platform::P_SEEK_SET: w = SEEK_SET; break;
    case Platform::P_SEEK_CUR: w = SEEK_CUR; break;
    case Platform::P_SEEK_END: w = SEEK_END; break;
    default: assert(false);
    }
    
    off_t r = lseek(fd, offset, w);
    if (r == (off_t)-1) {
        platform_error_printf("$ while calling lseek()");
        return 1;
    }
    
    if (out_offset) *out_offset = r;
    return 0;
}

int platform_truncate_try(int fd, u64 size) {
    int code = ftruncate(fd, size);
    if (code) {
        platform_error_printf("$ while calling ftruncate()");
        return 1;
    }
    return 0;
}

int platform_close_try(int fd) {
    int code = close(fd);
    if (code) {
        platform_error_printf("$ while calling close()\n");
        return 1;
    }
    return 0;
}

int platform_chmod_try(int fd, u32 mode) {
    int code = fchmod(fd, mode);
    if (code) {
        platform_error_printf("$ while calling fchmod()\n");
        return 1;
    }
    return 0;
}

Array_t<u8> platform_read_whole_file(Array_t<u8> path) {
    int fd;
    if (platform_open_try(path, Platform::OPEN_READ, 0, &fd)) goto error_nocontext;

    {u64 size;
    if (platform_seek_try(fd, 0, Platform::P_SEEK_END, &size)) goto error;
    if (platform_seek_try(fd, 0, Platform::P_SEEK_SET)) goto error;

    {Array_t<u8> result = array_create<u8>(size);

    if (platform_read_try(fd, result)) goto error;    
    if (platform_close_try(fd)) goto error;    
    
    return result;}}

  error:
    array_printf(&platform_error_buf, "while reading file at '");
    array_append(&platform_error_buf, path);
    array_printf(&platform_error_buf, "'\n");
  error_nocontext:
    platform_error_print();
    exit(6);
}

void _platform_init(Platform_state* platform) {
    _platform_init_gl_pointers();
    
    // WebGL has no vertex array objects, OpenGL requires them. Yay. However, creating a single one
    // is fine, and we can do everything else WebGL style.
    GLuint vao;
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    
    application_init(&platform->app_context);
}

void _platform_render(Platform_state* platform) {
    application_render(&platform->app_context);
    glXSwapBuffers(platform->display, platform->window_glx);
}


int main(int argc, char** argv) {
    bool flag_print_help = false;
    bool flag_print_font_license = false;
    bool flag_pack_assets = false;
    bool flag_fullscreen = false;
    
    if (argc == 2) {
        Array_t<u8> arg = {(u8*)argv[1], (s64)strlen(argv[1])};
        if (array_equal_str(arg, "--font-license")) {
            flag_print_font_license = true;
        } else if (array_equal_str(arg, "--pack")) {
            flag_pack_assets = true;
        } else if (array_equal_str(arg, "--fullscreen")) {
            flag_fullscreen = true;
        } else {
            flag_print_help = true;
        }
    } else if (argc != 1) {
        flag_print_help = true;
    }

    application_init_assets(&global_platform.app_context, flag_pack_assets);
    if (flag_pack_assets) exit(0);
    
    if (flag_print_help) {
        application_print_help(&global_platform.app_context, argv[0]);
        exit(2);
    }

    if (flag_print_font_license) {
        application_print_license(&global_platform.app_context);
        exit(0);
    }
    
    // Do the OpenGL and X dance. I would recommend everyone to not read this code, if at all
    // possible, to preserve sanity. This should have been a single function call. If you must,
    // refer to the GLX 1.4 specification, the GLX_ARB_create_context extension, and the Xlib
    // specification to understand what all of this does.

    if (setlocale(LC_ALL, "") == nullptr) {
        fprintf(stderr, "Warning: Could not set default locale.\n");
    }
    
    // Create the display, connect to the X server
    Display* display = XOpenDisplay(nullptr);
    if (not display) {
        fprintf(stderr, "Error: could not open display (is the DISPLAY environment variable set correctly?)\n");
        exit(101);
    }
    global_platform.display = display;

    int screen = DefaultScreen(display);
    
    // Check for GLX 1.4 and extensions
    int glx_version[] = {1, 4};
    if (not glXQueryVersion(display, &glx_version[0], &glx_version[1])) {
        fprintf(stderr, "Error: glX version %d.%d not present\n", glx_version[0], glx_version[1]);
        exit(102);
    }

    char* gl_ext_present = (char*)glXQueryExtensionsString(display, screen);
    char* gl_ext_want[] = {(char*)"GLX_ARB_create_context_profile"};
    for (s64 i = 0; i < (s64)(sizeof(gl_ext_want) / sizeof(gl_ext_want[0])); ++i) {
        if (not strstr(gl_ext_present, gl_ext_want[i])) {
            fprintf(stderr, "Error: OpenGL extension %s not present\n", gl_ext_want[i]);
            exit(105);
        }
    }

    int config_size;
    int config_attribs[] = {
        GLX_DOUBLEBUFFER, true,
        GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
        GLX_DEPTH_SIZE, 8,
        None
    };
    GLXFBConfig* config = glXChooseFBConfig(display, screen, config_attribs, &config_size);
    //defer { XFree(config); };
    if (not config or config_size == 0) {
        fprintf(stderr, "Error: did not find a GLXFBConfig with the required attributes\n");
        exit(103);
    }

    // TODO: This seems to be unnecessary? Try to test on some older systems.
    // Create old OpenGL context, to be able to get the function pointer for creating the new one.
    //GLXContext context_old = glXCreateNewContext(display, *config, GLX_RGBA_TYPE, 0, true);
    //if (not context_old) {
    //    fprintf(stderr, "Error: failed to initialise old GLXContext\n");
    //    exit(104);
    //}

    glXCreateContextAttribsARB_t glXCreateContextAttribsARB
        = (glXCreateContextAttribsARB_t)glXGetProcAddress((u8*)"glXCreateContextAttribsARB");
    assert(glXCreateContextAttribsARB); // We already checked for the presence of the extension

    //glXDestroyContext(display, context_old);

    // Create an OpenGL 3.3 context
    int context_attribs[] = {
        GLX_CONTEXT_MAJOR_VERSION_ARB, 3,
        GLX_CONTEXT_MINOR_VERSION_ARB, 3,
        GLX_CONTEXT_FLAGS_ARB, GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
        GLX_CONTEXT_PROFILE_MASK_ARB, GLX_CONTEXT_CORE_PROFILE_BIT_ARB,
        None
    };
    GLXContext context = glXCreateContextAttribsARB(display, *config, 0, true, context_attribs);

    // Create the window
    XVisualInfo* visual = glXGetVisualFromFBConfig(display, *config);
    assert(visual);
    //defer { XFree(visual); };

    XSetWindowAttributes window_attrs = {};
    window_attrs.colormap = XCreateColormap(display, DefaultRootWindow(display), visual->visual, AllocNone); // Apparently you need the colormap, else XCreateWindow gives a BadMatch error. No worries, this fact features prominently in the documentation and it was no bother at all.
    window_attrs.event_mask = ExposureMask | KeyPressMask | KeyReleaseMask | ButtonPressMask
        | ButtonReleaseMask | StructureNotifyMask | PointerMotionMask | FocusChangeMask;

    Window window = XCreateWindow(display, DefaultRootWindow(display), 0, 0, 1300, 800, 0,
        visual->depth, InputOutput, visual->visual, CWColormap | CWEventMask, &window_attrs); // We pass a type of InputOutput explicitly, as visual->c_class is an illegal value for some reason. A good reason, I hope.
    global_platform.window = window;

    // Set up xrandr
    int xrandr_event, xrandr_error;
    if (XRRQueryExtension(display, &xrandr_event, &xrandr_error)) {
        XRRSelectInput(display, window, RRScreenChangeNotifyMask);

        XRRScreenConfiguration* config = XRRGetScreenInfo(display, window);
        global_platform.rate = XRRConfigCurrentRate(config);
        XRRFreeScreenConfigInfo(config);
    } else {
        fprintf(stderr, "Warning: Xrandr extension not present on X server, assuming refresh rate of 60 Hz.\n");
        global_platform.rate = 60;
    }

    // Initialise window properties
    linux_set_wm_prop(display, window, "WM_NAME", PROGRAM_NAME " - " PROGRAM_TITLE);
    linux_set_wm_prop(display, window, "WM_ICON_NAME", PROGRAM_NAME);
    linux_set_wm_class(display, window, argc, argv);

    // Set WM_PROTOCOLS
    Atom wm_protocols = XInternAtom(display, "WM_PROTOCOLS", true);
    Atom wm_delete_window = XInternAtom(display, "WM_DELETE_WINDOW", true);
    Atom type_atom = XInternAtom(display, "ATOM", true);
    assert(wm_protocols != None and wm_delete_window != None and type_atom != None);
    XChangeProperty(display, window, wm_protocols, type_atom, 32, PropModeReplace, (u8*)&wm_delete_window, 1);


    // Query some atom we will later need for the clipboard
    Atom sel_primary   = XInternAtom(display, "PRIMARY", true);
    Atom sel_clipboard = XInternAtom(display, "CLIPBOARD", true);
    Atom sel_utf8str   = XInternAtom(display, "UTF8_STRING", true);
    Atom sel_string    = XInternAtom(display, "STRING", true);
    Atom sel_target    = XInternAtom(display, "SELECTION_TARGET", false); // This one is arbitrary
    Atom sel_incr      = XInternAtom(display, "INCR", true);
    global_platform.sel_primary   = sel_primary;
    global_platform.sel_clipboard = sel_clipboard;
    global_platform.sel_utf8str   = sel_utf8str;
    global_platform.sel_string    = sel_string;
    global_platform.sel_target    = sel_target;
    global_platform.sel_incr      = sel_incr;

    // And some atoms needed for fullscreen mode
    global_platform.net_wm_state            = XInternAtom(display, "_NET_WM_STATE", true);
    global_platform.net_wm_state_fullscreen = XInternAtom(display, "_NET_WM_STATE_FULLSCREEN", true);
    global_platform.type_atom               = type_atom;
    
    // Map the context to the window
    GLXWindow window_glx = glXCreateWindow(display, *config, window, nullptr);
    global_platform.window_glx = window_glx;
    glXMakeContextCurrent(display, window_glx, window_glx, context); // This returns a bool, but I cannot find what it means in the spec, so just ignore it. The greatness continues.

    // Do application-specific initialisation
    _platform_init(&global_platform);

    // Show the window
    XMapWindow(display, window);

    if (flag_fullscreen) linux_fullscreen(&global_platform);
    
    int x_fd = ConnectionNumber(display);

    clock_gettime(CLOCK_MONOTONIC_RAW, &global_platform.t_start);
    global_platform.now = 0;

    while (true) {
        bool redraw = false;
        if (XPending(display) <= 0) {
            // No events left to process, now we redraw or wait
            
            u64 wait = global_platform.redraw_next > global_platform.now
                     ? global_platform.redraw_next - global_platform.now : 0;
            if (global_platform.redraw_next == -1) {
                // Wait for next event
            } else if (wait > 5000000000ull / global_platform.rate) {
                fd_set fds;
                FD_ZERO(&fds);
                FD_SET(x_fd, &fds);
                
                timeval t;
                t.tv_sec  = (long)(wait / 1000000000ull);
                t.tv_usec = (long)(wait % 1000000000ull / 1000);
                
                int num = select(x_fd+1, &fds, nullptr, nullptr, &t);
                if (num == -1) {
                    fprintf(stderr, "Error: while executing select()\n");
                    perror("Error");
                    exit(105);
                } else if (num == 0) {
                    // Got timeout, redraw
                    redraw = true;
                } else {
                    // Check whether there is really an event there, or this is just one of select's
                    // classic wakeup pranks.
                    if (XPending(display) <= 0) continue;
                    
                    // An event arrived, process.
                }
            } else {
                // Next frame is imminent, draw
                redraw = true;
            }
        }
        
        if (redraw) {
            global_platform.now = platform_now_real();
            global_platform.redraw_next = -1;
            global_platform.keys_dirty = true;
            _platform_render(&global_platform);
            ++global_platform.frame_count;
            continue;
        }
        
        XEvent event;
        XNextEvent(display, &event);

        global_platform.now = platform_now_real();

        switch (event.type) {
        case Expose:
            platform_redraw(0);
            break;
            
        case ButtonPress: {
            u32 buttons[] = {Button1, Button2, Button3, Button4, Button5};
            u32 keys[] = {Key::LEFT_DOWN, Key::MIDDLE_DOWN, Key::RIGHT_DOWN,
                Key::SCROLL_UPWARDS, Key::SCROLL_DOWNWARDS};
            for (s64 i = 0; i < sizeof(keys) / sizeof(keys[0]); ++i) {
                if (event.xbutton.button == buttons[i]) {
                    Key key = Key::create_mouse(keys[i], event.xbutton.x, event.xbutton.y);
                    array_push_back(&global_platform.app_context.input_queue, key);
                    platform_redraw(0);
                }
            }
            if (event.xbutton.button == Button2) {
                XConvertSelection(display, sel_primary, sel_utf8str, sel_target, window, event.xbutton.time);
            }
        } break;
        case ButtonRelease: {
            u32 buttons[] = {Button1, Button2, Button3};
            u32 keys[] = {Key::LEFT_UP, Key::MIDDLE_UP, Key::RIGHT_UP};
            for (s64 i = 0; i < sizeof(keys) / sizeof(keys[0]); ++i) {
                if (event.xbutton.button == buttons[i]) {
                    Key key = Key::create_mouse(keys[i], event.xbutton.x, event.xbutton.y);
                    array_push_back(&global_platform.app_context.input_queue, key);
                    platform_redraw(0);
                }
            }
        } break;
        case MotionNotify: {
            Key key = Key::create_mouse(Key::MOTION, event.xmotion.x, event.xmotion.y);
            array_push_back(&global_platform.app_context.input_queue, key);
            platform_redraw(0);
        } break;

            
        case KeyPress: {
            auto* iq = &global_platform.app_context.input_queue;
            linux_get_event_key(iq, event.xkey);
            if (iq->size and (*iq)[iq->size-1].type == Key::SPECIAL and (*iq)[iq->size-1].special == Key::C_PASTE) {
                // Query the contents of the clipboard, we need to wait for them to arrive
                XConvertSelection(display, sel_clipboard, sel_utf8str, sel_target, window, event.xkey.time);
                --iq->size;
            } else if (iq->size and (*iq)[iq->size-1].type == Key::SPECIAL and (*iq)[iq->size-1].special == Key::F11) {
                // Toggle fullscreen
                linux_fullscreen(&global_platform);
                --iq->size;
            } else {
                platform_redraw(0);
            }
        } break;
            
        case SelectionNotify:
            linux_handle_selection_response(&global_platform, &event.xselection);
            platform_redraw(0);
            break;
        case SelectionRequest:
            linux_handle_selection_request(&global_platform, &event.xselectionrequest);
            break;

        case MappingNotify:
            if (event.xmapping.request == MappingModifier or event.xmapping.request == MappingKeyboard) {
                XRefreshKeyboardMapping(&event.xmapping);
            }
            break;
        case ClientMessage:
            if (event.xclient.message_type == wm_protocols and event.xclient.data.l[0] - wm_delete_window == 0) {
                array_push_back(&global_platform.app_context.input_queue, Key::create_special(Key::C_QUIT, 0));
                platform_redraw(0);
            }
            break;
        case ConfigureNotify:
            application_handle_resize(&global_platform.app_context, event.xconfigure.width, event.xconfigure.height);
            break;
        default:
            break;
        }

        if (event.type == xrandr_event + RRScreenChangeNotify) {
            assert(XRRUpdateConfiguration(&event));
            XRRScreenConfiguration* config = XRRGetScreenInfo(display, window);
            global_platform.rate = XRRConfigCurrentRate(config);
            XRRFreeScreenConfigInfo(config);
        }
    }

    // Memory freed by OS
}

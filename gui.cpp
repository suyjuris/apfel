
#define PROGRAM_NAME "cppsat"
#define PROGRAM_TITLE "Belt Balancers"

#ifndef PLATFORM_INCLUDES
#error "You did not compile the right file."
#endif

#include "hashmap.cpp"
#include "asset.cpp"
#include "opengl.cpp"
#include "spline.cpp"
#include "font.cpp"

#include "array_linux.cpp"
#include "cppsat.cpp"


// Keeps the necessary data to manage OpenGL and other data for the uil layer
struct Application {
    // Data required by the platform layer
    Array_dyn<Key> input_queue;

    // Other data
    Asset_store assets;
    bool asset_is_pack = false;
    
    Shader circle, spline;

    s64 screen_w = 0, screen_h = 0;
    float scale = 1.f; // Ratio of world-coordinates and pixels (world * scale = pixels)

    Font_data fonts;
    s64 font_sans_base, font_sans;
};

void application_handle_resize(Application* context, s64 width, s64 height) {
    if (width  != -1) context->screen_w = width;
    if (height != -1) context->screen_h = height;

    glViewport(0.0, 0.0, context->screen_w, context->screen_h);
}

void application_init_assets(Application* context, bool flag_pack_assets) {
    context->asset_is_pack = asset_init(&context->assets);
    if (not context->asset_is_pack) {
        asset_load_source(&context->assets, "gui.cpp"_arr);
        asset_load_source(&context->assets, "font.cpp"_arr);
        asset_load_source(&context->assets, "spline.cpp"_arr);
        asset_load_file(&context->assets, "font"_arr, "fonts_stripped/DejaVuSans.ttf"_arr);
        asset_load_file(&context->assets, "font_license"_arr, "fonts_stripped/LICENSE"_arr);
    }
    asset_finalize(&context->assets, flag_pack_assets);
}

void application_init(Application* context) {
    // Initialise the fonts
    font_init(&context->fonts, &context->assets, GL_TEXTURE2);
    context->font_sans_base = font_add(&context->fonts, asset_get(&context->assets, "font"_arr));
    context->font_sans = font_instantiate(&context->fonts, context->font_sans_base, 15.f);
    font_instantiate_defaults(&context->fonts, context->font_sans_base,
        context->font_sans_base, context->font_sans_base, context->font_sans_base);

    font_instance_scale_defaults(&context->fonts, 0.75f);
    
    // Now we load our shaders
    context->spline = opengl_shader_assets(&context->assets, "spline"_arr);
}

void application_render(Application* context) {
    for (Key i: context->input_queue) {
        if (i.type == Key::SPECIAL and i.special == Key::C_QUIT) {
            exit(0);
        }
    }
    context->input_queue.size = 0;

    opengl_clear_color(Palette::BG);
    
    glClear(GL_COLOR_BUFFER_BIT);
    glClear(GL_DEPTH_BUFFER_BIT);
}


void application_print_help(Application* context, char* argv0) {
    printf("Usage:\n  %s [--fullscreen]\n", argv0);
    if (context->asset_is_pack) {
        printf("  %s --font-license\n", argv0);
    } else {
        printf("  %s --pack\n", argv0);
    }
    printf("  %s --help\n\n", argv0);

    puts("This is " PROGRAM_NAME " written by Philipp Czerner in 2019. Running the program without any arguments starts the GUI, which is the main part of this application.\n");
    puts("  --fullscreen  You may be able to guess the meaning of this option from context.");
    if (context->asset_is_pack) {
        puts("  --font-license  You are running the packed version of " PROGRAM_NAME ", which means that the font data is included in the binary. To view the license under which the fonts are distributed, run " PROGRAM_NAME " with the --font-license option.");
    } else {
        puts("  --pack  You are running the unpacked version of the binary, which means that it will load assets from its CWD. (The source files may contain assets as well.) You can run this programm with the '--pack' option, which will create a '" PROGRAM_NAME "_packed' binary in the CWD that contains the asset data.");
    }
    puts("  --help  Prints this help.");
}

void application_print_license(Application* context) {
    Array_t<u8> license = asset_get(&context->assets, "font_license"_arr);
    fwrite(license.data, 1, license.size, stdout);
    puts("");
}

int old_main() {
    Array_dyn<s64> yoff_output, yoff_input;
    array_append(&yoff_output, {0});
    array_append(&yoff_input, {0, 1});
    Factorio_params p {3, 2, 10, 1, false, yoff_output, yoff_input};
    
    Sat_instance inst;
    sat_init(&inst);
    inst.debug_forbidden_id = 0x2880118012300000ull;

    factorio_balancer(&inst, p);

    Sat_dimacs dimacs;
    sat_write_dimacs(&inst, &dimacs);

    fwrite(dimacs.text.data, 1, dimacs.text.size, stdout);

    Array_dyn<u8> human;
    sat_write_human(&inst, &human);
    //fwrite(human.data, 1, human.size, stdout);
    return 0;
}

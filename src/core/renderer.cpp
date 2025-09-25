/* Basic OpenGL text renderer implementation */
#include "vizero/renderer.h"
#include "vizero/window.h"
#include "vizero/file_utils.h"
#include <GL/glew.h>
#include <SDL_image.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Simple bitmap font data - 8x16 monospace characters */
static const int CHAR_WIDTH = 8;
static const int CHAR_HEIGHT = 16;
static const int FONT_COLS = 16;
static const int FONT_ROWS = 16;

struct vizero_renderer_t { 
    GLuint shader_program;
    GLuint vao, vbo;
    GLint mvp_uniform;
    GLint colour_uniform;
    GLint char_uniform;
    GLint font_texture_uniform;
    GLint use_font_texture_uniform;
    GLuint font_texture;
    float projection[16];
    int width, height;
};

struct vizero_font_t { 
    GLuint texture;
    int char_width;
    int char_height;
};

/* Simple vertex shader for 2D text rendering */
static const char* vertex_shader_source = 
"#version 330 core\n"
"layout (location = 0) in vec2 aPos;\n"
"layout (location = 1) in vec2 aTexCoord;\n"
"uniform mat4 uMVP;\n"
"out vec2 TexCoord;\n"
"void main() {\n"
"    gl_Position = uMVP * vec4(aPos, 0.0, 1.0);\n"
"    TexCoord = aTexCoord;\n"
"}\n";

/* Fragment shader for bitmap font rendering */
static const char* fragment_shader_source = 
"#version 330 core\n"
"in vec2 TexCoord;\n"
"uniform vec4 uColour;\n"
"uniform int uChar;\n"
"uniform sampler2D uFontTexture;\n"
"uniform int uUseFontTexture;\n"
"out vec4 FragColour;\n"
"\n"
"void main() {\n"
"    if (uUseFontTexture == 1 && uChar > 0) {\n"
"        // Font atlas is 32 columns x 8 rows (256x128 pixels)\n"
"        // Each character is 8x16 pixels\n"
"        // ASCII value directly maps to character position\n"
"        int char_col = uChar % 32;\n"
"        int char_row = uChar / 32;\n"
"        \n"
"        // Calculate texture coordinates in atlas\n"
"        float atlas_x = (float(char_col) + TexCoord.x) / 32.0;\n"
"        // Flip Y coordinate since BMP is stored bottom-to-top, and flip row order\n"
"        float atlas_y = (float(7 - char_row) + (1.0 - TexCoord.y)) / 8.0;\n"
"        \n"
"        // Sample the font texture\n"
"        vec4 font_sample = texture(uFontTexture, vec2(atlas_x, atlas_y));\n"
"        \n"
"        // Use the red channel as alpha (assuming white font on black background)\n"
"        float alpha = font_sample.r;\n"
"        if (alpha < 0.5) {\n"
"            discard;\n"
"        }\n"
"        \n"
"        FragColour = vec4(uColour.rgb, uColour.a * alpha);\n"
"    } else if (uUseFontTexture == 1 && uChar < 0) {\n"
"        // Direct texture rendering (images)\n"
"        vec4 tex_sample = texture(uFontTexture, TexCoord);\n"
"        FragColour = tex_sample * uColour;\n"
"    } else {\n"
"        // Non-font rendering (rectangles, lines)\n"
"        FragColour = uColour;\n"
"    }\n"
"}\n";

/* Compile a shader */
static GLuint compile_shader(GLenum type, const char* source) {
    GLuint shader = glCreateShader(type);
    glShaderSource(shader, 1, &source, NULL);
    glCompileShader(shader);
    
    GLint success;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
    if (!success) {
        char infoLog[512];
        glGetShaderInfoLog(shader, 512, NULL, infoLog);
        printf("Shader compilation failed: %s\n", infoLog);
        glDeleteShader(shader);
        return 0;
    }
    
    return shader;
}

/* Create orthographic projection matrix */
static void create_ortho_matrix(float* matrix, float left, float right, float bottom, float top) {
    memset(matrix, 0, 16 * sizeof(float));
    matrix[0] = 2.0f / (right - left);
    matrix[5] = 2.0f / (top - bottom);
    matrix[10] = -1.0f;
    matrix[12] = -(right + left) / (right - left);
    matrix[13] = -(top + bottom) / (top - bottom);
    matrix[15] = 1.0f;
}

/* Simple BMP loader for font texture */
static GLuint load_bmp_texture(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        printf("Failed to open font file: %s\n", filename);
        return 0;
    }
    
    /* Read BMP header */
    unsigned char header[54];
    if (fread(header, 1, 54, file) != 54) {
        printf("Invalid BMP file: %s\n", filename);
        fclose(file);
        return 0;
    }
    
    /* Get image info */
    int width = *(int*)&header[18];
    int height = *(int*)&header[22];
    int bpp = *(int*)&header[28];
    
    if (bpp != 24) {
        printf("Only 24-bit BMP files are supported\n");
        fclose(file);
        return 0;
    }
    
    /* Calculate image size */
    int image_size = width * height * 3;
    unsigned char* data = (unsigned char*)malloc(image_size);
    
    /* Read image data */
    fseek(file, 54, SEEK_SET);
    if (fread(data, 1, image_size, file) != (size_t)image_size) {
        printf("Failed to read BMP data\n");
        free(data);
        fclose(file);
        return 0;
    }
    fclose(file);
    
    /* Convert BGR to RGB */
    for (int i = 0; i < image_size; i += 3) {
        unsigned char temp = data[i];
        data[i] = data[i + 2];
        data[i + 2] = temp;
    }
    
    /* Create OpenGL texture */
    GLuint texture;
    glGenTextures(1, &texture);
    glBindTexture(GL_TEXTURE_2D, texture);
    
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    
    /* Upload texture data (flip Y coordinate) */
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
    
    free(data);
    return texture;
}

vizero_renderer_t* vizero_renderer_create(vizero_window_t* window) {
    if (!window) return NULL;
    
    vizero_renderer_t* renderer = (vizero_renderer_t*)calloc(1, sizeof(vizero_renderer_t));
    if (!renderer) return NULL;
    
    /* Initialize GLEW */
    if (glewInit() != GLEW_OK) {
        printf("Failed to initialize GLEW\n");
        free(renderer);
        return NULL;
    }
    
    /* Create shaders */
    GLuint vertex_shader = compile_shader(GL_VERTEX_SHADER, vertex_shader_source);
    GLuint fragment_shader = compile_shader(GL_FRAGMENT_SHADER, fragment_shader_source);
    
    if (!vertex_shader || !fragment_shader) {
        if (vertex_shader) glDeleteShader(vertex_shader);
        if (fragment_shader) glDeleteShader(fragment_shader);
        free(renderer);
        return NULL;
    }
    
    /* Create shader program */
    renderer->shader_program = glCreateProgram();
    glAttachShader(renderer->shader_program, vertex_shader);
    glAttachShader(renderer->shader_program, fragment_shader);
    glLinkProgram(renderer->shader_program);
    
    GLint success;
    glGetProgramiv(renderer->shader_program, GL_LINK_STATUS, &success);
    if (!success) {
        char infoLog[512];
        glGetProgramInfoLog(renderer->shader_program, 512, NULL, infoLog);
        printf("Shader program linking failed: %s\n", infoLog);
        glDeleteProgram(renderer->shader_program);
        glDeleteShader(vertex_shader);
        glDeleteShader(fragment_shader);
        free(renderer);
        return NULL;
    }
    
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);
    
    /* Get uniform locations */
    renderer->mvp_uniform = glGetUniformLocation(renderer->shader_program, "uMVP");
    renderer->colour_uniform = glGetUniformLocation(renderer->shader_program, "uColour");
    renderer->char_uniform = glGetUniformLocation(renderer->shader_program, "uChar");
    renderer->font_texture_uniform = glGetUniformLocation(renderer->shader_program, "uFontTexture");
    renderer->use_font_texture_uniform = glGetUniformLocation(renderer->shader_program, "uUseFontTexture");
    
    /* Load font texture */
    char* font_path = vizero_get_resource_path("fonts/whitefont.bmp");
    if (font_path) {
        renderer->font_texture = load_bmp_texture(font_path);
        free(font_path);
    } else {
        renderer->font_texture = 0;
    }
    
    if (!renderer->font_texture) {
        printf("Warning: Failed to load font texture, text will not render properly\n");
    }
    
    /* Create VAO and VBO for text quads */
    glGenVertexArrays(1, &renderer->vao);
    glGenBuffers(1, &renderer->vbo);
    
    glBindVertexArray(renderer->vao);
    glBindBuffer(GL_ARRAY_BUFFER, renderer->vbo);
    
    /* Position attribute */
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);
    
    /* Texture coordinate attribute */
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)(2 * sizeof(float)));
    glEnableVertexAttribArray(1);
    
    /* Set initial viewport */
    int width, height;
    vizero_window_get_size(window, &width, &height);
    renderer->width = width;
    renderer->height = height;
    glViewport(0, 0, width, height);
    
    /* Create projection matrix */
    create_ortho_matrix(renderer->projection, 0.0f, (float)width, (float)height, 0.0f);
    
    /* Enable blending for text */
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
    return renderer;
}

void vizero_renderer_destroy(vizero_renderer_t* renderer) {
    if (renderer) {
        if (renderer->shader_program) glDeleteProgram(renderer->shader_program);
        if (renderer->vao) glDeleteVertexArrays(1, &renderer->vao);
        if (renderer->vbo) glDeleteBuffers(1, &renderer->vbo);
        if (renderer->font_texture) glDeleteTextures(1, &renderer->font_texture);
        free(renderer);
    }
}

void vizero_renderer_clear(vizero_renderer_t* renderer, vizero_colour_t colour) {
    if (!renderer) return;
    glClearColor(colour.r, colour.g, colour.b, colour.a);
    glClear(GL_COLOR_BUFFER_BIT);
}

void vizero_renderer_present(vizero_renderer_t* renderer) {
    (void)renderer;
    /* SwapBuffers is called by the window */
}

void vizero_renderer_update_viewport(vizero_renderer_t* renderer, int width, int height) {
    if (!renderer) return;
    
    /* Update OpenGL viewport */
    glViewport(0, 0, width, height);
    
    /* Update projection matrix for 2D rendering */
    renderer->projection[0] = 2.0f / width;
    renderer->projection[5] = -2.0f / height;
    renderer->projection[12] = -1.0f;
    renderer->projection[13] = 1.0f;
    renderer->projection[15] = 1.0f;
}

void vizero_renderer_draw_text(vizero_renderer_t* renderer, const char* text, vizero_text_info_t* info) {
    if (!renderer || !text || !info) return;
    
    glUseProgram(renderer->shader_program);
    glUniformMatrix4fv(renderer->mvp_uniform, 1, GL_FALSE, renderer->projection);
    glUniform4f(renderer->colour_uniform, info->colour.r, info->colour.g, info->colour.b, info->colour.a);
    glUniform1i(renderer->use_font_texture_uniform, renderer->font_texture ? 1 : 0);
    
    /* Bind font texture if available */
    if (renderer->font_texture) {
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, renderer->font_texture);
        glUniform1i(renderer->font_texture_uniform, 0);
    }
    
    glBindVertexArray(renderer->vao);
    
    float x = info->x;
    float y = info->y;
    
    /* Render each character */
    for (const char* c = text; *c; c++) {
        if (*c == '\n') {
            x = info->x;
            y += CHAR_HEIGHT;
            continue;
        }

        if (*c == '\t') {
            x += CHAR_WIDTH * 4; /* Tab = 4 spaces */
            continue;
        }

        /* Handle Unicode/non-ASCII characters by replacing with '?' */
        unsigned char char_code = (unsigned char)*c;
        if (char_code > 127) {
            /* Multi-byte UTF-8 sequence - skip additional bytes and use '?' */
            char_code = '?';
            
            /* Skip UTF-8 continuation bytes (0x80-0xBF) */
            while (*(c + 1) && ((unsigned char)*(c + 1) & 0xC0) == 0x80) {
                c++;
            }
        }        /* Create quad for character */
        float vertices[] = {
            x,              y,              0.0f, 0.0f,  /* Top-left */
            x + CHAR_WIDTH, y,              1.0f, 0.0f,  /* Top-right */
            x,              y + CHAR_HEIGHT, 0.0f, 1.0f,  /* Bottom-left */
            x + CHAR_WIDTH, y + CHAR_HEIGHT, 1.0f, 1.0f   /* Bottom-right */
        };
        
        glBindBuffer(GL_ARRAY_BUFFER, renderer->vbo);
        glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_DYNAMIC_DRAW);

        /* Set character code for shader - use processed char_code */
        glUniform1i(renderer->char_uniform, (int)char_code);

        /* Draw character */
        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
        
        x += CHAR_WIDTH;
    }
}

void vizero_renderer_get_text_size(vizero_renderer_t* renderer, const char* text, vizero_font_t* font, float* width, float* height) {
    (void)renderer; (void)font;
    if (!text) {
        if (width) *width = 0.0f;
        if (height) *height = 0.0f;
        return;
    }
    
    int max_width = 0;
    int current_width = 0;
    int lines = 1;
    
    for (const char* c = text; *c; c++) {
        if (*c == '\n') {
            if (current_width > max_width) max_width = current_width;
            current_width = 0;
            lines++;
        } else if (*c == '\t') {
            current_width += 4;
        } else {
            current_width++;
        }
    }
    
    if (current_width > max_width) max_width = current_width;
    
    if (width) *width = (float)(max_width * CHAR_WIDTH);
    if (height) *height = (float)(lines * CHAR_HEIGHT);
}

void vizero_renderer_draw_rect(vizero_renderer_t* renderer, float x, float y, float width, float height, vizero_colour_t colour) {
    if (!renderer) return;
    
    glUseProgram(renderer->shader_program);
    glUniformMatrix4fv(renderer->mvp_uniform, 1, GL_FALSE, renderer->projection);
    glUniform4f(renderer->colour_uniform, colour.r, colour.g, colour.b, colour.a);
    glUniform1i(renderer->char_uniform, 0); /* No character */
    glUniform1i(renderer->use_font_texture_uniform, 0); /* No font texture */
    
    /* Draw rectangle outline */
    float vertices[] = {
        x,         y,          0.0f, 0.0f,
        x + width, y,          1.0f, 0.0f,
        x + width, y + height, 1.0f, 1.0f,
        x,         y + height, 0.0f, 1.0f,
        x,         y,          0.0f, 0.0f
    };
    
    glBindVertexArray(renderer->vao);
    glBindBuffer(GL_ARRAY_BUFFER, renderer->vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_DYNAMIC_DRAW);
    glDrawArrays(GL_LINE_STRIP, 0, 5);
}

void vizero_renderer_fill_rect(vizero_renderer_t* renderer, float x, float y, float width, float height, vizero_colour_t colour) {
    if (!renderer) return;
    
    glUseProgram(renderer->shader_program);
    glUniformMatrix4fv(renderer->mvp_uniform, 1, GL_FALSE, renderer->projection);
    glUniform4f(renderer->colour_uniform, colour.r, colour.g, colour.b, colour.a);
    glUniform1i(renderer->char_uniform, 0); /* No character */
    glUniform1i(renderer->use_font_texture_uniform, 0); /* No font texture */
    
    /* Draw filled rectangle */
    float vertices[] = {
        x,         y,          0.0f, 0.0f,
        x + width, y,          1.0f, 0.0f,
        x,         y + height, 0.0f, 1.0f,
        x + width, y + height, 1.0f, 1.0f
    };
    
    glBindVertexArray(renderer->vao);
    glBindBuffer(GL_ARRAY_BUFFER, renderer->vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_DYNAMIC_DRAW);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
}

void vizero_renderer_draw_line(vizero_renderer_t* renderer, float x1, float y1, float x2, float y2, vizero_colour_t colour) {
    if (!renderer) return;
    
    glUseProgram(renderer->shader_program);
    glUniformMatrix4fv(renderer->mvp_uniform, 1, GL_FALSE, renderer->projection);
    glUniform4f(renderer->colour_uniform, colour.r, colour.g, colour.b, colour.a);
    glUniform1i(renderer->char_uniform, 0); /* No character */
    glUniform1i(renderer->use_font_texture_uniform, 0); /* No font texture */
    
    float vertices[] = {
        x1, y1, 0.0f, 0.0f,
        x2, y2, 1.0f, 1.0f
    };
    
    glBindVertexArray(renderer->vao);
    glBindBuffer(GL_ARRAY_BUFFER, renderer->vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_DYNAMIC_DRAW);
    glDrawArrays(GL_LINES, 0, 2);
}

vizero_font_t* vizero_font_load(const char* path, int size) {
    (void)path; (void)size;
    return (vizero_font_t*)calloc(1, sizeof(vizero_font_t));
}

void vizero_font_destroy(vizero_font_t* font) {
    free(font);
}

/* Image structure is now defined in header */

/* Image loading and rendering */
vizero_image_t* vizero_image_load(const char* path) {
    if (!path) return NULL;
    
    /* Initialize SDL_image if not already done */
    static int img_initialized = 0;
    if (!img_initialized) {
        if (IMG_Init(IMG_INIT_PNG | IMG_INIT_JPG) == 0) {
            fprintf(stderr, "Failed to initialize SDL_image: %s\n", IMG_GetError());
            return NULL;
        }
        img_initialized = 1;
    }
    
    /* Load image surface */
    SDL_Surface* surface = IMG_Load(path);
    if (!surface) {
        fprintf(stderr, "Failed to load image %s: %s\n", path, IMG_GetError());
        return NULL;
    }
    
    /* Create image structure */
    vizero_image_t* image = (vizero_image_t*)calloc(1, sizeof(vizero_image_t));
    if (!image) {
        SDL_FreeSurface(surface);
        return NULL;
    }
    
    image->width = surface->w;
    image->height = surface->h;
    
    /* Create OpenGL texture */
    GLuint texture;
    glGenTextures(1, &texture);
    image->texture = texture;
    glBindTexture(GL_TEXTURE_2D, texture);
    
    /* Set texture parameters */
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    
    /* Convert surface to a consistent format (RGBA) */
    SDL_Surface* converted_surface = SDL_ConvertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA32, 0);
    if (!converted_surface) {
        fprintf(stderr, "Failed to convert surface format: %s\n", SDL_GetError());
        SDL_FreeSurface(surface);
        free(image);
        return NULL;
    }
    
    /* Upload texture data */
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, converted_surface->w, converted_surface->h, 0, GL_RGBA, GL_UNSIGNED_BYTE, converted_surface->pixels);
    
    SDL_FreeSurface(converted_surface);
    
    SDL_FreeSurface(surface);
    return image;
}

void vizero_image_destroy(vizero_image_t* image) {
    if (image) {
        if (image->texture) {
            GLuint texture = image->texture;
            glDeleteTextures(1, &texture);
        }
        free(image);
    }
}

void vizero_renderer_draw_image(vizero_renderer_t* renderer, vizero_image_t* image, float x, float y, float width, float height) {
    if (!renderer || !image) return;
    
    /* Use the existing shader but set it up for direct texture rendering */
    glUseProgram(renderer->shader_program);
    
    /* Set uniforms */
    glUniformMatrix4fv(renderer->mvp_uniform, 1, GL_FALSE, renderer->projection);
    glUniform4f(renderer->colour_uniform, 1.0f, 1.0f, 1.0f, 1.0f); /* White tint */
    glUniform1i(renderer->use_font_texture_uniform, 1); /* Use texture */
    glUniform1i(renderer->char_uniform, -1); /* Set to -1 to bypass font atlas logic */
    
    /* Bind image texture */
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, (GLuint)image->texture);
    glUniform1i(renderer->font_texture_uniform, 0);
    
    /* Set up vertices for a textured quad - flip V coordinates to fix upside down issue */
    float vertices[] = {
        /* Position (x,y)  Texture (u,v) */
        x,         y,          0.0f, 0.0f,  /* Bottom left */
        x + width, y,          1.0f, 0.0f,  /* Bottom right */
        x,         y + height, 0.0f, 1.0f,  /* Top left */
        x + width, y + height, 1.0f, 1.0f   /* Top right */
    };
    
    /* Upload vertices */
    glBindVertexArray(renderer->vao);
    glBindBuffer(GL_ARRAY_BUFFER, renderer->vbo);
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(vertices), vertices);
    
    /* Draw quad */
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
}

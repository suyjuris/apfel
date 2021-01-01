// Auto-generated OpenGL pointer initialisation, see generate_opengl.py
#define glActiveTexture _glActiveTexture
#define glBindBuffer _glBindBuffer
#define glGenBuffers _glGenBuffers
#define glBufferData _glBufferData
#define glBufferSubData _glBufferSubData
#define glAttachShader _glAttachShader
#define glBindAttribLocation _glBindAttribLocation
#define glCompileShader _glCompileShader
#define glCreateProgram _glCreateProgram
#define glCreateShader _glCreateShader
#define glEnableVertexAttribArray _glEnableVertexAttribArray
#define glGetProgramiv _glGetProgramiv
#define glGetProgramInfoLog _glGetProgramInfoLog
#define glGetShaderiv _glGetShaderiv
#define glGetShaderInfoLog _glGetShaderInfoLog
#define glGetUniformLocation _glGetUniformLocation
#define glLinkProgram _glLinkProgram
#define glShaderSource _glShaderSource
#define glUseProgram _glUseProgram
#define glUniform1f _glUniform1f
#define glUniform2f _glUniform2f
#define glUniform3f _glUniform3f
#define glUniform4f _glUniform4f
#define glUniform1i _glUniform1i
#define glUniform2i _glUniform2i
#define glUniform3i _glUniform3i
#define glUniform4i _glUniform4i
#define glVertexAttribPointer _glVertexAttribPointer
#define glVertexAttribIPointer _glVertexAttribIPointer
#define glBindFramebuffer _glBindFramebuffer
#define glGenFramebuffers _glGenFramebuffers
#define glBindVertexArray _glBindVertexArray
#define glGenVertexArrays _glGenVertexArrays
#define glDrawArraysInstanced _glDrawArraysInstanced
#define glTexBuffer _glTexBuffer
#define glGetBufferParameteri64v _glGetBufferParameteri64v
#define glFramebufferTexture _glFramebufferTexture
#define glVertexAttribDivisor _glVertexAttribDivisor
#define glVertexAttribLPointer _glVertexAttribLPointer
typedef void (*glActiveTexture_t) (GLenum texture);
typedef void (*glBindBuffer_t) (GLenum target, GLuint buffer);
typedef void (*glGenBuffers_t) (GLsizei n, GLuint *buffers);
typedef void (*glBufferData_t) (GLenum target, GLsizeiptr size, const void *data, GLenum usage);
typedef void (*glBufferSubData_t) (GLenum target, GLintptr offset, GLsizeiptr size, const void *data);
typedef void (*glAttachShader_t) (GLuint program, GLuint shader);
typedef void (*glBindAttribLocation_t) (GLuint program, GLuint index, const GLchar *name);
typedef void (*glCompileShader_t) (GLuint shader);
typedef GLuint (*glCreateProgram_t) (void);
typedef GLuint (*glCreateShader_t) (GLenum type);
typedef void (*glEnableVertexAttribArray_t) (GLuint index);
typedef void (*glGetProgramiv_t) (GLuint program, GLenum pname, GLint *params);
typedef void (*glGetProgramInfoLog_t) (GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
typedef void (*glGetShaderiv_t) (GLuint shader, GLenum pname, GLint *params);
typedef void (*glGetShaderInfoLog_t) (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
typedef GLint (*glGetUniformLocation_t) (GLuint program, const GLchar *name);
typedef void (*glLinkProgram_t) (GLuint program);
typedef void (*glShaderSource_t) (GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length);
typedef void (*glUseProgram_t) (GLuint program);
typedef void (*glUniform1f_t) (GLint location, GLfloat v0);
typedef void (*glUniform2f_t) (GLint location, GLfloat v0, GLfloat v1);
typedef void (*glUniform3f_t) (GLint location, GLfloat v0, GLfloat v1, GLfloat v2);
typedef void (*glUniform4f_t) (GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3);
typedef void (*glUniform1i_t) (GLint location, GLint v0);
typedef void (*glUniform2i_t) (GLint location, GLint v0, GLint v1);
typedef void (*glUniform3i_t) (GLint location, GLint v0, GLint v1, GLint v2);
typedef void (*glUniform4i_t) (GLint location, GLint v0, GLint v1, GLint v2, GLint v3);
typedef void (*glVertexAttribPointer_t) (GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const void *pointer);
typedef void (*glVertexAttribIPointer_t) (GLuint index, GLint size, GLenum type, GLsizei stride, const void *pointer);
typedef void (*glBindFramebuffer_t) (GLenum target, GLuint framebuffer);
typedef void (*glGenFramebuffers_t) (GLsizei n, GLuint *framebuffers);
typedef void (*glBindVertexArray_t) (GLuint array);
typedef void (*glGenVertexArrays_t) (GLsizei n, GLuint *arrays);
typedef void (*glDrawArraysInstanced_t) (GLenum mode, GLint first, GLsizei count, GLsizei instancecount);
typedef void (*glTexBuffer_t) (GLenum target, GLenum internalformat, GLuint buffer);
typedef void (*glGetBufferParameteri64v_t) (GLenum target, GLenum pname, GLint64 *params);
typedef void (*glFramebufferTexture_t) (GLenum target, GLenum attachment, GLuint texture, GLint level);
typedef void (*glVertexAttribDivisor_t) (GLuint index, GLuint divisor);
typedef void (*glVertexAttribLPointer_t) (GLuint index, GLint size, GLenum type, GLsizei stride, const void *pointer);
glActiveTexture_t glActiveTexture;
glBindBuffer_t glBindBuffer;
glGenBuffers_t glGenBuffers;
glBufferData_t glBufferData;
glBufferSubData_t glBufferSubData;
glAttachShader_t glAttachShader;
glBindAttribLocation_t glBindAttribLocation;
glCompileShader_t glCompileShader;
glCreateProgram_t glCreateProgram;
glCreateShader_t glCreateShader;
glEnableVertexAttribArray_t glEnableVertexAttribArray;
glGetProgramiv_t glGetProgramiv;
glGetProgramInfoLog_t glGetProgramInfoLog;
glGetShaderiv_t glGetShaderiv;
glGetShaderInfoLog_t glGetShaderInfoLog;
glGetUniformLocation_t glGetUniformLocation;
glLinkProgram_t glLinkProgram;
glShaderSource_t glShaderSource;
glUseProgram_t glUseProgram;
glUniform1f_t glUniform1f;
glUniform2f_t glUniform2f;
glUniform3f_t glUniform3f;
glUniform4f_t glUniform4f;
glUniform1i_t glUniform1i;
glUniform2i_t glUniform2i;
glUniform3i_t glUniform3i;
glUniform4i_t glUniform4i;
glVertexAttribPointer_t glVertexAttribPointer;
glVertexAttribIPointer_t glVertexAttribIPointer;
glBindFramebuffer_t glBindFramebuffer;
glGenFramebuffers_t glGenFramebuffers;
glBindVertexArray_t glBindVertexArray;
glGenVertexArrays_t glGenVertexArrays;
glDrawArraysInstanced_t glDrawArraysInstanced;
glTexBuffer_t glTexBuffer;
glGetBufferParameteri64v_t glGetBufferParameteri64v;
glFramebufferTexture_t glFramebufferTexture;
glVertexAttribDivisor_t glVertexAttribDivisor;
glVertexAttribLPointer_t glVertexAttribLPointer;

void _platform_init_gl_pointers() {
    glActiveTexture = (glActiveTexture_t)glXGetProcAddress((u8*)"glActiveTexture"); assert(glActiveTexture);
    glBindBuffer = (glBindBuffer_t)glXGetProcAddress((u8*)"glBindBuffer"); assert(glBindBuffer);
    glGenBuffers = (glGenBuffers_t)glXGetProcAddress((u8*)"glGenBuffers"); assert(glGenBuffers);
    glBufferData = (glBufferData_t)glXGetProcAddress((u8*)"glBufferData"); assert(glBufferData);
    glBufferSubData = (glBufferSubData_t)glXGetProcAddress((u8*)"glBufferSubData"); assert(glBufferSubData);
    glAttachShader = (glAttachShader_t)glXGetProcAddress((u8*)"glAttachShader"); assert(glAttachShader);
    glBindAttribLocation = (glBindAttribLocation_t)glXGetProcAddress((u8*)"glBindAttribLocation"); assert(glBindAttribLocation);
    glCompileShader = (glCompileShader_t)glXGetProcAddress((u8*)"glCompileShader"); assert(glCompileShader);
    glCreateProgram = (glCreateProgram_t)glXGetProcAddress((u8*)"glCreateProgram"); assert(glCreateProgram);
    glCreateShader = (glCreateShader_t)glXGetProcAddress((u8*)"glCreateShader"); assert(glCreateShader);
    glEnableVertexAttribArray = (glEnableVertexAttribArray_t)glXGetProcAddress((u8*)"glEnableVertexAttribArray"); assert(glEnableVertexAttribArray);
    glGetProgramiv = (glGetProgramiv_t)glXGetProcAddress((u8*)"glGetProgramiv"); assert(glGetProgramiv);
    glGetProgramInfoLog = (glGetProgramInfoLog_t)glXGetProcAddress((u8*)"glGetProgramInfoLog"); assert(glGetProgramInfoLog);
    glGetShaderiv = (glGetShaderiv_t)glXGetProcAddress((u8*)"glGetShaderiv"); assert(glGetShaderiv);
    glGetShaderInfoLog = (glGetShaderInfoLog_t)glXGetProcAddress((u8*)"glGetShaderInfoLog"); assert(glGetShaderInfoLog);
    glGetUniformLocation = (glGetUniformLocation_t)glXGetProcAddress((u8*)"glGetUniformLocation"); assert(glGetUniformLocation);
    glLinkProgram = (glLinkProgram_t)glXGetProcAddress((u8*)"glLinkProgram"); assert(glLinkProgram);
    glShaderSource = (glShaderSource_t)glXGetProcAddress((u8*)"glShaderSource"); assert(glShaderSource);
    glUseProgram = (glUseProgram_t)glXGetProcAddress((u8*)"glUseProgram"); assert(glUseProgram);
    glUniform1f = (glUniform1f_t)glXGetProcAddress((u8*)"glUniform1f"); assert(glUniform1f);
    glUniform2f = (glUniform2f_t)glXGetProcAddress((u8*)"glUniform2f"); assert(glUniform2f);
    glUniform3f = (glUniform3f_t)glXGetProcAddress((u8*)"glUniform3f"); assert(glUniform3f);
    glUniform4f = (glUniform4f_t)glXGetProcAddress((u8*)"glUniform4f"); assert(glUniform4f);
    glUniform1i = (glUniform1i_t)glXGetProcAddress((u8*)"glUniform1i"); assert(glUniform1i);
    glUniform2i = (glUniform2i_t)glXGetProcAddress((u8*)"glUniform2i"); assert(glUniform2i);
    glUniform3i = (glUniform3i_t)glXGetProcAddress((u8*)"glUniform3i"); assert(glUniform3i);
    glUniform4i = (glUniform4i_t)glXGetProcAddress((u8*)"glUniform4i"); assert(glUniform4i);
    glVertexAttribPointer = (glVertexAttribPointer_t)glXGetProcAddress((u8*)"glVertexAttribPointer"); assert(glVertexAttribPointer);
    glVertexAttribIPointer = (glVertexAttribIPointer_t)glXGetProcAddress((u8*)"glVertexAttribIPointer"); assert(glVertexAttribIPointer);
    glBindFramebuffer = (glBindFramebuffer_t)glXGetProcAddress((u8*)"glBindFramebuffer"); assert(glBindFramebuffer);
    glGenFramebuffers = (glGenFramebuffers_t)glXGetProcAddress((u8*)"glGenFramebuffers"); assert(glGenFramebuffers);
    glBindVertexArray = (glBindVertexArray_t)glXGetProcAddress((u8*)"glBindVertexArray"); assert(glBindVertexArray);
    glGenVertexArrays = (glGenVertexArrays_t)glXGetProcAddress((u8*)"glGenVertexArrays"); assert(glGenVertexArrays);
    glDrawArraysInstanced = (glDrawArraysInstanced_t)glXGetProcAddress((u8*)"glDrawArraysInstanced"); assert(glDrawArraysInstanced);
    glTexBuffer = (glTexBuffer_t)glXGetProcAddress((u8*)"glTexBuffer"); assert(glTexBuffer);
    glGetBufferParameteri64v = (glGetBufferParameteri64v_t)glXGetProcAddress((u8*)"glGetBufferParameteri64v"); assert(glGetBufferParameteri64v);
    glFramebufferTexture = (glFramebufferTexture_t)glXGetProcAddress((u8*)"glFramebufferTexture"); assert(glFramebufferTexture);
    glVertexAttribDivisor = (glVertexAttribDivisor_t)glXGetProcAddress((u8*)"glVertexAttribDivisor"); assert(glVertexAttribDivisor);
    glVertexAttribLPointer = (glVertexAttribLPointer_t)glXGetProcAddress((u8*)"glVertexAttribLPointer"); assert(glVertexAttribLPointer);
}

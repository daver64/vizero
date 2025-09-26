# Lisp REPL Plugin CMakeLists.txt

# Find required libraries for process management
if(WIN32)
    # Windows process management uses built-in APIs
    set(PROCESS_LIBS "")
else()
    # Unix process management  
    set(PROCESS_LIBS "")
endif()

# Create Lisp REPL plugin
add_vizero_plugin(lisp_repl
    lisp_repl_plugin.c
)

# Link any additional libraries
if(PROCESS_LIBS)
    target_link_libraries(lisp_repl PRIVATE ${PROCESS_LIBS})
endif()

# Platform-specific compile definitions
if(WIN32)
    target_compile_definitions(lisp_repl PRIVATE _WIN32_WINNT=0x0600)
endif()

message(STATUS "Lisp REPL plugin: Configured for ${CMAKE_SYSTEM_NAME}")
# SQL REPL Plugin CMakeLists.txt

# Find database client libraries
set(SQL_LIBS "")
set(SQL_INCLUDES "")

# Try to find MySQL/MariaDB client library
find_path(MYSQL_INCLUDE_DIR mysql.h
    PATHS
    /usr/include/mysql
    /usr/local/include/mysql
    /opt/homebrew/include/mysql
    /usr/include/mariadb
    /usr/local/include/mariadb
    "C:/Program Files/MySQL/MySQL Server*/include"
    "C:/Program Files (x86)/MySQL/MySQL Server*/include"
    "C:/mysql/include"
    "C:/mariadb/include"
)

find_library(MYSQL_LIBRARY
    NAMES mysqlclient mysql mariadb
    PATHS
    /usr/lib
    /usr/local/lib
    /opt/homebrew/lib
    /usr/lib/x86_64-linux-gnu
    "C:/Program Files/MySQL/MySQL Server*/lib"
    "C:/Program Files (x86)/MySQL/MySQL Server*/lib"
    "C:/mysql/lib"
    "C:/mariadb/lib"
)

# Try to find PostgreSQL client library
find_path(PGSQL_INCLUDE_DIR libpq-fe.h
    PATHS
    "C:/Users/daver/LocalApps/pgsql/include"
    /usr/include/postgresql
    /usr/local/include/postgresql
    /opt/homebrew/include/postgresql
    /usr/include/pgsql
    "C:/Program Files/PostgreSQL/*/include"
    "C:/Program Files (x86)/PostgreSQL/*/include"
    "C:/postgresql/include"
)

find_library(PGSQL_LIBRARY
    NAMES pq libpq
    PATHS
    "C:/Users/daver/LocalApps/pgsql/lib"
    /usr/lib
    /usr/local/lib
    /opt/homebrew/lib
    /usr/lib/x86_64-linux-gnu
    "C:/Program Files/PostgreSQL/*/lib"
    "C:/Program Files (x86)/PostgreSQL/*/lib"
    "C:/postgresql/lib"
)

# Configure based on what we found
if(MYSQL_INCLUDE_DIR AND MYSQL_LIBRARY)
    list(APPEND SQL_INCLUDES ${MYSQL_INCLUDE_DIR})
    list(APPEND SQL_LIBS ${MYSQL_LIBRARY})
    set(HAVE_MYSQL TRUE)
    message(STATUS "Found MySQL/MariaDB: ${MYSQL_LIBRARY}")
else()
    set(HAVE_MYSQL FALSE)
    message(WARNING "MySQL/MariaDB not found - MySQL support will be disabled")
endif()

if(PGSQL_INCLUDE_DIR AND PGSQL_LIBRARY)
    list(APPEND SQL_INCLUDES ${PGSQL_INCLUDE_DIR})
    list(APPEND SQL_LIBS ${PGSQL_LIBRARY})
    set(HAVE_POSTGRESQL TRUE)
    message(STATUS "Found PostgreSQL: ${PGSQL_LIBRARY}")
else()
    set(HAVE_POSTGRESQL FALSE)
    message(WARNING "PostgreSQL not found - PostgreSQL support will be disabled")
endif()

# Create SQL REPL plugin
add_vizero_plugin(sql_repl
    sql_repl_plugin.c
)

# Add include directories
if(SQL_INCLUDES)
    target_include_directories(sql_repl PRIVATE ${SQL_INCLUDES})
endif()

# Link database libraries
if(SQL_LIBS)
    target_link_libraries(sql_repl PRIVATE ${SQL_LIBS})
endif()

# Add compile definitions for available databases
if(HAVE_MYSQL)
    target_compile_definitions(sql_repl PRIVATE HAVE_MYSQL=1)
endif()

if(HAVE_POSTGRESQL)
    target_compile_definitions(sql_repl PRIVATE HAVE_POSTGRESQL=1)
endif()

# Platform-specific settings
if(WIN32)
    target_compile_definitions(sql_repl PRIVATE _WIN32_WINNT=0x0600)
    # Add Windows socket library for networking
    target_link_libraries(sql_repl PRIVATE ws2_32)
endif()

message(STATUS "SQL REPL plugin: MySQL=${HAVE_MYSQL}, PostgreSQL=${HAVE_POSTGRESQL}")
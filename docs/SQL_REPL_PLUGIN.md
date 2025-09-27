# SQL REPL Plugin for Vizero

The SQL REPL plugin provides comprehensive database interaction capabilities within the Vizero editor, supporting multiple database systems with vi-style command integration and graceful degradation when database libraries are unavailable.

## Features

### ✅ **Multi-Database Support**
- **PostgreSQL**: Full native support via libpq
- **MySQL/MariaDB**: Native support via MySQL client libraries  
- **Graceful Degradation**: Plugin loads successfully even when database libraries are missing

### ✅ **Comprehensive SQL Commands**
- **Connection Management**: Connect, disconnect, status monitoring
- **Query Execution**: SELECT queries with formatted result display
- **DDL/DML Operations**: INSERT, UPDATE, DELETE, CREATE, DROP statements
- **Schema Exploration**: Table listing, structure description
- **Transaction Support**: BEGIN, COMMIT, ROLLBACK with state tracking

### ✅ **Vi-Style Integration**
- **Colon Commands**: All SQL operations use `:sql-*` command syntax
- **Buffer Integration**: Results display directly in editor buffer at cursor position
- **Error Handling**: Clear error messages and status feedback
- **Multi-line Support**: Proper formatting for complex result sets

## Installation & Setup

### Build-Time Configuration

The SQL REPL plugin automatically detects available database libraries during build:

```bash
# PostgreSQL detected
-- Found PostgreSQL: C:/path/to/libpq.lib
-- SQL REPL plugin: MySQL=FALSE, PostgreSQL=TRUE

# No databases detected  
CMake Warning: MySQL/MariaDB not found - MySQL support will be disabled
CMake Warning: PostgreSQL not found - PostgreSQL support will be disabled
-- SQL REPL plugin: MySQL=FALSE, PostgreSQL=FALSE
```

### Runtime Status

Check what database support is available:
```
[18:24:25] SQL: SQL REPL plugin initialized - Database support: PostgreSQL
```

Possible statuses:
- `PostgreSQL` - PostgreSQL only
- `MySQL` - MySQL/MariaDB only  
- `MySQL PostgreSQL` - Both databases
- `None` - No database support

## Command Reference

### Connection Management

#### `:sql-connect <connection_string>`
Connect to a database using standard connection URI format.

**PostgreSQL:**
```
:sql-connect postgresql://username:password@hostname:port/database
:sql-connect postgresql://postgres:mypass@localhost:5432/mydb
:sql-connect postgresql://user@localhost:5432/testdb
```

**MySQL:**
```  
:sql-connect mysql://username:password@hostname:port/database
:sql-connect mysql://root:password@localhost:3306/sakila
```

#### `:sql-disconnect`
Disconnect from current database and clean up resources.

#### `:sql-status`
Show current connection status, database type, and connection details.

### Query Operations

#### `:sql-query <SELECT_statement>`
Execute SELECT queries and display formatted results in buffer.

**Examples:**
```
:sql-query SELECT * FROM users LIMIT 10
:sql-query SELECT name, email FROM customers WHERE active = true
:sql-query SELECT COUNT(*) as total FROM orders
:sql-query SELECT 'Hello World' as message
```

**Result Format:**
```
name                 email               
-------------------- --------------------
John Doe             john@example.com    
Jane Smith           jane@example.com    
```

#### `:sql-exec <DDL_or_DML_statement>`
Execute non-SELECT statements (INSERT, UPDATE, DELETE, CREATE, etc.).

**Examples:**
```
:sql-exec CREATE TABLE test (id INTEGER, name TEXT)
:sql-exec INSERT INTO users (name, email) VALUES ('Bob', 'bob@test.com')
:sql-exec UPDATE products SET price = 99.99 WHERE id = 1 
:sql-exec DELETE FROM temp_data WHERE created < '2025-01-01'
:sql-exec DROP TABLE IF EXISTS old_table
```

### Schema Exploration

#### `:sql-show-tables`
List all user tables in the database, organized by schema.

**PostgreSQL Output:**
```
schemaname           tablename           
-------------------- --------------------
public               users               
public               orders              
public               products            
myschema             custom_table        
```

**MySQL Output:**
```
tablename           
--------------------
users               
orders              
products            
```

#### `:sql-describe <table_name>`
Show detailed structure of a specific table.

**Example:**
```
:sql-describe users
```

**Output:**
```
column_name          data_type           nullable    default     
-------------------- ------------------- ----------- ------------
id                   integer             NO          nextval()   
name                 character varying   YES         NULL        
email                character varying   NO          NULL        
created_at           timestamp           YES         now()       
```

### Transaction Management

#### `:sql-begin`
Start a new database transaction.

#### `:sql-commit`  
Commit the current transaction.

#### `:sql-rollback`
Rollback the current transaction.

**Transaction Workflow:**
```
:sql-begin
:sql-exec INSERT INTO users (name) VALUES ('Test User')
:sql-query SELECT * FROM users WHERE name = 'Test User'
:sql-commit    # or :sql-rollback to undo
```

## Usage Examples

### Database Setup and Connection
```
# Connect to local PostgreSQL
:sql-connect postgresql://postgres:password@localhost:5432/mydb

# Check connection
:sql-status

# Explore database structure
:sql-show-tables
:sql-describe users
```

### Development Workflow
```
# Start transaction for safe testing
:sql-begin

# Create test data
:sql-exec INSERT INTO products (name, price) VALUES ('Test Product', 29.99)

# Verify changes
:sql-query SELECT * FROM products WHERE name LIKE 'Test%'

# Rollback if needed
:sql-rollback

# Or commit to keep changes
:sql-commit
```

### Data Analysis
```
# Connect to database
:sql-connect postgresql://analyst:pass@db.company.com:5432/analytics

# Run analysis queries
:sql-query SELECT category, COUNT(*), AVG(price) FROM products GROUP BY category
:sql-query SELECT DATE(created), COUNT(*) FROM orders GROUP BY DATE(created) ORDER BY DATE(created) DESC LIMIT 30

# Export data
:sql-query SELECT * FROM monthly_report WHERE month = '2025-09'
```

## Error Handling

### Graceful Degradation

When database libraries are not available:

```
:sql-connect postgresql://user:pass@host:5432/db
ERROR: PostgreSQL support not available in this build
```

### Connection Errors

```
:sql-connect postgresql://user:wrongpass@localhost:5432/db  
ERROR: PostgreSQL connection failed: FATAL: password authentication failed for user "user"

:sql-connect postgresql://user:pass@badhost:5432/db
ERROR: PostgreSQL connection failed: could not translate host name "badhost" to address
```

### Query Errors

```
:sql-query SELECT * FROM nonexistent_table
ERROR: PostgreSQL query failed: relation "nonexistent_table" does not exist

:sql-exec INSERT INTO users (invalid_column) VALUES ('test')
ERROR: PostgreSQL query failed: column "invalid_column" of relation "users" does not exist
```

## Build Configuration

### PostgreSQL Support

**Requirements:**
- PostgreSQL client libraries (libpq)
- Development headers (libpq-dev on Ubuntu, postgresql-devel on RHEL)

**Windows:**
```bash
# Set environment variables
set POSTGRESQL_ROOT=C:\path\to\postgresql
.\build.bat
```

**Linux:**
```bash  
# Install development packages
sudo apt install libpq-dev          # Ubuntu/Debian
sudo yum install postgresql-devel    # RHEL/CentOS

# Build
./build.sh
```

### MySQL Support

**Requirements:**
- MySQL or MariaDB client libraries
- Development headers

**Windows:**
```bash
# Set environment variables  
set MYSQL_ROOT=C:\path\to\mysql
.\build.bat
```

**Linux:**
```bash
# Install development packages
sudo apt install libmysqlclient-dev     # Ubuntu/Debian  
sudo yum install mysql-devel            # RHEL/CentOS
# Or for MariaDB
sudo apt install libmariadb-dev         # Ubuntu/Debian

# Build
./build.sh
```

## Architecture

### Plugin Structure
```
plugins/sql_repl/
├── sql_repl_plugin.c        # Main plugin implementation
├── CMakeLists.txt           # Build configuration with database detection
└── README.md               # This documentation
```

### Database Abstraction
- **Connection Management**: Unified interface for different database types
- **Query Execution**: Consistent result formatting across databases  
- **Error Handling**: Database-specific error translation to user messages
- **Transaction State**: Automatic transaction state tracking

### Integration Points
- **Command System**: Registers 10 SQL commands with Vizero command processor
- **Buffer System**: Results inserted at cursor position using editor API
- **Settings System**: Connection parameters and state persistence
- **Error System**: Unified error reporting through Vizero message system

## Troubleshooting

### Common Issues

**Plugin not loading:**
- Check that plugin appears in startup log: `Loaded plugin: sql_repl v1.0.0`
- Verify database support: `Database support: PostgreSQL` (or `None`)

**Connection failures:**
- Verify database server is running and accessible
- Check connection parameters (host, port, username, password, database)
- Ensure user has proper database permissions
- Test connection with standard database client first

**No results displayed:**
- Verify cursor is positioned in an editable buffer
- Check that query executed successfully (no error messages)
- Try simple test query: `:sql-query SELECT 'test' as result`

**Build issues:**
- Ensure database development libraries are installed
- Check CMake output for database detection messages
- Verify environment variables for database paths (Windows)

### Debugging

Enable debug logging by checking console output during plugin operation:
```
[18:24:25] SQL: SQL REPL plugin initialized - Database support: PostgreSQL
[18:25:10] SQL: Connected to PostgreSQL database  
[18:25:15] SQL: SELECT * FROM users LIMIT 5
```

## Development

### Adding New Database Support

1. **Add detection in CMakeLists.txt:**
```cmake
# Find new database libraries
find_library(NEWDB_LIBRARY newdb PATHS ${NEWDB_ROOT}/lib)
find_path(NEWDB_INCLUDE_DIR newdb.h PATHS ${NEWDB_ROOT}/include)
```

2. **Implement database functions:**
```c
#ifdef HAVE_NEWDB
static int newdb_connect() { /* ... */ }
static int newdb_execute_query(const char* query, char** result, size_t* result_size) { /* ... */ }
static void newdb_disconnect() { /* ... */ }
#endif
```

3. **Add to connection parsing:**
```c
} else if (strncmp(conn_str, "newdb://", 8) == 0) {
#ifdef HAVE_NEWDB
    *db_type = DB_TYPE_NEWDB;
    *port = 1234; /* Default port */
#else
    sql_display_error("NewDB support not available in this build");
    return -1;
#endif
```

### Testing

**Unit Testing:**
```bash
# Run SQL REPL specific tests
cd build && ctest -R sql_repl
```

**Manual Testing:**
```
# Test graceful degradation
:sql-connect postgresql://test  # Should show clear error if not available

# Test connection
:sql-connect postgresql://postgres:pass@localhost:5432/postgres
:sql-status  # Should show connected status

# Test queries
:sql-query SELECT 1 as test
:sql-query SELECT current_database()
:sql-show-tables
```

## Future Enhancements

### Planned Features
- **SQLite Support**: Embedded database for local development
- **Connection Pooling**: Multiple simultaneous database connections
- **Query History**: Persistent query history and favorites
- **Result Export**: Save query results to files
- **Schema Browser**: Interactive database schema exploration
- **Query Builder**: Visual query construction assistance

### Advanced Integration
- **LSP Integration**: SQL language server support for completion and validation
- **Syntax Highlighting**: SQL-aware syntax highlighting in query buffers
- **Result Formatting**: Enhanced result display with column alignment and paging
- **Connection Profiles**: Named connection configurations for quick access

---

*The SQL REPL plugin provides robust database interaction capabilities while maintaining Vizero's philosophy of graceful degradation and vi-style integration. Whether you're doing database administration, data analysis, or application development, the SQL REPL seamlessly integrates database work into your editing workflow.*
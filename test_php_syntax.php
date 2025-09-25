<?php
/**
 * PHP Syntax Highlighting Test File
 * This file demonstrates various PHP language features
 */

namespace MyProject\Controllers;

use DateTime;
use Exception;

class UserController
{
    private $database;
    public $config;
    
    const MAX_USERS = 1000;
    
    public function __construct($db_connection)
    {
        $this->database = $db_connection;
        $this->config = $_GET['config'] ?? 'default';
    }
    
    /**
     * Get user by ID
     * @param int $user_id The user ID
     * @return array|null User data or null
     * @throws Exception If user not found
     */
    public function getUserById($user_id): ?array
    {
        // Validate input
        if (!is_numeric($user_id) || $user_id <= 0) {
            throw new Exception("Invalid user ID: $user_id");
        }
        
        // SQL query with prepared statement
        $sql = "SELECT * FROM users WHERE id = ? AND status = 'active'";
        $stmt = $this->database->prepare($sql);
        $stmt->bind_param('i', $user_id);
        $stmt->execute();
        
        $result = $stmt->get_result();
        $user = $result->fetch_assoc();
        
        if ($user) {
            // Process user data
            $user['full_name'] = trim($user['first_name'] . ' ' . $user['last_name']);
            $user['is_admin'] = in_array($user['role'], ['admin', 'superuser']);
            $user['created_date'] = date('Y-m-d H:i:s', strtotime($user['created_at']));
            
            return $user;
        }
        
        return null;
    }
    
    public function createUser(array $data): bool
    {
        // Superglobals and validation
        $username = $_POST['username'] ?? '';
        $email = $_POST['email'] ?? '';
        $password = $_POST['password'] ?? '';
        
        // Check required fields
        if (empty($username) || empty($email) || empty($password)) {
            return false;
        }
        
        // Hash password
        $hashed_password = password_hash($password, PASSWORD_BCRYPT);
        
        // Insert into database
        $query = <<<SQL
            INSERT INTO users (username, email, password, created_at) 
            VALUES (?, ?, ?, NOW())
        SQL;
        
        $stmt = $this->database->prepare($query);
        return $stmt->execute([$username, $email, $hashed_password]);
    }
    
    public static function formatUserData($user): string
    {
        $output = '';
        
        foreach ($user as $key => $value) {
            if (is_array($value)) {
                $value = json_encode($value);
            } elseif (is_bool($value)) {
                $value = $value ? 'true' : 'false';
            } elseif (is_null($value)) {
                $value = 'NULL';
            }
            
            $output .= "$key: $value\n";
        }
        
        return $output;
    }
}

// Usage example
try {
    $controller = new UserController($pdo);
    $user = $controller->getUserById(42);
    
    if ($user !== null) {
        echo "User found: " . $user['username'] . "\n";
        echo UserController::formatUserData($user);
    } else {
        echo "User not found!\n";
    }
} catch (Exception $e) {
    error_log("Error: " . $e->getMessage());
    echo "An error occurred while fetching user data.";
}

// Various PHP features
$numbers = [1, 2, 3.14, 0xFF, 0b1010, 0o777];
$strings = ['single', "double", <<<'NOWDOC'
This is a nowdoc string
with multiple lines
NOWDOC
, <<<HTML
<div class="user">Hello $username!</div>
HTML
];

// PHP 8 features
function processUser(?User $user = null): string|false
{
    return $user?->getName() ?? false;
}

echo "PHP Version: " . PHP_VERSION . "\n";
echo "Current time: " . date('Y-m-d H:i:s') . "\n";

?>

<!DOCTYPE html>
<html>
<head>
    <title>PHP Test</title>
</head>
<body>
    <h1>Hello from PHP!</h1>
    <p>This demonstrates PHP embedded in HTML.</p>
    <?php echo "Server time: " . date('c'); ?>
</body>
</html>
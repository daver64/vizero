// JavaScript Syntax Highlighting Test
/* Multi-line comment
   with multiple lines */

// ES6+ Features
const API_URL = 'https://api.example.com';
let isLoading = false;
var userData = null;

// Class definition
class UserManager {
    constructor(apiUrl) {
        this.apiUrl = apiUrl;
        this.users = new Map();
    }
    
    async fetchUser(id) {
        try {
            const response = await fetch(`${this.apiUrl}/users/${id}`);
            const user = await response.json();
            this.users.set(id, user);
            return user;
        } catch (error) {
            console.error('Failed to fetch user:', error);
            throw new Error('User fetch failed');
        }
    }
    
    getUserById(id) {
        return this.users.get(id) || null;
    }
}

// Function definitions
function calculateTotal(items) {
    return items.reduce((sum, item) => sum + item.price, 0);
}

const processOrder = (order) => {
    const total = calculateTotal(order.items);
    const tax = total * 0.08;
    
    return {
        ...order,
        subtotal: total,
        tax: tax,
        total: total + tax
    };
};

// Array and object handling
const products = [
    { id: 1, name: 'Laptop', price: 999.99, inStock: true },
    { id: 2, name: 'Mouse', price: 29.99, inStock: false },
    { id: 3, name: 'Keyboard', price: 79.99, inStock: true }
];

// Template literals
const formatProduct = (product) => {
    return `Product: ${product.name} - $${product.price.toFixed(2)}`;
};

// Regular expressions
const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
const phoneRegex = /^\(\d{3}\)\s\d{3}-\d{4}$/;

// DOM manipulation (if in browser)
if (typeof document !== 'undefined') {
    document.addEventListener('DOMContentLoaded', () => {
        const button = document.querySelector('#submit-btn');
        button.addEventListener('click', handleSubmit);
    });
}

function handleSubmit(event) {
    event.preventDefault();
    console.log('Form submitted');
}

// Modern JavaScript features
const asyncProcessor = async function* () {
    for (let i = 0; i < 10; i++) {
        yield await new Promise(resolve => setTimeout(() => resolve(i), 100));
    }
};

// Destructuring and spread
const { name, price, ...rest } = products[0];
const newProducts = [...products, { id: 4, name: 'Monitor', price: 299.99 }];

// Numbers and literals
const hexNumber = 0xFF;
const octalNumber = 0o755;
const binaryNumber = 0b1010;
const bigIntNumber = 123456789012345678901234567890n;
const floatNumber = 3.14159e-10;

// Boolean and special values
const isActive = true;
const isDisabled = false;
const nothing = null;
const notDefined = undefined;
const infinity = Infinity;
const notANumber = NaN;

export { UserManager, processOrder };
export default calculateTotal;
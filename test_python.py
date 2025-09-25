#!/usr/bin/env python3
"""
Sample Python file to test syntax highlighting
with REPL-awareness features
"""

import sys
import os
from typing import List, Dict, Optional
import numpy as np

class DataProcessor:
    """A sample class for data processing."""
    
    def __init__(self, data: List[int]):
        self.data = data
        self._processed = False
    
    @property
    def is_processed(self) -> bool:
        """Check if data has been processed."""
        return self._processed
    
    def process_data(self) -> Dict[str, float]:
        """Process the data and return statistics."""
        if not self.data:
            raise ValueError("No data to process")
        
        stats = {
            'mean': sum(self.data) / len(self.data),
            'max': max(self.data),
            'min': min(self.data),
            'count': len(self.data)
        }
        
        self._processed = True
        return stats

def main():
    # Sample data
    numbers = [1, 2, 3, 4, 5, 42, 100]
    
    # Create processor
    processor = DataProcessor(numbers)
    
    try:
        # Process data
        results = processor.process_data()
        print(f"Results: {results}")
        
        # Use f-strings and raw strings
        pattern = r'\d+\.\d+'
        message = f"Pattern: {pattern}"
        print(message)
        
        # Number literals
        hex_num = 0xff
        bin_num = 0b1010
        oct_num = 0o755
        float_num = 3.14159
        scientific = 1.23e-4
        
        print(f"Numbers: {hex_num}, {bin_num}, {oct_num}, {float_num}, {scientific}")
        
    except Exception as e:
        print(f"Error: {e}")
    
    # Lambda and comprehensions
    squares = [x**2 for x in range(10) if x % 2 == 0]
    doubled = list(map(lambda x: x * 2, squares))
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
using System;
using System.Collections.Generic;
using System.Linq;

namespace TestApp
{
    [Serializable]
    public class Program
    {
        private static readonly string AppName = "Vizero Test";
        
        public static void Main(string[] args)
        {
            var numbers = new int[] { 1, 2, 3, 4, 5 };
            var result = numbers.Where(x => x % 2 == 0).ToList();
            
            Console.WriteLine($"App: {AppName}");
            Console.WriteLine($"Even numbers: {string.Join(", ", result)}");
            
            foreach (var num in result)
            {
                ProcessNumber(num);
            }
        }
        
        private static async Task ProcessNumber(int number)
        {
            await Task.Delay(100);
            Console.WriteLine($"Processed: {number}");
        }
    }
}
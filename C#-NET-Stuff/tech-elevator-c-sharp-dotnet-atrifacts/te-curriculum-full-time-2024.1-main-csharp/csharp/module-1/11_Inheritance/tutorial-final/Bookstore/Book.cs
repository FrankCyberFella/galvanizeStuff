﻿using System;
using System.Collections.Generic;
using System.Text;

namespace TechElevator.Bookstore
{
    public class Book : MediaItem
    {
        // Constructors for the Book class
        public Book() : base() { }

        public Book(string title, string author, decimal price) : base(title, price)
        {
            this.Author = author;
        }

        public string Author { get; set; }

        // Return a string representation of this book
        public override string ToString()
        {
            return $"Title: {this.Title}, Author: {this.Author}, Price: ${this.Price}";
        }
    }
}

# Collections (Part 2) lecture notes

Yesterday, you showed the students how to use `List`s. `List`s are data structures that are similar to arrays, but they can grow and shrink as needed, which is why they're easier to work with.

Next they'll learn about a new data structure called a `Dictionary`. `Dictionary`s also don't have a fixed length, but that's about where the similarities end.

## Agenda

* Collections recap
* `Dictionary<TKey,TValue>`

### Collections recap

* Collections classes live in a namespace and come from the framework's standard library of classes.
* These are written for you and are generic enough that they're useful under many circumstances.
* Fundamentally, namespaces are ways of organizing code.

### `Dictionary<TKey,TValue>`

A `Dictionary` is a collection of key-value pairs that allows values to be located using user-defined keys. Keys must be unique within the collection, but the values may be duplicated.

Remind students that `<T>` (or any value in angular braces) is shorthand for "replace with a data type." `Dictionary <TKey, TValue>` shows that a `Dictionary` takes two data types: one for the key, and one for the value. The data types don't have to be the same.

A relatable real-life example is a snack machine. The *key* is something like `A5`, which gets you the *value* at that key, a bag of chips. Chips can be at other locations too, but nothing else can be at `A5`.

| key  | value        |
| ---- | ------------ |
| "A5" | "Chips"      |
| "A6" | "Chips"      |
| "A7" | "Candy bars" |

> Note: Students might have a hard time with the concept of a key relating to a value. *Try not to directly relate it to a List's index because that can cause more confusion.* Be sure to explain that the key is what labels a specific location in the `Dictionary`, and then you put the value at that location.

#### Lecture code walkthrough

The lecture code for today's lesson is in the lecture-final `CollectionsPart2Lecture\Program.cs`. Walk through the code in the `Main()` method.

You'll cover and demonstrate:

1. Declaring and initializing a `Dictionary`.
2. Adding an item to a `Dictionary` using the square bracket notation: `nameToZip["David"] = "44120"`.
3. Retrieving an item from a `Dictionary` using the square bracket notation: `nameToZip["David"]`.
4. Retrieving just the keys from a `Dictionary`.
5. Checking to see if a key already exists in the `Dictionary`.
   1. Useful for avoiding an exception when adding a duplicate key.
6. Replacing an item in the `Dictionary` using square bracket notation.
   1. Demonstrate why using the `Add()` method isn't preferred because it'll throw an exception for a key that already exists.
7. Iterating through the key-value pairs in the `Dictionary`, using `KeyValuePair` type.
   1. When using `KeyValuePair`, you access the key and value with the `TKey` and `TValue` properties.
   2. Remind students that the items in a `Dictionary` are unordered, and items in a loop may return in a different order than they were added.
8. Removing an item from the `Dictionary`.
   1. Loop again to confirm the removal of an item.

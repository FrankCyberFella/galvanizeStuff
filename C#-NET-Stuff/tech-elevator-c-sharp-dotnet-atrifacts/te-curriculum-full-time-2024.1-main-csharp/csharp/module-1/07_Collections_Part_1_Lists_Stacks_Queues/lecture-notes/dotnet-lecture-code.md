# Collections (Part 1) lecture notes

Up until now, the students have learned that arrays can hold objects of the same type, and depending on the program you're writing, this might be exactly what you need.

But arrays have two downsides: they're fixed in size and have linear storage. Now that you have objects, you can solve these two problems.

If you can now group data and behavior together in an object, how can you add extra behavior to arrays to make them more powerful? You've already seen this in action with `string`s.

`string`s are really an array of characters, but by putting that array in an object, you can add functionality that makes sense for `string`s, like finding substrings or uppercasing the `string`.

In C#, there's a library you can use to create powerful arrays and other data structures. This library is called the **Collections** library. It provides you with a group of objects that you can use when working with collections of data.

## Classroom preparation

* Prepare to share an example `for` loop and a `foreach` loop.

## Agenda

* Array recap
* Collections classes
* `List<T>`
* `foreach` loops

### Array recap

- Arrays are data structures that you can use to work with a collection of data.
- Arrays aren't very flexible. Adding a new element to an array or modifying the array is often difficult.
- Collections make creating and modifying collections of data easier.

### Collections classes

* Collections classes live in a namespace and come from the framework's standard library of classes.
* These are written for you and are generic enough that they're useful under many circumstances.
* Fundamentally, namespaces are ways of organizing code.

### List<T>

* A `List`:
    * Is zero-indexed like an array.
    * Is an ordered set of elements accessible by index.
    * Allows duplicates.
    * Can grow and shrink as you add and remove items.
* You can add and remove items from the middle of a `List`.

Remind students that `<T>` is shorthand for "replace `T` with a data type", and `List<string>`, or "list of strings", is a different data type from `List<int>`, or "list of `int`s."

> Note: Students have just started to understand arrays. Many of them may confuse syntax and terms, so make sure to say `List` when you mean `List`, and array when you mean array. Let them know that they may struggle with this at first.

### Lecture code walkthrough

The lecture code for today's lesson is in the lecture-final `CollectionsPart1Lecture\Program.cs`. Walk through the code from the `LISTS` heading up until the `FOREACH` section.

You'll cover and demonstrate:

1. Declaring a `List`
2. Adding items to a `List`
3. How `List`s are ordered:
   1. Elements are in the same order they were added
   2. Accessible by index - seen in a `for` loop
4. How `List`s allow duplicate items
5. Inserting elements in the middle
6. Removing elements by index
7. Checking if an element is in a `List`
8. Finding the index of an element
9. Converting `Lists` to arrays
10. Reversing a `List`:
    1.  *Optionally mention*: `Reverse()` is an *in-place* method which acts directly on the `List` and can't be undone except by calling `Reverse()` again on the reversed `List`.
11. Sorting a `List`
    1.  *Optionally mention*: `Sort()` is an *in-place* method which acts directly on the `List` and can't be undone. You can't "unsort" a `List`.

#### Foreach loops

Some things to note:

- `foreach` loops are convenience methods used to iterate through a collection.
- You can't modify the contents of a collection during iteration.
- `foreach` loops are only useful if you want to work with the elements, don't need the index, and want to go through each element one by one starting at the beginning of the collection.
- `foreach` can also be used with arrays.

Continue through the `FOREACH` section in `Program.cs`.
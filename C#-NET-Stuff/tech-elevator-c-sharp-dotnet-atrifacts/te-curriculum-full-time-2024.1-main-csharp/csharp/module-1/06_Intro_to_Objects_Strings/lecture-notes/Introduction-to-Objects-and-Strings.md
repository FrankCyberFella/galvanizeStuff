<link rel="stylesheet" type="text/css" media="all" href="./styles/style.css" />

# Introduction to Objects Using Strings - Lecture Notes
###### (05/14/2018)

## **Overview of Session**

Until now we've primarily worked with the primitive data types. These data types are simple because they only hold a single literal value and require a fixed amount of memory. But what if we want to model something more complex than that? What if, instead of holding a single character in a variable, we want to hold a name, or an address or an unknown number of characters all together?

This week we will begin learning about objects and how we can use this new concept to create more complex, and more useful, programs.

## **Session Objectives:**
* Students should be able to explain the concept of an object as a programming construct
* Students should be able to describe the difference between objects and classes and how those two concepts are related
* Students should be able to instantiate and use objects
* Students should understand the terms Declare, Instantiate and Initialize
* Students should understand how objects are stored in RAM
* Students should be able to explain how the Stack and Heap are used with objects and primitives
* Students should understand and be able to define the terms Value-type and Reference-type
* Students should be able to describe the String class, its purpose and use
* Students should be able to call methods on an object and understand that they return values
* Students should understand immutability and what that means for handling certain objects
* (**More so Java**) Students should be able to explain object equality and the difference between `==` and `equals()`

## **Classroom Preparation**

1. Write various string methods on board

    | method        | use    |
    |---------------|--------|
    | `Length()`    | Returns how many characters are in the string |
    | `Substring()` | Returns a certain part of the string |
    | `IndexOf()`   | Returns the index of a search string |
    | `Contains()`  | Returns `true` of the string contains the search string |
    | | And many more... |

2. This is a heavy theory day. For lecture code, if you want, it might make more sense to write examples of each topic yourself instead of stepping through the existing lecture code as is.

## **Agenda and Timing for Session**

* Objects & Classes (0:45)
* Heap & Stack / Reference & Value Types (0:20)
* Break (0:10)
* Immutability (0:20)
* String Equality (0:10)
* String Methods (0:30)

## **Topic List w/Notes**

### What is an Object

- Be sure to describe in terms of Objects and not in terms of Classes

<div class="definition note">
An <span>object</span> is an in-memory data structure that combines state and behavior into a usable and useful abstraction.
</div>

- An object lives in memory and each object is different and separate from every other object in our program.

### What is a Class

- We don't technically write objects in our code. Objects only exist when our code is running because an object is an in-memory data structure. In order to make objects, we have to write classes.

<div class="definition note">
A <span>class</span> is a grouping of variables and methods in a source code file that we can generate objects out of.
</div>

- A class is to an object like a blueprint is to a house. A class defines what an object will be like once the object is created.
- We can even create our own classes, but we'll talk more about that later in the week.

### Creating Objects

- `House houseAt443WinstonSt = new House(3, 2.5, "Cornflower Blue");`
- First we **Declare** the variable that will hold the object, then **Instantiate** a new object from a class while we **Initialize** the variables inside the object with some initial values.

### Objects in Memory

- Reference vs. Value Types

    - All of the simple/primitive types that we have worked with are considered primitive types.

        <div class="definition note">A <strong>value type</strong> references a single static space in memory to hold the value. This memory is allocated on the <em>stack</em></div>

        <div class="definition note">The stack is a region of computer memory that manages temporary variables created by each function.</div>

        <div class="definition note">A <strong>reference type</strong> is a data type whose value is a pointer to a memory location that holds the data. This memory location is on the <em>heap</em>. Multiple variables can point to the same memory location and thus modify the underlying object.</div>

        <div class="definition note">The heap is a free-floating region of memory managed by the computer and referenced by pointers.</div>

        <div class="analogy note">Sending an attachment via email is an example of a value type. Sending a link to an attachment hosted on Dropbox/Google Drive would be an example of a reference type.</div>

   - Immutability

     <div class="caution note">While strings are reference types, they are also <strong>immutable</strong>. They are unable to be changed or altered and thus performing any operation on a string creates a new string.

     That new string *must* be saved to a variable if you want to keep it. If you want to change a string to all upper case, you can't just call:

       ```csharp
       firstName.ToUpper();
       ```

       That doesn't change `firstName`. It creates a new object and then throws it away. If you want `firstName` to hold the now upper cased string, you have to write this:

       ```csharp
       firstName = firstName.ToUpper();
       ```

     </div>

- Initializing an Object

    - All objects need to be initialized or *instantiated*.
    - We won't cover this flow today because strings are a special form of object and support the ability to type in the *literal* value.
    - `string name = "Alex";`
    - Objects can be declared and the value can be left as `null`.
    - `string name;`

### Strings
- The `string` class
- String methods
    - `Length()`
    - `Substring()`
    - `Contains()`
    - `StartsWith()` and `EndsWith()`
    - `IndexOf()`
    - `Replace()`
    - `ToLower()` and `ToUpper()`
    - `Equals()`
    - `Split()`
    - `string.Join()`

| Reference |
|-----------|
|[C# String Reference](https://learn.microsoft.com/en-us/dotnet/api/system.string?view=net-7.0)|

<link rel="stylesheet" type="text/css" media="all" href="./styles/style.css" />

# File I/O - Reading Files - Lecture Notes
###### (05/25/2018)

## **Overview of Session**

We've seen last week how to take input from the user and use it in our program. But data can come from many other places. Today, we'll talk about how to read data in from a file. We'll also talk about how to write code that intelligently handles when something goes wrong.

## **Session Objectives**
* Students should be able to describe the concept of exception handling
* Students should be able to implement a try/catch structure in a program
* Students should know about `System.IO` namespace (C#) / `java.io` library File and Directory classes
* Students should be able to explain what a character stream is
* Students should be able to use a try-with-resources block
* Students should know how to handle File I/O exceptions and how to recover from them
* Students should know how File I/O might be used on a job

## Classroom Preparation
* **Java** - Create a file on your computer of RTN numbers, one per line. This is needed for the Java lecture code. [List of Routing Numbers](https://www.usbank.com/bank-accounts/checking-accounts/checking-customer-resources/aba-routing-number.html)

## **Customary schedule for session:**

* Exception Handling (1:00)
* Break (0:10)
* File I/O (0:45)
* Handling exceptions when reading from a file stream (0:20)

## **Topic List w/Notes** <div class=topicNote>(and <span class='link'>links</span> to e-book section when available)</div>

### Exception Handling

<div class="note caution">

#### Caution

After just having wrapped their heads around OOP, Exception Handling can seem very foreign to students. The idea of jumping out of the normal control flow they've been learning and when and why to use exceptions escapes a lot of people just learning this. Explain it as best you can and explain that it's not 100% vital that they learn how to make their own exceptions, but they do need to understand that they have to watch for them when dealing with outside resources, like files and websites.

</div>

 * Two different types of errors
    - Compile-time
    - Run-time

<div class="definition note">

A **compile-time error** occurs when there is a syntactical error or the compiler identifies code that cannot execute—for example, a method not existing, accessibility being not public, redefined constant, or type mismatch.
</div>

<div class="definition note">

A **run-time error** occurs while the program is executing. The program often tries to access memory that is inaccessible or may be asked to perform an operation it is incapable of, like accessing a restricted file or parsing a value.
</div>

<div class="definition note">

**Exception handling** is the process of responding to exceptional errors in the programming. This processing often changes the flow of the program so that it can recover.</div>

- What would be considered an exception?
    - A file is deleted while the program is executing
    - The network shuts down while calling an API
    - A negative value is passed into a method that can't handle negative values (typically not handled at compile time, but by throwing an InvalidArgumentException)

<div class="note instructorDirective">

Walk through the exception lecture code as you explain exceptions and exception handling.

</div>

- **Important Points of Exception Handling**
    - try/catch/finally structure
    - every other Exception class inherits from Exception
    - You can make your own exceptions to handle control flow in the application
    - stack trace
    - checked exceptions (**Java**) - You must handle (either rethrowing or catching) checked exceptions

### File I/O

- **System.IO && java.io Library**
    - File, Directory classes (Brief introduction, can dive in more tomorrow)

- **Reading a File**
    - streaming vs. reading the entire file in at one time

<div class="analogy note">

Methods exist to read all text in very quickly with one line of code. They pull the entire file into memory though. Typically, we don't want to do that, especially for large files. This is equivalent of sitting to watch a NetFlix movie and waiting for the entire movie to load before you start watching it.
</div>


<div class="definition note">

A **stream** refers to a sequence of bytes that can read and write to some sort of backing data store.

Streams have an end-of-file marker or end-of-stream marker to indicate when the program reaches the end of the stream.
</div>

<div class="analogy note">

A stream is like an assembly line, where you process each thing as it comes through, in order.

</div>

- Reading Files
    - emphasize the `StreamReader` for .NET
    - emphasize `Scanner` for Java

<div class="note caution">

#### Java

Only show students the Scanner for reading files. The other 15 ways of doing it in Java will be too confusing at this point.

</div>

- **Clean Up**
    - using / using w/resources, try-with-resources
    - we need to explicitly close and free up these resources

- **Pattern for reading a file**

#### csharp
```csharp
while (!sr.EndOfStream)
{
    // Read in the line
    string line = sr.ReadLine();

    //do something with line input
}
```

#### Java
```java
while (input.hasNextLine()) {
    String line = input.nextLine();

    // do something with input
}
```

### Handling exceptions when reading from a file stream


Exceptions can often occur when reading streams
1. **Directory not found**
2. **End of stream reached**
3. **File not found**
4. **Path too long (Windows only)**

## Real World Usage Scenarios

1. Importing Bulk Data Sets
2. Desktop Applications - Reading in Configuration Settings
3. Video Games - Data File
4. Transmitting data to other systems



## Instructor References

[.NET Common I/O Tasks](https://learn.microsoft.com/en-us/dotnet/standard/io/common-i-o-tasks)

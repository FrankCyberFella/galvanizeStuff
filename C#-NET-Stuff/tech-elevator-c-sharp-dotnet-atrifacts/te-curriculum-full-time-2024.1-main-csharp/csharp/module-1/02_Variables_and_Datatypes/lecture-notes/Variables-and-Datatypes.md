<link rel="stylesheet" type="text/css" media="all" href="./styles/style.css" />

# Variables and Data Types - Lecture Notes
###### (05/07/2018)

## **Session Objectives**

Students should be able to:

* identify and explain the fundamental concepts and components of Java / .NET
* read and write code that uses variables
* declare a variable and assign a value to a variable
* know and use industry acceptable naming conventions for variables
* choose the appropriate primitive data type to represent different kinds of data in a program
* utilize arithmetic operators to form mathematical expressions
* explain the concept of data conversion (or casting), when it occurs, why it's used
* explain the purpose and use of literal suffixes and why they should be used
* code and test a simple Java or .NET program using an Integrated Development Environment (IDE).
* perform simple tasks in an IDE such as:
    * Organizing code into projects
    * Understand feedback on syntax errors
    * Utilize the "IntelliSense" feature of an IDE to assist in code development

## **Overview of Session**

Yesterday, we talked about the tools we can use to create applications. Today, we'll be writing our first program.

All applications that you write consist of two aspects: data and behavior. We'll define **data** that holds information that our program stores and **behavior** that manipulates that data and transforms it into something valuable.

The focus of today's lecture is to look at how the data part of our application is stored and used in a program.

## **Classroom Preparation**
Prepare to share the following:

1. Hello World Program

    ```csharp
    using System;
    class HelloWorldClass
    {
        static void Main(string[] args)
        {
            // Prints out Hello World
            Console.WriteLine("Hello World");
        }
    }
    ```

2. Data Types

| C# data type | Range                                                                       |
|--------------|-----------------------------------------------------------------------------|
| `bool`       | true or false                                                               |
| `byte`       | 0 to 255                                                                    |
| `short`      | -32,768 to 32,767 or <br> -2<sup>15</sup> to 2<sup>15</sup>-1               |
| `int`        | -2,147,483,648 to 2,147,483,647 or <br> -2<sup>31</sup> to 2<sup>31</sup>-1 |
| `long`       | -2<sup>63</sup> to 2<sup>63</sup>-1                                         |
| `decimal`    | -7.9 × 10<sup>28</sup> to 7.9 × 10<sup>28</sup>                             |
| `float`      | -3.4 x 10<sup>38</sup> to 3.4 x 10<sup>38</sup>                             |
| `double`     | -1.798 × 10<sup>308</sup> to 1.798 × 10<sup>308</sup>                       |
| `char`       | `U+0000` to `U+FFFF` ('a', 'b', etc.)                                       |

> Historically, it was important to use the smallest possible data type as early computers had smaller amounts of memory available. Since this is seldom a concern with modern computers, the smaller types like `byte`, `short`, and `float` are rarely used.

3. Arithmetic Operators

| Operator | Description                        | Example     |
|----------|------------------------------------|-------------|
| `+`      | Adds two operands                  | 15 + 2 = 17 |
| `-`      | Subtracts two operands             | 15 - 2 = 13 |
| `*`      | Multiplies two operands            | 15 * 2 = 30 |
| `/`      | Divides two operands               | 15 / 2 = 7  |
| `%`      | Finds the remainder after division | 15 % 2 = 1  |

## **Agenda and Timing for Session**

* [Introduction to .NET](Variables-and-Data-Types-.NET.md) (0:25)
    * Intro to language (0:05)
    * Your IDE (0:10)
    * Hello World (0:10)
* Variables (0:35)
    * Data Types (int, double, String, boolean) (0:15)
    * Declaring a variable (0:05)
    * Naming Conventions (0:10)
* Break (0:10)
* Expressions (0:10)
* Arithmetic Operators (0:15)
* Type Conversion (0:20)
* Literal suffixes (0:10)
* Questions

## **Topic List w/Notes**

Use the [Variables and Data Types - .NET notes](Variables-and-Data-Types-.NET.md) which covers:

* Introduction to .NET Core framework
* Introduction to Visual Studio
* C# Hello World Program

### Variables

<div class="definition note">A <span>variable</span> is a storage container paired with a symbolic name or identifier. It holds some known or unknown amount
of information referred to as a value. It is used to assign and access the value of data.</div>

<div class="analogy note">A variable is like a box. The name on the outside should tell you what is inside.</div>

![Variable Visual](./variable-visual.png)

#### Data Types

<div class="definition note">A <span>bit</span> can have two values. 0 or 1, off or on.</div>

<div class="definition note">A <span>byte</span> is made up of 8 bits.</div>

<div class="analogy note">

Use an example to tell the students that knowing the data is not enough; they need to know the *type* of the data, too.

Ask the students what number **10** is. Indicate that in the Base 10 system, this is the number 10, but in Base 2, it is the number 2. The actual value of the data depends on the type.

</div>

All data in a computer is represented by bits. When we create a variable, we have to tell the computer how we want it to treat these bits. We do this by defining data types on variables that defines how the bits and bytes inside it are interpreted by the system.

<div class="note instructorDirective">

Go over the data types written on the board in preparation for Step 2 and discuss what the data type primitives would be best used for.

</div>

#### Declaration Statements

<div class="definition note">A <span>declaration statement</span> introduces a new variable or constant. Variable declarations can optionally assign a value in the declaration. Every variable declaration consists of a type and a name.</div>

You define the value in a variable with a single `=`.

#### Naming Rules

<div class="definition note"><span>Camel case</span> is a convention that requires that the first character in the name is lower-case and
each subsequent word begins with an upper-case character.</div>

<div class="note instructorDirective">

Show good variable names that describe the data they hold and bad variable names that don't follow the rules.

Mention some rules are conventions and some rules are required. For examples, variable names can't be keywords, and they can't start with a number.

</div>

#### Strings

<div class="definition note">A <span>string</span> represents a sequence of zero or more Unicode characters.</div>

* **Declaring a String**

    With `"..."`

* **Escape Characters**

    Like `\n` and `\t`. Needed because you can't really type a tab or return in a string directly.

<div class="note instructorDirective">

Ask the students to import the lecture code into their IDE. You have to very explicitly walk them through this process today (and for a couple of days following). Make sure everyone gets the code into the IDE and have the lecture code open on their screen before moving forward.

Go over lecture code **1** through **9**. Have the students try it on their own and then do them yourself. Remember to talk about your decisions and dive into any questions they ask.

</div>

### Arithmetic Operators

#### Expressions

<div class="definition note">An <span>expression</span> is statement of code which can be evaluated to produce a result. We use the result to often
assign the value to another variable or as the input to another expression.</div>

<div class="note instructorDirective">

Go over the arithmetic operators from Preparation Step 3.

</div>

* **Order of Operations**

| Category | Operators |
|----------|-----------|
| multiplicative | `*` or `/` or `%` |
| additive | `+` or `-` |
| assignment | `=` |

* **Assigning a Value from an Expression**
* **Combining Multiple Expressions**

    Show the use of `()` to override the order of operations and emphasize being explicit for readability and maintenance.

<div class="note instructorDirective">

Go through lecture code **10** through **15**. Show how variables can be reused and their value changed by using the variable in an expression that then reassigns back to itself.

</div>

### Type Conversion

There are two types of casting that occur.

<div class="definition note">Widening / Implicit Casting occurs when we convert from one type with less size to a type with more size.</div>
<div class="analogy note">A good example of this is upgrading from a smaller suitcase to a larger suitcase. We have room for all of our items plus additional space.</div>

<div class="definition note"><span>Narrowing </span> occurs when we convert from a type with more size to a type with less size.</div>
<div class="definition note"><span>Truncation</span> occurs when you go from larger to smaller and have to lose some of the data in the process.</div>
<div class="analogy note">Think about downgrading from a larger suitcase to a smaller suitcase. If it was fully packed, you'd need to truncate or remove some of the data at the end. </div>

<div class="note instructorDirective">

Go through the rest of the lecture code.

Use **16** and **17** to talk about explicit conversion from `int` to `String`.

Use **19** to talk about safe conversion from int to double.

Use **20** to talk about integer math and any time you divide two ints, you will always get an int back. If you want a double, you need to cast one of the ints to a double before doing the operation.

Use **21** to talk about how a double and int will return a double.

Use **22** to show double impreciseness. The answer to 66.6/100 should show a strange answer of 0.66599999999. Talk about how doubles are not precise and shouldn't be used for real world financial applications.

Use **23** to talk about modulo and finding remainders.

Use **24** to talk about overflow of an `int` and that you'll get no warning from it. It's important to know what capacity your data types have and how using a `long` solves the problem. This is a good time to discuss why you wouldn't always just use a `long` to avoid this problem (it's rare and using longs everywhere could cause a memory issue.)

Use **25** to talk about boolean literals.

</div>

## Cheat Sheets & References
<div class='purpose'>(Summaries or Links to Reference Material/Cheat Sheets)</div>

| Reference | Description |
|-----------|-------------|
| [C# Data Types](https://drive.google.com/open?id=0Bz4DHj0l-C66eVpPa0RBbk5TUlU) | C# Data Type Reference |
| [Visual Studio Cheat Sheet](https://drive.google.com/open?id=0Bz4DHj0l-C66bGF1ejZvMndOVmc) | Visual Studio Cheat Sheet Reference |
| [IntelliJ keyboard shortcuts](https://www.jetbrains.com/help/idea/mastering-keyboard-shortcuts.html) | IntelliJ keyboard shortcuts |
| [Java Data Type Conversion](https://drive.google.com/open?id=0Bz4DHj0l-C66MnV4cnRiUm1UYWc) | Java Data Type Conversion Chart |
| [Java Operator Summary](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/opsummary.html) | Summary of Java Operators |

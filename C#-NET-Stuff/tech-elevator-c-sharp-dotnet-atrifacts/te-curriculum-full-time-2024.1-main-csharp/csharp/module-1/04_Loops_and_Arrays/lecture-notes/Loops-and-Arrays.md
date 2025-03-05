# Loops and Arrays - Lecture Notes
###### (05/08/2018)

## **Overview of Session**

Yesterday, we learned about conditionals and how they can let us make decisions while our code is running. We also know about variables, and how we can keep track of individual pieces of data in our code.

But what if we wanted to keep a series or group of data together, like student names? And what if we wanted to perform an operation on all of those pieces of data, like print a roster of who is in the class?

Today we'll see how to do that with arrays and loops.

## **Session Objectives**

* Students should be able to explain the concepts of arrays
* Students should be able to perform the following tasks associated with arrays:
    * Create an array
    * Initialize an array
    * Retrieve values stored in an array
    * Set/Change values in an array
    * Find the length of an array
    * Use a for-loop to "walk-through" the elements in an array
* Students should be able to explain the limitations when using arrays
    * Can't change the length of an existing array
    * Arrays can only hold the data types it was declared with
* Students should be able to describe how to perform the following manipulations on arrays:
    * Get the first element of an array
    * Get the last element of an array
    * Change each element of an array
* Students should be able to explain the concepts related to variable scope and why it is important
* Students should be able to use the increment/decrement short assignments properly in a program
* Students should be able to use the debugger in their IDE to walk through their code

## **Classroom Preparation**

1. Prepare to share the following For-Loop Diagram:

![For loop diagram](img/for-loop.png)

## **Agenda and Timing for Session**

* Arrays (0:30)
    * Declaring and Initializing Arrays
    * Elements and Indexes
    * Array Length
* Variable Scope (0:20)
* Break (0:10)
* Introduction to Loops
    * For-Loops (0:30)
    * Looping Through an Array (0:10)
* Increment / Decrement Shorthand Assignment (0:10)
* Breakpoints and Debugging (0:20)

## **Topic List w/Notes**

### Arrays

<div class="definition note">An array is a series of objects of which all are the same size and type. Each object in an array is called an element.</div>

You want to use arrays when you have data that is all related and act as the same kind of thing in the code, but you need multiple values for, like student names, quiz scores or inning scores for a baseball game.

![Array](img/arrays.png)

1. Declaring Arrays
    - `int[] scores = new int[18];`
2. Address Array Elements
    - `scores[0] = 90;`
    - `int halfOfScore = scores[0] / 2;`
    - `scores[1] = scores[0];`
    - `scores[1] = scores[1] * 2;`
    - Get the first element `scores[0]`
    - How do we get the last element?
    <div class="note instructorDirective">

    Go through lecture code **1** and **2**
    </div>
3. Get the length of an array
    - `scores.Length`
    - Get the last element `scores[scores.Length - 1]` **Always the last element in any array**
    <div class="note instructorDirective">

    Go through lecture code **3** to **5**
    </div>
4. Static initialization of an array
    - `int[] scores = { 10, 32, 63, 48, 54, 96, 67, 88, 94 };`

### Variable Scope

Recap:

> Code that needs to belong together as a single unit can be written in **blocks**.
>
> <div class="definition note">
>
> A <span>block</span> is a group of zero or more statements between balanced braces and can be used anywhere a single statement is allowed.
>
> </div>
>
> Blocks have a list of statements within them and are enclosed with braces **`{..}`**

<div class="definition note">A variable's <span>scope</span> defines where in the program that the variable is exists, or in other words, where it can be referenced. When code execution reaches a point where a variable can't be referenced, the variable is said to be *out of scope*.</div>

Rules of Scope:

1. Variables declared inside of a function or block **`{..}`** are local variables and only available within that block. This includes loops.
2. Blocks can be nested within other blocks. Therefore, if a variable is declared outside of a block, it is accessible within the inner block.

<div class="note instructorDirective">

Demonstrate this with an example. Then go through lecture code **6** to **8**.

</div>

### Loops

Often, we want to write code that will run multiple times. To do that, we can use the for-loop:

![For Loop State Diagram](img/for-loop.png)

The most common for-loop structure that you will see is a for-loop that runs over every element in an array. This is a good one to memorize:

```csharp
for (int i = 0; i < scores.Length; i++) {
    ...
}
```

<div class="note instructorDirective">

This would be a good time to slowly step through this code a number of times. This is one of the hardest concepts for a student to grasp, so take your time on it. There's a lot going on here that a seasoned programmer will gloss over but a novice will not comprehend. Be sure to point out all the moving parts and what order they run in.

Go through **9** to **12**. Jump back to the scope discussion to also explain why, in a problem like **9**, you need to have the `counter` variable outside the loop.

</div>

### Increment / Decrement Shorthand Assignment

<div class="note instructorDirective">

This concept is typically explained along with the loops above or when a student asks what that funny syntax is doing.

</div>

1. Short-Hand Notation **`++`** and **`--`**
2. Difference between `x++;` and `++x;`

<div class="definition note">With prefix, the expression returns the incremented value of the variable. With postfix, the expression returns the current value of the variable
before incrementing it.</div>

<div class="caution note">

Don't use these operators, except in loops or in lines by themselves. They are dangerous and cause unforeseen *side effects*.

</div>

3. Shorthand assignment for `x = x + n;` which is **`x += n;`**. This applies to `-=`, `*=`, and `/=`.

### Breakpoints and Debugging

<div class="note instructorDirective">

Feel free to show off debugging on some of those last lecture exercises. Demonstrate it and then show the class how to do it, too. Showing them how to do it may eat up a bunch of time but will be worth it for them in the long run.

</div>

<div class="definition note"><span>Debugging</span> is the process of locating and fixing bugs or errors in computer program code.</div>

1. Breakpoints

<div class="definition note">A <span>breakpoint</span> is a marker in our IDE (not a part of our code) that indicates our program should pause execution when it reaches a set location before continuing.</div>

When debugging code, it is best to:

* Locate the breakpoint at the start of the code that you want to validate
* Make a single change, then test. Repeat.
* Read any error messages that show up
* Explain your code step by step to another person

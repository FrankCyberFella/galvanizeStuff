# Introduction to Client-Side Scripting with JavaScript

## Problem statement

**Client-Side Scripting** takes static websites and gives them behavior. It supports the creation of interactive and responsive applications. 


## Daily objectives

- Client Side Scripting
- JavaScript
    - Differences vs. C#/Java
    - Strict and Loose Equality
    - Arrays and Simple Objects
- Chrome Console & Debugging

## Outline

1. **Client Side Scripting**
    - Executes code on the user's browser, allowing us to interact with the HTML rendered and the CSS sent by the server

    - Creates less stress on the server and more interactive engaging experiences for users.

    - Useful for:
        - Responding to events (click, keypress, scroll, resize)
        - Can interact with other web services / APIs to dynamically update the page
        - Can manipulate the loaded page without refresh

    - Separation of Concerns
        - **HTML provides content** for a web page
        - **CSS provides the look and feel** for the web site
        - **JavaScript provides behaviors** when the user interacts.

<div class="definition note">

**Client Side Scripting** refers to computer programs on the web that run in the user's web browser</div>

2. **JavaScript**
    - A programming language very similar to Java and C#.

    - Main differences between Java/C# and JavaScript
        - Java/C# requires a runtime, JavaScript requires the browser
        - Java/C# is compiled, JavaScript gets interpreted
        - Java/C# is a statically typed language, JavaScript is dynamically typed

<div class="definition note">

A **statically typed** language enforces the data type constraints at compile-time</div>


<div class="definition note">

A **dynamically typed** language infers the data type of what the variable holds at run-time.</div>

<div class="caution note">
    The lecture example code demonstrates a lot of the basics of the JavaScript language. 
    Use that to determine the scope of what JavaScript to cover for this day.
</div>

3. **Semantics**
    - Declaring variables
        - use `const` and `let` instead of `var`

    - **String equality vs. Loose equality**
        - Strict Equality compares two operands for type and value equality
        - Loose Equality compares two operands for value only after converting to a common type. It will consider two values equal if they have the same value

    - **Functions**
        - lack a return type
        - don't specify access modifiers
        - params don't have defined data types

    - **Scope**
        - variables declared within and outside of functions

    - **Arrays / Maps**
        - not fixed in size
        - methods:
            - `.concat()`
            - `.push()`
            - `.pop()`

    - **Object Literals**
        - Declaring them and using them (the exercises don't get into this but they can start to get the hang of it)

    - **Numbers, Math, and Dates Library**
        - For reference on Numbers and Dates see the [Mozilla documentation on Numbers and Dates](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Numbers_and_dates).

    - **String Methods**
        - For reference on Text Formatting with JavaScript see the [Mozilla documentation on Text Formatting](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Text_formatting).

4. **Chrome Console & Debugging**
    - While lecturing, it's helpful to show the students that they can write JavaScript code & functions in the browser Console.
    - Call the functions in the Console and show how the debugger also works.

## Supplementary materials and student reading

1. Reference [Mozilla JavaScript Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)


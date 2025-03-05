# Expressions, Statements, Blocks, and Branches - Lecture Notes
##### (05/08/2018)

## **Overview of Session**

We talked yesterday about how applications are made up of two aspects: data and behavior. We've now seen how to handle, store, and do calculations on data in our applications, and today we'll see how to add behavior to our applications.

Behavior in our application is all about running certain blocks of code in our application depending on what data we have. If a number is larger than a certain amount, we may want to alert the user with a different message than if a number is below a certain amount. These kinds of decisions are called conditions, and we'll learn how to tell our application how to make decisions based on what we want to do.

## **Session Objectives**

* Students should be able to explain the what types of things can comprise an `expression`
* Students should be able to define what is meant by a `statement` in a programming language
* Students should be able to describe the purpose and use of a `block` in reference to a programming language
* Students should know what is meant by a `boolean expression` and how it is used in a program
* Students should understand what a `comparison operator` is and how to use it
* Students should understand what a `logical operator` is and how to use it
* Students should understand how `()` work with boolean expressions and why using them makes code more clear
* Students should understand the Truth Table and how to figure out AND and OR interactions

## **Classroom Preparation**
Prepare to share the following:
1. Comparison Operators

    | Operator | Meaning                    |
    |----------|----------------------------|
    | `==`     | Equal To                   |
    | `!=`     | Not Equal To               |
    | `>`      | Greater Than               |
    | `<`      | Less Than                  |
    | `>=`     | Greater Than or Equal To   |
    | `<=`     | Less Than or Equal To      |

2. Logical Operator Truth Table

    | `A`       | `B`       | `!A`  | `A && B` | <code>A &#124;&#124; B</code>      | `A ^ B` |
    |-----------|-----------|-------|----------|------------------------------------|---------|
    | **TRUE**  | **TRUE**  | FALSE | TRUE     | TRUE                               | FALSE   |
    | **TRUE**  | **FALSE** | FALSE | FALSE    | TRUE                               | TRUE    |
    | **FALSE** | **TRUE**  | TRUE  | FALSE    | TRUE                               | TRUE    |
    | **FALSE** | **FALSE** | TRUE  | FALSE    | FALSE                              | FALSE   |

## **Agenda and Timing for Session**

* Expressions, Statements, and Blocks (0:20)
* Introduction to Methods (0:15)
* Boolean Expressions
    * Comparison Operators (0:05)
    * Logical Operators (0:10)
* Break (0:10)
* Conditional Statements (0:25)
* Lecture Code (0:35)

## **Topic List w/Notes**

### Expressions and Statements

When we write code, we write expressions and statements.

<div class="definition note">An <span>expression</span> is a construct made up of variables, operators, and method invocations, which are constructed according to the syntax of the language, that evaluates to a single value.
</div>

<div class="definition note">A <span>statement</span> forms a complete unit of execution.</div>

<div class="analogy note">Statements are roughly equivalent to sentences in natural languages</div><br/>

<div class="note instructorDirective">

Write some simple statements on the board and diagram what parts are expressions and how those fit into statements. Some examples you could use:

* Variable declaration, like **`int x;`**
* Assignment Expressions, like **`int x = 5 + 1;`** The literals are expressions, the `+` is an expression, the `int x` is an expression, and the `=` is an expression.
* More complex statements, like **`int area = length * height;`**
* Any usage of ++ or --, like **`x++;`**
* Emphasize that the computer runs each expression separately even though we wrote it all as one line of code.

</div>

### Blocks

Code that needs to belong together as a single unit can be written in **blocks**.

<div class="definition note">A <span>block</span> of code permits multiple statements to be grouped together.</div>

Blocks have a list of statements within them and are enclosed with curly braces **`{..}`**

<div class="caution note">Be careful when introducing the concept of <span>scope</span>. This will move to day 4; if it comes up, make it clear that variables declared
within a block cannot be accessed outside of that block, and variables declared in outer blocks can be seen inside of the inner block.</div>

### Introduction to Methods

Introduce Methods as a concept to students so that they can understand how to do their exercises. Be careful diving too deep as methods will be covered in more detail later.

<div class="definition note">

A <span>method</span> is similar to a mathematical function. The output of it is often directly correlated to the input of it—for example, **`f(n) = n^2`**.

or

A <span>method</span> is a named block of code. It can take multiple values and return back a single value.

</div>

Methods have **Method Signatures**:
1. Descriptive Names
2. Return Type such as int, long, double, float, etc.
3. Input parameters. Parameters are variables that live only in the method.

<div class="note instructorDirective">

Go through lecture code **1** through **5**.

</div>

### Boolean Expressions

In programming, we often want to conditionally execute sections of code. Before we can do that we need to know how to check when we should run a section of code.

<div class="definition note">

A <span>boolean expression</span> is an expression that produces a boolean value (**true** or **false**) when evaluated.

</div>

#### Comparison Operators

* Go over operators from Preparation Step 1
* **Java:** Mention, but gloss over, that you can't use these operators with String.

#### Logical Operators

##### Truth Tables

* Go over table from Preparation Step 2
* Talk about operator precedence and why using `()` is so important in complex boolean expressions.

### If Statements and Blocks

<div class="definition note">Conditional blocks allow a program to take a different path depending on one or more conditions determined while the program runs.</div>

![Conditional If-Else Block](img/if-else.png)

<div class="note instructorDirective">

Go over the rest of lecture code. The methods will get progressively more complex in their if statement structure.

</div>
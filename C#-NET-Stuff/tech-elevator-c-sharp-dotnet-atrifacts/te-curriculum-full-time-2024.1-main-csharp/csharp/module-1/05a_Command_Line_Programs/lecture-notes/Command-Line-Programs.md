<link rel="stylesheet" type="text/css" media="all" href="./styles/style.css" />

# Command-Line Programs - Lecture Notes
###### (05/08/2018)

## **Overview of Session**

The programs we've written up until now have been self-contained, with all the data already specified in the program. But what if we want to interact with a user?

So how do we ask the user for information, turn that into data we can use and process and then send an answer back to the user for them to see?

## **Session Objectives:**

* Students should be able to use `Console.ReadLine()` and `Console.WriteLine()` to perform console I/O in a program
* Students should be able to correctly parse input from the input to primitive data types
* Students should be able to check for string equality
* Students should be able to split a string apart using known split character
* Students should be able to explain the process of a command-line application (Take input, calculate data, give output)
* Students should be able to run their command-line apps in their IDE

## Classroom Preparation

* Think of a command-line application that you can build in the class.
    * Examples could be a program that takes three prices and a discount and tells you what the new prices are, or a program that takes three words and turns them into Pig Latin. You can start with this example right from the beginning of class.
    * The student-lecture code starts with the discount calculator example.
* Today is about reiterating this weeks major concepts, so feel free to throw in arrays and loops and if statements to help flesh that out.

## **Agenda and Timing for Session**

* `Console.ReadLine()` (0:20)
* Parsing Strings (0:10)
* Splitting Strings (0:15)
* `Console.WriteLine()` (0:20)
* A Fun Program! (The rest of class)

## **Topic List w/Notes**

### Standard I/O

### Reading from `Console.ReadLine()`

Often we may want to pause the execution of the program and wait for the user to provide input. Another approach to allowing user input is by using `Console.ReadLine()`.

**Note**: `Console.ReadLine` and `Console.WriteLine` are abstractions that exist to interact with the standard input/output stream.

<div class="note instructorDirective">

As you write your command-line application in the IDE, show how standard methods from the input and output streams show up in the IntelliSense and how important it is to use those as an initial attempt at figuring out what method to use.

</div>

### Parsing Strings

* What data type is the input from the command line? *Always Strings*
* How do we allow users to enter numbers, dates, times, decimals, etc.?

<div class="definition note">Computers <strong>parse</strong> input and look for symbols that follow an expected syntax or set of rules.</div><br/>

A parse method is available to Java and C# developers that is able to convert a string into the correct data type.

<div class="caution note">If the input string cannot resolve the input to the correct data type, then our program fails and display an error message to the user</div><br/>

``` csharp
int.Parse(string s);
bool.Parse(string s);
double.Parse(string s);
decimal.Parse(string s);
char.Parse(string s);
```

### Splitting Strings

This concept is not covered in the reading, but it is required to perform the individual and pair exercises for the day.

Sometimes we want the user to provide multiple inputs (space/comma/tab separated). This information still arrives to our program in the form of a single string. Strings have a built-in method called `Split()` that enables a string to be divided into an array of strings using a *delimiter character*.

## `Console.WriteLine()`

You can print back to the screen a message when your calculation is done. To do that, you'll use the standard output.

You can use `Console.WriteLine()` to print out a new line. To format a string, you can use `string.Format()`, but string interpolation is much easier: `$"Original Price: {originalPrice}"`.

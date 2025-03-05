# Lecture Code Walkthrough

Show that we have our UI View in `ui.html` and unit tests at `test.html`. Today we're going to talk about functions and documentation.

Show the simplest function, the `returnOne()`. Has a method signature and a body.

Functions also take parameters, like in `printToConsole()`. After opening the `ui.html`, show that you can open the console in the browser and run the function. You can pass in all kinds of values and it prints in the console.

Read the documentation for the next function. We need to take two parameters and return the multiplied result. Have the students walk you through writing this function:

```javascript
function multiplyTogether(firstParameter, secondParameter) {
    return firstParameter * secondParameter;
}
```

After, we can see that the UI has updated and it gives a NaN when no parameters are passed in. How can we write a function to handle that?

```javascript
function multiplyNoUndefined(firstParameter = 0, secondParameter = 0) {
    return firstParameter * secondParameter;
}
```

Now show the students the `returnBeforeEnd()` function. Here you want to show that functions can have multiple `return`s and the rest of the function won't run if the earlier `return` is hit. First, pass `firstParameter` as zero to see the first `return`, then change it to a non-zero value to see the second `return`.

Next, take a look at the `scopeTest` example. Make sure everyone understands what scoping is and how, if you want a variable after a block is finished, you have to create that variable before the block begins.

Questions about `var` might come up here. Let everyone know that `var` is an older style of JavaScript that we will be avoiding, but that it does ignore block scoping and may be seen in a work environment, but it's error-prone and we don't want it used in this course.

Once everyone is comfortable with functions (and they generally should be pretty quickly since they've used them already a lot so far), move on to documentation.

We use JSDoc to document functions. Document the `createSentenceFromUser` with the class. First define what the parameters are, then the optional parameters, then the return type and then a paragraph saying what the function is doing:

```javascript
/**
 * JSDoc Example
 *
 * Take the details of a person and create an English readable sentence
 * that uses that information to describe them. We join the quirks together
 * with the separator, or `, ` by default.
 *
 * @param {string} name the name of the person we're describing
 * @param {number} age the age of the person
 * @param {string[]} [listOfQuirks] a list of funny quirks that we'll list out
 * @param {string} [separator=', '] the string to separate the quirks by
 * @returns {string} the full descriptive string
 */
 ```

Go to `sumAllNumbers` and add an anonymous function to the reduce to add up all the numbers within the given array. Go to the [documentation of `reduce`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduce#Description) to show how to know what parameters you will be given in the anonymous function. Talk about how you don't need them all, just the first two:

```javascript
/**
  * Add each number to the previous sum using reduce
  */
(sum, number) => {
    return sum + number;
}
```

For `allDivisibleByThree`, ask the students how we could achieve this effect to only have an array with numbers divisible by three.

Talk about the `filter` function and [show the documentation for the `filter` function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter#Description). Again, you don't need every parameter, here we just need the first one.

```javascript
return numbersToFilter.filter(
    (number) => {
        return (number % 3) === 0;
    }
);
```

You can also show how you can use a non-anonymous function for the callback too. The non-anonymous functions will use the same parameters in the same order as the anonymous functions.

```javascript
function sumAllNumbersNonAnon(numbersToSum) {
  return numbersToSum.reduce(addNumbers);
}
function addNumbers(sum, number) {
  return sum + number;
}


function allDivisibleByThreeNonAnon(numbersToFilter) {
  return numbersToFilter.filter(isDivisibleByThree);
}
function isDivisibleByThree(number) {
  return number % 3 === 0;
}
```
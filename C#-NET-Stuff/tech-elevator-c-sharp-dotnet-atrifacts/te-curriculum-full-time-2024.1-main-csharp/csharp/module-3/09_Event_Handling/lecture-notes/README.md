# Events Lecture

##### (2018-06-25)

## Overview of Session

In order to interact with a user through the browser, you need a way to get notified in your JavaScript about what the user is doing. Those links between user actions and your code are browser events and the browser event model. Today, you'll be investigating how to tie your code to user actions by the means of events and which events to use in certain situations.

## Session Objectives

- Select DOM objects and attach anonymous functions with `addEventListener()`
- Describe event bubbling and how it works
- Describe default browser behavior and what elements default behavior needs handled for (`form`,`a`)
- List possible event types and what elements get those events
- Describe how to add listeners to newly created DOM elements
- Remove an event listener with `removeEventListener()`

## Classroom Preparation

1. Write an event handler up on the board to talk about the anatomy of adding an event listener:

   ```javascript
   // Get the DOM elements you want to handle events for
   let button = document.getElementById('save_button');

   // Write a function that will handle the event
   function saveForm(event) {
       event.preventDefault();

       ...
   }

   // Add the function to the DOM element by adding is as a listener
   button.addEventListener('click', saveForm); // No parentheses on the function name!
   ```

2. This is completely optional but if you want to cover event delegation in class before the lecture code you can use the readme and example located in the `event-delegation` folder.

## Instructor References

- [MDN UIEvent Documentation](https://developer.mozilla.org/en-US/docs/Web/API/UIEvent)
- [MDN Event Handling Documentation](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Building_blocks/Events)

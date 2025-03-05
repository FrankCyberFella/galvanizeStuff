# CSS Selectors and Box Model Lecture Code Walkthrough

First, instruct students to add a link to `style.css` in their `index.html`.

## Reset page styles

1. Reset some of the page styles for box-sizing and font:

```css
html {
  height: 100%;

  /* Define the default element size to be 16px */
  font-size: 16px;
}

* {
  /* Indicate that the width of the box should include padding and border */
  box-sizing: border-box;
}
```

2. Define the page font and page background:

```css
body {
  /* Change the default page font */
  font-family: 'Raleway', sans-serif;

  /* Gradient Background Image */
  background-image: linear-gradient(to bottom, #858c90, #bbb);

  /* Do not repeat the background, stretch it */
  background-repeat: no-repeat;

  /* Do not scroll the background with the page */
  background-attachment: fixed;
}
```

## Trello list

1. Define each trello-list as `inline-block`:

```css
div.trello-list {
  /* Create multi-column layout */
  display: inline-block;

  /* Fixed width column */
  width: 300px;

  /* Look & Feel */
  background-color: #ddd;

  /* Rounded corners */
  border-radius: 2px 3px;
}
```

2. Style the contents of a `trello-list`:

```css
div.trello-list h2 {
  /* Push H2 from the edge */
  margin: 8px;

  /* Keep font consistent with root document size */
  font-size: 1rem;
  font-weight: bold;
}

div.trello-list ol {
  /* Remove any margin and padding pushing the cards in */
  margin: 0;
  padding: 0;
}

div.trello-list li {
  /* Remove numerical formatting for each card */
  list-style: none;

  /* Set background color of each item; Hex or RGB */
  background-color: rgb(255, 255, 255);

  /* Adjust the font to be smaller than the list title */
  font-size: 0.9rem;

  /* Round each card edge */
  border-radius: 1px;

  /* Spacing between and within list items */
  margin-bottom: 10px;
  padding: 8px;
}

div.trello-list li:hover {
  /* Make the item darker on hover */
  background-color: #f5f6f7;
  color: #092d42;
}

/* The last list item needs to reset its margin */
div.trello-list li:last-child {
  margin-bottom: 0px;
}

div.trello-list a {
  /* Layout */
  display: block;

  /* Look */
  text-decoration: none;
  color: #888;

  /* Spacing */
  padding: 8px 0px 8px 5px; /* top right bottom left */

  font-size: 0.85rem;
}

div.trello-list a:hover {
  background-color: rgba(9, 45, 66, 0.1);
}
```

3. Uncomment out the remaining trello lists. Now you need to add
   margin to space the lists out from each other:

```css
div.trello-list {
  /* Space to the right */
  margin-right: 10px;

  /* Align the topics of each list */
  vertical-align: top;
}
```

4. Unfortunately, everything wraps, so you need to indicate that the elements should not wrap:

```css
body {
  /* Ensures elements don't wrap and forces horizontal scroll */
  white-space: nowrap;
}

div.trello-list li {
  /* The default setting to ensure that the list titles properly wrap */
  white-space: normal;
}
```

## Header

1. Build out the header by defining the child elements as `inline-block` and creating spacing between each of them:

```css
header > h1,
header > p {
  /* Render elements side by side */
  display: inline-block;

  /* Push border away on the right */
  padding-right: 20px;

  /* Middle align content within block */
  vertical-align: middle;

  /* Slight off-shade of white */
  color: rgba(255, 255, 255, 0.8);

  /* Vertical line between items */
  border-right: 1px solid rgba(255, 255, 255, 0.5);
}

header > h1 {
  /* Upper-case the board name */
  text-transform: uppercase;

  /* 1.1 * the root font size */
  font-size: 1.1rem;
}

header > p {
  /* .9 * the root font size */
  font-size: 0.9rem;
  /* Push content away from the left */
  padding-left: 5px;
}

header > p:last-child {
    /* Remove border from last paragraph in the header */
    border: none;
}
```

## Labels

1. The labels on each list card should show colors based on which class is assigned:

```css
.labels span {
  display: inline-block;
  vertical-align: top;
  width: 35px;
  height: 8px;
  border-radius: 3px;
}

.label.pathway {
  background-color: red;
}

.label.individual {
  background-color: green;
}

.label.pair {
  background-color: teal;
}

.label.general {
  background-color: orange;
}
```

## Fixed header

The header could be fixed so that it doesn't scroll offscreen:

```css
header {
  /* Fixed to the window */
  position: fixed;

  /* Up against the top of container */
  top: 0;

  /* Takes up 50px height */
  height: 50px;
}
```

Unfortunately, that makes the main content on the page move up to take its spot, as shown below:

```css
main {
  margin-top: 50px;
}
```

## Scrollbar

Optionally, the scrollbar can be customized:

```css
/* A pseudo-element consists of two colons (::) followed by the name of the pseudo-element. */
body::-webkit-scrollbar {
  width: 10px;
}

body::-webkit-scrollbar-track {
  background: #bbb;
}

body::-webkit-scrollbar-thumb {
  background: #888;
  border-radius: 5px;
}

body::-webkit-scrollbar-thumb:hover {
  background: #888;
}
```

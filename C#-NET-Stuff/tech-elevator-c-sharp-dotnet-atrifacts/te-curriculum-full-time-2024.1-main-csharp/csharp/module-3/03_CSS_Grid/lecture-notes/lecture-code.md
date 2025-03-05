# CSS Grid and Responsive Design Lecture Code Walkthrough

First, show the students the Holy Grail demo. The code is in `lecture-notes/holy-grail-demo`, and you can demo it by opening the folder in Visual Studio Code and running it with Live Server.

Describe how the layout—particularly multiple equal-height columns and full width on the screen—got the name from being almost impossible to do in earlier days of web development, but everyone tried to create it with varying levels of success. CSS Grid allows you to do this with ease.

With the demo page still open, open up the developer tools in Chrome or Firefox, and set the device width to 768px or less. Show how the layout changes to a stacked layout. You'll create both layouts during this lecture. You'll use a media query for the second one.

## Starting code

Open the `sandwiches.html` file with the students and walk through the HTML code. Make sure to highlight that everything is in a `div` with the class of `container`, and there are standard HTML elements they've seen before: `<header>`, two `<nav>`s, `<main>`, `<aside>`, and `<footer>`. Point out that the first `nav` is in the `header`, and that the second `nav` is adjacent to the `header` and other elements and has an `id` of `menu-nav`.

Next, show the students that the HTML file links to a CSS file, `style.css`, which contains the styling for the page and where you'll add code today. You don't need to show the current contents of the file which are just "decoration" styles like colors, text alignment, paddings, and margins. However, if you think it'd be helpful to review some basic CSS, you can do so.

Then run `sandwiches.html` in a browser by right-clicking on the file and selecting "Open with Live Server." You can show the students that the rendered HTML doesn't look like the "Holy Grail" layout yet.

## Setting up the grid

Open `style.css`. Set up five CSS rules that select the `header`, `nav#menu-nav`, `main`, `aside`, and `footer` elements. For each selector, define a `grid-area` property with a reasonable name for each:

```CSS
header {
    grid-area: header;
}

nav#menu-nav {
    grid-area: menu-nav;
}

main {
    grid-area: main;
}

aside {
    grid-area: upcoming-events;
}

footer {
    grid-area: footer;
}
```

Remind the students that everything in the HTML body is in a `div` with the class of `container`. The next rule you'll set up uses a `.container` selector, or `div.container` if you prefer:

```CSS
.container {
    display: grid;
    grid-template-columns: 200px 1fr 200px;
    grid-template-areas:
        "header header header"
        "menu-nav main upcoming-events"
        "footer footer footer"
    ;
    height: 100vh;
    gap: 10px;
}
```

Have the students help you define the value for the `display` property. Define the `grid-template-columns` for them, so they understand that the two `200px` declarations are for the sides, and the `1fr` is for the remaining space.

Now that you've set up the dimensions of the grid, define the `grid-template-areas` property. Have the students help you define each block of the grid.

Tell the students that you want the container to be the height of the entire viewport, and ask them what value accomplishes that. See if anyone answers `100vh`. Remind them that `vh` stands for "viewport height", and one unit represents 1% of that value, so 100 means 100%.

You can define the `gap` value for the students. Explain that it adds some space between the grids of the layout.

Save the CSS and refresh the page in the browser. You'll see a layout similar to the "Holy Grail" demo. Shrink the viewport by using the browser developer tools, and show that it doesn't look good on smaller screens.

> Note: you may see the sidebar content begin to rearrange at widths less than 768px. There are already some media queries in `style.css`.

## Setting up the mobile view with media queries

Visit the "Holy Grail" demo again and show how the layout changes to stacked items on smaller screen widths. Tell the students that this is what's known as "responsive design": using multiple CSS rules to have the same webpage fit appropriately on multiple screen widths. The demo uses a screen width of 768px or less to apply the CSS rules for the stacked layout. Media queries make this possible.

In `style.css`, add a media query with a `max-width` of 768px, and add the same selector you used for the grid—`.container` or `div.container`:

```CSS
@media screen and (max-width: 768px) {
    .container {
        grid-template-areas:
            "header"
            "menu-nav"
            "main"
            "upcoming-events"
            "footer";
        grid-template-columns: 1fr;
    }
}
```

See if you can get the students to help you fill in `grid-template-areas` and `grid-template-columns`. Only one column is sufficient for the entire layout, and it must take up the entire width. `100%` is acceptable too, but persuade them to use `1fr` to reinforce the `fr` units.

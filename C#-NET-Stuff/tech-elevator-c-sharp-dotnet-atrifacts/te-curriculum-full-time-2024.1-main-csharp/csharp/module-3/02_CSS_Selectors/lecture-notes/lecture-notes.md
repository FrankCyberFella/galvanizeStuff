# Introduction to CSS Selectors and Layout Positioning

## Notes and examples

### HTML document structure

- Review the hierarchical nature of an HTML Document, including parents, descendants, and siblings.

### CSS selectors

- **CSS can be included**:

  - inline on any element with a `style` attribute
  - within the document under a `<style>` element
  - through an external document reference with a `<link href=>` element

- **CSS Rules**

  - selector
  - declaration block

- **Selectors**

  - universal
  - element
  - id
  - class
  - descendant combinator
    - descendant selector (space)
    - child selector (>)
    - adjacent sibling selector (+)
    - general sibling selector (~)
  - pseudo-class (for example, `a:hover`)

- Create a system that organizes the rules to keep organized

### Box model

![Image of Box Model](https://www.w3schools.com/css/box-model.gif)

- Every element in web design is a rectangular box.
- The content, padding, border, and margin can be used to calculate the amount of space that an element takes up.
- Use the image above as an analogy to draw.
  - Margin is the space **outside an element**. It does not affect the size of the box but affects other content that interacts with the box.
  - Padding is the space **inside an element**.
- The default width of a box isn't 100% but moreso _"whatever is left."_
  - There are various circumstances where it is useful to **set** or **not set** a width.

**New CSS3 Property**

- `box-sizing: content-box|border-box|initial|inherit` tells the browser what sizing properties it should include.

### Display attribute

<div class="definition note">The display property specifies if and how an element should be displayed.</div>

- Every HTML element has a default display value depending on what type of element it is. The default display value for most elements is **block** or **inline**.

- A **block-level element** always starts on a **new line** and takes up the **full width** available—meaning it stretches out to the left and right as far as it can.

  Examples of block level elements include:

  - `<div>`
  - `<h1>` - `<h6>`
  - `<p>`
  - `<form>`
  - `<header>`
  - `<footer>`
  - `<section>`

- An **inline element** does not start on a new line and only takes up **as much width as necessary**.

  Examples of inline elements include:

  - `<span>`
  - `<a>`
  - `<img>`

- Can use `display:none` to hide an element. This is the default for the `<script>` tag.

- **inline** is the default value for elements like `<span>` and `<em>`.
  - It **accepts margin and padding** while ignoring height and width.
- **inline-block** is very similar to inline, but **it allows you to set a width and height**.

### Positioning

- The **normal flow** of a page is for elements to appear left to right and top to bottom based on the order in which they appear in the HTML document and the rules of block and inline display.
- **Static** position by default means the element **conforms to normal flow**.
- **Relative** position means **relative to where it would otherwise be positioned in the normal flow**.
  - You can set the _top, right, bottom, and left_ positioning attributes.
- **Absolute** position places the element relative to the parent ancestor—that is, the containing element—**exactly where you specify**.
  - These elements are removed from the flow of the page.
  - Setting both _top_ and _bottom_, or both _left_ and _right_ allows you to "stretch" an element's dimensions.
- **Fixed** position is **relative to the browser window** and does not scroll with the page.
  - You can set the _top, right, bottom, and left_ positioning attributes.

### Floats

<div class="definition note">The float property specifies if the element should be taken from the normal flow and placed along the left or right side of the container. Text
and inline elements wrap around it.</div>

The `float` property can have several values:

- **none**: element does not float.
- **left**: element floats to the left of its container.
- **right**: element floats to the right of its container.
- **inherit**: element inherits the float direction of its parent.
- Floated elements automatically display as block.

Floats should be avoided in modern web applications. In most cases, they were historically used to create multi-column layouts in older browsers.

## Exercise

The students should use the provided image template to build out their GitHub portfolio page.

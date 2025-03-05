# Lecture Code Walkthrough

During this lecture, you'll build out an application that lists out product reviews for the book, Cigar Parties for Dummies. When you're done with this application it'll look like this.

![Product Reviews Complete](img/product-reviews-complete.png)

## Lecture code

The starting code is in this directory in the `product-reviews` folder. The students can find the same starting code in their `lecture` folder. You can get the final solution for lecture code in the `lecture-final` directory.

## Lecture overview

The entire UI you see in the screenshot gets built from data. You'll start with some variables like page title, description, and reviews, and from there, build out the UI. You'll build this out by creating elements and adding them to the DOM.

In the first approach, you'll see how cumbersome it can be to write even some simple markup to the DOM. When you start to feel the pain of this approach, you'll show them another solution using HTML Template `<template></template>`. You won't dive into this much today but you just want them to know its available. The students get a more in depth tutorial on the template tag later in this module.

## DOM lecture code

If you want to start off by showing the final solution you can do so by going to `/lecture-final/product-reviews` and running the `index.html` file. This might give them a better idea of what you're building to in the lecture today.

When you're ready to begin, open up `index.html` and show what code you'll be starting off with. There is an heading 2 element with an ID and an incomplete page title. You'll fill in the span with the product name. There is also a page description that'll go in the paragraph element that has a class of description:

```html
<div id="main" class="mx-auto">
    <h2 id="page-title">Product Reviews for <span class="name"></span></h2>
    <p class="description"></p>
    <!-- reviews go here -->
</div>
```

Next you can open up `app.js` and talk about the variables that you see declared:

```javascript
const bookName = 'Cigar Parties for Dummies'
const description = 'Host and plan the perfect cigar party for all of your squirrelly friends.';
const reviews = [
    {
        reviewer: "Malcolm Madwell",
        title: 'What a book!',
        review: "It certainly is a book. I mean, I can see that. Pages kept together with glue and there's writing on it, in some language. Yes indeed, it is a book!",
        rating: 3
    },
    {
        reviewer: "Tim Ferriss",
        title: 'Had a cigar party started in less than 4 hours.',
        review: "It should have been called the four hour cigar party. That's amazing. I have a new idea for muse because of this.",
        rating: 4
    },
    {
        reviewer: "Ramit Sethi",
        title: 'What every new entrepreneurs needs. A door stop.',
        review: "When I sell my courses, I'm always telling people that if a book costs less than $20, they should just buy it. If they only learn one thing from it, it was worth it. Wish I learned something from this book.",
        rating: 1
    },
    {
        reviewer: "Gary Vaynerchuk",
        title: 'And I thought I could write',
        review: "There are a lot of good, solid tips in this book. I don't want to ruin it, but prelighting all the cigars is worth the price of admission alone.",
        rating: 3
    }
];
```

Using those variables you're going to fill in the method stubs in `app.js` to add the information to the page.

### Page title

The first thing you're going to do is to set the page title. To do so you're going to get at the h2 by it's ID. This is a good time to talk about `getElementById()` vs `querySelector()`. Always use `getElementById()` when you can because it performs better:

```javascript
function setPageTitle() {
    const pageTitle = document.getElementById('page-title');
    pageTitle.querySelector('.name').innerText = bookName;
}

```

This is a good time to talk about the dangers of using innerHTML if you haven't already covered it. The following was in the student book and it probably needs repeating.

> #### Caution::Using innerHTML could get you hacked
>
> Anything passed to `innerHTML` gets read and rendered into the living DOM of the browser. That could be really dangerous. If you ever take input from a user and then use `innerHTML` to put that into an element, you're setting yourself up for what's called a Cross Site Scripting Attack (XSS).
>
> If a user is able to add HTML to your page, that means that they can embed JavaScript into your page using a `<script>` element and *that* means they can use all these methods to completely rewrite your page, including making it look like a login page that sends usernames and passwords to their own site instead of yours.
>
> Never, ever send user inputted data to an `innerHTML` call. When taking user input, always use `innerText` to add their content to a DOM element.

If you want to show off a quick demo of this create a new HTML file and paste in the following code and run it:

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>innerHTML demo</title>
</head>
<body>

    <h1>innerHTML</h1>
    <div class="content">

    </div>

    <script>
    const el = document.querySelector('.content');
    const name = "<img src='x' onerror='alert(1)'>";
    el.innerHTML = name; // shows the alert
    </script>
</body>
</html>
```

### Page description

The description doesn't have an id so you need another way to get at that element. Talk through some of the ways you could get a reference to that element. You don't really have a good example to show of `querySelectorAll()` so if you want to pop open any web page, inspect the DOM and write something in the console to show this off:

```javascript
function setPageDescription() {
    document.querySelector('.description').innerText = description
}
```

### Reviews

In the previous examples you had some existing markup in place. For the reviews you are going to start with nothing and build up all of the markup you need and add it to the DOM. You may like to start off by showing the little markup it takes to display a single review. You could throw it in the `index.html` and show the single review. Just remember to remove it before moving on:

```html
<div class="review">
    <h4>Malcolm Madwell</h4>
    <div class="rating">
        <img src="img/star.png" class="ratingStar"/>
    </div>
    <h3>What a book!</h3>
    <p>It certainly is a book. I mean, I can see that. Pages kept together with glue and there's writing on it, in some language. Yes indeed, it is a book!</p>
</div>
```

Now that you know what the markup looks like, how can you create that for every review in your array. In the `displayReviews()` method you are going get a reference to the main `div` because inside of this element is where you are going to append each review.

From there you'll loop over each element in the array, create a new div element with a class of review and add it to your main element using [insertAdjacentElement()](https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentHTML):

```javascript
function displayReviews() {
    const main = document.getElementById('main')

    reviews.forEach(review => {
        const container = document.createElement('div')
        container.setAttribute('class','review')

        // ... add reviewer, rating, title and review

        main.insertAdjacentElement('beforeend',container)
    })

}
```

The point of adding all of this content to the DOM with JavaScript is to just show how much control you have and at the same time how cumbersome it can be. Libraries and Frameworks take away a lot of this pain but before you know what problems they solve you must experience them first.

#### Add reviewer's name

```javascript
function displayReviews() {
    const main = document.getElementById('main')

    reviews.forEach(review => {
        const container = document.createElement('div')
        container.setAttribute('class','review')
        addReviewer(container,review.reviewer)
    })

}
```

```javascript
function addReviewer(parent,name) {
    const reviewer = document.createElement('h4')
    reviewer.innerText = name
    parent.appendChild(reviewer)
}
```

#### Add rating

```javascript
function displayReviews() {
    const main = document.getElementById('main')

    reviews.forEach(review => {
        const container = document.createElement('div')
        container.setAttribute('class','review')
        addReviewer(container,review.reviewer)
        addRating(container,review.rating)
        main.insertAdjacentElement('beforeend',container)
    })

}
```

```javascript
function addRating(parent,numberOfStars) {
    const rating = document.createElement('div')
    rating.setAttribute('class','rating')
    for(let n = 0; n < numberOfStars; ++n ) {
        const star = document.createElement('img')
        star.setAttribute('class','ratingStar')
        star.src = 'img/star.png'
        rating.appendChild(star)
    }
    parent.appendChild(rating)
}
```

#### Add review title

```javascript
function displayReviews() {
    const main = document.getElementById('main')

    reviews.forEach(review => {
        const container = document.createElement('div')
        container.setAttribute('class','review')
        addReviewer(container,review.reviewer)
        addRating(container,review.rating)
        addTitle(container,review.title)
        main.insertAdjacentElement('beforeend',container)
    })

}
```

```javascript
function addTitle(parent,title) {
    const h3 = document.createElement('h3')
    h3.innerText = title
    parent.appendChild(h3);
}
```

#### Add review

```javascript
function displayReviews() {
    const main = document.getElementById('main')

    reviews.forEach(review => {
        const container = document.createElement('div')
        container.setAttribute('class','review')
        addReviewer(container,review.reviewer)
        addRating(container,review.rating)
        addTitle(container,review.title)
        addReview(container,review.review)
        main.insertAdjacentElement('beforeend',container)
    })

}
```

```javascript
function addReview(parent,review) {
    const feedback = document.createElement('p')
    feedback.innerText = review
    parent.appendChild(feedback)
}
```

## Another solution

The idea here is to just show them that there is an easier solution. You aren't going too much into this today because they won't need it in the exercise and you'll cover this in much more detail later on in in this module. You can also mention that when you get into **Vue** they'll see the template tag.

>The HTML Content Template `<template>` element is a mechanism for holding client-side content that's not rendered when a page gets loaded but may be subsequently instantiated during runtime using JavaScript.

Instead of creating all of the elements needed for your markup, you can start with a template and just fill in the data. If you open up the `index-template.html` you'll see the following template:

```html
<template id="review-template">
<div class="review">
    <h4></h4>
    <div class="rating">
        <img src="img/star.png" class="ratingStar"/>
    </div>
    <h3></h3>
    <p></p>
</div>
</template>
```

Any markup between the `<template></template>` won't get rendered when the page loads. You give the template an ID so you can refer to it later. If you open `js/app-template.js` you'll notice the following code that replaces all of that code you wrote before to display the reviews:

```javascript
function displayReviews() {
  if ('content' in document.createElement('template')) {
    const main = document.getElementById('main');
    reviews.forEach(review => {
      const tmpl = document.getElementById('review-template').content.cloneNode(true)
      tmpl.querySelector('h4').innerText = review.reviewer
      tmpl.querySelector('h3').innerText = review.title
      tmpl.querySelector('p').innerText = review.review
      // there will always be 1 star because it is a part of the template
      for(let i=1; i<review.rating; ++i) {
        const img = tmpl.querySelector('img').cloneNode()
        tmpl.querySelector('.rating').appendChild(img);
      }
      main.appendChild(tmpl)
    })
  } else {
    console.error('Your browser does not support templates');
  }
}
```

If you want you can briefly cover what's going on with this code but again, you'll cover this in depth a little bit later in the module.

## Conclusion

Just to recap these were your session objectives:

* Describe the difference between the DOM and HTML
* Select elements from the DOM using `getElementById`, `querySelector`, `querySelectorAll`
* Describe the DOM and how it's structured (tree)
* Set `innerText` on HTML elements
* Describe why `innerHTML` can be dangerous
* Create new DOM elements using `createElement()` and `insertAdjacentElement()`
* Traverse the DOM
* Investigate the living DOM in the browser

And this lecture went the painful way of writing elements to show that you can do it, but that anytime you start to write any decent amount of markup you can reach for the template tag.

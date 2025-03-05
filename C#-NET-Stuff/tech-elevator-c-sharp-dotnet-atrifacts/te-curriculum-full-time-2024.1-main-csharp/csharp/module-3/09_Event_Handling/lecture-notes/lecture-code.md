# Event Handling: Lecture Code Walkthrough

In this lecture you'll be working with the product reviews application that you built out yesterday. The starting code for this lecture is in the `product-reviews`. If you want to start off by showing the finished solution that's located in the `lecture-final/product-reviews` folder.

You are only going to have a single review when the application loads this time. This is because you'll be using a form to add new reviews and you wanted to leave room for this.

## DOM content loaded

Yesterday while you were building out the product reviews application you just called some methods at the bottom of the template to setup and build your application:

```javascript
// Set the product reviews page title.
setPageTitle();
// Set the product reviews page description.
setPageDescription();
// Display all of the product reviews on our page.
displayReviews();
```

This is a good opportunity to write your first event listener.

>The `DOMContentLoaded` event fires when the initial HTML document has been completely loaded and parsed, without waiting for stylesheets, images, and subframes to finish loading.
>
>A different event—`load`—only detects a fully loaded page. It's a very common mistake to use `load` where `DOMContentLoaded` would be much more appropriate, so be cautious:

```javascript
document.addEventListener('DOMContentLoaded', () => {
  setPageTitle()
  setPageDescription()
  displayReviews()
})
```

Now you can be sure that when you are getting references to elements they're available. Everything else you'll do is inside of this event handler.

## Description

The description at the top of the page is currently a variable in `app.js` and the `setPageDescription()` writes that value to the DOM. You are going to add some features that allows the user to update that description:

![Description](img/product-reviews-description.gif)

If you look at `index.html` you can see that there is a paragraph tag to hold your description text and then a hidden input field:

```html
<p class="description"></p>
<input id="inputDesc" class="d-none form-control form-control-lg" type="text"/>
```

The way this works is when the user clicks on the description you swap out hide the description and show the text box. If the user moves out of the text box or hits escape you can swap them back with no changes. Finally if the user changes the text and hits enter you can update the description.

The first thing you'll need to do is get a reference to the description element. From there you can add a click event listener and in the event handler you'll call a method called `toggleDescriptionEdit()` which is looking for the description. You already have a reference to it but this is a good chance to show off the event argument and that you can get the target and pass that:

```javascript
const desc = document.querySelector('.description')
desc.addEventListener('click',(event) => {
  toggleDescriptionEdit(event.target)
})
```

At this point you can talk about what the `toggleDescriptionEdit()` function does but you won't need to write this from scratch:

```javascript
function toggleDescriptionEdit(desc) {
  let textBox = desc.nextElementSibling
  textBox.value = description
  textBox.classList.remove('d-none')
  desc.classList.add('d-none')
  textBox.focus()
}
```

Next you are going to get a reference to the input and add a couple of event listers. The first is going to be listening for the `keyup` event. If the user is in the text box and hits the `enter` key you can assume that they want to save the text. If they hit the `esc` key it means they want to exit without saving.

> This is also a good time to talk about keyboard events https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent


```javascript
const inputDesc = document.getElementById('inputDesc')
inputDesc.addEventListener('keyup',(event) => {
  if( event.key === 'Enter' ) {
    exitDescriptionEdit(event.target,true)
  }
  if( event.key === 'Escape' ) {
    exitDescriptionEdit(event.target,false)
  }
})
```

The second argument to the method `exitDescriptionEdit` instructs your program whether or not to update the DOM with the new description. That's the only difference so to stay DRY you'll use a single method for both. This would be a good time to take a look at the logic for that method:

```javascript
function exitDescriptionEdit(textBox,save) {
  let desc = textBox.previousElementSibling
  if( save ) {
    desc.innerText = textBox.value
  }
  textBox.classList.add('d-none')
  desc.classList.remove('d-none')
}
```

The other event listener listens for the `mouseleave` event and do the same thing the `esc` key does:

```javascript
inputDesc.addEventListener('mouseleave',(event) => {
  exitDescriptionEdit(event.target,false)
})
```

## Add new review

Next, you're tasked with allowing users to add new reviews. There isn't any form validation to keep this simple and focus on events but this is something you would want to think about in a real world application.

### Show/hide add review form

When the user clicks on the **Add Review** button you'll show the add review form which is in the markup but it's hidden by default. You'll also change the text of the button so that it now says **Hide Form**:

![Show/Hide Form](img/show-hide-form.gif)

To make this work you'll get a reference to the `btnToggleForm` button. With that you can add a `click` event listener and call the `showHideForm()` method:

```javascript
const btnToggleForm = document.getElementById('btnToggleForm')
btnToggleForm.addEventListener('click',() => {
  showHideForm()
})
```

The `showHideForm()` function is already available but this would be a good time to walk through the logic:

```javascript
function showHideForm() {
  const form = document.querySelector('form')
  const btn = document.getElementById('btnToggleForm')

  if( form.classList.contains('d-none') ) {
    form.classList.remove('d-none')
    btn.innerText = 'Hide Form'
    document.getElementById('name').focus()
  } else {
    resetFormValues()
    form.classList.add('d-none')
    btn.innerText = 'Add Review'
  }
}
```

> You can highlight the line that put the focus on the name field so the user can start typing.

### Save review

Now that you can show the Add Review form it's time to make it work. You'll start off by getting a reference to save review button and adding a click event listener to it. You'll need the event object in your handler so that you can call `event.preventDefault()` before you call `saveReview()`. This would be a good time to discuss why you need to do this:

https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault

```javascript
const btnSaveReview = document.getElementById('btnSaveReview')
btnSaveReview.addEventListener('click', (event) => {
  event.preventDefault()
  saveReview()
})
```

Now that you are calling `saveReview()` you need to build out this method. You are going to get a reference to each of the form elements and construct a new review object with each of the values:

```javascript
function saveReview() {
  const name = document.getElementById('name')
  const title = document.getElementById('title')
  const review = document.getElementById('review')
  const rating = document.getElementById('rating')

  const newReview = {
    reviewer: name.value,
    title: title.value,
    review: review.value,
    rating: rating.value
  }
  reviews.push(newReview)
  displayReview(newReview)
  showHideForm()
}
```

> You could do this without getting the reference first but without that you don't get the IntelliSense letting you know that value is a property of the element.

Next you'll push the new review to the array and display it. This is a good time to talk about how JavaScript isn't reactive. If you just add a new item to the array it doesn't automatically display it so you have to manually call `displayReview()`. This is where something like Vue really shines with it's reactivity system:

```javascript
function saveReview() {
  const name = document.getElementById('name')
  const title = document.getElementById('title')
  const review = document.getElementById('review')
  const rating = document.getElementById('rating')

  const newReview = {
    reviewer: name.value,
    title: title.value,
    review: review.value,
    rating: rating.value
  }
  reviews.push(newReview)
  displayReview(newReview)
  showHideForm()
}
```

The last line of the code hides the form and there is a line in there that calls the `resetFormValues()` method so the next time it's shown you are starting with a fresh form:

```javascript
function showHideForm() {
  const form = document.querySelector('form')
  const btn = document.getElementById('btnToggleForm')

  if( form.classList.contains('d-none') ) {
    form.classList.remove('d-none')
    btn.innerText = 'Hide Form'
    document.getElementById('name').focus()
  } else {
    resetFormValues()
    form.classList.add('d-none')
    btn.innerText = 'Add Review'
  }
}
```

```javascript
function resetFormValues() {
  const form = document.querySelector('form')
  const inputs = form.querySelectorAll('input')
  inputs.forEach(input => {
    input.value = ''
  })
  document.getElementById('rating').value = 1
  document.getElementById('review').value = ''
}
```


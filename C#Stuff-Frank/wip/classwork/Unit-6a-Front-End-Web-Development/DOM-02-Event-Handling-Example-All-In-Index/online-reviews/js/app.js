const productName = "Quark's Bar";
const description = 'Located in Deep Space 9, near Bajor in the Alpha Quadrant';
const reviews = [
  {
    reviewer: 'Marcus Aurilious',
    title: 'Better have a lot of latinum!',
    review:
      "About what you would expect on a space station.  VERY expensive",
    rating: 3
  },
  {
    reviewer: 'Morg',
    title: 'My favorite place in the Alpha Quadrant',
    review:
      "I am a regular at this place an love it!",
    rating: 5
  },
  {
    reviewer: 'Garak',
    title: 'Ferengi owned - you know what that means! Premium prices, low quality',
    review:
      "Can't trust the Ferengi.  Gaming Room is fixed, no one wins.  Dabo girls will rob you blind.  Holosuites substandard.",
    rating: 1
  },
  {
    reviewer: 'Nog',
    title: 'Uncle Quark is a great host!',
    review:
      "Patrons seem to enjoy themselves and love my Uncle Quark.  He is bit hard on me though",
    rating: 4
  }
];

/**
 * Add our product name to the page title
 * Get our page page title by the id and the query the .name selector
 * once you have the element you can add the product name to the span.
 */
function setPageTitle() {
  const pageTitle = document.getElementById('page-title');
  pageTitle.querySelector('.name').innerHTML = productName;
}

/**
 * Add our product description to the page.
 */
function setPageDescription() {
  document.querySelector('.description').innerHTML = description;
}

/**
 * I will display all of the reviews in the reviews array
 */
function displayReviews() {
  // If there is template in the HTML... use it to add to the DOM
  // This will create HTML based on the template in the HTML
  if ('content' in document.createElement('template')) {
    // Loop through the array of review objects
    reviews.forEach((review) => {
      displayReview(review);
    });
  } else {
    console.error('Your browser does not support templates');
  }
}

/**
 *
 * @param {Object} review The review to display
 */
function displayReview(review) {
  // get a reference to the main main div
  const main = document.getElementById('main');
  // get a reference to a copy of the template defined in the main div
  const tmpl = document.getElementById('review-template').content.cloneNode(true);
  // Set the various elements in the template to the values we want 
  tmpl.querySelector('h4').innerHTML = review.reviewer;
  tmpl.querySelector('h3').innerHTML = review.title;
  tmpl.querySelector('p').innerHTML = review.review;
  // there will always be 1 star because it is a part of the template
  // so the for loop starts at 1 instead of 0
  for (let i = 1; i < review.rating; ++i) {
    const img = tmpl.querySelector('img').cloneNode(); // Make a copy of the img element
    tmpl.querySelector('.rating').appendChild(img);    // add the new img element to the div
  }
  main.appendChild(tmpl);
}

// New Stuff STARTS HERE ---------------------------------------------------------------
//
// Any time the user interacts with a web page an EVENT is generated
// An EVENT is a user interaction with a web page or a point in the life of the web page
//
// We tell the browser to "listen" for a particular event and let us process it
//  .addEventListener() is how you tell the browser which events you want to process
// you provide an anonymous method to process the event (named method may also be used)


// When the DOM is created by the browser (DOMContentLoaded event)/
//      run the the three functions we have to set up the page
//      and set up an event listener for a click by the user on the element with class="description"
//
// Is complicated HTML it make the the browser a tiny bit of time to get the DOM built
// Since you JavaScript processing depends on the DOM being fully built...
//       you don't want to start manipulating the DOM until it's completes
document.addEventListener('DOMContentLoaded', () => {
  setPageTitle();
  setPageDescription();
  displayReviews();

  // When a user clicks on the description show input box
  const desc = document.querySelector('.description'); // Find the element with class="description"
  desc.addEventListener('click', (event) => {          // Listen for a click on it....
    toggleDescriptionEdit(event.target);               //     if click event occurs, run the named method
  });

  // When the user presses enter in the new description text box
  //     run the save the changes and redisplay with new text
  const inputDesc = document.getElementById('inputDesc');
  inputDesc.addEventListener('keyup', (event) => { 
    if (event.key === 'Enter') {
      exitDescriptionEdit(event.target, true);  // call method to save new description
    if (event.key === 'Escape') {
      exitDescriptionEdit(event.target, false); // call the method to discard the new descripitons
    }
  }});


  // When the mouse exits the new description text box
  inputDesc.addEventListener('mouseleave', (event) => {
    exitDescriptionEdit(event.target, false);
  });

  // Show/Hide the Add Review Form
  const btnToggleForm = document.getElementById('btnToggleForm');
  btnToggleForm.addEventListener('click', () => { // When the "add Review" button is clicked
    showHideForm();                               // call this function to handle
  });

  // save the review and display it
  const btnSaveReview = document.getElementById('btnSaveReview');
  btnSaveReview.addEventListener('click', (event) => {  // When the "Save Review" is clicked...
    // Prevent the default behavior of a HTML for to send the data to an API URL with an HTTP Request
    // (We want to process the data ourselves)
    event.preventDefault();                             // DO NOT SEND THE FORM TO AN API URL!
    saveReview();                                       // Call the method to save the data
  });
});

/**
 * Take an event on the description and swap out the description for a text box.
 *
 * @param {Event} event the event object
 *                when the event happens and event object is passed to the method
 */
function toggleDescriptionEdit(desc) {
  const textBox = desc.nextElementSibling;  // Get a reference to the next sibling of the element that was clicked
  textBox.value = description; // Remember the text it has now
  textBox.classList.remove('d-none');  // Remove the "d-none" class to make it visible
  desc.classList.add('d-none');        // Add the "d-none" class to hide the current description
  textBox.focus();                     // Put the cursor (focus) on the textbox element
}

/**
 * Take an event on the text box and set the description to the contents
 * of the text box and then hide the text box and show the description.
 *
 * @param {Event} event the event object
 * @param {Boolean} save should we save the description text
 */
function exitDescriptionEdit(textBox, save) {
  let desc = textBox.previousElementSibling;
  if (save) {  // if true was passed save ne description
    desc.innerText = textBox.value;
  }
  textBox.classList.add('d-none');  // Hide the new description text box
  desc.classList.remove('d-none');  // Redisplay the description field
}

/**
 * I will show / hide the add review form
 */
function showHideForm() {
  const form = document.querySelector('form');          // Get a reference to the new review form
  const btn = document.getElementById('btnToggleForm'); // Get a reference tp the "Add Review" button

  if (form.classList.contains('d-none')) {  // If the form is hidden
    form.classList.remove('d-none');        // un-hide it
    btn.innerText = 'Hide Form';            // change its caption to "Hide Form"
    document.getElementById('name').focus();// Put cursor in the id="name" element on the form
  } else {
    resetFormValues();                      // If the form is being displayed
    form.classList.add('d-none');           // Hide it 
    btn.innerText = 'Add Review';           // set caption to "Add Review"
  }
}

/**
 * I will reset all of the values in the form.
 */
function resetFormValues() {
  const form = document.querySelector('form');
  const inputs = form.querySelectorAll('input');
  inputs.forEach((input) => {
    input.value = '';
  });
  document.getElementById('rating').value = 1;
  document.getElementById('review').value = '';
}

/**
 * I will save the review that was added using the add review from
 */
function saveReview() {
  // Copy the data from form to the JavaScript code
  // 1. Get a reference to each input element on the form
  // 2. Assign the data from the input element to an JSON attribute in the array element
  const name   = document.getElementById('name');
  const title  = document.getElementById('title');
  const review = document.getElementById('review');
  const rating = document.getElementById('rating');

  const newReview = {
    reviewer: name.value, // Assign name from form
    title: title.value,   // Assign title from form
    review: review.value, // Assign review from form
    rating: rating.value  // Assign rating from form
  };
  reviews.push(newReview); // Add the new review to our array of reviews
  displayReview(newReview);// DIsplay teh new review (add it to the DOM)
  showHideForm();          // Hide the form
}

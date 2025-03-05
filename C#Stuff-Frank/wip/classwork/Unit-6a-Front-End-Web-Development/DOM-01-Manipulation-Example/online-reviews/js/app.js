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
  // .getElementId() - return a reference to the element with that id in the DOM
  const pageTitle = document.getElementById('page-title');  // Get a pointer to 'page-title' element
  // Change the HTML text for element will class="name" in the pageTitle
  // .querySelelctor - return a reference to the first thingit finds that matches the CSS selector given
  pageTitle.querySelector('.name').innerHTML = productName; // change the item with class=name to have what's in name
//  Alternative Technique
//  const titleWord = pageTitle.querySelector('.name');
//  titleWord.innerHTML = name;
}

/**
 * Add our product description to the page.
 */
function setPageDescription() {
  // Find me the reference to the element with class="description"
  document.querySelector('.description').innerText = description; // Get to the tag with the class 'description'
                                                                  // assign the value in variable description
}

/**
 * I will display all of the reviews on the page.
 * I will loop over the array of reviews and use some helper functions
 * to create the elements needed for our markup and add them to the DOM
 */
function displayReviews() {
  const main = document.getElementById('main');  // Get to the div that will hold the reviews

  // forEach in JavaScript will loop through an array one element at a time
  // giving the current element the name specified (aReview in this example)
  // Notice use of an anonymous function  arrow functions
  reviews.forEach((aReview) => {
    const container = document.createElement('div');  // Create a div hold the review
    container.setAttribute('class','review');         // add class='review' to the div
    addReviewer(container, aReview.reviewer);         // Send the method the parent and data to add
    addRating(container, aReview.rating);             // Send the method the parent and data to add           
    addTitle(container, aReview.title);               // Send the method the parent and data to add 
    addReview(container, aReview.review);             // Send the method the parent and data to add
    main.appendChild(container);                      // Once the element is contructed, add to the parent
  } ) 

}

/**
 * I will creating a new h4 element with the name of the reviewer and append it to
 * the parent element that is passed to me.
 *
 * @param {HTMLElement} el: The element to append the reviewer to
 * @param {string} name The name of the reviewer
 */
function addReviewer(parent, name) {
const reviewer = document.createElement('h4');  // Create an h4 element
reviewer.innerText = name;                      // assign the name to the h4 tag
parent.appendChild(reviewer);                   // add the reviewer as a child to the parent
}

/**
 * I will add the rating div along with a star image for the number of ratings 1-5
 * @param {HTMLElement} parent
 * @param {Number} numberOfStars
 */
function addRating(parent, numberOfStars) {
  const rating = document.createElement('div');  // Create a div to hold rating
  rating.setAttribute('class','rating');         // class=rating to the div
  for(let i=0; i < numberOfStars; i++) {           // Add one star based on the rating number
    const star = document.createElement('img');    // create an img tag for the star
    star.setAttribute('class','ratingStar');       // add a class=rating star to img tag
    star.src='img/star.png';                       // add a src=image-file-productName to img tag
    rating.appendChild(star);                      // Çonnect the star to rating
  }
  parent.appendChild(rating);                    // Connect the div to the parent
}

/**
 * I will add an h3 element along with the review title
 * @param {HTMLElement} parent
 * @param {string} title
 */
function addTitle(parent, title) {
  const theTitle = document.createElement('h3');
  theTitle.innerText = title;
  parent.appendChild(theTitle);
}

/**
 * I will add the product review
 * @param {HTMLElement} parent
 * @param {string} review
 */
function addReview(parent, review) {
  const feedback = document.createElement('p');
  feedback.innerText = review;
  parent.appendChild(feedback);
}

// This is where the functions we wrote get called to manipulate the DOM
// Remember functions don't execute unless they are called

// set the product reviews page title
setPageTitle();
// set the product reviews page description
setPageDescription();
// display all of the product reviews on our page
displayReviews();

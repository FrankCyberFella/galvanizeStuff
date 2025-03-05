# Lecture Code Walkthrough

> Note: It's recommended **not** to walk-through new Vue project creation in class. This can be time consuming and opens up potential individual student troubleshooting issues best addressed outside of class.

## Vue project structure

Walk the students through the initial project structure, starting with the `/src` folder. The `/src` folder contains all of the Vue code. The `/src/components` folder contains the Vue components, and each `.vue` file contains all the code for a single component.

The `/src/assets` folder contains the files for images or other assets used by the application, like the Vue logo used by the starter code or the `star.png` used later in the product reviews application.

## Create a new component

Make a new `ProductReview.vue` in the `/src/components` folder.

Add HTML to the template:

``` HTML
<template>
    <div class="main">
        <h2>Product Reviews for {{ name }}</h2>

        <p class="description">{{ description }}</p>
    </div>
</template>
```

This is the HTML of the component - the display to the user. When a parent component contains this component as a child, everything _inside_ the `template` tag gets included in the parent page's HTML. You won't see the `<template>` tag in the parent, just its contents. The double curly brackets signify one-way data binding.

Next, you'll create data properties that hold the values you want to show in those spots. The curly brackets tie the values of those properties to those elements.

```html
<script>
export default {
  data() {
    return {
      name: 'Head First Design Patterns',
      description:
        'A brain friendly guide to building extensible and maintainable object-oriented software.',
    }
  }
}
</script>
```

This defines your Vue component object. Everything after the `export default` is a JavaScript object that holds all the component's data properties and methods.

Here, you've defined a name for the component, `product-review`, and data properties that you data bound to the view.

> #### Warning::`data()` must return an object
>
> One thing to point out to students is that the data method on the Vue object must return another object that holds the data properties. This is because every component needs to have its own data properties. This is a standard pattern that they'll always use for a component's `data`.

Finally, add the following CSS:

``` CSS
<style scoped>
div.main {
  margin: 1rem 0;
}
</style>
```

This is the component's default style. This is where you put anything that works with the component's layout or look.

That's all you need for a working component.

## Add the component to App.vue

You can now add the new component to a page by adding it to the main `App.vue` component. This is the component's parent component.

``` HTML
<template>
  <div id="product-review-app">
    <product-review></product-review>
    <img alt="Vue logo" src="./assets/logo.png">
    <HelloWorld msg="Welcome to Your Vue.js App"/>
  </div>
</template>

<script>
import HelloWorld from './components/HelloWorld.vue'
import ProductReview from './components/ProductReview.vue'

export default {
  components: {
    HelloWorld,
    ProductReview
  }
}
</script>
```

To make this page look nicer, add a `max-width: 800px` and some margins so that it doesn't hit the top of the page and auto to center align the component. All you need to focus on are the last two styles.

```css
#product-review-app {
  font-family: "Avenir", Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  max-width: 800px;
  margin: 60px auto 0 auto;
}
```

Run the application using `npm run dev`. Visit http://localhost:5173, and you'll see your component at the top of the page. It has the product name in a larger `h2` tag and the description underneath that. Inspect the page to examine the component's HTML on the DOM.

> #### Removing the HelloWorld Component
>
> If you want to remove the boilerplate code that's generated, you need to do the following: remove the HelloWorld component from the template, and then in the script block, remove the import statement and HelloWorld from the components block.

## More complex component with data binding

Now that you have a simple component up and running, make it handle reviews on the book as an array of objects. Add this to the component's data:

``` JavaScript
data() {
  return {
    name: 'Head First Design Patterns',
    description:
      'A brain friendly guide to building extensible and maintainable object-oriented software.',
    reviews: [
      {
        id: 1000,
        reviewer: 'R Pérez',
        title: 'Approachable pattern guide',
        review:
          'I love the uncomplicated, informal narrative style. I highly recommend this text for anyone trying to understand Design Patterns in a super simple way.',
        rating: 4
      },
    ]
  };
}
```

If needed, feel free to add more objects to that array as well. Talk about how these objects are just JavaScript objects that the students have seen before.

Next, go to the HTML portion of your component and add the following HTML under the book details:

``` HTML
<div class="review" v-for="review in reviews" v-bind:key="review.id">
    <h4>{{ review.reviewer }}</h4>
    <div class="rating">
        <img src="../assets/star.png" v-bind:title="review.rating + ' Star Review'" class="ratingStar" v-for="n in review.rating" v-bind:key="n"/>
    </div>
    <h3>{{ review.title }}</h3>

    <p>{{ review.review }}</p>
</div>
```

You've seen most of this in the tutorial, but there are a few new things here. You use the `v-for` directive to loop through the reviews, but you also use `v-for` to print out the correct number of stars for the review. If you pass `v-for` an integer, it prints that element that many times.

You also use the `v-bind` directive to add a `title` attribute to each image. This shows that you can use logic in your `v-bind` to do more complex things than just put values into the attributes. (Hover over a star image to reveal the title.)

Finally, add some CSS classes to the component to layout the reviews better:

``` CSS
<style scoped>
.main {
  margin: 1rem 0;
}

.well-display {
  display: flex;
  justify-content: space-around;
  margin-bottom: 1rem;
}

.well {
  display: inline-block;
  width: 15%;
  border: 1px black solid;
  border-radius: 6px;
  text-align: center;
  margin: 0.25rem;
  padding: 0.25rem;
}
.amount {
  color: darkslategray;
  display: block;
  font-size: 2.5rem;
}

.favorited {
  background-color: lightyellow;
}

.rating {
  height: 2rem;
  display: inline-block;
  vertical-align: top;
  margin: 0 0.5rem;
}

.rating img {
  height: 100%;
}

.review {
  border: 1px black solid;
  border-radius: 6px;
  padding: 1rem;
  margin: 10px;
}
.review p {
  margin: 20px;
}

.review h3 {
  display: inline-block;
}

.review h4 {
  font-size: 1rem;
}
</style>
```

## Adding ratings with computed properties

> ### Why not Vue methods?
>
> The following portion of the lecture code contains some duplication, and opportunity for refactoring. Given the project size there were limited chances to showcase computed properties and provide repetition.
> A callout to refactor this portion of the code using `methods` resides in `02_Vue_Event_Handling`.

Now that you have the reviews showing, add some boxes to the top of the reviews list that shows how many reviews the list contains. You want to use your data to fill in those fields at the top.

Since you already have all the review data, you can make computed properties to show how many of each review type you have. You already know how many 1-star reviews you have because you have the reviews. If a new review ever gets added, you want that number to also update programmatically, hence the computed properties.

First, add sections to the top of the page that reserve space for these properties to be shown. Add this below the description:

``` HTML
<div class="well-display">
    <div class="well">
        <span class="amount"><!-- data binding goes here --></span>
        Average Rating
    </div>

    <div class="well">
        <span class="amount"><!-- data binding goes here --></span>
        1 Star Review
    </div>

    <div class="well">
        <span class="amount"><!-- data binding goes here --></span>
        2 Star Review
    </div>

    <div class="well">
        <span class="amount"><!-- data binding goes here --></span>
        3 Star Review
    </div>

    <div class="well">
        <span class="amount"><!-- data binding goes here --></span>
        4 Star Review
    </div>

    <div class="well">
        <span class="amount"><!-- data binding goes here --></span>
        5 Star Review
    </div>
</div>
```

You can fill these values in by using `computed` properties. `computed` properties are properties that are calculated off of `data` properties, but then that calculation is cached to speed up the component. If the underlying data ever changes, the property is computed again. These are often called derived properties in other languages.

So create a `computed` property for the average rating. For this, add up all the review ratings and then divide that by the number of reviews:

``` JavaScript
computed: {
    averageRating() {
      if (this.reviews.length === 0) {
        return 0;
      }

      // Use reduce to get the total of all the ratings
      let sum = this.reviews.reduce((currentSum, review) => {
        return currentSum + review.rating;
      }, 0);

      // Divide by the number of reviews
      return sum / this.reviews.length;
    },
}
```

`computed` properties are just functions that return the value you want. You can then use them as you would normal `data` properties, as shown below:

``` HTML
<div class="well">
    <span class="amount">{{ averageRating }}</span>
    Average Rating
</div>
```

Then you can add these kinds of properties for each review count as well:

``` JavaScript
computed: {
    numberOfOneStarReviews() {
      const oneStarReviews = this.reviews.filter((review) => {
        return review.rating === 1;
      });
      return oneStarReviews.length;
    },
    numberOfTwoStarReviews() {
      const twoStarReviews =  this.reviews.filter((review) => {
        return review.rating === 2;
      });
      return twoStarReviews.length;
    },
    numberOfThreeStarReviews() {
      const threeStarReviews =  this.reviews.filter((review) => {
        return review.rating === 3;
      });
      return threeStarReviews.length;
    },
    numberOfFourStarReviews() {
      const fourStarReviews =  this.reviews.filter((review) => {
        return review.rating === 4;
      });
      return fourStarReviews.length;
    },
    numberOfFiveStarReviews() {
      const fiveStarReviews =  this.reviews.filter((review) => {
        return review.rating === 5;
      });
      return fiveStarReviews.length;
    }
}
```

Now, you can use those new `computed` properties in your UI, also showing an example of toggling the 's' on reviews if the number of reviews is 1 or not:

``` HTML
<div class="well">
    <span class="amount">{{ numberOfOneStarReviews }}</span>
    1 Star Review{{ numberOfOneStarReviews === 1 ? '' : 's' }}
</div>

<div class="well">
    <span class="amount">{{ numberOfTwoStarReviews }}</span>
    2 Star Review{{ numberOfTwoStarReviews === 1 ? '' : 's' }}
</div>

<div class="well">
    <span class="amount">{{ numberOfThreeStarReviews }}</span>
    3 Star Review{{ numberOfThreeStarReviews === 1 ? '' : 's' }}
</div>

<div class="well">
    <span class="amount">{{ numberOfFourStarReviews }}</span>
    4 Star Review{{ numberOfFourStarReviews === 1 ? '' : 's' }}
</div>

<div class="well">
    <span class="amount">{{ numberOfFiveStarReviews }}</span>
    5 Star Review{{ numberOfFiveStarReviews === 1 ? '' : 's' }}
</div>
```

Now they've seen `computed` properties and how to create aggregated data from existing component values.

## Favorites

Finally, add a new checkbox to the review, under the description:

``` HTML
<p>Favorite? <input type="checkbox" v-bind:id="'favorite_' + review.id" v-model="review.favorited"/></p>
```

This connects directly to a `favorited` data property on the data. Add that data property to all the reviews in the list, defaulting them to false.

> #### Two-Way Data Binding
>
> You use the v-model directive to create two-way data bindings on form inputs, textareas, and select elements. This is Two-Way data binding because when the input is updated, the data property is updated and when the data property is updated, the input element is updated.

Use that `favorited` data property to toggle a class on the review. You can use the `v-bind` directive to set new classes on the `class` attribute. You can use this in combination with the existing classes that are already on the `div`. Add a new CSS class:

``` CSS
div.main div.review.favorited {
    background-color: lightyellow;
}
```

Then, go to the review `div` and add the data binding. For classes, add a new `v-bind` directive for `class` that describes an object where the key is the class to be added, and the value is a boolean that says whether the class should added or not.

``` HTML
<div class="review" v-bind:class="{ favorited: review.favorited }" v-for="review in reviews" v-bind:key="review.id">
```

## _(Optional)_ Debugging

At any point during this lecture, or subsequent Vue lectures, you can introduce the students to debugging Vue applications in the browser.

Refer to [this document](debugging-vue.md) for details.

## Summary

This project showed:

- That components contain three sections: HTML for UI, JavaScript for logic, and CSS for look and layout
- Data properties in the component can be bound to UI elements on the screen
- `v-for` directive can be used to loop through data properties and create repeating elements
- Computed properties can be used to bind derived calculations of data properties
- Properties can be bound to attributes on HTML elements using `v-bind` directive

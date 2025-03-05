# Vue Router Lecture Code Walkthrough

In today's class, you'll use the product review application from a previous lecture. The starting code has Vue Router installed with all default routes removed. There's some initial seed data for products and reviews in `/src/store/index.js`, which the components use.

## Existing application

The topics to cover are:

- `/src/router/index.js`: Vue Router Configuration
- `/src/store/index.js`: Vuex Store
- `/src/views/`: Vue Router has added this directory
- `/src/main.js`: Changes made here to import Vue Router
- `/src/components/ProductsList.vue`: A component to display a list of products from the store
- `/src/App.vue`: Does nothing currently

If you want, you can run the application to show that there are no errors and no output. This is because `App.vue` isn't doing anything right now, but you'll fix that later.

## Products list

There is a new `ProductsList` component in `/src/components` that's responsible for retrieving a list of products from the store, iterating over them, and displaying each product. Your first task is to create a new view in the `/views` folder called `ProductsView.vue`:

```html
<template>
  <div class="products">
    <h1>Products</h1>
    <products-list />
  </div>
</template>

<script>
  import ProductsList from '../components/ProductsList.vue';

  export default {
    components: {
      ProductsList
    }
  };
</script>
```

### View versus components

This is a great time to discuss the difference between the `/components` and `/views` folders. Both folders contain Vue components; semantically, views are similar to other components. What distinguishes a view component from other components is that have an associated route. Views get data for and display the various logic components for a route. The components in the `/components` folder are logic components meant to hold business logic, serve a single purpose, and are often reusable.

You can also think of `/views` as *pages* that you can navigate to in a Single Page Application. Each view assembles other components and static data necessary for that *page*. The `/src/views/ProductsView.vue` view is the page that uses the `/src/components/ProductsList.vue` component to display a list of products.

### Adding routes

Start by defining a new route in `/src/router/index.js`. This route loads the `ProductsView` view when someone visits the root of the application, `/`:

```js
import { createRouter as _createRouter, createWebHistory } from 'vue-router';
import ProductsView from '../views/ProductsView.vue';

const routes = [
  {
    path: '/',
    name: 'products',
    component: ProductsView
  }
];

export function createRouter () {
  return _createRouter({
    history: createWebHistory(),
    routes: routes
  })
}
```

### Router view

You can run this and ask the students why nothing displays. If you look in `App.vue`, there is nothing defined in the template. To display the correct view, you need to include the `<router-view>` component:

```html
<template>
  <router-view />
</template>

<script>
  export default {};
</script>
```

> The `<router-view>` component is a functional component that renders the matched component for the given path.

If you run the application again, you'll see the list of products.

## Product detail

When this next step is complete, you'll be able to click the product name from the list of products to view the product detail page. This page shows product details and a list of reviews. You need to update the `ProductsList` to link to the `ProductDetailView` page, but before you do that, you'll need to create a new route and view.

### Product detail route

First, you'll add a new route in `/src/router/index.js`. This is a dynamic route that a user can visit. Below are some examples:

- /products/1
- /products/2
- /products/3

The `ProductDetailView` view doesn't exist yet, but you'll create it in the next step.

Add the `product-detail` route to the `routes` array in `/src/router/index.js`:

```js
import ProductsView from '../views/ProductsView.vue';
import ProductDetailView from '../views/ProductDetailView.vue';

const routes = [
  {
    path: '/',
    name: 'products',
    component: ProductsView
  },
  {
    path: '/products/:id',
    name: 'product-detail',
    component: ProductDetailView
  }
];
```

> You create a dynamic segment by using a colon `:`. When matching a route, the value of the dynamic segments becomes `this.$route.params`. The name of the param follows the colon, so in this case the id is accessible as `this.$route.params.id`.

### Product detail view

With the route created, you need to create a new view: `/src/views/ProductDetailView.vue`. The view gets the product id from the URL and uses that to get the product to display:

```js
export default {
  computed: {
    product() {
      // Get product id from the URL
      let productId = this.$route.params.id;
      let product = this.$store.state.products.find(p => p.id == productId);
      return product;
    },
  },
}
```

The rest of this component is similar to what was in `App.vue` from a previous lecture, but passes in the `reviews` prop to the `AverageSummary`, `StarSummary`, and `ReviewList` components. This is a good time to explain that a view is just a component that can consist of data and other components. 

```html
<template>
  <h1>{{ product.name }}</h1>
  <p class="description">{{ product.description }}</p>
  <div class="actions">
      <a href="#">Back to Products</a>&nbsp;|
      <a href="#">Add Review</a>
  </div>
  <div class="well-display">
    <average-summary v-bind:reviews="product.reviews"/>
    <star-summary 
        v-for="i in 5" 
        v-bind:rating="i" 
        v-bind:key="i"
        v-bind:reviews="product.reviews" />
  </div>
  <review-list v-bind:reviews="product.reviews"/>
</template>

<script>
import AverageSummary from '../components/AverageSummary.vue';
import StarSummary from '../components/StarSummary.vue';
import ReviewList from '../components/ReviewList.vue';

export default {
  components: {
    AverageSummary,
    StarSummary,
    ReviewList
  },
  computed: {
    product() {
      // Get product id from the URL
      let productId = this.$route.params.id;
      let product = this.$store.state.products.find(p => p.id == productId);
      return product;
    },
  },
};
</script>

<style scoped>
.well-display {
  display: flex;
  justify-content: space-around;
  margin-bottom: 1rem;
}
.actions {
  margin: 2rem;
}
</style>
```

The `AverageSummary`, `StarSummary`, and `ReviewList` components were already updated to declare and use the new `reviews` prop. You may want to review these updates with the students.

You can test the new view manually navigating to a URL like `/products/1`. In the next section, you'll add links to the `ProductsList` page so you can click through to the detail page.

### Router links

If you look at the list of products (`src/components/ProductsList.vue`), there's no way to click through to each product to see the list of reviews. To make the product name clickable, navigating to `/products/:id`, add a `<router-link>` tag around the product name:

```html
<template>
  <table>
    <thead>
      <tr>
        <th>ID</th>
        <th>Name</th>
        <th># Reviews</th>
      </tr>
    </thead>
    <tbody>
      <tr v-for="product in $store.state.products" v-bind:key="product.id">
        <td>{{ product.id }}</td>
        <td>
          <router-link
            v-bind:to="{ name: 'product-detail', params: { id: product.id }}">
            {{ product.name }}
          </router-link>
        </td>
        <td>{{ product.reviews.length }}</td>
      </tr>
    </tbody>
  </table>
</template>
```

In `/src/views/ProductDetailView.vue`, update the `Back to Products` link to use `<router-link>`:

```html
<div class="actions">
  <a href="#">Back to Products</a>&nbsp;|
  <a href="#">Add Review</a>
</div>
```

You could do something like this:

```html
<div class="actions">
  <router-link to="/">Back to Products</router-link>&nbsp;|
  <a href="#">Add Review</a>
</div>
```

However, a better practice is to use named routes. If the path for the `ProductsListView` changes to `/products` in the future, and you hard-coded `/` everywhere, you'd need to go through and update each of those links. With named routes, you don't have to worry about that:

```html
<div class="actions">
  <router-link v-bind:to="{ name: 'products' }">Back to Products</router-link>&nbsp;|
  <a href="#">Add Review</a>
</div>
```

## Add new review

The last feature in this application is to take the `AddReview` form from the previous lecture that was inline and move it into a view.

First, add the `add-review` route to the `routes` array in `/src/router/index.js`. The path includes the product ID, so the `AddReview` form knows what product you're adding a review to.

```js
import ProductsView from '../views/ProductsView.vue';
import ProductDetailView from '../views/ProductDetailView.vue';
import AddReviewView from '../views/AddReviewView.vue';

const routes = [
  {
    path: '/',
    name: 'products',
    component: ProductsView
  },
  {
    path: '/product/:id',
    name: 'product-detail',
    component: ProductDetailView
  },
  {
    path: '/product/:id/add-review',
    name: 'add-review',
    component: AddReviewView
  }
];
```

Next, create the view `/src/views/AddReviewView.vue`. This view imports the `AddReview` component, which is responsible for creating a new review and committing the mutation to the store.

Similar to the `ProductDetailView`, use a computed property to get the `productId` from the URL. Then pass it into the `AddReview` child component using `props`:

```html
<template>
  <h1>Add Review</h1>
  <add-review v-bind:productId="productId"/>
</template>

<script>
import AddReview from '../components/AddReview.vue';

export default {
  components: {
    AddReview
  },
  computed: {
    productId() {
      // Get product id from the URL
      return this.$route.params.id
    },
  }
};
</script>
```

Open `/src/components/AddReview.vue`, and review the updates to declare the new `productId` prop and use it in the `addNewReview` method.

There is one last update to make in lecture:

```js
methods: {
  addNewReview() {
    this.newReview.productId = this.productId;
    this.$store.commit('ADD_REVIEW', this.newReview);
    // TODO: send the visitor back to the product page to see the new review
  },
  resetForm() {
    this.newReview = {};
  }
}
```

Ask the students if they know a way to programmatically send the user to the product detail page after adding the new review:

```js
this.$router.push({ name: 'product-detail', params: { id: this.productId } });
```

Finally, update the Add Review link in `/src/views/ProductDetailView.vue`:

```html
<div class="actions">
  <router-link v-bind:to="{ name: 'products' }">Back to Products</router-link>&nbsp;|
  <router-link v-bind:to="{ name: 'add-review', params: { id: product.id } }">
    Add Review
  </router-link>
</div>
```

## Vue DevTools

You may also want to demonstrate using the Router view in the Vue DevTools to show the active route object. This object contains information about the matched route including route params.

## Note: Prop declaration

The components in the reading and this lecture use the array syntax to declare props. For example:

```js
props: ['productId'],
```

However today's exercise code as well as some of the code provided in later projects uses the longer object syntax to declare props. 

```js
    props: {
        book: Object,
        enableAdd: {
            type: Boolean,
            default: false
        }
    },
```

This allows the specification of the prop type and can mark it as required. Using this can eliminate the need to convert types and produce better error messages in situations where a required prop isn't passed in. 

Students aren't expected to declare props using this syntax, but may encounter it in documentation and other resources as well. If there is time, you may want to show and discuss this syntax with your students. In the exercise, the `BookCard` component uses this syntax. For more information, see the [Vue documentation](https://vuejs.org/guide/components/props.html#props-declaration)


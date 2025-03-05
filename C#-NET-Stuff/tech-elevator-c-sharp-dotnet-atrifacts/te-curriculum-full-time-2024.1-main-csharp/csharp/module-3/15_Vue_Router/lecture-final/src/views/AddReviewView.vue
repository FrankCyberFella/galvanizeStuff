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

<style>
</style>

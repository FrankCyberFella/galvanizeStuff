<template>
  <form class="new-book-form" v-on:submit.prevent="saveBook">
    <input name="title-input" class="title-input" type="text" placeholder="Title" v-model="book.title" />
    <input name="author-input" class="author-input" type="text" placeholder="Author" v-model="book.author" />
    <input name="isbn-input" class="isbn-input" type="text" placeholder="ISBN" v-model="book.isbn" />
    <button>Save</button>
  </form>
</template>

<script>
export default {
  data() {
    return {
      book: {}
    }
  },
  methods: {
    saveBook() {
      this.$store.commit('SAVE_BOOK', this.book);
      this.book = {};
    }
  }
}
</script>

<style>
.new-book-form {
  margin: 20px;
}

.new-book-form input, .new-book-form button {
  margin: 10px;
  font-size: 1rem;
}
</style>
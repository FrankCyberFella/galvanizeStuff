<template>
  <div id="book-app">
    <h1>Reading List</h1>
    <reading-list />
    <new-book-form />
  </div>
</template>

<script>
import ReadingList from './components/ReadingList.vue';
import NewBookForm from './components/NewBookForm.vue';

export default {
  components: {
    ReadingList,
    NewBookForm
  }
}
</script>

<style>
#book-app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin: 60px;
}
</style>

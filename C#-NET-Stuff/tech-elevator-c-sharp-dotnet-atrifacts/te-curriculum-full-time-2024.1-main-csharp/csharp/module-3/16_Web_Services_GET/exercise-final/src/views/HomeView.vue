<template>
  <div class="loading" v-if="isLoading">
    <p>Loading...</p>
  </div>
  <div v-else>
    <header class="flex">
      <h1>Topics</h1>
    </header>
    <topic-list v-bind:topics="topics"/>
  </div>
</template>

<script>
import topicService from '../services/TopicService.js';
import TopicList from '../components/TopicList.vue';

export default {
  components: {
    TopicList
  },
  data() {
    return {
      topics: [],
      isLoading: true
    };
  },
  methods: {
    getTopics() {
      // TODO - Get data from API and set `topics` property
      topicService.list()
        .then(response => {
          this.topics = response.data;
          this.isLoading = false;
        })
    },
  },
  created() {
    this.getTopics();
  }
}
</script>

<style scoped>
</style>
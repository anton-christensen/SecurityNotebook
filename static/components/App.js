

export default {
  el: '#app',
  template: `
    <div id="app">
      <div id="notebookElements">
        <div class="notebook-component" v-for="element in elements">
          <span class="name">#{{ element.name }} - {{element.id}}</span>
          <span class="deleteComponent" v-on:click="deleteComponent(element.id)">x</span>
          <component :key="element.id" v-bind:is="element.name"></component>
        </div>
      </div>

      <span>Add component</span>
      <select v-model="selected">
        <option v-for="analysis in analyses" v-bind:value="analysis.value">
          {{ analysis.text }}
        </option>
      </select>
    </div>
  `,
  data: {
    analyses: [
      {value: "", text: "-- Select new component --"},
      {value: "markdown", text: "Markdown"},
      {value: "analysis", text: "Analysis"},
    ],
    selected: '',
    id: 0,

    elements: [],
  },
  watch: {
    selected: function(newVal) {
      if(newVal) {
        this.elements.push({id: this.id++, name: newVal});
        this.selected = "";
      }
    }
  },
  methods: {
    deleteComponent: function(id) {
      this.elements = this.elements.filter(elm => elm.id != id);

    }
  },
  mounted: function() {
    this.elements.push({id: this.id++, name: 'markdown'})
    this.elements.push({id: this.id++, name: 'analysis'})
  },
};


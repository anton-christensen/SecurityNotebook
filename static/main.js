
import App from './components/App.js';

import Analysis from './components/Analysis.js';
import Markdown from './components/Markdown.js';

import GraphvizDisplay from './components/generic/GraphvizDisplay.js';
import MarkdownDisplay from './components/generic/MarkdownDisplay.js';
import CodeInput from './components/generic/CodeInput.js';


Vue.component('Analysis', Analysis);
Vue.component('Markdown', Markdown);

Vue.component('GraphvizDisplay', GraphvizDisplay);
Vue.component('MarkdownDisplay', MarkdownDisplay);
Vue.component('CodeInput', CodeInput);

const app = new Vue(App);
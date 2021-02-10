
import App from './components/App.js';

import Analysis from './components/Analysis.js';
import Markdown from './components/Markdown.js';
import TextInput from './components/TextInput.js';
import TextOutput from './components/TextOutput.js';
import AnalysisOutput from './components/AnalysisOutput.js';
import MarkdownOutput from './components/MarkdownOutput.js';


import GraphvizDisplay from './components/generic/GraphvizDisplay.js';
import MarkdownDisplay from './components/generic/MarkdownDisplay.js';
import CodeInput from './components/generic/CodeInput.js';
import ComponentRef from './components/generic/ComponentRef.js';


Vue.component('Analysis', Analysis);
Vue.component('Markdown', Markdown);
Vue.component('TextInput', TextInput);
Vue.component('TextOutput', TextOutput);
Vue.component('AnalysisOutput', AnalysisOutput);
Vue.component('MarkdownOutput', MarkdownOutput);

Vue.component('GraphvizDisplay', GraphvizDisplay);
Vue.component('MarkdownDisplay', MarkdownDisplay);
Vue.component('CodeInput', CodeInput);
Vue.component('ComponentRef', ComponentRef);

const app = new Vue(App);
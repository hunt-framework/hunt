var Elm = require('./src/Main.elm');

require('./hunt.scss');

console.group('Hunt client');
console.log('Starting up...');

var container = document.getElementById('hunt-app');
var app = Elm.Main.embed(container);

console.log('Running, have fun! :-)');
console.groupEnd();

const move = require('glob-move');

move('dist/*.{js,css,txt,gif,jpg,png,ttf,eot,svg,woff,woff2}', 'dist/static')
    .then(() => 'Static files moved successfully!')
    .catch(console.error);

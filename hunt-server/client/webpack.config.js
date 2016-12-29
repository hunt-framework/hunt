// IMPORTS

const webpack = require('webpack');
const ExtractTextPlugin = require('extract-text-webpack-plugin');

// STYLE

const prodStyle = {
  test: /\.(css|scss)$/,
  loader: ExtractTextPlugin.extract('style-loader', [
    'css-loader',
    'sass-loader'
  ])
};

const devStyle = {
  test: /\.(css|scss)$/,
  loaders: [
    'style-loader', 'css-loader', 'sass-loader'
  ]
};


// PLUGINS

const prodPlugins = [
  new ExtractTextPlugin('hunt.css'),
  new webpack.optimize.OccurenceOrderPlugin(), 
  new webpack.optimize.UglifyJsPlugin({
    compressor: { warnings: false }
  })
];


// CONFIG

const PRODUCTION = process.env.npm_lifecycle_event === 'build';

module.exports = {
  entry: './index.js',

  output: {
    path: __dirname + '/build',
    publicPath: '/build/',
    filename: 'hunt-bundle.js'
  },

  module: {
    noParse: /\.elm$/,
    loaders: [{
      test: /\.(eot|ttf|woff|woff2|svg)$/,
      loader: 'file-loader'
    }, {
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      loader: 'elm-webpack-loader?debug=' + (!PRODUCTION)
    }, PRODUCTION ? prodStyle : devStyle
    ]
  },

  plugins: PRODUCTION ? prodPlugins : []
};



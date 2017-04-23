// IMPORTS

const webpack = require('webpack');
const ExtractTextPlugin = require('extract-text-webpack-plugin');

// STYLE

const prodStyle = {
  test: /\.(css|scss)$/,
  loader: ExtractTextPlugin.extract([
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
  new webpack.LoaderOptionsPlugin({
    minimize: true,
    debug: false
  }),
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

  devServer: {
    port: 8000,
    proxy: {
      '/api': {
        target: 'http://localhost:3000',
        pathRewrite: {'^/api' : '' }

      }
    }
  },

  module: {
    noParse: /\.elm$/,

    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      loader: 'elm-webpack-loader?debug=' + (!PRODUCTION)
    }, PRODUCTION ? prodStyle : devStyle
    ]
  },

  plugins: PRODUCTION ? prodPlugins : []
};



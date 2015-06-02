var webpack = require('webpack')

module.exports = {
  entry: {
    app:  ['./app.js']
  },

  output: {
    publicPath: "/build/",
    path: __dirname + "/build/",
    filename: "bundle.js"
  },

  externals: {
    "react" : "React",
    "react-router" : "ReactRouter",
    "lodash" : "_",
    "moment" : "moment"
  },

  resolve: {
    extensions: ['', '.js']
  },

  module: {
    loaders: [
      { test: /\.js/,
        exclude: /node_modules/,
        loaders: ["jsx-loader", "babel-loader"]
      }
    ]
  },

  plugins: [
    new webpack.NoErrorsPlugin()
  ]
};

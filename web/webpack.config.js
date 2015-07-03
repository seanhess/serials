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

  resolve: {
    extensions: ['', '.js']
  },

  module: {
    loaders: [
      { test: /\.js/,
        exclude: /node_modules/,
        loaders: ["react-hot", "jsx-loader", "babel-loader"]
      },
      { test: /\.css$/, loader: "style-loader!css-loader" }
    ]
  },

  plugins: [
    new webpack.NoErrorsPlugin()
  ]
};

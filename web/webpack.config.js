var webpack = require('webpack')

module.exports = {
  entry: {
    app:  [
      'webpack-dev-server/client?http://localhost:3000',
      'webpack/hot/only-dev-server',
      './index.js',
    ]
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
      }
    ]
  },

  plugins: [
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoErrorsPlugin(),
    new webpack.IgnorePlugin(/vertx/)
  ]
};

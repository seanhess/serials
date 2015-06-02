var webpack = require('webpack');
var WebpackDevServer = require('webpack-dev-server');
var config = require('./webpack.config');


// hack to make it work for both production and hmr
config.entry.app = [
  'webpack-dev-server/client?http://localhost:3000',
  'webpack/hot/only-dev-server'
].concat(config.entry.app)


config.plugins = [
  new webpack.HotModuleReplacementPlugin()
].concat(config.plugins)

new WebpackDevServer(webpack(config), {
  publicPath: config.output.publicPath,
  hot: true,
  historyApiFallback: true,
  stats: { colors: true },
  proxy: { "*" :{target: "http://localhost:3001"}}
}).listen(3000, 'localhost', function (err, result) {

  if (err) {
    console.log(err);
  }

  console.log('Listening at localhost:3000');
});

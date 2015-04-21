// @flow

var React = window.React = require('react')

var Router = require('react-router')
var {Route, DefaultRoute, RouteHandler, NotFoundRoute, Redirect} = require('react-router')

var {Admin} = require('./app/admin/admin')
var {Sources} = require('./app/admin/sources')
var {Source} = require('./app/admin/source')
var {Main} = require('./app/books/main')
var {Gallery} = require('./app/books/gallery')
var {Book} = require('./app/books/book')

import {assign} from 'lodash'

class App extends React.Component {
  render() {
    return <RouteHandler {...this.props}/>
  }
}

class Home extends React.Component {
  render() {
    return <ul>
      <li><a href="#/admin/sources">Sources</a></li>
    </ul>
  }
}

var routes = (
  <Route handler={App} path="/">
    <Redirect from="/" to="books" />
    <Route name="books" handler={Main}>
      <DefaultRoute handler={Gallery}/>
      <Route name="book" path=":id" handler={Book} />
    </Route>
    <Route name="admin" handler={Admin}>
      <Route name="sources" handler={Sources}/>
      <Route name="source"  path="sources/:id" handler={Source}/>
    </Route>
  </Route>
)

Router.run(routes, function (Handler, state) {
  React.render(<Handler />, document.body)

  loadAll(state.routes, state.params)
  .then(function(data) {
    React.render(<Handler {...data} params={state.params}/>, document.body)
  })
})

function loadAll(routes, params) {
  var data = {};
  return Promise.all(routes
    .filter(route => route.handler.load)
    .map(function(route) {
      return route.handler.load(params)
      .then(function(d) {
        data = assign(data, d)
      }, onError)
    })
  ).then(() => data, onError);
}

function onError(err) {
  throw err
}

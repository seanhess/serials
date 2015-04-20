// @flow

var React = window.React = require('react')
var ask = require('ask')
var axios = require('axios')

import Router from 'react-router'
import {Route, DefaultRoute, RouteHandler, NotFoundRoute} from 'react-router'

import {Admin} from './app/admin/admin.js'
import {Sources} from './app/admin/sources.js'
import {Source} from './app/admin/source.js'

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
    <DefaultRoute handler={Home} />
    <Route name="admin" handler={Admin}>
      <Route name="sources" handler={Sources}/>
      <Route name="source"  path="sources/:id" handler={Source}/>
    </Route>
  </Route>
)

//class Home extends React.Comp

    //<NotFoundRoute handler={NotFound}/>
    //<Route name="about" handler={About} />
    //<Route name="users" handler={Users}>
      //<Route name="recent-users" path="recent" handler={RecentUsers} />
      //<Route name="user" path="/user/:userId" handler={User} />
      //<NotFoundRoute handler={UserRouteNotFound}/>
    //</Route>
    //<Redirect from="company" to="about" />

Router.run(routes, function (Handler, state) {
  React.render(<Handler data={{}}/>, document.body)

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
      })
    })
  ).then(() => data);
}

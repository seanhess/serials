// @flow

var React = window.React = require('react')
var ask = require('ask')
var axios = require('axios')

import Router from 'react-router'
import {Route, DefaultRoute, RouteHandler, NotFoundRoute} from 'react-router'

import {Admin} from './app/admin/admin.js'
import {Sources} from './app/admin/sources.js'
import {Source} from './app/admin/source.js'

class App extends React.Component {
  render() {
    return <RouteHandler {...this.props}/>
  }
}

var routes = (
  <Route handler={App} path="/">
    <Route name="admin" handler={Admin}>
      <Route name="sources" handler={Sources}/>
      <Route name="source"  path="sources/:id" handler={Source}/>
    </Route>
  </Route>
)

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
    React.render(<Handler data={data} params={state.params}/>, document.body)
  })
})

function loadAll(routes, params) {
  var data = {};
  return Promise.all(routes
    .filter(route => route.handler.load)
    .map(route => {
      return route.handler.load(params).then(d => data[route.name] = d)
    })
  ).then(() => data);
}

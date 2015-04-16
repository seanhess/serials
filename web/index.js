// @flow

var React = window.React = require('react')

import Router from 'react-router'
import {Route, DefaultRoute, RouteHandler, NotFoundRoute} from 'react-router'

import {Admin, AdminSource, AdminSources} from './app/admin/admin.js'

class App extends React.Component {
  render() {
    return <RouteHandler />
  }
}

var routes = (
  <Route handler={App} path="/">
    <Route name="admin" handler={Admin}>
      <Route name="sources" handler={AdminSources}/>
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

Router.run(routes, function (Handler) {
  React.render(<Handler/>, document.body)
})


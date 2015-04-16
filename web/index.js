// @flow

var React = window.React = require('react')

import {Test} from './app/blah.js'

import Router from 'react-router'
import {Route, DefaultRoute, RouteHandler, NotFoundRoute} from 'react-router'

class App extends React.Component {
  constructor(props) {
    super(props)
  }

  render() {
    return <div>App: <Test /> <RouteHandler /></div>
  }
}

class Home extends React.Component {
  render() {
    return <div>Hello! Goodbye die die die</div>
  }
}

class NotFound extends React.Component {
  render() {
    return <div>Not Found</div>
  }
}

var routes = (
  <Route handler={App} path="/">
    <Route name="test" handler={Test} />
    <NotFoundRoute handler={NotFound}/>
  </Route>
)

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


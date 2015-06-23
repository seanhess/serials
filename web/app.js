// @flow


// Import react and support deubgger
import React from 'react'
window.React = React

import Router from 'react-router'
import {Route, DefaultRoute, RouteHandler, NotFoundRoute, Redirect, Link} from 'react-router'

import {loadAll, run, render} from './app/data/load'

import {ImportLog, AdminDashboard} from './app/admin/admin'
import {Sources} from './app/admin/sources'
import {Source} from './app/admin/source'
import {Invites} from './app/admin/invites'
import {AdminUsers} from './app/admin/users'

import {Header} from './app/layout/header'
import {Main} from './app/layout/main'
import {NotFound} from './app/layout/notFound'
import {Library} from './app/books/library'
import {Bookshelf} from './app/books/bookshelf'
import {Book} from './app/books/book'
import {Read} from './app/books/read'
import {Login} from './app/pages/login'
import {SignupPage} from './app/pages/signup'
import {About} from './app/pages/about'

import {assign} from 'lodash'
import {Users} from './app/model/user'
import {Alerts} from './app/model/alert'
import {pageview} from './app/model/analytics'

import {background} from './app/style'
import {start, query, Routes, transitionTo} from './app/router'

class App extends React.Component {

  constructor(props) {
    super(props)
  }

  componentDidMount() {

  }

  render() {

    // don't show them the admin if not logged in
    if (this.props.pathname && this.props.pathname.match("admin") && (!this.props.currentUser || !this.props.currentUser.admin)) {
      return <div><NotFound /></div>
    }

    return <div>
      <RouteHandler {...this.props} />
    </div>
  }
}

// root should redirect to user's gallery if logged in


var routes = (
  <Route name={Routes.root} handler={App} path="/">

    <Redirect from="/" to={Routes.bookshelf} params={{id: Users.currentUserId()}}/>

    <Route name={Routes.login} handler={Login}/>
    <Route name={Routes.signup} path="signup/:code" handler={SignupPage}/>

    <Route path="books" handler={Main}>
      <DefaultRoute name={Routes.library} handler={Library}/>
      <Route name={Routes.book} path=":id" handler={Book} />
    </Route>

    <Route path="pages" handler={Main}>
      <Route name={Routes.about} path="about" handler={About} />
    </Route>

    <Route path="admin" handler={Main}>
      <DefaultRoute name={Routes.admin} handler={AdminDashboard}/>
      <Route name={Routes.sources} handler={Sources}/>
      <Route name={Routes.invites} handler={Invites}/>
      <Route name={Routes.users}   handler={AdminUsers}/>
      <Route name={Routes.source}  path="sources/:id" handler={Source}/>
      <Route path="import-log/:n" handler={ImportLog}/>
    </Route>

    <Route path="users" handler={Main}>
      <Route name={Routes.bookshelf} path=":id/bookshelf" handler={Bookshelf}/>
    </Route>

    <NotFoundRoute handler={NotFound} />
  </Route>
)


var lastHandler:any
var lastState:any
var lastData:any

start(routes, function(Handler, state, data) {
  React.render(
    <Handler
      {...data}
      query={query()}
      currentUser={Users.currentUser}
      alert={Alerts.alert}
      params={state.params}
      pathname={state.pathname}
    />, document.body)
}, onUrlChange)

function onUrlChange(Handler, state) {
  Alerts.urlChange()
  pageview(state.path)
}

Users.bind(() => render())
Alerts.bind(() => render())

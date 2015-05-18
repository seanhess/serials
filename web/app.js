// @flow


// Import react and support deubgger
import React from 'react'
window.React = React

import Router from 'react-router'
import {Route, DefaultRoute, RouteHandler, NotFoundRoute, Redirect, Link} from 'react-router'

import {loadAll, run, render} from './app/data/load'

import {Admin, ImportLog, AdminDashboard} from './app/admin/admin'
import {Sources} from './app/admin/sources'
import {Source} from './app/admin/source'
import {Invites} from './app/admin/invites'

import {Main} from './app/books/main'
import {MainContainer, Header} from './app/layout/main'
import {NotFound} from './app/layout/notFound'
import {Gallery} from './app/books/gallery'
import {Library} from './app/books/library'
import {Book} from './app/books/book'
import {Read} from './app/books/read'
import {About} from './app/pages/about'
import {Login} from './app/pages/login'
import {Signup} from './app/pages/signup'

import {assign} from 'lodash'
import {Users} from './app/model/user'

import {updateLocalStorage} from './app/helpers'

import {background} from './app/style'

declare var SETTINGS;
console.log("SETTINGS", SETTINGS)

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

var routes = (
  <Route handler={App} path="/">
    <Redirect from="/" to="books" />

    <Route name="pages" handler={Main}>
      <Route name="about" handler={About}/>
    </Route>

    <Route name='login' handler={Login}/>
    <Route name='signup' path="signup/:beta" handler={Signup}/>

    <Route name="books" path="books" handler={Main}>
      <DefaultRoute handler={Gallery}/>
      <Route name="book" path=":id" handler={Book} />
    </Route>

    <Route name="chapters" path="chapters">
      <Route name="chapter" path=":id" handler={Read} />
    </Route>

    <Route name="admin" handler={Admin}>
      <DefaultRoute handler={AdminDashboard}/>
      <Route name="sources" handler={Sources}/>
      <Route name="invites" handler={Invites}/>
      <Route name="source"  path="sources/:id" handler={Source}/>
      <Route name="import-log" path="import-log/:n" handler={ImportLog}/>
    </Route>
    <Route name="user" handler={Main}>
      <Route name="library" path=":id/library" handler={Library}/>
    </Route>

    <NotFoundRoute handler={NotFound} />
  </Route>
)


var lastHandler:any
var lastState:any
var lastData:any

Router.run(routes, run(function(Handler, state, data) {
  React.render(
    <Handler 
      {...data} 
      currentUser={Users.currentUser}
      params={state.params} 
      pathname={state.pathname}
    />, document.body)
}))

//function checkLoginRedirect() {
  //return Users.auth().then(function(user) {
    //if (!user && lastState.path !== "/login") {
      //console.log("CAUGHT YOU!")
      //window.location.hash = "/login"
      //return false
    //}

    //return true
  //})
//}

Users.bind(() => render())

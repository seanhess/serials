// @flow


// Import react and support deubgger
import React from 'react'
window.React = React

import Router from 'react-router'
import {Route, DefaultRoute, RouteHandler, NotFoundRoute, Redirect, Link} from 'react-router'

import {loadAll} from './app/data/load'

import {Admin, ImportLog} from './app/admin/admin'
import {Sources} from './app/admin/sources'
import {Source} from './app/admin/source'
import {Main} from './app/books/main'
import {MainContainer, Header} from './app/layout/main'
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

class App extends React.Component {

  constructor(props) {
    super(props)
  }

  componentDidMount() {
    // if it hasn't kicked off a check auth yet, do it now
    Users.auth()
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

class NotFound extends React.Component {
  render() {
    return <div>
      <Header />
      <MainContainer>
        <div><Link to="books">Home</Link></div>
      </MainContainer>
    </div>
  }
}

var routes = (
  <Route handler={App} path="/">
    <Redirect from="/" to="books" />
    <Route name="pages" handler={Main}>
      <Route name='login' handler={Login}/>
      <Route name='signup' handler={Signup}/>
      <Route name="about" handler={About}/>
    </Route>

    <Route name="books" path="books" handler={Main}>
      <DefaultRoute handler={Gallery}/>
      <Route name="book" path=":id" handler={Book} />
    </Route>

    <Route name="chapters" path="chapters">
      <Route name="chapter" path=":id" handler={Read} />
    </Route>

    <Route name="admin" handler={Admin}>
      <Route name="sources" handler={Sources}/>
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

Router.run(routes, function(Handler, state) {
  lastHandler = Handler
  lastState = state
  lastData = {}

  // render once without any data
  render()

  // render again every time any of the promises resolve
  loadAll(state.routes, state.params, render)
})

function render(data = lastData) {
  lastData = data
  var Handler = lastHandler
  var state = lastState

  React.render(
    <Handler 
      {...data} 
      currentUser={Users.currentUser}
      params={state.params} 
      pathname={state.pathname}
    />, document.body)
}

Users.bind(() => render())

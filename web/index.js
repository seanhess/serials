// @flow


// Import react and support deubgger
import React from 'react'
window.React = React

import Router from 'react-router'
import {Route, DefaultRoute, RouteHandler, NotFoundRoute, Redirect} from 'react-router'

import {loadAll} from './app/data/load'

import {Admin, ImportLog} from './app/admin/admin'
import {Sources} from './app/admin/sources'
import {Source} from './app/admin/source'
import {Main} from './app/books/main'
import {Gallery} from './app/books/gallery'
import {Book} from './app/books/book'
import {About} from './app/pages/about'
import {Login} from './app/pages/login'
import {Signup} from './app/pages/signup'

import {assign} from 'lodash'
import {UserModel} from './app/model/user'
import {updateLocalStorage} from './app/helpers'

class App extends React.Component {

  constructor(props) {
    super(props)
    this.state = {currentUser: this.props.currentUser}
  }

  componentDidMount() {
    UserModel.checkAuth()
    .then((user) => this.setState({currentUser: user}))
  }

  setCurrentUser(user) {
    this.setState({currentUser: user})
  }

  logout(e) {
    e.preventDefault()
    UserModel.logout()
    .then(() => this.setState({currentUser: null}))
  }

  render() {
    return <RouteHandler {...this.props} currentUser={this.state.currentUser} logout={this.logout.bind(this)} setCurrentUser={this.setCurrentUser.bind(this)}/>
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
    <Route name="pages" handler={Main}>
      <Route name='login' handler={Login}/>
      <Route name='signup' handler={Signup}/>
      <Route name="about" handler={About}/>
    </Route>
    <Route name="books" handler={Main}>
      <DefaultRoute handler={Gallery}/>
      <Route name="book" path=":id" handler={Book} />
    </Route>
    <Route name="admin" handler={Admin}>
      <Route name="sources" handler={Sources}/>
      <Route name="source"  path="sources/:id" handler={Source}/>
      <Route name="import-log" path="import-log/:n" handler={ImportLog}/>
    </Route>
  </Route>
)

Router.run(routes, function (Handler, state) {
  function render(data) {
    React.render(<Handler {...data} params={state.params}/>, document.body)
  }

  // render once without any data
  render({})

  // render again every time any of the promises resolve
  loadAll(state.routes, state.params, render)
})

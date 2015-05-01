// @flow

var React = window.React = require('react')
var Router = require('react-router')
var {Route, DefaultRoute, RouteHandler, NotFoundRoute, Redirect} = require('react-router')

var {Admin, ImportLog} = require('./app/admin/admin')
var {Sources} = require('./app/admin/sources')
var {Source} = require('./app/admin/source')
var {Main} = require('./app/books/main')
var {Gallery} = require('./app/books/gallery')
var {Book} = require('./app/books/book')
var {About} = require('./app/pages/about')
var {Login} = require('./app/pages/login')
var {Signup} = require('./app/pages/signup')

import {assign} from 'lodash'
import {UserModel} from './app/model'
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
    updateLocalStorage('userToken', user.token)
    this.setState({currentUser: user})
  }

  logout(e) {
    e.preventDefault()
    updateLocalStorage('userToken', null)
    this.setState({currentUser: null})
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
  React.render(<Handler params={state.params}/>, document.body)

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

      // ok, they're allowed to do more than one, right?
      var promises = route.handler.load(params)

      var names = Object.keys(promises)

      return Promise.all(names.map(function(name) {
        var promise = promises[name]
        if (!promise.then) {
          return promise
        }

        return promises[name].then(function(d) {
          data[name] = d
        }, onError)
      }))
    })
  ).then(() => data, onError);
}




function onError(err) {
  throw err
}

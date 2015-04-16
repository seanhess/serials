
// @flow

var React = require('react')

import {Sources} from './sources.js'
import {Source} from './source.js'

import {RouteHandler} from 'react-router'

export class Admin extends React.Component {
  render() {
    return <div>
      <nav className="top-bar" data-topbar role="navigation">
        <ul className="title-area">
          <li className="name">
            <h1><a href="#">Serials</a></h1>
          </li>
        </ul>

      </nav>

      <div className="row columns small-12">
        <RouteHandler />
      </div>
    </div>
  }
}

export var AdminSources = Sources
export var AdminSource = Source

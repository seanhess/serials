// @flow

import React from 'react'
import {find, filter, assign} from 'lodash'
import {clickable, Colors} from '../style'

// set the following:
// main content
// menu items
// main header / title

export var MenuHeaderStyle = {
  background: Colors.darkTone,
  color: Colors.offWhite
}

export var MenuLinkStyle = {
  color: Colors.light
}

export class MenuHeader extends React.Component {
  render():React.Element {
    return <label style={MenuHeaderStyle}>
      {this.props.children}
    </label>
  }
}

// close it when they change urls...
export class OffCanvas extends React.Component {

  constructor() {
    this.state = {move: ''}
  }

  onClickBody() {
    this.setState({move: ''})
  }

  onClickLeft() {
    this.setState({move: 'move-right'})
  }

  onClickRight() {
    this.setState({move: 'move-left'})
  }

  clickAnywhere() {
    if (this.state.move !== '') {
      this.setState({move: ''})
    }
  }

  render():React.Element {

    var classes = "off-canvas-wrap " + this.state.move

    var title     = find(this.props.children, child => child.key === 'title')

    var menu  = find(this.props.children, child => child.key === 'menu')

    var content = filter(this.props.children, function(child) {
      return !child.key
    })

    return <div className={classes} data-offcanvas onClick={this.clickAnywhere.bind(this)}>
      <div className="inner-wrap">
        <nav className="tab-bar" style={{backgroundColor: Colors.dark}}>

          <section className="tab-bar-section">
            {title}
          </section>

          <section className="right-small">
            <a className="menu-icon" style={assign({color: Colors.light}, clickable)} onClick={this.onClickRight.bind(this)}>
              <span></span>
            </a>
          </section>
        </nav>

        <aside className="right-off-canvas-menu" style={{backgroundColor: Colors.dark}}>
          {menu}
        </aside>

        <section className="main-section">
          {content}
        </section>

      <a className="exit-off-canvas" onClick={this.onClickBody.bind(this)}>
      </a>
      </div>
    </div>
  }
}



